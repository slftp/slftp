import json
import os
import subprocess
import sys
import xml.etree.ElementTree as ET
import tempfile
from pathlib import Path
import unittest
import ftplib
from unittest.mock import patch

from report import compare, metrics, percentile, validate
from workload import transfer_metrics, RetryingFTPS, publish_times, prepare, SIZE, Workload


class Measurements(unittest.TestCase):
    def valid(self):
        return {"scenario": "timing", "elapsed_s": 2.0, "cpu_s": 0.75, "files_full": 6, "files_expected": 6,
                "bytes_expected": 6 * 1024**2, "peak_rss_bytes": 10 * 1024**2,
                "producer_elapsed_s": 1.0, "max_sample_gap_ms": 51,
                "start_latencies_ms": [0, 10, 20, 30, 40, 100],
                "release_completion_ms": [100, 200, 300], "error": None, "hash_errors": [],
                "transfers": [
                    {"site": site, "release": "release", "file": str(file),
                     "source_ready_s": 1.0, "destination_seen_s": 1.1,
                     "destination_full_s": 1.5}
                    for site in (2, 3, 4) for file in (1, 2)]}

    def test_incomplete_or_corrupt_work_is_not_faster(self):
        result = self.valid()
        self.assertIsNone(validate(result))
        result["files_full"] = 3
        self.assertEqual(validate(result), "incomplete payload")
        result["files_full"] = 6
        result["hash_errors"] = ["one file"]
        self.assertIn("payload hash mismatch", validate(result))

    def test_cpu_units_and_throughput(self):
        value = metrics(self.valid())
        self.assertEqual(value["cpu_ms_per_file"], 125)
        self.assertEqual(value["files_per_s"], 3)
        self.assertEqual(value["mib_per_s"], 3)
        self.assertEqual(value["start_latency_p95_ms"], 100)

    def test_percentile_uses_tail(self):
        self.assertEqual(percentile(range(1, 101), 0.95), 95)
        with self.assertRaises(ValueError):
            percentile([], 0.95)

    def test_source_to_destination_is_not_release_age(self):
        release = [{"name": "release", "files": {"file.rar": "digest"}}]
        starts = {(site, "release", "file.rar"): 12.0 for site in (2, 3, 4)}
        ends = {(site, "release", "file.rar"): 13.0 for site in (1, 2, 3, 4)}
        ends[(1, "release", "file.rar")] = 11.0
        latency, completion, _ = transfer_metrics(release, {"release": 10.0}, starts, ends)
        self.assertEqual(latency, [1000, 1000, 1000])
        self.assertEqual(completion, [3000, 3000, 3000])

    def test_missing_trial_invalidates_comparison(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.assertEqual(compare(root, "dev-sha", "branch-sha"), 1)
            self.assertIn("INVALID", (root / "summary.md").read_text())

    def test_uploader_retries_only_temporary_pasv_failure(self):
        with patch.object(ftplib.FTP_TLS, "makepasv",
                          side_effect=[ftplib.error_temp("425 Busy"), ("127.0.0.1", 42)]), \
                patch("workload.time.sleep"):
            ftp = RetryingFTPS()
            self.assertEqual(ftp.makepasv(), ("127.0.0.1", 42))
            self.assertEqual(ftp.pasv_retries, 1)

    def test_uploader_does_not_retry_other_errors(self):
        with patch.object(ftplib.FTP_TLS, "makepasv", side_effect=ftplib.error_temp("421 Closed")):
            with self.assertRaises(ftplib.error_temp):
                RetryingFTPS().makepasv()

    def test_successful_four_trial_report_and_ranges(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for number, variant in enumerate(("baseline", "branch", "branch", "baseline"), 1):
                folder = root / f"{number}-{variant}"
                folder.mkdir()
                (folder / "result.json").write_text(json.dumps(self.valid()))
            self.assertEqual(compare(root, "dev", "branch"), 0)
            report = json.loads((root / "comparison.json").read_text())
            self.assertEqual(report["ranges"]["baseline"]["elapsed_s"], [2, 2])
            self.assertAlmostEqual(report["per_site_start_p95_ms"]["2"]["branch"], 100)
            suite = ET.parse(root / "junit.xml").getroot()
            self.assertEqual(suite.get("failures"), "0")
            self.assertEqual(suite.get("skipped"), "0")

    def test_broken_json_still_writes_reports_and_skips_unrun_trials(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "1-baseline").mkdir()
            (root / "1-baseline/result.json").write_text('{"elapsed_s":')
            self.assertEqual(compare(root, "dev", "branch"), 1)
            self.assertIn("INVALID", (root / "summary.md").read_text())
            suite = ET.parse(root / "junit.xml").getroot()
            self.assertEqual(suite.get("failures"), "1")
            self.assertEqual(suite.get("skipped"), "3")

    def test_missing_wrong_type_and_nonfinite_fields_are_rejected(self):
        for field in ("peak_rss_bytes", "producer_elapsed_s", "max_sample_gap_ms",
                      "start_latencies_ms", "release_completion_ms", "transfers", "hash_errors"):
            with self.subTest(field=field):
                result = self.valid()
                del result[field]
                self.assertIsNotNone(validate(result))
        for value in ("1", True, float("nan"), float("inf"), -1, 10**1000):
            result = self.valid()
            result["cpu_s"] = value
            self.assertIsNotNone(validate(result))
        for value in (None, [], "invalid"):
            self.assertIsNotNone(validate(value))
        result = self.valid()
        result["start_latencies_ms"][0] = float("nan")
        self.assertIsNotNone(validate(result))

    def test_duplicate_transfer_rejected(self):
        result = self.valid()
        result["transfers"][1] = result["transfers"][0]
        self.assertEqual(validate(result), "duplicate transfer")

    def test_file_rewrite_truncation_and_deletion_reset_timestamps(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            folder = root / "2/RACE/release"
            folder.mkdir(parents=True)
            path = folder / "file.rar"
            releases = [{"name": "release", "files": {"file.rar": "unused"}}]
            starts, finishes, directories, states = {}, {}, {}, {}
            key = (2, "release", "file.rar")

            def sample(now):
                publish_times(root, releases, now, starts, finishes, directories, states)

            path.write_bytes(b"a" * SIZE)
            sample(1)
            sample(2)
            self.assertEqual(finishes[key], 1)
            old_mtime = path.stat().st_mtime_ns
            path.write_bytes(b"b" * SIZE)
            os.utime(path, ns=(old_mtime + 1000000, old_mtime + 1000000))
            sample(3)
            self.assertEqual((starts[key], finishes[key]), (3, 3))
            path.write_bytes(b"short")
            sample(4)
            self.assertNotIn(key, finishes)
            path.write_bytes(b"a" * SIZE)
            sample(5)
            self.assertEqual((starts[key], finishes[key]), (4, 5))
            path.unlink()
            sample(6)
            self.assertNotIn(key, finishes)
            self.assertNotIn(key, starts)
            path.write_bytes(b"a" * SIZE)
            sample(7)
            self.assertEqual((starts[key], finishes[key]), (7, 7))

    def test_blocked_upload_worker_does_not_hold_process_open(self):
        code = ("import threading; from workload import background_call; "
                "f = background_call(threading.Event().wait); assert not f.done()")
        subprocess.run([sys.executable, "-c", code], cwd=Path(__file__).parent,
                       check=True, timeout=5)

    def test_timeout_returns_with_blocked_upload_and_skips_hashing(self):
        import threading
        from types import SimpleNamespace
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "manifest.json").write_text(json.dumps([
                {"name": "release", "files": {"file.rar": "unused"}}]))
            (root / "go").touch()
            args = SimpleNamespace(payload=root, sites=root, out=root, ports="1,2,3,4",
                                   pid=0, scenario="timing")
            app = Workload(args)
            app.joined.set()
            blocked = threading.Event()
            try:
                with patch.object(app, "serve"), patch.object(app, "verify_recovery"), \
                        patch.object(app, "upload", side_effect=lambda *args: blocked.wait()), \
                        patch.object(app, "verify") as verify, \
                        patch("workload.TIMEOUT", 0.02), \
                        patch("workload.cpu_sample", return_value={"rss_bytes": 0, "cpu_s": 0}):
                    self.assertEqual(app.run(), 1)
                    verify.assert_not_called()
                    self.assertTrue(app.stop.is_set())
                    self.assertIn("timeout", json.loads((root / "result.json").read_text())["error"])
            finally:
                blocked.set()

    def test_progressive_cd1_reproducer_keeps_all_volumes(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            prepare(root, "progressive-cd1")
            releases = json.loads((root / "manifest.json").read_text())
            self.assertEqual(sum(len(release["files"]) for release in releases), 36)
            self.assertTrue(all(len(release["files"]) == 6 for release in releases if release["sub"]))

    def test_verification_rejects_mutation_after_measurement(self):
        import hashlib
        from types import SimpleNamespace
        from workload import file_state
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            payload = b"a" * SIZE
            releases = [{"name": "release", "files": {"file.rar": hashlib.sha256(payload).hexdigest()}}]
            states = {}
            for site in range(1, 5):
                path = root / str(site) / "RACE/release/file.rar"
                path.parent.mkdir(parents=True)
                path.write_bytes(payload)
                states[(site, "release", "file.rar")] = file_state(path)
            app = Workload.__new__(Workload)
            app.args = SimpleNamespace(sites=root)
            app.releases = releases
            self.assertEqual(app.verify(states), [])
            path = root / "2/RACE/release/file.rar"
            stamp = path.stat().st_mtime_ns + 1000000
            os.utime(path, ns=(stamp, stamp))
            self.assertEqual(app.verify(states), ["E2E2/release/file.rar"])


if __name__ == "__main__":
    unittest.main()
