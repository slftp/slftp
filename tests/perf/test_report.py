import tempfile
from pathlib import Path
import unittest
import ftplib
from unittest.mock import patch

from report import compare, metrics, percentile, validate
from workload import transfer_metrics, RetryingFTPS


class Measurements(unittest.TestCase):
    def valid(self):
        return {"elapsed_s": 2.0, "cpu_s": 0.5, "files_full": 4, "files_expected": 4,
                "bytes_expected": 4 * 1024**2, "peak_rss_bytes": 10 * 1024**2,
                "start_latencies_ms": [0, 10, 20, 100],
                "release_completion_ms": [100, 200], "error": None, "hash_errors": []}

    def test_incomplete_or_corrupt_work_is_not_faster(self):
        result = self.valid()
        self.assertIsNone(validate(result))
        result["files_full"] = 3
        self.assertEqual(validate(result), "incomplete payload")
        result["files_full"] = 4
        result["hash_errors"] = ["one file"]
        self.assertEqual(validate(result), "payload hash mismatch")

    def test_cpu_units_and_throughput(self):
        value = metrics(self.valid())
        self.assertEqual(value["cpu_ms_per_file"], 125)
        self.assertEqual(value["files_per_s"], 2)
        self.assertEqual(value["mib_per_s"], 2)
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


if __name__ == "__main__":
    unittest.main()
