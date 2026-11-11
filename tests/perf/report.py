#!/usr/bin/env python3
"""Aggregate paired trials; incomplete/corrupt output must never look like a speedup."""
import json
import math
from pathlib import Path
import statistics
import sys
import xml.etree.ElementTree as ET


def percentile(values, fraction):
    values = sorted(values)
    if not values:
        raise ValueError("empty measurement")
    return values[max(0, math.ceil(len(values) * fraction) - 1)]


def nonnegative(value):
    try:
        return (type(value) in (int, float) and math.isfinite(value) and value >= 0)
    except OverflowError:
        return False


def validate(result):
    if not isinstance(result, dict):
        return "result must be an object"
    if result.get("error"):
        return str(result["error"])
    if result.get("scenario") not in ("timing", "progressive-cd1", "active-recovery"):
        return "missing/invalid scenario"
    if result.get("hash_errors") != []:
        return "missing hash verification or payload hash mismatch"
    for key in ("elapsed_s", "cpu_s", "bytes_expected", "files_expected", "files_full",
                "peak_rss_bytes", "producer_elapsed_s", "max_sample_gap_ms"):
        if not nonnegative(result.get(key)):
            return f"missing/invalid {key}"
    if result["files_full"] != result["files_expected"]:
        return "incomplete payload"
    if not result["elapsed_s"] or not result["files_expected"] or not result["bytes_expected"]:
        return "empty workload"
    if any(type(result[key]) is not int for key in ("files_full", "files_expected", "bytes_expected")):
        return "non-integer payload count"
    for key in ("start_latencies_ms", "release_completion_ms"):
        values = result.get(key)
        if not isinstance(values, list) or not values or not all(nonnegative(v) for v in values):
            return f"missing/invalid {key}"
    if len(result["start_latencies_ms"]) != result["files_expected"]:
        return "latency count does not match payload"
    if result["producer_elapsed_s"] > result["elapsed_s"]:
        return "producer finished outside measured interval"
    transfers = result.get("transfers")
    if not isinstance(transfers, list) or len(transfers) != result["files_expected"]:
        return "missing/incomplete transfers"
    keys = set()
    for transfer in transfers:
        if not isinstance(transfer, dict):
            return "invalid transfer"
        if (type(transfer.get("site")) is not int or transfer["site"] not in (2, 3, 4) or
                not isinstance(transfer.get("release"), str) or not transfer["release"] or
                not isinstance(transfer.get("file"), str) or not transfer["file"]):
            return "invalid transfer identity"
        for field in ("source_ready_s", "destination_seen_s", "destination_full_s"):
            if not nonnegative(transfer.get(field)):
                return f"invalid transfer {field}"
        if transfer["destination_full_s"] < transfer["destination_seen_s"]:
            return "transfer completes before it appears"
        key = (transfer["site"], transfer["release"], transfer["file"])
        if key in keys:
            return "duplicate transfer"
        keys.add(key)
    if {item["site"] for item in transfers} != {2, 3, 4}:
        return "missing destination site"
    if len(result["release_completion_ms"]) != len({(t["site"], t["release"]) for t in transfers}):
        return "completion count does not match releases"
    return None


def metrics(result):
    return {
        "elapsed_s": result["elapsed_s"],
        "producer_elapsed_s": result["producer_elapsed_s"],
        "drain_after_producer_ms": (result["elapsed_s"] - result["producer_elapsed_s"]) * 1000,
        "max_sample_gap_ms": result["max_sample_gap_ms"],
        "files_per_s": result["files_full"] / result["elapsed_s"],
        "mib_per_s": result["bytes_expected"] / result["elapsed_s"] / 1024**2,
        "cpu_ms_per_file": result["cpu_s"] * 1000 / result["files_full"],
        "start_latency_p95_ms": percentile(result["start_latencies_ms"], 0.95),
        "payload_completion_p95_ms": percentile(result["release_completion_ms"], 0.95),
        "peak_rss_mib": result["peak_rss_bytes"] / 1024**2,
    }


def compare(root, baseline_sha, branch_sha):
    trials = []
    errors = []
    skipped = []
    for number, variant in enumerate(("baseline", "branch", "branch", "baseline"), 1):
        path = root / f"{number}-{variant}" / "result.json"
        if not path.exists():
            skipped.append(f"{number}-{variant}: not run / no result")
            continue
        try:
            result = json.loads(path.read_text())
        except (OSError, UnicodeError, ValueError) as error:
            result = {"error": f"cannot read result: {error}"}
        error = validate(result)
        trials.append({"trial": number, "variant": variant, "result": result})
        if error:
            errors.append(f"{number}-{variant}: {error}")
    scenarios = {t["result"].get("scenario") for t in trials
                 if isinstance(t["result"], dict) and isinstance(t["result"].get("scenario"), str)}
    if len(scenarios) > 1:
        for trial in trials:
            errors.append(f"{trial['trial']}-{trial['variant']}: mixed scenarios")
    lines = ["# slFtp race performance comparison", "",
             f"- Branch: {branch_sha}", f"- Baseline (resolved commit): {baseline_sha}",
             "- Trial order: dev / branch / branch / dev; fresh runtime and sites each time.",
             "- Same deterministic payload, configuration, FPC toolchain and TLS servers.", ""]
    summary = {"branch_sha": branch_sha, "baseline_sha": baseline_sha,
               "trials": trials, "errors": errors, "skipped": skipped,
               "scenarios": sorted(scenarios)}
    if errors or skipped:
        lines += ["**INVALID comparison: correctness or environment failure.**", ""]
        lines += [f"- {error}" for error in errors + skipped]
    else:
        groups = {name: [metrics(t["result"]) for t in trials if t["variant"] == name]
                  for name in ("baseline", "branch")}
        medians = {name: {key: statistics.median(row[key] for row in rows)
                          for key in rows[0]} for name, rows in groups.items()}
        summary["medians"] = medians
        summary["ranges"] = {
            name: {key: [min(row[key] for row in rows), max(row[key] for row in rows)]
                   for key in rows[0]} for name, rows in groups.items()}
        lines += ["| Metric | dev median [min, max] | branch median [min, max] | Change |",
                  "|---|---:|---:|---:|"]
        for key in medians["baseline"]:
            old, new = medians["baseline"][key], medians["branch"][key]
            delta = (new / old - 1) * 100 if old else None
            text = f"{delta:+.1f}%" if delta is not None else "n/a"
            ranges = summary["ranges"]
            old_range, new_range = ranges["baseline"][key], ranges["branch"][key]
            lines.append(f"| {key} | {old:.2f} [{old_range[0]:.2f}, {old_range[1]:.2f}] | "
                         f"{new:.2f} [{new_range[0]:.2f}, {new_range[1]:.2f}] | {text} |")
        per_site = {}
        lines += ["", "## Per-site source-ready to destination-appearance p95 (ms)", "",
                  "| Site | dev median | branch median |", "|---|---:|---:|"]
        for site in (2, 3, 4):
            per_site[site] = {}
            for variant in ("baseline", "branch"):
                values = [percentile([
                    max(0, item["destination_seen_s"] - item["source_ready_s"]) * 1000
                    for item in trial["result"]["transfers"] if item["site"] == site], 0.95)
                    for trial in trials if trial["variant"] == variant]
                per_site[site][variant] = statistics.median(values)
            lines.append(f"| E2E{site} | {per_site[site]['baseline']:.2f} | "
                         f"{per_site[site]['branch']:.2f} |")
        summary["per_site_start_p95_ms"] = per_site
        lines += ["", "Performance deltas are observations, not an automatic pass/fail threshold.",
                  "Higher files/s and MiB/s are better; lower time, CPU/file and memory are better."]
    lines += ["", "## Interpretation", "",
              "- Source-ready to first destination appearance includes detection, scheduling and transfer",
              "  setup. It is not an internal queue-wait measurement.",
              "- Filesystem sampling targets 50 ms; actual maximum gaps are retained in each result.",
              "- CPU is slFtp process user+system CPU, excluding servers, uploader and observer.",
              "- Payload hashes are verified after timing. SFV and NFO transfers are not counted as payload.",
              "- Completion requires unchanged full payloads for 500 ms plus finished producers.",
              "- CPU and elapsed time cover that same interval, including the stability window.",
              "- This does not confirm slFtp task teardown or the STATS event. Rewrites between samples",
              "  can be missed; observed replacement/truncation invalidates the old timestamps.",
              "- Site E2E3 has one slot and the highest route rank; E2E4 has a 25-ms delay per",
              "  server-to-client control chunk and a six-second control outage. TLS stays end-to-end.",
              "- All scenarios verify DOWN -> UP before timing; active-recovery repeats it during work.",
              "- Half the releases use CD1 (six volumes in progressive-cd1, one otherwise).",
              "- Some E2E2 directories already exist.",
              "- This measures loopback scheduling/CPU under a paced producer schedule, not WAN bandwidth.",
              "- Two samples per variant expose large regressions; repeat before attributing small deltas.",
              "- Shared CI hosts add noise. Inspect individual trials and run.log before drawing conclusions.",
              "- Internal task creation, blocked reasons, queue wakes/sorts and lock waits are not inferred",
              "  from these external timings. Use a separate instrumented run for those causes.", ""]
    (root / "summary.md").write_text("\n".join(lines))
    (root / "comparison.json").write_text(json.dumps(summary, indent=2))
    suite = ET.Element("testsuite", name="race-performance", tests="4")
    failures = 0
    for number, variant in enumerate(("baseline", "branch", "branch", "baseline"), 1):
        case = ET.SubElement(suite, "testcase", classname="race-performance",
                             name=f"{number}-{variant}")
        prefix = f"{number}-{variant}:"
        messages = [error for error in errors if error.startswith(prefix)]
        if messages:
            failures += 1
            message = "; ".join(messages)
            ET.SubElement(case, "failure", message=message).text = message
        for reason in skipped:
            if reason.startswith(prefix):
                ET.SubElement(case, "skipped", message=reason)
    suite.set("failures", str(failures))
    suite.set("skipped", str(len(skipped)))
    ET.ElementTree(suite).write(root / "junit.xml", encoding="utf-8", xml_declaration=True)
    return 1 if errors or skipped else 0


if __name__ == "__main__":
    raise SystemExit(compare(Path(sys.argv[1]), sys.argv[2], sys.argv[3]))
