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


def validate(result):
    if result.get("error"):
        return str(result["error"])
    if result.get("hash_errors"):
        return "payload hash mismatch"
    if result.get("files_full") != result.get("files_expected"):
        return "incomplete payload"
    for key in ("elapsed_s", "cpu_s", "bytes_expected", "files_expected"):
        if key not in result or not math.isfinite(result[key]) or result[key] < 0:
            return f"missing/invalid {key}"
    if not result["elapsed_s"] or not result["files_expected"]:
        return "empty workload"
    return None


def metrics(result):
    return {
        "elapsed_s": result["elapsed_s"],
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
    for number, variant in enumerate(("baseline", "branch", "branch", "baseline"), 1):
        path = root / f"{number}-{variant}" / "result.json"
        if not path.exists():
            errors.append(f"{number}-{variant}: no result")
            continue
        result = json.loads(path.read_text())
        error = validate(result)
        trials.append({"trial": number, "variant": variant, "result": result})
        if error:
            errors.append(f"{number}-{variant}: {error}")
    lines = ["# slFtp race performance comparison", "",
             f"- Branch: {branch_sha}", f"- Baseline (dev pinned at job start): {baseline_sha}",
             "- Trial order: dev / branch / branch / dev; fresh runtime and sites each time.",
             "- Same deterministic payload, configuration, FPC toolchain and TLS servers.", ""]
    summary = {"branch_sha": branch_sha, "baseline_sha": baseline_sha,
               "trials": trials, "errors": errors}
    if errors:
        lines += ["**INVALID comparison: correctness or environment failure.**", ""]
        lines += [f"- {error}" for error in errors]
    else:
        groups = {name: [metrics(t["result"]) for t in trials if t["variant"] == name]
                  for name in ("baseline", "branch")}
        medians = {name: {key: statistics.median(row[key] for row in rows)
                          for key in rows[0]} for name, rows in groups.items()}
        summary["medians"] = medians
        lines += ["| Metric | dev median | branch median | Change |",
                  "|---|---:|---:|---:|"]
        for key in medians["baseline"]:
            old, new = medians["baseline"][key], medians["branch"][key]
            delta = (new / old - 1) * 100 if old else None
            text = f"{delta:+.1f}%" if delta is not None else "n/a"
            lines.append(f"| {key} | {old:.2f} | {new:.2f} | {text} |")
        lines += ["", "Performance deltas are observations, not an automatic pass/fail threshold.",
                  "Higher files/s and MiB/s are better; lower time, CPU/file and memory are better."]
    lines += ["", "## Interpretation", "",
              "- Source-ready to first destination appearance includes detection, scheduling and transfer",
              "  setup. It is not an internal queue-wait measurement.",
              "- Filesystem sampling targets 50 ms; actual maximum gaps are retained in each result.",
              "- CPU is slFtp process user+system CPU, excluding servers, uploader and observer.",
              "- Payload hashes are verified after timing. SFV and NFO transfers are not counted as payload.",
              "- Completion means all expected payload bytes observed, not slFtp's STATS completion event.",
              "- Site E2E3 has one slot and the highest route rank; E2E4 has a 25-ms delay per",
              "  server-to-client control chunk and a six-second control outage. TLS stays end-to-end.",
              "- The six-second control outage and DOWN -> UP verification occur before timing.",
              "- Half the releases use single-volume CD1; some E2E2 directories already exist.",
              "- This measures loopback scheduling/CPU under a fixed arrival schedule, not WAN bandwidth.",
              "- Two samples per variant expose large regressions; repeat before attributing small deltas.",
              "- Shared CI hosts add noise. Inspect individual trials and run.log before drawing conclusions.",
              "- Internal task creation, blocked reasons, queue wakes/sorts and lock waits are not inferred",
              "  from these external timings. Use a separate instrumented run for those causes.", ""]
    (root / "summary.md").write_text("\n".join(lines))
    (root / "comparison.json").write_text(json.dumps(summary, indent=2))
    suite = ET.Element("testsuite", name="race-performance", tests="4")
    for number, variant in enumerate(("baseline", "branch", "branch", "baseline"), 1):
        case = ET.SubElement(suite, "testcase", classname="race-performance",
                             name=f"{number}-{variant}")
        prefix = f"{number}-{variant}:"
        for error in errors:
            if error.startswith(prefix):
                ET.SubElement(case, "failure", message=error).text = error
    suite.set("failures", str(len(errors)))
    ET.ElementTree(suite).write(root / "junit.xml", encoding="utf-8", xml_declaration=True)
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(compare(Path(sys.argv[1]), sys.argv[2], sys.argv[3]))
