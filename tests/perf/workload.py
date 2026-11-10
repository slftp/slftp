#!/usr/bin/env python3
"""Deterministic four-site load; measures slFtp externally without changing its binary."""
import argparse
import concurrent.futures
import datetime
import ftplib
import hashlib
import json
import os
from pathlib import Path
import random
import select
import socket
import socketserver
import ssl
import threading
import time
import zlib

RELEASES = 6
VOLUMES = 6
SIZE = 512 * 1024
TIMEOUT = 150


def prepare(root):
    rng = random.Random(845)
    releases = []
    for i in range(RELEASES):
        name = f"Perf.Load.Volume{i:02d}.2026.1080p.WEB.h264-E2EGRP"
        stem = f"perf{i:02d}"
        sub = "CD1" if i % 2 else ""
        files = {}
        folder = root / name / sub
        folder.mkdir(parents=True, exist_ok=True)
        for j in range(VOLUMES):
            filename = f"{stem}.rar" if j == 0 else f"{stem}.r{j - 1:02d}"
            data = rng.randbytes(SIZE)
            (folder / filename).write_bytes(data)
            files[str(Path(sub) / filename)] = hashlib.sha256(data).hexdigest()
        sfv = "".join(f"{Path(p).name} {zlib.crc32((root / name / p).read_bytes()):08X}\n"
                      for p in files)
        (folder / f"{stem}.sfv").write_text(sfv)
        (root / name / f"{stem}.nfo").write_text("Reproducible slFtp workload\n")
        releases.append({"name": name, "stem": stem, "sub": sub, "files": files})
    (root / "manifest.json").write_text(json.dumps(releases))


class Server(socketserver.ThreadingTCPServer):
    allow_reuse_address = True
    daemon_threads = True


class RetryingFTPS(ftplib.FTP_TLS):
    pasv_retries = 0

    def makepasv(self):
        # PASV has not sent STOR yet, so retrying a 425 cannot duplicate a partial file.
        for attempt in range(8):
            try:
                return super().makepasv()
            except ftplib.error_temp as error:
                if not str(error).startswith("425") or attempt == 7:
                    raise
                self.pasv_retries += 1
                time.sleep(0.1)


class Relay(socketserver.BaseRequestHandler):
    """Opaque TCP forwarding preserves AUTH TLS and data-channel session reuse."""
    def handle(self):
        app, site = self.server.app, self.server.site
        if site == 4 and app.outage.is_set():
            return
        try:
            with socket.create_connection(("127.0.0.1", app.ports[site - 1]), 5) as backend:
                peers = (self.request, backend)
                while not app.stop.is_set():
                    if site == 4 and app.outage.is_set():
                        return
                    readable, _, _ = select.select(peers, [], [], 0.1)
                    for source in readable:
                        data = source.recv(65536)
                        if not data:
                            return
                        if source is backend and site == 4:
                            time.sleep(0.025)
                        (backend if source is self.request else self.request).sendall(data)
        except OSError:
            return


class IRC(socketserver.StreamRequestHandler):
    """Only the local IRC handshake/channel needed to exercise the real precatcher."""
    def handle(self):
        app = self.server.app
        nick = "slftp"
        try:
            for raw in self.rfile:
                text = raw.decode(errors="replace").strip()
                parts = text.split()
                if not parts:
                    continue
                command = parts[0].upper()
                if command == "NICK":
                    nick = parts[1].lstrip(":")
                elif command == "USER":
                    self.request.sendall(
                        f":perf 001 {nick} :Welcome\r\n:perf 376 {nick} :End of MOTD\r\n".encode())
                elif command == "PING":
                    self.request.sendall(("PONG " + text[5:] + "\r\n").encode())
                elif command == "JOIN":
                    channel = parts[1].lstrip(":")
                    self.request.sendall(
                        f":{nick}!slftp@localhost JOIN :{channel}\r\n"
                        f":perf 353 {nick} = {channel} :{nick} perfbot\r\n"
                        f":perf 366 {nick} {channel} :End of NAMES\r\n".encode())
                    app.irc = self.request
                    app.joined.set()
        except OSError:
            pass


def cpu_sample(pid):
    # /proc fields after the final ')' start at field 3; comm may contain spaces.
    fields = Path(f"/proc/{pid}/stat").read_text().rsplit(")", 1)[1].split()
    return {"cpu_s": (int(fields[11]) + int(fields[12])) / os.sysconf("SC_CLK_TCK"),
            "rss_bytes": int(fields[21]) * os.sysconf("SC_PAGE_SIZE")}


def publish_times(root, releases, now, starts, finishes, directories):
    """Bounded 50-ms observations, same observer and workload for both binaries."""
    for site in range(1, 5):
        for release in releases:
            folder = root / str(site) / "RACE" / release["name"]
            if folder.is_dir():
                directories.setdefault((site, release["name"]), now)
            for filename in release["files"]:
                path = folder / filename
                try:
                    size = path.stat().st_size
                except FileNotFoundError:
                    continue
                key = (site, release["name"], filename)
                starts.setdefault(key, now)
                if size == SIZE:
                    finishes.setdefault(key, now)


def transfer_metrics(releases, announced, starts, finishes):
    latencies, completions, transfers = [], [], []
    for release in releases:
        name = release["name"]
        for site in range(2, 5):
            end = max(finishes[(site, name, f)] for f in release["files"])
            completions.append((end - announced[name]) * 1000)
            for filename in release["files"]:
                source = finishes[(1, name, filename)]
                first = starts[(site, name, filename)]
                last = finishes[(site, name, filename)]
                latencies.append(max(0, first - source) * 1000)
                transfers.append({"site": site, "release": name, "file": filename,
                                  "source_ready_s": source, "destination_seen_s": first,
                                  "destination_full_s": last})
    return latencies, completions, transfers


class Workload:
    def __init__(self, args):
        self.args = args
        self.ports = [int(p) for p in args.ports.split(",")]
        self.stop = threading.Event()
        self.outage = threading.Event()
        self.joined = threading.Event()
        self.irc = None
        self.announce_lock = threading.Lock()
        self.announced = {}
        self.events = []
        self.pasv_retries = []
        self.servers = []
        self.releases = json.loads((args.payload / "manifest.json").read_text())

    def serve(self):
        for site, port in enumerate(self.ports, 1):
            if site != 4:
                continue
            server = Server(("127.0.0.1", port + 2000), Relay)
            server.app, server.site = self, site
            self.servers.append(server)
        server = Server(("127.0.0.1", 16667), IRC)
        server.app = self
        self.servers.append(server)
        for server in self.servers:
            threading.Thread(target=server.serve_forever, daemon=True).start()
        (self.args.out / "ready").touch()

    def upload(self, release, index, t0):
        time.sleep(max(0, t0 + index - time.monotonic()))
        name, sub = release["name"], release["sub"]
        stem = release["stem"]
        # Half the destination directories already exist, including a nested case.
        if index % 3 == 0:
            folder = self.args.sites / "2" / "RACE" / name
            folder.mkdir(parents=True, exist_ok=True)
            folder.chmod(0o777)
            if sub:
                (folder / sub).mkdir(exist_ok=True)
                (folder / sub).chmod(0o777)
        with RetryingFTPS(context=ssl._create_unverified_context(), timeout=15) as ftp:
            ftp.connect("127.0.0.1", self.ports[0])
            ftp.login("glftpd", "glftpd")
            ftp.prot_p()
            ftp.mkd("/RACE/" + name)
            ftp.cwd("/RACE/" + name)
            with (self.args.payload / name / f"{stem}.nfo").open("rb") as stream:
                ftp.storbinary(f"STOR {stem}.nfo", stream)
            if sub:
                ftp.mkd(sub)
                ftp.cwd(sub)
            with (self.args.payload / name / sub / f"{stem}.sfv").open("rb") as stream:
                ftp.storbinary(f"STOR {stem}.sfv", stream)
            # Announce an incomplete release, as in production, before the first volume.
            with self.announce_lock:
                self.announced[name] = time.monotonic()
                self.announce(name)
            for filename in release["files"]:
                time.sleep(0.10)
                with (self.args.payload / name / filename).open("rb") as stream:
                    ftp.storbinary("STOR " + Path(filename).name, stream)
            self.pasv_retries.append(ftp.pasv_retries)
            with self.announce_lock:
                self.irc.sendall(
                    f":perfbot!bot@localhost PRIVMSG #race :COMPLETE RACE {name}\r\n".encode())

    def interrupt_site(self, t0):
        if self.stop.wait(max(0, t0 + 7 - time.monotonic())):
            return
        self.outage.set()
        self.events.append({"event": "site4_down", "time_s": time.monotonic() - t0})
        if self.stop.wait(6):
            return
        self.outage.clear()
        self.events.append({"event": "site4_up", "time_s": time.monotonic() - t0})
        # Wake the normal login/status path after the simulated network outage.
        import subprocess
        subprocess.run(["tmux", "send-keys", "-t", self.args.session,
                        "!bnctest E2E4", "Enter"], check=True)
        if not self.stop.wait(2):
            with self.announce_lock:
                for name in self.announced:
                    self.announce(name)
            self.events.append({"event": "reannounce_after_recovery",
                                "time_s": time.monotonic() - t0})

    def announce(self, name):
        self.irc.sendall(
            f":perfbot!bot@localhost PRIVMSG #race :NEW RACE {name}\r\n".encode())

    def verify(self):
        errors = []
        for site in range(1, 5):
            for release in self.releases:
                folder = self.args.sites / str(site) / "RACE" / release["name"]
                for filename, digest in release["files"].items():
                    path = folder / filename
                    if not path.is_file() or hashlib.sha256(path.read_bytes()).hexdigest() != digest:
                        errors.append(f"E2E{site}/{release['name']}/{filename}")
        return errors

    def run(self):
        self.serve()
        deadline = time.monotonic() + 120
        while not (self.args.out / "go").exists():
            if time.monotonic() > deadline:
                raise TimeoutError("slFtp setup did not finish")
            time.sleep(0.1)
        if not self.joined.wait(10):
            raise TimeoutError("slFtp did not join the benchmark IRC channel")
        initial = cpu_sample(self.args.pid)
        peak_rss = initial["rss_bytes"]
        starts, finishes, directories = {}, {}, {}
        t0 = time.monotonic()
        started_at = datetime.datetime.now(datetime.timezone.utc).isoformat()
        expected = RELEASES * VOLUMES * 4
        error = None
        sample_gaps = []
        previous = t0
        next_note = t0
        with concurrent.futures.ThreadPoolExecutor(max_workers=RELEASES) as pool:
            futures = [pool.submit(self.upload, release, i, t0)
                       for i, release in enumerate(self.releases)]
            threading.Thread(target=self.interrupt_site, args=(t0,), daemon=True).start()
            while time.monotonic() - t0 < TIMEOUT:
                now = time.monotonic()
                sample_gaps.append((now - previous) * 1000)
                previous = now
                publish_times(self.args.sites, self.releases, now, starts, finishes, directories)
                peak_rss = max(peak_rss, cpu_sample(self.args.pid)["rss_bytes"])
                if now >= next_note:
                    counts = {site: sum(key[0] == site for key in finishes) for site in range(1, 5)}
                    print(f"LOAD {now - t0:.1f}s full payload files by site: {counts}", flush=True)
                    next_note = now + 10
                for future in futures:
                    if future.done() and future.exception():
                        raise future.exception()
                if len(finishes) == expected and all(f.done() for f in futures):
                    break
                time.sleep(0.05)
            else:
                error = f"timeout: {len(finishes)}/{expected} full-size payload files"
            final = cpu_sample(self.args.pid)
            elapsed = time.monotonic() - t0
        self.stop.set()
        errors = self.verify()  # Hashing is outside the measured interval.
        if errors:
            print("VERIFY missing/corrupt examples: " + json.dumps(errors[:12]), flush=True)
            for site in range(1, 5):
                base = self.args.sites / str(site) / "RACE"
                actual = [(str(path.relative_to(base)), path.stat().st_size)
                          for path in sorted(base.rglob("*"))
                          if path.is_file() and
                          (path.suffix == ".rar" or
                           (len(path.suffix) == 4 and path.suffix[1] == "r" and path.suffix[2:].isdigit()))]
                print(f"VERIFY actual payload E2E{site}: " + json.dumps(actual), flush=True)
        result = {"elapsed_s": elapsed, "cpu_s": final["cpu_s"] - initial["cpu_s"],
                  "started_at_utc": started_at, "started_monotonic_s": t0,
                  "peak_rss_bytes": peak_rss, "files_expected": RELEASES * VOLUMES * 3,
                  "files_full": sum(key[0] != 1 for key in finishes),
                  "bytes_expected": RELEASES * VOLUMES * SIZE * 3,
                  "max_sample_gap_ms": max(sample_gaps),
                  "uploader_pasv_retries": sum(self.pasv_retries),
                  "error": error, "hash_errors": errors, "events": self.events,
                  "directories": [
                      {"site": site, "release": name, "first_seen_s": stamp,
                       "ready_after_announce_ms": max(0, stamp - self.announced[name]) * 1000,
                       "already_present_at_announce": stamp < self.announced[name]}
                      for (site, name), stamp in directories.items() if name in self.announced]}
        if not error and not errors:
            latency, completion, transfers = transfer_metrics(
                self.releases, self.announced, starts, finishes)
            result.update(start_latencies_ms=latency, release_completion_ms=completion,
                          transfers=transfers)
        (self.args.out / "result.json").write_text(json.dumps(result, indent=2))
        return 1 if error or errors else 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--prepare", type=Path)
    parser.add_argument("--payload", type=Path)
    parser.add_argument("--out", type=Path)
    parser.add_argument("--sites", type=Path, default=Path("/sites"))
    parser.add_argument("--ports")
    parser.add_argument("--pid", type=int)
    parser.add_argument("--session")
    args = parser.parse_args()
    if args.prepare:
        prepare(args.prepare)
        return 0
    app = Workload(args)
    try:
        return app.run()
    except Exception as error:
        (args.out / "result.json").write_text(json.dumps({"error": repr(error)}))
        raise
    finally:
        app.stop.set()


if __name__ == "__main__":
    raise SystemExit(main())
