"""Read each Cabal file revision from the Hackage index."""

import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
import re
import subprocess
import tarfile
import tempfile


PACKAGE_PATH = re.compile(r"([A-Za-z0-9]+(?:-[A-Za-z0-9]+)*)/([0-9]+(?:\.[0-9]+)*)/([^/]+)\.cabal")


def cases(index):
    revisions = Counter()
    with tarfile.open(index, "r|*", stream=True) as archive:
        for member in archive:
            if not member.name.endswith(".cabal"):
                continue
            match = PACKAGE_PATH.fullmatch(member.name)
            if not match or match[1] != match[3] or not member.isfile():
                raise ValueError(f"Invalid Cabal entry: {member.name}")
            revision = revisions[member.name]
            revisions[member.name] += 1
            with archive.extractfile(member) as source:
                data = source.read()
            yield {
                "path": member.name,
                "revision": revision,
                "offset": member.offset_data,
                "size": member.size,
                "sha256": hashlib.sha256(data).hexdigest(),
            }, data


def scan(index, manifest):
    packages = set()
    versions = set()
    count = 0
    for case, _ in cases(index):
        name, version, _ = case["path"].split("/")
        packages.add(name)
        versions.add((name, version))
        count += 1
        manifest.write(json.dumps(case, sort_keys=True) + "\n")
    if not count:
        raise ValueError("The index has no Cabal files.")
    return {"packages": len(packages), "versions": len(versions), "revisions": count}


def run(index, command, report, timeout):
    if not command:
        raise ValueError("Specify a parser command after --.")
    count = failures = 0
    with tempfile.TemporaryDirectory() as directory:
        target = Path(directory) / "package.cabal"
        for case, data in cases(index):
            target.write_bytes(data)
            try:
                result = subprocess.run(
                    [*command, str(target)], timeout=timeout,
                    stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                    check=False,
                )
                status = result.returncode
            except subprocess.TimeoutExpired:
                status = "timeout"
            count += 1
            if status != 0:
                failures += 1
                report.write(json.dumps({**case, "status": status}, sort_keys=True) + "\n")
    if not count:
        raise ValueError("The index has no Cabal files.")
    return {"cases": count, "failures": failures}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="action", required=True)
    scan_parser = commands.add_parser("scan")
    scan_parser.add_argument("index", type=Path)
    scan_parser.add_argument("manifest", type=Path)
    scan_parser.add_argument("summary", type=Path)
    scan_parser.add_argument("--expect", type=Path)
    run_parser = commands.add_parser("run")
    run_parser.add_argument("--timeout", type=float, default=30)
    run_parser.add_argument("index", type=Path)
    run_parser.add_argument("report", type=Path)
    run_parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    if args.action == "scan":
        with args.manifest.open("w") as manifest:
            summary = scan(args.index, manifest)
        args.summary.write_text(json.dumps(summary, indent=2, sort_keys=True) + "\n")
        if args.expect and summary != json.loads(args.expect.read_text())["counts"]:
            raise ValueError("The test data counts do not agree with the fixed counts.")
    else:
        command = args.command
        if command[:1] == ["--"]:
            command = command[1:]
        if args.timeout <= 0:
            parser.error("The timeout must be positive.")
        with args.report.open("w") as report:
            summary = run(args.index, command, report, args.timeout)
        print(json.dumps(summary, sort_keys=True))
        raise SystemExit(1 if summary["failures"] else 0)


if __name__ == "__main__":
    main()
