"""Select the Cabal files of a Stackage snapshot from the Hackage index."""

import argparse
import json
from pathlib import Path
import re
import tarfile

PIN = re.compile(r"\s*- ([A-Za-z0-9-]+) ==([0-9.]+)\s*")


def snapshot(lines):
    """Read the name and pinned packages from a nixpkgs stackage.yaml file."""
    name = None
    packages = []
    for line in lines:
        if name is None and line.startswith("# Stackage "):
            name = line[2:].strip()
        match = PIN.fullmatch(line)
        if match:
            packages.append((match[1], match[2]))
        elif line.strip().startswith("- "):
            raise ValueError(f"Invalid package line: {line.strip()}")
    if name is None or not packages:
        raise ValueError("The file does not contain a Stackage snapshot.")
    if len(set(packages)) != len(packages):
        raise ValueError("The snapshot contains a package more than one time.")
    return name, packages


def latest(manifest, packages):
    """Find the last revision in the index of each package version."""
    wanted = {f"{name}/{version}/{name}.cabal" for name, version in packages}
    found = {}
    for line in manifest:
        case = json.loads(line)
        if case["path"] in wanted:
            previous = found.get(case["path"])
            if previous is None or case["revision"] > previous["revision"]:
                found[case["path"]] = case
    missing = sorted(wanted - found.keys())
    if missing:
        raise ValueError(f"The index does not contain: {', '.join(missing[:10])}")
    return [found[path] for path in sorted(found)]


def write(index, cases, destination):
    """Write the selected revisions to a new tar archive."""
    with open(index, "rb") as source, tarfile.open(destination, "w", format=tarfile.USTAR_FORMAT) as archive:
        for case in cases:
            source.seek(case["offset"])
            member = tarfile.TarInfo(case["path"])
            member.size = case["size"]
            member.mtime = 0
            member.mode = 0o644
            archive.addfile(member, _Reader(source, case["size"]))


class _Reader:
    def __init__(self, source, size):
        self.source = source
        self.remaining = size

    def read(self, size=-1):
        if size < 0 or size > self.remaining:
            size = self.remaining
        self.remaining -= size
        return self.source.read(size)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("stackage", type=Path)
    parser.add_argument("index", type=Path)
    parser.add_argument("manifest", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    with args.stackage.open() as lines:
        name, packages = snapshot(lines)
    with args.manifest.open() as manifest:
        cases = latest(manifest, packages)
    args.output.mkdir()
    write(args.index, cases, args.output / "index.tar")
    with (args.output / "cases.jsonl").open("w") as report:
        for case in cases:
            report.write(json.dumps(case, sort_keys=True) + "\n")
    summary = {"snapshot": name, "packages": len(packages)}
    (args.output / "summary.json").write_text(json.dumps(summary, sort_keys=True) + "\n")


if __name__ == "__main__":
    main()
