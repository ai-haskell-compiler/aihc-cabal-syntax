"""Check benchmark file counts, repeated paths, and archive errors."""
import io
from pathlib import Path
import subprocess
import sys
import tarfile
import tempfile

with tempfile.TemporaryDirectory() as directory:
    index = Path(directory) / "index.tar"
    def run(parser):
        return subprocess.run([sys.argv[1], parser, str(index)], capture_output=True, text=True)
    with tarfile.open(index, "w") as archive:
        for name, data in [
            ("sample.cabal", b"cabal-version: 3.0\nname: sample\nversion: 1.0\n"),
            ("sample.cabal", b"invalid input\n"),
            ("package.json", b"{}"),
            ("old.cabal", b"name: old\nversion: 1\n"),
            # Both parsers accept format version 3.16.
            ("new.cabal", b"cabal-version: 3.16\nname: new\nversion: 1\n"),
        ]:
            entry = tarfile.TarInfo(name)
            entry.size = len(data)
            archive.addfile(entry, io.BytesIO(data))
    for parser in ["aihc", "Cabal-syntax"]:
        result = run(parser)
        assert result.returncode == 0, result.stderr
        assert result.stdout.strip() == "4 3", result.stdout
    with tarfile.open(index, "w"):
        pass
    for parser in ["aihc", "Cabal-syntax"]:
        assert run(parser).returncode != 0
    index.write_bytes(b"incomplete header")
    for parser in ["aihc", "Cabal-syntax"]:
        assert run(parser).returncode != 0
