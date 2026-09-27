"""Check benchmark file counts, repeated paths, and archive errors."""
import io
from pathlib import Path
import subprocess
import sys
import tarfile
import tempfile

with tempfile.TemporaryDirectory() as directory:
    index = Path(directory) / "index.tar"
    def run():
        return subprocess.run([sys.argv[1], str(index)], capture_output=True, text=True)
    with tarfile.open(index, "w") as archive:
        for name, data in [
            ("sample.cabal", b"cabal-version: 3.0\nname: sample\nversion: 1.0\n"),
            ("sample.cabal", b"invalid input\n"),
            ("package.json", b"{}"),
        ]:
            entry = tarfile.TarInfo(name)
            entry.size = len(data)
            archive.addfile(entry, io.BytesIO(data))
    result = run()
    assert result.returncode == 0, result.stderr
    assert result.stdout.strip() == "2 1", result.stdout
    with tarfile.open(index, "w"):
        pass
    assert run().returncode != 0
    index.write_bytes(b"incomplete header")
    assert run().returncode != 0
