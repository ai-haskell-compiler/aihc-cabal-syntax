import io
import json
from pathlib import Path
import tarfile
import tempfile
import unittest

from check_stackage import check
from stackage import latest, snapshot, write


class StackageTests(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.root = Path(directory.name)

    def test_snapshot_lines(self):
        name, packages = snapshot([
            "# Stackage LTS 1.2\n", "default-package-overrides:\n",
            "  - abc ==1.0\n", "  - a-b-c ==0.1.2\n"])
        self.assertEqual(name, "Stackage LTS 1.2")
        self.assertEqual(packages, [("abc", "1.0"), ("a-b-c", "0.1.2")])
        for lines in [["  - abc ==1.0\n"], ["# Stackage LTS 1\n"],
                      ["# Stackage LTS 1\n", "  - abc >=1.0\n"],
                      ["# Stackage LTS 1\n", "  - abc ==1\n", "  - abc ==1\n"]]:
            with self.assertRaises(ValueError):
                snapshot(lines)

    def test_latest_revision_and_archive(self):
        index = self.root / "index.tar"
        entries = [("abc/1.0/abc.cabal", b"first"), ("abc/1.0/abc.cabal", b"second"),
                   ("other/1/other.cabal", b"unused")]
        manifest = []
        with tarfile.open(index, "w") as archive:
            for revision, (name, data) in enumerate(entries):
                member = tarfile.TarInfo(name)
                member.size = len(data)
                archive.addfile(member, io.BytesIO(data))
        with tarfile.open(index) as archive:
            counts = {}
            for member in archive:
                manifest.append(json.dumps({"path": member.name, "offset": member.offset_data,
                                            "size": member.size, "revision": counts.get(member.name, 0)}))
                counts[member.name] = counts.get(member.name, 0) + 1
        cases = latest(manifest, [("abc", "1.0")])
        self.assertEqual([case["revision"] for case in cases], [1])
        write(index, cases, self.root / "subset.tar")
        with tarfile.open(self.root / "subset.tar") as archive:
            members = archive.getmembers()
            self.assertEqual([m.name for m in members], ["abc/1.0/abc.cabal"])
            self.assertEqual(archive.extractfile(members[0]).read(), b"second")
        with self.assertRaises(ValueError):
            latest(manifest, [("missing", "1")])

    def test_check_requires_equal_data(self):
        report = self.root / "report"
        report.mkdir()
        pin = self.root / "stackage.json"
        pin.write_text(json.dumps({"packages": 2, "snapshot": "Stackage LTS 1.2"}))
        (report / "snapshot.json").write_text(pin.read_text())
        def summary(match, mismatch):
            (report / "summary.json").write_text(json.dumps(
                {"total": 2, "outcomes": {"match": match, "mismatch": mismatch}}))
        summary(2, 0)
        check(report, pin)
        summary(1, 1)
        with self.assertRaises(ValueError):
            check(report, pin)
        summary(2, 0)
        pin.write_text(json.dumps({"packages": 2, "snapshot": "Stackage LTS 1.3"}))
        with self.assertRaises(ValueError):
            check(report, pin)


if __name__ == "__main__":
    unittest.main()
