import io
import json
from pathlib import Path
import sys
import tarfile
import tempfile
import unittest
from unittest.mock import patch
import subprocess

from corpus import cases, run, scan
from prefix import copy_prefix


class CorpusTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.index = Path(self.directory.name) / "index.tar"

    def archive(self, entries):
        with tarfile.open(self.index, "w") as archive:
            for name, data, kind in entries:
                member = tarfile.TarInfo(name)
                member.size = len(data)
                member.type = kind
                archive.addfile(member, io.BytesIO(data))

    def test_revisions_and_bytes(self):
        self.archive([
            ("demo/1.0/demo.cabal", b"name: demo\r\n", tarfile.REGTYPE),
            ("demo/1.0/package.json", b"{}", tarfile.REGTYPE),
            ("demo/1.0/demo.cabal", b"name: demo\n\xff", tarfile.REGTYPE),
            ("demo/2.0/demo.cabal", b"name: demo", tarfile.REGTYPE),
        ])
        found = list(cases(self.index))
        self.assertEqual([case[0]["revision"] for case in found], [0, 1, 0])
        self.assertEqual(found[1][1], b"name: demo\n\xff")
        manifest = io.StringIO()
        self.assertEqual(scan(self.index, manifest), {"packages": 1, "versions": 2, "revisions": 3})
        with self.index.open("rb") as source:
            for case, data in found:
                source.seek(case["offset"])
                self.assertEqual(source.read(case["size"]), data)

    def test_invalid_entries(self):
        for name, kind in [("../demo.cabal", tarfile.REGTYPE),
                           ("demo/1/other.cabal", tarfile.REGTYPE),
                           ("demo/1/demo.cabal", tarfile.SYMTYPE)]:
            with self.subTest(name=name, kind=kind):
                self.archive([(name, b"", kind)])
                with self.assertRaises(ValueError):
                    list(cases(self.index))

    def test_empty_index(self):
        self.archive([])
        with self.assertRaises(ValueError):
            scan(self.index, io.StringIO())

    def test_parser_failures(self):
        self.archive([(f"demo/{n}/demo.cabal", str(n).encode(), tarfile.REGTYPE) for n in range(3)])
        report = io.StringIO()
        command = [sys.executable, "-c", "import pathlib,sys; sys.exit(int(pathlib.Path(sys.argv[1]).read_text()))"]
        self.assertEqual(run(self.index, command, report, 10), {"cases": 3, "failures": 2})
        self.assertEqual([json.loads(line)["status"] for line in report.getvalue().splitlines()], [1, 2])

    def test_timeout(self):
        self.archive([("demo/1/demo.cabal", b"", tarfile.REGTYPE)])
        report = io.StringIO()
        with patch("corpus.subprocess.run", side_effect=subprocess.TimeoutExpired("parser", 30)):
            run(self.index, ["parser"], report, 30)
        self.assertEqual(json.loads(report.getvalue())["status"], "timeout")

    def test_command_line(self):
        self.archive([("demo/1/demo.cabal", b"name: demo", tarfile.REGTYPE)])
        report = Path(self.directory.name) / "failures.jsonl"
        result = subprocess.run([
            sys.executable, str(Path(__file__).with_name("corpus.py")), "run",
            str(self.index), str(report), "--", sys.executable, "-c", "raise SystemExit(1)",
        ], capture_output=True, text=True, check=False)
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertEqual(json.loads(result.stdout), {"cases": 1, "failures": 1})
        self.assertEqual(json.loads(report.read_text())["path"], "demo/1/demo.cabal")

    def test_prefix(self):
        output = io.BytesIO()
        copy_prefix(io.BytesIO(b"a" * 512 + b"b" * 512), output, 512)
        self.assertEqual(output.getvalue(), b"a" * 512)
        with self.assertRaises(ValueError):
            copy_prefix(io.BytesIO(b"a"), io.BytesIO(), 512)
        with self.assertRaises(ValueError):
            copy_prefix(io.BytesIO(b"a"), io.BytesIO(), 1)


if __name__ == "__main__":
    unittest.main()
