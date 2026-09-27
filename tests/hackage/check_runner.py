"""Check the compiled comparison command with a small archive."""

import io
import json
from pathlib import Path
import subprocess
import sys
import tarfile
import tempfile
import unittest


class RunnerTests(unittest.TestCase):
    runner = sys.argv[1]

    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        self.index = self.root / "index.tar"
        self.report = self.root / "report"

    def archive(self, entries):
        with tarfile.open(self.index, "w") as archive:
            for path, data in entries:
                member = tarfile.TarInfo(path)
                member.size = len(data)
                archive.addfile(member, io.BytesIO(data))

    def run_report(self):
        return subprocess.run([self.runner, str(self.index), str(self.report)],
                              capture_output=True, text=True, check=False)

    def test_counts_and_revisions(self):
        header = b"cabal-version: 3.0\nname: sample\nversion: 1.0\n"
        self.archive([
            ("sample/1.0/sample.cabal", header + b"unknown-field: warning\n"),
            ("sample/1.0/package.json", b"{}"),
            # Cabal-syntax 3.12 does not accept format version 3.14.
            ("sample/1.0/sample.cabal", b"cabal-version: 3.14\nname: sample\nversion: 1.0\n"),
            ("old/1/old.cabal", b"name: old\n"),
        ])
        result = self.run_report()
        self.assertEqual(result.returncode, 0, result.stderr)
        summary = json.loads((self.report / "summary.json").read_text())
        self.assertEqual(summary["total"], 3)
        self.assertEqual(summary["parser_accepted"], 2)
        self.assertEqual(summary["reference_accepted"], 1)
        self.assertEqual(summary["reference_with_warnings"], 1)
        counts = summary["outcomes"]
        self.assertEqual(sum(counts.values()), 3)
        self.assertEqual(counts["match"], 1)
        self.assertEqual(counts["reference_error"], 1)
        self.assertEqual(counts["both_rejected"], 1)
        failures = [json.loads(line) for line in (self.report / "failures.jsonl").read_text().splitlines()]
        self.assertEqual([(f["path"], f["revision"]) for f in failures],
                         [("sample/1.0/sample.cabal", 1), ("old/1/old.cabal", 0)])
        self.assertNotEqual(self.run_report().returncode, 0)
        self.assertEqual(json.loads((self.report / "summary.json").read_text()), summary)

    def test_empty_and_truncated_archives(self):
        self.archive([])
        self.assertNotEqual(self.run_report().returncode, 0)
        self.assertFalse((self.report / "summary.json").exists())
        self.report = self.root / "truncated-report"
        self.index.write_bytes(b"incomplete header")
        self.assertNotEqual(self.run_report().returncode, 0)
        self.assertFalse((self.report / "summary.json").exists())


if __name__ == "__main__":
    unittest.main(argv=[sys.argv[0]])
