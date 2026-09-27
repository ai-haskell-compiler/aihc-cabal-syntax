"""Check changed results, unchanged results, and benchmark failures."""
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


class StatsTests(unittest.TestCase):
    script = str(Path(sys.argv[1]).resolve())

    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.root = Path(directory.name)
        self.data = self.root / "tests/hackage"
        self.data.mkdir(parents=True)
        self.summary = dict(total=3, parser_accepted=2, reference_accepted=3,
                            reference_version="3.12.1.0", outcomes=dict(match=1))
        (self.root / "summary.json").write_text(json.dumps(self.summary))
        (self.data / "results.json").write_text(json.dumps(self.summary))
        (self.data / "baseline.json").write_text("old baseline\n")
        (self.data / "benchmark.json").write_text("old benchmark\n")
        (self.root / "README.md").write_text("old README\n")
        self.runner = self.root / "runner"
        self.runner.write_text(f"#!{sys.executable}\nprint('3 2')\n")
        self.runner.chmod(0o755)

    def run_update(self, *options):
        return subprocess.run([
            sys.executable, self.script, "summary.json",
            "--runner", str(self.runner), "--index", "unused",
            "--system", "test-system", "--ghc-version", "test-version", *options,
        ], cwd=self.root, capture_output=True, text=True)

    def change_results(self):
        (self.data / "results.json").write_text("{}")

    def test_unchanged_results_do_not_measure_or_write(self):
        self.runner.unlink()
        before = {path: path.read_bytes() for path in self.root.rglob("*") if path.is_file()}
        result = self.run_update("--refresh-stats")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(before, {path: path.read_bytes() for path in before})

    def test_changed_results_update_all_outputs(self):
        self.change_results()
        result = self.run_update("--refresh-stats")
        self.assertEqual(result.returncode, 0, result.stderr)
        for name in ["results.json", "baseline.json"]:
            self.assertEqual(json.loads((self.data / name).read_text()), self.summary)
        self.assertEqual(json.loads((self.data / "benchmark.json").read_text())["total"], 3)
        self.assertIn("66.67%", (self.root / "README.md").read_text())
        self.assertEqual(self.run_update("--check").returncode, 0)
        self.change_results()
        self.assertNotEqual(self.run_update("--check").returncode, 0)

    def test_failed_benchmark_does_not_publish(self):
        self.change_results()
        self.runner.write_text(f"#!{sys.executable}\nraise SystemExit(1)\n")
        result = self.run_update("--refresh-stats")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual((self.data / "results.json").read_text(), "{}")
        self.assertEqual((self.data / "baseline.json").read_text(), "old baseline\n")
        self.assertEqual((self.root / "README.md").read_text(), "old README\n")


if __name__ == "__main__":
    unittest.main(argv=[sys.argv[0]])
