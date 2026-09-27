"""Require equal data for each package of the pinned Stackage snapshot."""

import json
from pathlib import Path
import sys


def check(report, expected):
    summary = json.loads((report / "summary.json").read_text())
    snapshot = json.loads((report / "snapshot.json").read_text())
    pin = json.loads(expected.read_text())
    if snapshot != pin:
        raise ValueError(f"The Stackage snapshot changed. Expected {pin}; got {snapshot}")
    outcomes = summary["outcomes"]
    if summary["total"] != pin["packages"] or sum(outcomes.values()) != summary["total"]:
        raise ValueError("The report does not contain all snapshot packages.")
    if outcomes["match"] != summary["total"]:
        failures = {key: value for key, value in outcomes.items() if key != "match" and value}
        raise ValueError(f"Stackage packages without equal data: {failures}")


if __name__ == "__main__":
    check(Path(sys.argv[1]), Path(sys.argv[2]))
