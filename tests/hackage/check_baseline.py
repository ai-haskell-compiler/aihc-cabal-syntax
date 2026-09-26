"""Compare the complete report counts with the stored baseline."""

from collections import Counter
import json
from pathlib import Path
import sys


def check(report, baseline, snapshot):
    actual = json.loads((report / "summary.json").read_text())
    expected = json.loads(baseline.read_text())
    pin = json.loads(snapshot.read_text())
    if actual != expected:
        raise ValueError(f"Hackage counts changed. Expected {expected}; got {actual}")
    outcomes = actual["outcomes"]
    if actual["total"] != pin["counts"]["revisions"] or sum(outcomes.values()) != actual["total"]:
        raise ValueError("The report does not contain all test cases.")
    if outcomes["exception"]:
        raise ValueError("The comparison raised an exception.")
    both = outcomes["match"] + outcomes["mismatch"] + outcomes["conversion_error"]
    if actual["parser_accepted"] != both + outcomes["reference_error"]:
        raise ValueError("The project parser count does not agree with the outcomes.")
    if actual["reference_accepted"] != both + outcomes["parser_error"]:
        raise ValueError("The reference parser count does not agree with the outcomes.")
    failures = Counter()
    with (report / "failures.jsonl").open() as source:
        for line in source:
            failures[json.loads(line)["status"]] += 1
    if failures != Counter({key: value for key, value in outcomes.items() if key != "match"}):
        raise ValueError("Failure records do not agree with the summary.")


if __name__ == "__main__":
    check(*(Path(argument) for argument in sys.argv[1:]))
