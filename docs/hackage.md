# Hackage compliance tests

Hackage supplies package descriptions in one
[package index](https://hackage-content.haskell.org/01-index.tar.gz).
The [Hackage API](https://hackage-content.haskell.org/api) describes this archive.
Package source code is not necessary for these tests.

The fixed test data contains 200,642 Cabal file revisions from 153,921 versions of 19,488 packages.
The download is approximately 133 MiB. The uncompressed index needs approximately 1 GiB.
Different revisions use the same archive path. Normal tar extraction can overwrite earlier revisions.
The test reader processes each revision separately.

## Run the comparison

Run:

```sh
nix build .#hackage-compliance --no-update-lock-file -o result-compliance
cat result-compliance/summary.json
```

The comparison uses one process for all files.
It does not execute package code.
The build and test steps do not use the network.
Nix gets the fixed dependencies before these steps.

Each file is input to `Aihc.Cabal.parsePackage` and Cabal-syntax's `parseGenericPackageDescription`.
The converter receives only the project AST.
Typed fields are converted to Cabal types.
Fields stored as text are converted with Cabal field grammars.
The test uses `(==)` on the complete `GenericPackageDescription` values.
It does not simplify version ranges, remove fields, or select conditional branches.

Equality means that the converted AST contains the same data as the Cabal-syntax result.
It does not mean that comments, source formatting, or unknown fields survive.
Cabal-syntax can discard such data itself.
A conversion error does not show which parser is defective.
The converter can also have limits or defects.

The output directory contains:

| File | Contents |
| --- | --- |
| `summary.json` | Counts and the Cabal-syntax version |
| `failures.jsonl` | One record for each case without equal structures |
| `snapshot.json` | The exact test data specification |

The summary contains these counts:

| Count | Meaning |
| --- | --- |
| `total` | All Cabal file revisions read |
| `parser_accepted` | Files that our parser accepts without errors |
| `reference_accepted` | Files that Cabal-syntax accepts without errors |
| `parser_with_warnings` | Files with project parser warnings |
| `reference_with_warnings` | Files with Cabal-syntax warnings |
| `outcomes.match` | Both parsers accept the file and the complete structures are equal |
| `outcomes.mismatch` | Both parsers accept the file, but the converted structure is different |
| `outcomes.parser_error` | Only the project parser rejects the file |
| `outcomes.reference_error` | Only Cabal-syntax rejects the file |
| `outcomes.both_rejected` | Both parsers reject the file |
| `outcomes.conversion_error` | Both parsers accept the file, but conversion fails |
| `outcomes.exception` | The comparison raises an exception |

The outcome counts add up to `total`.
Files with warnings can count as matches.
A comparison exception does not count as parser acceptance or a match.
Each failure record contains the archive path and a revision number that starts at zero.
Error text has a 2,000-character limit. This limit does not affect equality or counts.

The command returns success when it completes the measurement, even if some structures differ.
Empty archives and archive errors cause command failure.
The command refuses to overwrite an existing report directory.

Cabal-syntax changes 57 known Hackage files before it parses them.
This parser applies the same changes to these files and gives a "Legacy cabal file" warning.

The Nix check compares the full report with `tests/hackage/baseline.json`.
It also verifies the total case count and the failure record counts.
A change in counts causes a check failure.
Review the failures before you update the baseline.

## Stackage LTS comparison

The Stackage comparison uses the packages of one Stackage LTS snapshot.
The pinned Nixpkgs revision supplies the list in
`pkgs/development/haskell-modules/configuration-hackage2nix/stackage.yaml`.
The list pins one version of each package.
The test selects the last revision of each package version in the fixed Hackage index.
It does not download other data.

Run:

```sh
nix build .#stackage-compliance --no-update-lock-file -o result-stackage
cat result-stackage/summary.json
```

The report has the same format as the full comparison.
`snapshot.json` contains the snapshot name and the number of packages.
The Nix check compares this data with `tests/hackage/stackage.json`.
The check fails if a package does not have equal converted data.

If a Nixpkgs update changes the snapshot, update `tests/hackage/stackage.json`.
The index must contain each package version of the new snapshot.
If it does not, the build of `.#stackage-corpus` fails. Then update the index as described below.

## Repeat a failed case

Build the test data:

```sh
nix build .#hackage-corpus --no-update-lock-file -o result-corpus
```

`result-corpus/manifest.jsonl` contains one record for each revision.
Each record has its path, revision, byte offset, byte size, and SHA-256 hash.
Use the offset and size to copy the original bytes from `result-corpus/index.tar`.
Alternatively, use `cases()` in `tests/hackage/corpus.py` to read each revision separately.
Do not use normal tar extraction to select a revision.

Run the test suite on the selected file in the Nix development environment:

```sh
nix develop --no-update-lock-file -c cabal test hackage-compliance --offline \
  --test-options='--file case.cabal' --test-show-details=direct
```

`hackage-compliance` is a test suite, not an installed executable.
With no arguments, it runs the conversion tests.
The Nix comparison uses a private test runner. The library output does not contain this runner.

## Fixed dependencies

The Nixpkgs revision from `origin/main` stays unchanged.
`flake.lock` sets exact versions of GHC, Python, curl, and the other tools and libraries.
The pinned Nixpkgs revision does not contain Cabal-syntax 3.18.1.0.
`flake.nix` gets this version from Hackage and verifies its SHA-256 hash.
GHC contains Cabal-syntax 3.12.1.0. The test runners hide it.
`tests/hackage/snapshot.json` sets the Hackage prefix length, SHA-256 hash, and case counts.

The Hackage index grows when Hackage adds entries.
The Nix fetch step decompresses the archive and keeps only the fixed prefix.
The prefix ends after a complete tar entry, before the archive end blocks.
The test data build adds two zero blocks to complete the tar archive.
This does not change the Cabal file bytes or their offsets.
New entries and changes to gzip compression do not change this prefix.
Nix verifies its SHA-256 hash before the build can use it.
If Hackage changes an earlier entry or removes the index, the fetch step fails.
Keep the Nix output in a binary cache for long-term availability.

For a data update, download a new index in `nix develop`.
Set the prefix length to the end of the last complete tar entry.
Calculate its SHA-256 hash and case counts with the Nix tools and the reader.
Update `tests/hackage/snapshot.json` with these values.
Run the full comparison before you change expected counts.
Explain each dependency update and each change in counts in the pull request.

Run the required checks:

```sh
nix build --no-update-lock-file
nix flake check --no-update-lock-file
```

## README measurements

Run `nix run .#update-readme --no-update-lock-file` from the repository root.
The command uses the full comparison report and measures each parser in a separate process.
It writes `tests/hackage/benchmark.json` and generates `README.md` from both results.
The Nix check requires the README to agree with the comparison and the saved measurement.
CI does not compare elapsed time with a fixed limit.

The benchmark reads the archive in order, including all revisions and rejected files.
It forces each complete parse result through `length (show result)` before the next file.
Thus, elapsed time includes input, parsing, result evaluation, and conversion to text.
It excludes compilation and the comparison converter.
Both parsers use the same archive reader, compiler, optimization, and result evaluation method.
Cabal-syntax runs first. The aihc parser runs second. Each parser has one measured run.
The README shows absolute measurements and the aihc-to-Cabal-syntax ratios.
A ratio below 1 means that aihc uses less time or memory.
The parsers have different output types and accept different numbers of files.
These ratios measure the complete archive workload, including those differences.
The measurement uses a monotonic clock and the child process peak resident set size (RSS).
RSS includes the runtime and archive buffers. It is not the Haskell heap size.
The saved data records both measurements, their file counts, the Cabal-syntax version, the Nix system, and the GHC version.
Run the update command on the same machine for performance comparisons.

## Weekly results

The `Update Hackage results` workflow runs each Monday at 05:17 UTC.
You can also start it with `workflow_dispatch`.
It compares the full report with `tests/hackage/results.json`.
If the results are unchanged, it keeps the saved benchmark and opens no new pull request.
A change in benchmark time alone does not cause a pull request.

If the results change, the workflow measures a new benchmark and updates the README and baseline.
It runs both required Nix checks before it opens or updates `codex/weekly-hackage-results`.
Review the changed counts before merge. The workflow does not merge the pull request.
It does not change the Hackage index or dependency pins.

The schedule starts after the workflow file is on `main`.
The repository must permit GitHub Actions to create pull requests.
The workflow uses `GITHUB_TOKEN`. Its pull request does not start another CI run.
The weekly workflow runs the checks before it creates the pull request.
