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

The Nix check compares the full report with `tests/hackage/baseline.json`.
It also verifies the total case count and the failure record counts.
A change in counts causes a check failure.
Review the failures before you update the baseline.

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

Build the comparison command:

```sh
nix build --no-update-lock-file
```

Run it on the selected file:

```sh
./result/bin/hackage-compliance --file case.cabal
```

## Fixed dependencies

The Nixpkgs revision from `origin/main` stays unchanged.
`flake.lock` sets exact versions of GHC, Cabal-syntax, Python, curl, and the other tools and libraries.
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
