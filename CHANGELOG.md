# Changelog

All notable changes to this project are recorded in this file.

This project uses the format from [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [2.0.0.0] - 2026-09-30

### Added

- Add `parseDependency` to read one `build-depends` entry.
- Add `parsePackageIdentifier` to read a package name with an optional version.
- Add `renderDiagnostic` to show a diagnostic as text for a user.
- Add `simplifyVersionRange`. It gives the same versions as a union of separate intervals in increasing order.
- Add `fieldPaths` to read a custom field as a path list, with the rules of `c-sources`.

### Fixed

- Apply the Cabal-syntax aliases for operating system and architecture names.
  The parser changes an alias in `os(...)` to its canonical name, for example `darwin` to `osx`.
  `evaluateCondition` applies the aliases for host names to the `Environment` names.
  A name in `arch(...)` has no aliases, as in Cabal-syntax.

## [1.0.0.1] - 2026-09-29

### Changed

- Add dependency bounds and package metadata.

## [1.0.0.0] - 2026-09-29

### Added

- Add a Cabal parser that is 100% compatible with Cabal-syntax.
