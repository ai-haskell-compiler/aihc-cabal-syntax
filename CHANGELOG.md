# Changelog

All notable changes to this project are recorded in this file.

This project uses the format from [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

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
