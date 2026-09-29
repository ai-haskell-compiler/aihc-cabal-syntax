# Changelog

All notable changes to this project are recorded in this file.

This project uses the format from [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [1.0.0.0] - 2026-09-29

### Added

- Add a Cabal file parser for the fields that aihc needs.
- Add support for Cabal format versions 1.0, 3.16, and 3.18.
- Add support for repository sections, flag descriptions, extension ranges, and text fields.
- Add tests that compare parser results with Cabal-syntax and test pretty-print round trips.

### Changed

- Expose one module, `Aihc.Cabal`, as the public API.

### Fixed

- Accept empty Cabal sections and syntax used by Hackage packages.
- Apply fixes for known Hackage files.
