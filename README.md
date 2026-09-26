# aihc-cabal-syntax

Parse `.cabal` files with a small, pure Haskell API.
This library uses Megaparsec. It has no runtime dependency on Cabal or Cabal-syntax.

## Use

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Aihc.Cabal
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

loadPackage path = do
  bytes <- BS.readFile path
  case parseValue (parsePackage bytes) of
    Left errors -> print errors
    Right package -> do
      let environment = Environment "linux" "x86_64" Map.empty
      print (resolvePackage environment Map.empty package)
```

Set `compilerVersions` to the compiler versions that the target emulates.
For example, use `Map.singleton "ghc" version` to evaluate `impl(ghc ...)`.
Use `parseVersion` to construct `version`.
The library does not read the host platform or the installed compiler.

`Package` contains flag declarations and conditional components.
`resolvePackage` applies explicit flags over flag defaults.
It evaluates conditions and merges active fields.
It returns all components, including components with `buildable: False`.
The caller selects the components to build.

`BuildInfo` stores partial fields. `Nothing` means that a scalar field is absent.
Resolution sets an absent `buildable` to `True`.
Resolution sets an empty source directory list to `["."]`.
An absent language stays `Nothing`. The caller can use this to detect the Haskell98 default.
Do not apply defaults before condition evaluation.

`parseBuildInfo` reads configure output from `.buildinfo` files.
It returns library fields and executable fields separately.
The caller applies these fields to the build inputs.

## MVP scope

The parser supports these features:

- UTF-8 input, indentation, complete-line comments, and multiline fields.
- Package name, version, build type, and Cabal format version.
- Main libraries, named libraries, executables, tests, benchmarks, and foreign libraries.
- Flags, Boolean conditions, `os`, `arch`, `impl`, and nested `if`/`else` sections.
- Common stanzas and imports from earlier common stanzas.
- Package dependencies, library targets, and modern and legacy build tools.
- Haskell source fields, language fields, extensions, C and C++ source fields, headers, and compiler options.
- Quoted paths and options.
- Custom fields, including `x-aihc-lir-sources`.
- Version comparisons, intersections, unions, wildcards, major bounds, and version sets.

The parser accepts format versions from 1.10 through 3.14.
It supports an exact `cabal-version` and the older `>=` form before 2.2.
This range is an input limit, not a claim of complete format conformance.

The MVP has these limits:

- Explicit layout braces, semicolon layout, and `elif` sections are not supported.
- Signatures, mixins, and module reexports produce errors, including in inactive branches.
- Package fields not used by this API stay in `packageFields` as text.
- Component fields not used by this API stay in `extraFields` as text.
- `custom-setup` and `source-repository` section contents are not interpreted or retained.
- The parser does not perform all Cabal package validation or all format-version checks.
- Duplicate package fields produce errors. Cabal can accept some such inputs with warnings.
- Syntax diagnostics identify the field or section line. Package checks can report line 1.
  Diagnostics do not identify an exact value column.
- The MVP stops at the first error. `parseWarnings` is reserved and is currently empty.
- Version rendering preserves meaning. It does not preserve the original spelling.
- No package-file printer or version-range simplifier is provided.

Library targets remain separate from package names.
A dependency on a declared internal library name is converted to a dependency on the current package.
The parser retains the internal library target.
The solver can inspect `Conditional` values before it selects flags.

The library does not solve dependencies, search for source files, run configure scripts,
generate `Paths_*` modules, or manage installed packages.
It has no `Compat` API, package database types, unit IDs, or GHC installation types.
It retains `ghc-options` and `impl(ghc ...)` because `aihc` uses them.

## Build and test

Run the required checks:

```sh
nix build --no-update-lock-file
nix flake check --no-update-lock-file
```

Use the pinned development environment:

```sh
nix develop
```

`flake.lock` pins Nixpkgs. This pins GHC, Megaparsec, Cabal-syntax, and the other tools and libraries.
The initial pin uses the same Nixpkgs revision as the inspected `aihc` checkout.
Builds and tests do not fetch packages from Hackage.

The tests compare selected results with Cabal-syntax 3.12.1.0 in the Nix environment.
They cover version ranges, common imports, condition selection, source fields,
native build fields, named libraries, configure output, and invalid input.
Cabal-syntax is a test dependency only. The library does not depend on it.
`hackage-compliance` is a Cabal test suite. The installed library has no comparison executable.

## Hackage comparison

Run the full comparison:

```sh
nix build .#hackage-compliance --no-update-lock-file -o result-compliance
cat result-compliance/summary.json
```

The command reads all 200,642 Cabal file revisions in the fixed Hackage index.
It parses each file with this library and Cabal-syntax 3.12.1.0.
It converts our AST to `GenericPackageDescription` and compares the complete values with `(==)`.
It does not select conditions or remove fields before comparison.

`parser_accepted` counts files that this library parses without errors.
`outcomes.match` counts files that both parsers accept and produce equal structures after conversion.
The report separates parse errors, conversion errors, unequal structures, and exceptions.
Warnings do not count as parse errors.

The converter uses our typed fields for dependencies, conditions, flags, and component data.
It uses Cabal field grammars to convert fields that our API retains as text.
It cannot read the original file or the reference result.
Lost sections and flag descriptions cause unequal structures.
This is a data comparison against Cabal-syntax, not a byte-for-byte source comparison.
Cabal-syntax does not retain comments or source formatting in these structures.

See [the Hackage test instructions](docs/hackage.md) for report details and repeat tests.

The files in `test/fixtures` contain package and component fields from `aihc` revision
`21ada6eb2d668904bb2cacbb5be25bad692975d6`.
Unused package metadata was removed.
These fixtures cover `aihc-hackage`, `aihc-package-plan`, and `aihc-haddock`.
Their source files use the Unlicense.

This repository does not yet replace Cabal-syntax in `aihc`.
That change also needs a separate replacement for Cabal's `Paths_*` generator.
