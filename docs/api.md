# API use and limits

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
`flagDescription` keeps the description text, including line breaks and dot lines.
An absent description has the value `""`.
The `Flag` constructor has a new final argument for the description.
Add `""` to constructor calls that have no description.
Set `flagDescription` when you create a `Flag` with record syntax.
Update constructor patterns for the new argument.

`packageSourceRepositories` contains source repository sections in source order.
Each `SourceRepository` contains a kind and a map of field names to text values.
Repeated fields keep their values in source order. The parser keeps unknown fields.
The values keep quotation marks for conversion by the caller.

The `Package` constructor has a new final argument for source repositories.
Add `[]` to constructor calls that have no source repositories.
Set `packageSourceRepositories` when you create a `Package` with record syntax.
Update constructor patterns for the new argument.

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
- Source repository sections, with their kinds and fields stored as text.
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
- The parser does not interpret or keep `custom-setup` section contents.
- The parser does not validate source repository fields. Nested sections produce errors.
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

## Test fixtures

The files in `test/fixtures` contain fields from aihc revision
`21ada6eb2d668904bb2cacbb5be25bad692975d6`.
They cover `aihc-hackage`, `aihc-package-plan`, and `aihc-haddock`.
Their source files use the Unlicense.
