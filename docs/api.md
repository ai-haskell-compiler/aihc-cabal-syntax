# API use and limits

The Haddock documentation of the `Aihc.Cabal` module is the reference for
each type and function. This page gives the use, the scope, and the limits.

## Use

`Aihc.Cabal` is the only public module. Import it qualified.

```haskell
import qualified Aihc.Cabal as Cabal
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

loadLibrary :: FilePath -> IO Cabal.BuildInfo
loadLibrary path = do
  bytes <- BS.readFile path
  package <- either (fail . show) pure (Cabal.parseValue (Cabal.parsePackage bytes))
  ghc <- either (fail . show) pure (Cabal.parseVersion "9.12.2")
  let environment = Cabal.Environment "linux" "x86_64" "ghc" ghc
      resolved = Cabal.resolvePackage environment Map.empty package
  case [bi | Cabal.Component (Cabal.Library Cabal.MainLibrary) bi <- Cabal.resolvedComponents resolved] of
    [library] -> pure library
    _ -> fail "No main library"
```

The flow has three steps:

1. `parsePackage` reads the bytes and gives a `Package`. Conditions stay in
   the `Conditional` values of each component. A dependency solver can
   inspect them before it selects flags.
2. `resolvePackage` evaluates the conditions for an `Environment` and a
   `FlagAssignment`. It merges the active parts of each component and
   applies defaults. Unknown flags in the assignment have no effect.
3. The caller selects the components to build. The result has all
   components, also components with `buildable: False`.

The `Environment` names the target, not the host. Set `compiler` and
`compilerVersion` to the compiler that the target emulates. The library does
not read the host platform or an installed compiler.

Fields that the API does not type stay as text. Use `packageFieldText` for
package fields such as `description`. It applies the Cabal free text rules of
the file's format version. Use `fieldText` for component fields in
`extraFields`, for example `x-aihc-lir-sources`.

`parseHookedBuildInfo` reads the `.buildinfo` file that a configure script
writes. The caller applies its fields to the build inputs.

`parseDependency` reads one `build-depends` entry, for example `base >=4 && <5`.
`parsePackageIdentifier` reads a package name with an optional version, for example `foo-1.2`.
Use them for values outside a Cabal file, such as command line arguments.
They follow `simpleParsec` of Cabal-syntax. They use the rules of the newest Cabal format version,
so `-any` and `-none` are not accepted. Spaces after the value are permitted. Spaces before the value are not permitted.

## Scope

The parser follows the package parser of Cabal-syntax 3.18.1.0. The tests
compare the parser with this version. It supports these features:

- UTF-8 input, indentation with spaces or tabs, comments, and multiline fields.
- Explicit braces for sections and fields.
- Spaces between a field name and its colon, and line breaks with `\r`.
- Files without sections. The parser moves their build fields into a library and executables, as Cabal does.
- Main libraries, named libraries, executables, tests, benchmarks, and foreign libraries.
- Flags, Boolean conditions, `os`, `arch`, `impl`, and nested `if`/`elif`/`else` sections.
- Common stanzas and imports from earlier common stanzas.
- Source repository sections and `custom-setup` sections.
- Package dependencies, library targets, mixins, and modern and legacy build tools.
- The rules of each Cabal format version for list separators, version ranges, and fields.
- The Cabal-syntax rules for repeated fields, unknown fields, and unknown sections.
- The Cabal-syntax patches for 57 known Hackage files. A patched file gets the warning `Legacy cabal file`.

The parser accepts format versions from 1.0 through 3.18.
The build type `Hooks` is available from format version 3.14. It needs a `custom-setup` section.
The build type `Make` is not available from format version 3.18.
Absolute paths are permitted in `hs-source-dirs`, as in Cabal-syntax 3.18.

Before `cabal-version` 3.4, a dependency or mixin on a declared internal library name refers to the current package.
The parser changes such a dependency to the package name and the `NamedLibrary` target.
From `cabal-version` 3.4, a dependency name always identifies a package, as in Cabal.

## Limits

- Package fields other than `name`, `version`, `cabal-version`, and `build-type` stay in `packageFields` as text.
- Component fields without a `BuildInfo` field stay in `extraFields` as text.
- The parser does not validate these text fields. Cabal-syntax can reject a value that this parser keeps.
- The parser does not check that each test suite, benchmark, and foreign library has a type.
- Syntax diagnostics identify the field or section line. Checks of the complete package have no position.
  Diagnostics do not identify an exact value column.
- The parser stops at the first error.
- Version range rendering preserves structure. It does not preserve the original spelling.
- No package printer and no version range simplifier is provided.
  `intersectRanges` and `unionRanges` build a larger range value each time.

The library does not solve dependencies, search for source files, run configure scripts,
generate `Paths_*` modules, or manage installed packages.
It has no `Compat` API, package database types, unit IDs, or GHC installation types.

## Test fixtures

The files in `test/fixtures` contain fields from aihc revision
`21ada6eb2d668904bb2cacbb5be25bad692975d6`.
They cover `aihc-hackage`, `aihc-package-plan`, and `aihc-haddock`.
Their source files use the Unlicense.
