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

For `VersionRange` equality, a `^>=` bound and its expanded intersection are different values.
Use `withinRange` to test version membership.
Range rendering keeps `^>=` bounds.
Unparenthesized `&&` and `||` operators associate to the right in dependency ranges, as in Cabal-syntax.
In `impl` conditions, these operators associate to the left.
Explicit parentheses keep the specified structure.

`Package` contains flag declarations and conditional components.
`flagDescription` contains the description text after the Cabal free text rules.
Before `cabal-version` 3.0, each line has no leading or trailing spaces, and a dot line becomes an empty line.
From `cabal-version` 3.0, the text keeps blank lines and relative indentation.
An absent description has the value `""`.

`cabalVersion` contains the Cabal specification version that Cabal-syntax uses for the file.
For example, `cabal-version: >=1.9` gives version 1.10.
A file without a `cabal-version` field has version 1.0.
`buildType` contains the build type that Cabal-syntax uses.
Without a `build-type` field, the value is `Simple` from `cabal-version` 2.2.
If a `custom-setup` section is present, the value is `Custom`.
Before `cabal-version` 2.2, the value is `Custom`.

## Field values

`packageFields`, `extraFields`, and the fields of a `SourceRepository` contain `FieldValue` values.
A `FieldValue` contains the position of the field name and each line of the value with its position.
Use `fieldText` to get the lines joined with line breaks.
Columns count UTF-8 bytes, as in Cabal-syntax.
The parser does not include comment lines and blank lines in the lines.
Cabal free text rules and the order of `x-` fields use these positions.

`packageFields` contains all fields before the first section, in source order for each name.
This includes `name`, `version`, `cabal-version`, and `build-type`.
Cabal-syntax ignores package fields after the first section. The parser also ignores them.

To migrate from version 0.1 of the field maps, replace each `Text` value with `fieldText value`.
To make a new value, use `FieldValue (Position 1 1) [FieldLine (Position 1 1) text]`.

`packageSourceRepositories` contains source repository sections in source order.
Each `SourceRepository` contains a kind and a map of field names to values.
Repeated fields keep their values in source order. The parser keeps unknown fields.
The values keep quotation marks for conversion by the caller.

`packageSetupDependencies` contains the `setup-depends` values of a `custom-setup` section.
The value is `Nothing` if the file has no `custom-setup` section.
Set it to `Nothing` when you create a `Package` without this data.

## Build information

`VersionRange` exports its constructors.
Use them to examine, simplify, or show a range in a different notation.
`parseVersionRange` accepts the range syntax of all Cabal format versions.
The package parser applies the rules of the file's Cabal format version.

`resolvePackage` applies explicit flags over flag defaults.
It evaluates conditions and merges active fields.
It returns all components, including components with `buildable: False`.
The caller selects the components to build.
Condition evaluation compares `os`, `arch`, and compiler names without case.
Use lower case for the keys of `compilerVersions`.

`BuildInfo` stores partial fields. `Nothing` means that a scalar field is absent.
`extensions` contains the `default-extensions` values.
`legacyExtensions` contains the older `extensions` values.
`otherExtensions` contains the `other-extensions` values.
Use `extensions` and `legacyExtensions` when you select compiler extensions.

`BuildInfo` also has these typed fields:
`virtualModules`, `otherLanguages`, `mixins`, `asmSources`, `cmmSources`, `jsSources`,
`includes`, `extraLibDirs`, `extraLibDirsStatic`, `frameworks`, and `extraFrameworkDirs`.
These fields are not in `extraFields`.
Use record syntax and `emptyBuildInfo` to make a `BuildInfo` value.
Positional constructor calls and patterns must add the new fields.

In one section, list fields keep all values, also repeated values.
For a field with one value, the last value wins, as in Cabal-syntax.
`sourceDirs` contains the `hs-source-dirs` values and then the older `hs-source-dir` values.
`buildTools` contains the `build-tools` values and then the `build-tool-depends` values.

`mergeBuildInfo` merges two parts as Cabal-syntax merges build information.
The parser uses it for common stanza imports.
Some lists do not keep a value that occurs in both parts.
Examples are `dependencies`, `sourceDirs`, and `otherModules`.
Options, `mixins`, `buildTools`, and `exposedModules` keep all values.

A field that the file's Cabal format version does not support is absent.
For example, before `cabal-version` 2.2, `cxx-sources` is absent.
Before `cabal-version` 1.10, `default-language` and `default-extensions` are absent.
Fields that the format version removed cause errors.

Resolution sets an absent `buildable` to `True`.
Resolution sets an empty source directory list to `["."]`.
An absent language stays `Nothing`. The caller can use this to detect the Haskell98 default.
Do not apply defaults before condition evaluation.

`parseBuildInfo` reads configure output from `.buildinfo` files.
It returns library fields and executable fields separately.
The caller applies these fields to the build inputs.

## Scope

The parser follows the package parser of Cabal-syntax 3.12.1.0.
The tests compare the parser with Cabal-syntax 3.18.1.0.
It supports these features:

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

The parser accepts format versions from 1.0 through 3.14.
Cabal-syntax 3.18 also accepts format version 3.16. This parser does not.

The parser has these limits:

- Cabal-syntax changes 59 known Hackage files before it parses them. This parser does not.
- Package fields not used by this API stay in `packageFields` as text.
- Component fields not used by this API stay in `extraFields` as text.
- The parser does not validate these text fields. Cabal-syntax can reject a value that this parser keeps.
- The parser does not check that each test suite, benchmark, and foreign library has a type.
- Syntax diagnostics identify the field or section line. Package checks can report line 0.
  Diagnostics do not identify an exact value column.
- The parser stops at the first error. `parseWarnings` is reserved and is currently empty.
- Version rendering preserves meaning. It does not preserve the original spelling.
- No package-file printer or version-range simplifier is provided.

Library targets remain separate from package names.
Before `cabal-version` 3.4, a dependency or mixin on a declared internal library name refers to the current package.
From `cabal-version` 3.4, a dependency name always identifies a package, as in Cabal.
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
