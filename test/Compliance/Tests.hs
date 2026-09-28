{-# LANGUAGE OverloadedStrings #-}
module Compliance.Tests (testCompliance) where

import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Aihc.Cabal as A
import Compliance.Adapter (toCabal)
import Compliance.Compare
import qualified Distribution.PackageDescription.Parsec as C

assert :: String -> Bool -> IO ()
assert label success = unless success (fail label)

header :: BS.ByteString
header = "cabal-version: 3.0\nname: sample\nversion: 1.0\n"

value :: Text -> A.FieldValue
value text = A.FieldValue (A.Position 1 1) [A.FieldLine (A.Position 1 1) text]

testCompliance :: IO ()
testCompliance = do
  forM_ ["1.0", "1.2", "1.4", "1.6", "1.8"] $ \spec ->
    forM_ ["", ">="] $ \prefix -> do
      let bytes = "cabal-version: " <> prefix <> spec
            <> "\nname: sample\nversion: 1\nbuild-type: Simple\nlibrary\n  exposed-modules: Sample\n  build-depends: base >=3 && <5\n"
      assert ("Compare older format " ++ BSC.unpack (prefix <> spec))
        (outcome (compareBytes bytes) == Match)
  forM_
    [ "synopsis: Sample text  \nauthor: A. Person \nhomepage: https://example.com/ \ndescription: First line  \n  Second line \nlibrary  \n  exposed-modules: Sample\n"
    , "library\n  exposed-modules: Sample\n  build-depends: base >=4 && <5\n"
    , "synopsis: Sample text \r\n  \r\nlibrary \r\n  -- A comment\r\n  exposed-modules: Sample \r\n"
    , "library\n"
    , "library\n  -- No fields\n"
    , "flag fast\nlibrary\n  if flag(fast)\n  else\n    cpp-options: -DSLOW\n"
    , "library\n  if True\n    cpp-options: -DFAST\n  else\n"
    , "library\n  if True\n  else\n  other-modules: Sample\n"
    , "library\n  if True\n    if False\n    else\n  else\n    cpp-options: -DSLOW\n"
    , "common shared\nlibrary\n  import: shared\n"
    , "library internal\nlibrary\nexecutable tool\n  main-is: Main.hs\n"
    , "executable tool\n"
    , "flag fast\n  default: False\nlibrary\n  if flag(fast)\n    cpp-options: -DFAST\n  else\n    buildable: False\n"
    , "common shared\n  hs-source-dirs: src\n  ghc-options: -Wall\nlibrary\n  import: shared\n"
    , "library internal\n  exposed-modules: Internal\nexecutable tool\n  main-is: Main.hs\n  build-depends: sample:internal\n"
    , "test-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Test.hs\nbenchmark bench\n  type: exitcode-stdio-1.0\n  main-is: Bench.hs\n"
    , "foreign-library native\n  type: native-shared\n  options: standalone\n  c-sources: native.c\n"
    , "synopsis: A sample\nauthor: A. Person\nlibrary\n  extra-libraries: z\n  x-example: retained\n"
    , "source-repository head\n  type: git\n  location: https://example.com/sample\nlibrary\n  buildable: True\n"
    , "source-repository head\n  type: cvs\n  location: example.com:/source\n  module: sample\n  branch: main\nsource-repository this\n  type: git\n  location: https://example.com/sample\n  tag: v1.0\n  subdir: \"source files\"\nsource-repository head\n  type: darcs\n  location: https://example.com/mirror\n"
    , "source-repository head\n  type: hg\n  location: https://example.com/first\n  location: https://example.com/second\n  x-note: first\n    second\n"
    , "source-repository future\n  type: future\n"
    , "source-repository HEAD\n  type: GIT\n  location: https://example.com/sample\n"
    , "source-repository head\n  type: git\n  location: https://example.com/sample\n  subdir:\n"
    , "flag fast\n  description: Use fast code\nlibrary\n  buildable: True\n"
    , "flag fast\n  description: First line\n    second line\n    .\n    Last line\n  default: False\n  manual: True\n"
    , "flag fast\n  description:\nflag slow\n  description: \"Use slow code\"\n"
    , "description:\n  First line\n  Second line\nflag fast\n  description:\n    First line\n    Second line\n"
    , "description: First line\n\n    Indented line\n  .\n  Last line\nlibrary\n"
    , "build-type: Custom\ncustom-setup\n  setup-depends: base, Cabal\nlibrary\n  buildable: True\n"
    , "library { exposed-modules: Sample }\n"
    , "library\n  {\n    exposed-modules: Sample\n  }\n  if os(linux) {\n    cpp-options: -DLINUX\n  } else {\n    cpp-options: -DOTHER\n  }\n"
    , "library\n\tbuildable: True\n\texposed-modules: Sample\n"
    , "library\n    buildable: True\n  other-modules: Sample\n"
    , "Synopsis  : Text\nlibrary\n  Default-Language : Haskell2010\n"
    , "library\n  else\n    buildable: False\n"
    , "library\n  other-modules: Sample\n  import: missing\n"
    , "unknown-section\n  field: value\nlibrary\n"
    , "library\n  build-depends: base, base >=4\n  hs-source-dirs: src src\n"
    , "common one\n  build-depends: base\n  hs-source-dirs: src\n  frameworks: A\nlibrary\n  import: one\n  build-depends: base\n  hs-source-dirs: src\n  frameworks: A\n"
    , "common one\n  exposed-modules: Lost\n  visibility: public\n  x-a: 1\nlibrary\n  import: one\n  x-b: 2\n  exposed-modules: Sample\n"
    , "library\n  x-b: 2\n  x-a: 1\n  x-b: 3\n"
    , "library\n  mixins: base hiding (Prelude), containers (Data.Map as Map) requires (Sig as Impl)\n"
    , "library\n  default-language: Haskell98\n  default-language: Haskell2010\n  buildable: False\n  buildable: True\n"
    , "library\n  build-depends:\n    , base\n    , containers\n  other-modules:\n    , A\n    , B\n"
    , "flag Fast\n  default: false\nlibrary\n  if flag(FAST) || !impl(ghc >= 9.0) && os(Linux)\n    buildable: False\n  if impl(ghc == 9.*) || impl(ghc >= 7 && < 8) || true\n    buildable: True\n"
    , "library\n  if arch(x86_64)\n    buildable: False\n  elif os(windows)\n    buildable: False\n  else\n    buildable: True\n"
    , "library\r  exposed-modules: Sample\r  other-modules: Other\r"
    , "library\n  -- comment\n  exposed-modules:\n    Sample\n    -- comment\n\n    Other\n"
    , "library\n  build-depends: base >=4 && <5 || ==3.* , text ^>=2.0\n"
    ] $ \body -> do
      let result = compareBytes (header <> body)
      assert ("Expected equal structures: " ++ show result ++ "\n" ++ BSC.unpack body) (outcome result == Match)
  forM_ ["1.10", "2.0"] $ \spec ->
    forM_
      [ "  extensions: CPP, ForeignFunctionInterface\n"
      , "  extensions: CPP\n  default-extensions: OverloadedStrings\n"
      , "  default-extensions: CPP\n  extensions: CPP\n"
      , "  extensions: CPP\n  if os(linux)\n    extensions: ForeignFunctionInterface\n"
      ] $ \body -> do
        let bytes = "cabal-version: " <> spec <> "\nname: sample\nversion: 1\nbuild-type: Simple\nlibrary\n" <> body
        assert ("Keep extension field values: " ++ show (compareBytes bytes) ++ BSC.unpack bytes)
          (outcome (compareBytes bytes) == Match)
  forM_
    [ "^>=1", "^>=1.2.3", "^>={1.2,2.3,3.4}"
    , ">=1 && <2 && >1.1", "==1 || ==2 || ==3"
    , "(==1 || ==2) || ==3", "^>=1.2 && (<1.3 || ==2)"
    ] $ \range -> do
      let bytes = header <> "library\n  build-depends: base " <> range <> "\n"
      assert ("Keep version range structure: " ++ BSC.unpack range)
        (outcome (compareBytes bytes) == Match)
  let extensionBytes = "cabal-version: 2.2\nname: sample\nversion: 1\nbuild-type: Simple\ncommon shared\n  extensions: CPP\nlibrary\n  import: shared\n  default-extensions: OverloadedStrings\n"
  extensionPackage <- either (fail . show) pure (A.parseValue (A.parsePackage extensionBytes))
  extensionReference <- either (fail . show) pure
    (snd (C.runParseResult (C.parseGenericPackageDescription extensionBytes)))
  assert "Convert imported older extensions"
    (fst (comparePackage extensionPackage extensionReference) == Match)
  let changedExtensions = extensionPackage { A.packageComponents =
        [component { A.componentData = tree { A.unconditional =
            (A.unconditional tree) { A.legacyExtensions = ["BangPatterns"] } } }
        | component <- A.packageComponents extensionPackage, let tree = A.componentData component] }
  assert "Use older extensions from the AST"
    (fst (comparePackage changedExtensions extensionReference) == Mismatch)
  forM_ ["==1 || ==2 || ==3", ">=1 && <3 && >1.1", "(==1 || ==2) || ==3"] $ \range -> do
    let bytes = header <> "library\n  if impl(ghc " <> range <> ")\n    buildable: False\n"
    assert "Keep compiler range structure" (outcome (compareBytes bytes) == Match)
  let toolBytes = header <> "library\n  build-tool-depends: alex:alex ^>=3.2.4\n  if impl(ghc ^>=9.2)\n    buildable: False\n"
  assert "Keep tool and compiler major bounds" (outcome (compareBytes toolBytes) == Match)
  forM_
    [ (">=1.10", "name: old\nversion: 1\nexposed-modules: Old\nbuild-depends: base\nexecutable: tool\nmain-is: Main.hs\n")
    , (">=1.10", "name: old\nversion: 1\nbuild-type: Default\nlibrary\n  cxx-sources: a.cpp\n  autogen-modules: A\n  default-language: Haskell2010\n")
    , (">=1.10", "name: old\nversion: 1\nlibrary\n  build-depends: sub\n  mixins: sub\nlibrary sub\n")
    , (">=1.10", "name: old\nversion: 1\nlibrary\n  if os(linux)\n    buildable: False\n  elif os(osx)\n    buildable: False\n")
    , (">=1.2", "name: old\nversion: 1\ndescription: First\n  .\n  Second\nlibrary\n  default-language: Haskell2010\n")
    , ("3.0", "name: old\nversion: 1\nlibrary\n  build-depends: sub, sub:{sub, other}\n  mixins: sub\nlibrary sub\nlibrary other\n")
    , ("2.2", "name: old\nversion: 1\ncommon one\n  build-depends: base\nlibrary\n  import: one\n  if os(linux)\n    import: one\n")
    , (">=0.9", "name: old\nversion: 1\n")
    ] $ \(spec, body) -> do
      let bytes = "cabal-version: " <> spec <> "\n" <> body
          result = compareBytes bytes
      assert ("Expected equal structures for an older format: " ++ show result ++ "\n" ++ BSC.unpack bytes) (outcome result == Match)
  let setupBytes = header <> "build-type: Custom\ncustom-setup\n  setup-depends: base, Cabal\nlibrary\n  buildable: True\n"
  setupPackage <- either (fail . show) pure (A.parseValue (A.parsePackage setupBytes))
  setupReference <- either (fail . show) pure (snd (C.runParseResult (C.parseGenericPackageDescription setupBytes)))
  assert "Lost data must fail equality"
    (fst (comparePackage setupPackage { A.packageSetupDependencies = Nothing } setupReference) == Mismatch)
  assert "Both parsers reject invalid input" (outcome (compareBytes "not a package") == BothRejected)
  assert "Count a reference parse error" (outcome (compareBytes (header <> "test-suite bad\n  type: exitcode-stdio-1.0\n")) == ReferenceError)
  let bytes = header <> "library\n  exposed-modules: Sample\n"
  pkg <- either (fail . show) pure (A.parseValue (A.parsePackage bytes))
  ref <- either (fail . show) pure (snd (C.runParseResult (C.parseGenericPackageDescription bytes)))
  let changed = pkg { A.packageName = "changed" }
  assert "Use the typed AST instead of the original name field" (fst (comparePackage changed ref) == Mismatch)
  let oldSpelling = pkg { A.packageFields = Map.insert "version" [value "1.00"] (A.packageFields pkg) }
  assert "Convert typed versions without reading the old spelling" (fst (comparePackage oldSpelling ref) == Match)
  let changedModule = pkg { A.packageComponents =
        [A.Component (A.Library Nothing) (A.Conditional
          (A.emptyBuildInfo { A.exposedModules = ["Changed"] }) [])] }
  assert "Compare component fields" (fst (comparePackage changedModule ref) == Mismatch)
  let invalid = pkg { A.packageComponents =
        [A.Component (A.Library Nothing) (A.Conditional
          (A.emptyBuildInfo { A.defaultLanguage = Just "invalid language" }) [])] }
  assert "Count conversion errors" (fst (comparePackage invalid ref) == ConversionError)
  let metadata = pkg { A.packageFields = Map.insert "synopsis" [value "Changed"] (A.packageFields pkg) }
  assert "Convert retained metadata" (fst (comparePackage metadata ref) == Mismatch)
  let repositoryBytes = header <> "source-repository head\n  type: git\n  location: https://example.com/sample\n"
  repositoryPackage <- either (fail . show) pure (A.parseValue (A.parsePackage repositoryBytes))
  repositoryReference <- either (fail . show) pure
    (snd (C.runParseResult (C.parseGenericPackageDescription repositoryBytes)))
  let changedRepository = repositoryPackage { A.packageSourceRepositories =
        [A.SourceRepository "this" (Map.fromList [("type", [value "git"]), ("tag", [value "v1.0"])])] }
  assert "Compare repository data"
    (fst (comparePackage changedRepository repositoryReference) == Mismatch)
  let removedRepository = repositoryPackage { A.packageSourceRepositories = [] }
  assert "Detect a missing repository"
    (fst (comparePackage removedRepository repositoryReference) == Mismatch)
  converted <- either fail pure (toCabal pkg)
  assert "Full Cabal equality" (converted == ref)
  forM_ ["1.10", "2.0", "3.0"] $ \spec -> do
    let flagBytes = "cabal-version: " <> spec <> "\nname: sample\nversion: 1\nbuild-type: Simple\nflag fast\n  description: First line\n    second line\n    .\n    Last line\n  default: False\n  manual: True\n"
    flagPackage <- either (fail . show) pure (A.parseValue (A.parsePackage flagBytes))
    flagReference <- either (fail . show) pure
      (snd (C.runParseResult (C.parseGenericPackageDescription flagBytes)))
    let expected = if spec == "3.0" then "First line\nsecond line\n.\nLast line" else "First line\nsecond line\n\nLast line"
    assert "Keep flag description text"
      (A.packageFlags flagPackage == [A.Flag "fast" False True expected])
    assert "Compare flag descriptions" (fst (comparePackage flagPackage flagReference) == Match)
    let changedFlag = flagPackage { A.packageFlags =
          [f { A.flagDescription = "Changed" } | f <- A.packageFlags flagPackage] }
    assert "Use flag descriptions from the AST"
      (fst (comparePackage changedFlag flagReference) == Mismatch)
