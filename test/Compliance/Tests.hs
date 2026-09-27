{-# LANGUAGE OverloadedStrings #-}
module Compliance.Tests (testCompliance) where

import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Aihc.Cabal as A
import Compliance.Adapter (toCabal)
import Compliance.Compare
import qualified Distribution.PackageDescription.Parsec as C

assert :: String -> Bool -> IO ()
assert label success = unless success (fail label)

header :: BS.ByteString
header = "cabal-version: 3.0\nname: sample\nversion: 1.0\n"

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
  let toolBytes = header <> "library\n  build-tool-depends: alex:alex ^>=3.2.4\n  if impl(ghc ^>=9.2)\n    buildable: False\n"
  assert "Keep tool and compiler major bounds" (outcome (compareBytes toolBytes) == Match)
  forM_
    [ "build-type: Custom\ncustom-setup\n  setup-depends: base, Cabal\nlibrary\n  buildable: True\n"
    ] $ \body -> assert "Lost data must fail equality" (outcome (compareBytes (header <> body)) == Mismatch)
  assert "Both parsers reject invalid input" (outcome (compareBytes "not a package") == BothRejected)
  assert "Count a project parse error" (outcome (compareBytes "name: old\nversion: 1\n") == ParserError)
  assert "Count a reference parse error" (outcome (compareBytes (header <> "test-suite bad\n  type: exitcode-stdio-1.0\n")) == ReferenceError)
  let bytes = header <> "library\n  exposed-modules: Sample\n"
  pkg <- either (fail . show) pure (A.parseValue (A.parsePackage bytes))
  ref <- either (fail . show) pure (snd (C.runParseResult (C.parseGenericPackageDescription bytes)))
  let changed = pkg { A.packageName = "changed" }
  assert "Use the typed AST instead of the original name field" (fst (comparePackage changed ref) == Mismatch)
  let oldSpelling = pkg { A.packageFields = Map.insert "version" ["1.00"] (A.packageFields pkg) }
  assert "Convert typed versions without reading the old spelling" (fst (comparePackage oldSpelling ref) == Match)
  let changedModule = pkg { A.packageComponents =
        [A.Component (A.Library Nothing) (A.Conditional
          (A.emptyBuildInfo { A.exposedModules = ["Changed"] }) [])] }
  assert "Compare component fields" (fst (comparePackage changedModule ref) == Mismatch)
  let invalid = pkg { A.packageComponents =
        [A.Component (A.Library Nothing) (A.Conditional
          (A.emptyBuildInfo { A.defaultLanguage = Just "invalid language" }) [])] }
  assert "Count conversion errors" (fst (comparePackage invalid ref) == ConversionError)
  let metadata = pkg { A.packageFields = Map.insert "synopsis" ["Changed"] (A.packageFields pkg) }
  assert "Convert retained metadata" (fst (comparePackage metadata ref) == Mismatch)
  let repositoryBytes = header <> "source-repository head\n  type: git\n  location: https://example.com/sample\n"
  repositoryPackage <- either (fail . show) pure (A.parseValue (A.parsePackage repositoryBytes))
  repositoryReference <- either (fail . show) pure
    (snd (C.runParseResult (C.parseGenericPackageDescription repositoryBytes)))
  let changedRepository = repositoryPackage { A.packageSourceRepositories =
        [A.SourceRepository "this" (Map.fromList [("type", ["git"]), ("tag", ["v1.0"])])] }
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
    assert "Keep flag description text"
      (A.packageFlags flagPackage == [A.Flag "fast" False True "First line\nsecond line\n.\nLast line"])
    assert "Compare flag descriptions" (fst (comparePackage flagPackage flagReference) == Match)
    let changedFlag = flagPackage { A.packageFlags =
          [f { A.flagDescription = "Changed" } | f <- A.packageFlags flagPackage] }
    assert "Use flag descriptions from the AST"
      (fst (comparePackage changedFlag flagReference) == Mismatch)
