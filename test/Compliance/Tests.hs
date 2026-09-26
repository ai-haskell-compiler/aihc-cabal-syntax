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
  forM_
    [ "library\n  exposed-modules: Sample\n  build-depends: base >=4 && <5\n"
    , "flag fast\n  default: False\nlibrary\n  if flag(fast)\n    cpp-options: -DFAST\n  else\n    buildable: False\n"
    , "common shared\n  hs-source-dirs: src\n  ghc-options: -Wall\nlibrary\n  import: shared\n"
    , "library internal\n  exposed-modules: Internal\nexecutable tool\n  main-is: Main.hs\n  build-depends: sample:internal\n"
    , "test-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Test.hs\nbenchmark bench\n  type: exitcode-stdio-1.0\n  main-is: Bench.hs\n"
    , "foreign-library native\n  type: native-shared\n  options: standalone\n  c-sources: native.c\n"
    , "synopsis: A sample\nauthor: A. Person\nlibrary\n  extra-libraries: z\n  x-example: retained\n"
    ] $ \body -> do
      let result = compareBytes (header <> body)
      assert ("Expected equal structures: " ++ show result ++ "\n" ++ BSC.unpack body) (outcome result == Match)
  forM_
    [ "flag fast\n  description: This text is lost\nlibrary\n  buildable: True\n"
    , "source-repository head\n  type: git\n  location: https://example.com/sample\nlibrary\n  buildable: True\n"
    , "build-type: Custom\ncustom-setup\n  setup-depends: base, Cabal\nlibrary\n  buildable: True\n"
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
  converted <- either fail pure (toCabal pkg)
  assert "Full Cabal equality" (converted == ref)
