module Compliance.Compare
  ( Outcome (..), Comparison (..), compareBytes, comparePackage, outcomeName ) where

import qualified Data.ByteString as BS
import qualified Aihc.Cabal as A
import Compliance.Adapter (runResult, toCabal)
import qualified Distribution.PackageDescription as C
import qualified Distribution.PackageDescription.Parsec as C

data Outcome = Match | Mismatch | ParserError | ReferenceError | BothRejected | ConversionError
  deriving (Eq, Ord, Show, Enum, Bounded)

data Comparison = Comparison
  { outcome :: Outcome
  , parserAccepted :: Bool
  , referenceAccepted :: Bool
  , parserWarnings :: Int
  , referenceWarnings :: Int
  , detail :: String
  } deriving (Eq, Show)

outcomeName :: Outcome -> String
outcomeName value = case value of
  Match -> "match"
  Mismatch -> "mismatch"
  ParserError -> "parser_error"
  ReferenceError -> "reference_error"
  BothRejected -> "both_rejected"
  ConversionError -> "conversion_error"

compareBytes :: BS.ByteString -> Comparison
compareBytes bytes = case (A.parseValue ours, reference) of
  (Left errors, Left refErrors) -> report BothRejected False False (show errors ++ "\n" ++ show refErrors)
  (Left errors, Right _) -> report ParserError False True (show errors)
  (Right _, Left errors) -> report ReferenceError True False (show errors)
  (Right pkg, Right ref) -> let (status, message) = comparePackage pkg ref
                          in report status True True message
  where
    ours = A.parsePackage bytes
    (warnings, reference) = runResult (C.parseGenericPackageDescription bytes)
    report status accepted refAccepted message = Comparison status accepted refAccepted
      (length (A.parseWarnings ours)) (length warnings) message

comparePackage :: A.Package -> C.GenericPackageDescription -> (Outcome, String)
comparePackage pkg reference = case toCabal pkg of
  Left errors -> (ConversionError, errors)
  Right converted
    | converted == reference -> (Match, "")
    | otherwise -> (Mismatch, "Different fields: " ++ show (differences converted reference))

-- These labels explain failures. Equality always uses the complete value.
differences :: C.GenericPackageDescription -> C.GenericPackageDescription -> [String]
differences a b = [name | (name, different) <-
  [ ("packageDescription", C.packageDescription a /= C.packageDescription b)
  , ("gpdScannedVersion", C.gpdScannedVersion a /= C.gpdScannedVersion b)
  , ("genPackageFlags", C.genPackageFlags a /= C.genPackageFlags b)
  , ("condLibrary", C.condLibrary a /= C.condLibrary b)
  , ("condSubLibraries", C.condSubLibraries a /= C.condSubLibraries b)
  , ("condForeignLibs", C.condForeignLibs a /= C.condForeignLibs b)
  , ("condExecutables", C.condExecutables a /= C.condExecutables b)
  , ("condTestSuites", C.condTestSuites a /= C.condTestSuites b)
  , ("condBenchmarks", C.condBenchmarks a /= C.condBenchmarks b)
  ], different]
