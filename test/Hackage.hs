{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Codec.Archive.Tar as Tar
import Control.Exception (AsyncException, SomeException, evaluate, fromException, tryJust)
import Control.Monad (unless, when)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.List (isSuffixOf)
import System.Directory (createDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..), hPutStrLn, stderr, withBinaryFile)
import Compliance.Compare

data Counts = Counts
  { total :: !Int
  , accepted :: !Int
  , refAccepted :: !Int
  , warned :: !Int
  , refWarned :: !Int
  , outcomes :: !(Map.Map String Int)
  }

emptyCounts :: Counts
emptyCounts = Counts 0 0 0 0 0 (Map.fromList
  ([(outcomeName status, 0) | status <- [minBound .. maxBound]] ++ [("exception", 0)]))

summary :: Counts -> Value
summary counts = object
  [ "reference" .= ("Cabal-syntax" :: String)
  , "reference_version" .= (VERSION_Cabal_syntax :: String)
  , "total" .= total counts
  , "parser_accepted" .= accepted counts
  , "reference_accepted" .= refAccepted counts
  , "parser_with_warnings" .= warned counts
  , "reference_with_warnings" .= refWarned counts
  , "outcomes" .= outcomes counts
  ]

record :: Handle -> Value -> IO ()
record handle value = LBS.hPut handle (encode value <> "\n")

classify :: BS.ByteString -> IO (Either SomeException Comparison)
classify bytes = tryJust synchronous $ do
  let comparison = compareBytes bytes
  _ <- evaluate (length (show comparison))
  pure comparison
  where
    synchronous err = case fromException err :: Maybe AsyncException of
      Just _ -> Nothing
      Nothing -> Just err

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--file", path] -> do
      result <- BS.readFile path >>= classify
      LBS.putStr (encode (case result of
        Left err -> object ["status" .= ("exception" :: String), "detail" .= show err]
        Right comparison -> comparisonJSON comparison) <> "\n")
    [index, destination] -> do
      -- Refuse to overwrite an earlier report.
      createDirectory destination
      counts <- withBinaryFile index ReadMode $ \source ->
        withBinaryFile (destination </> "failures.jsonl") WriteMode $ \report -> do
          bytes <- LBS.hGetContents source
          walk report Map.empty emptyCounts (Tar.read bytes)
      unless (total counts > 0) (fail "The index has no Cabal files")
      LBS.writeFile (destination </> "summary.json") (encode (summary counts) <> "\n")
      LBS.putStr (encode (summary counts) <> "\n")
    _ -> fail "Use: hackage-compliance INDEX.tar REPORT-DIRECTORY, or --file FILE.cabal"

comparisonJSON :: Comparison -> Value
comparisonJSON result = object
  [ "status" .= outcomeName (outcome result)
  , "parser_accepted" .= parserAccepted result
  , "reference_accepted" .= referenceAccepted result
  , "parser_warnings" .= parserWarnings result
  , "reference_warnings" .= referenceWarnings result
  , "detail" .= take 2000 (detail result)
  ]

walk :: Handle -> Map.Map FilePath Int -> Counts -> Tar.Entries Tar.FormatError -> IO Counts
walk _ _ counts Tar.Done = pure counts
walk _ _ _ (Tar.Fail err) = fail (show err)
walk report revisions counts (Tar.Next entry remaining)
  | not (".cabal" `isSuffixOf` path) = walk report revisions counts remaining
  | otherwise = case Tar.entryContent entry of
      Tar.NormalFile bytes _ -> do
        let revision = Map.findWithDefault 0 path revisions
            revisions' = Map.insert path (revision + 1) revisions
        result <- classify (LBS.toStrict bytes)
        let (status, ours, reference, oursWarnings, refWarningsCount, details) = case result of
              Left err -> ("exception", False, False, 0, 0, object ["detail" .= take 2000 (show err)])
              Right comparison -> (outcomeName (outcome comparison), parserAccepted comparison,
                referenceAccepted comparison, parserWarnings comparison, referenceWarnings comparison,
                comparisonJSON comparison)
            counts' = Counts (total counts + 1)
              (accepted counts + fromEnum ours) (refAccepted counts + fromEnum reference)
              (warned counts + fromEnum (oursWarnings > 0)) (refWarned counts + fromEnum (refWarningsCount > 0))
              (Map.insertWith (+) status 1 (outcomes counts))
        when (status /= "match") $ record report $ object
          [ "path" .= path, "revision" .= revision, "status" .= status, "result" .= details ]
        when (total counts' `mod` 10000 == 0) (hPutStrLn stderr ("Compared " ++ show (total counts') ++ " files"))
        walk report revisions' counts' remaining
      _ -> fail ("Invalid Cabal entry: " ++ path)
  where path = Tar.entryPath entry
