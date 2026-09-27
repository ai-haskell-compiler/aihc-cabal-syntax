module Main (main) where

import Aihc.Cabal (parsePackage, parseValue)
import qualified Codec.Archive.Tar as Tar
import Control.Exception (evaluate)
import qualified Data.ByteString.Lazy as LBS
import Data.List (isSuffixOf)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), withBinaryFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> withBinaryFile path ReadMode $ \handle -> do
      bytes <- LBS.hGetContents handle
      (count, accepted) <- walk 0 0 (Tar.read bytes)
      if count == 0 then fail "The index has no Cabal files"
        else putStrLn (show count ++ " " ++ show accepted)
    _ -> fail "Use: hackage-benchmark INDEX.tar"

walk :: Int -> Int -> Tar.Entries Tar.FormatError -> IO (Int, Int)
walk count accepted Tar.Done = pure (count, accepted)
walk _ _ (Tar.Fail err) = fail (show err)
walk count accepted (Tar.Next entry remaining) = case Tar.entryContent entry of
  Tar.NormalFile bytes _ | ".cabal" `isSuffixOf` Tar.entryPath entry -> do
    let result = parsePackage (LBS.toStrict bytes)
    -- Force all result fields before the next file.
    _ <- evaluate (length (show result))
    let success = either (const 0) (const 1) (parseValue result)
    nextCount <- evaluate (count + 1)
    nextAccepted <- evaluate (accepted + success)
    walk nextCount nextAccepted remaining
  _ -> walk count accepted remaining
