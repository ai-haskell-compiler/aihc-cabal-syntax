{-# LANGUAGE OverloadedStrings #-}
module Aihc.Cabal.Version
  ( Version, versionNumbers, mkVersion, parseVersion, renderVersion
  , VersionRange (..), anyVersion, noVersion, thisVersion, withinVersion, withinRange, intersectRanges
  , unionRanges, parseVersionRange, renderVersionRange
  , Parser, specVersion, latestSpec, versionParser, versionDigits, rangeParser
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Char (isAlphaNum, isDigit)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, eof, errorBundlePretty, many, runParser, satisfy, sepBy1, some, takeWhile1P)
import Text.Megaparsec.Char (char, space, string)

newtype Version = Version (NonEmpty Integer) deriving (Eq, Ord, Show)

-- | A version range. 'AnyVersion' is the same set of versions as @>= 0@.
data VersionRange
  = AnyVersion | Equal Version | Later Version | Earlier Version
  | AtLeast Version | AtMost Version | MajorBound Version | Both VersionRange VersionRange
  | EitherRange VersionRange VersionRange
  deriving (Eq, Show)

type Parser = Parsec Void Text

versionNumbers :: Version -> NonEmpty Integer
versionNumbers (Version ns) = ns

mkVersion :: NonEmpty Integer -> Maybe Version
mkVersion ns | all (>= 0) ns = Just (Version ns)
             | otherwise = Nothing

-- | A known Cabal specification version, for example @specVersion [2, 2]@.
specVersion :: [Integer] -> Version
specVersion = Version . NE.fromList

-- | The newest Cabal specification version that the parser knows.
latestSpec :: Version
latestSpec = specVersion [3, 14]

-- | A version number part: at most nine digits and no leading zero.
versionDigits :: Parser Integer
versionDigits = do
  ds <- some (satisfy isDigit)
  case ds of
    "0" -> pure 0
    '0' : _ -> fail "Version digit with leading zero"
    _ | length ds > 9 -> fail "At most 9 numbers are allowed per version number part"
      | otherwise -> pure (read ds)

-- | A version with optional tags, for example @1.2.3-rc1@. The tags are not kept.
versionParser :: Parser Version
versionParser = Version . NE.fromList <$> versionDigits `sepBy1` char '.' <* tags

tags :: Parser ()
tags = () <$ many (char '-' *> some (satisfy isAlphaNum))

parseWith :: Parser a -> Text -> Either Text a
parseWith p input = case runParser (space *> p <* space <* eof) "" input of
  Left err -> Left (T.pack (errorBundlePretty err))
  Right value -> Right value

parseVersion :: Text -> Either Text Version
parseVersion = parseWith versionParser

renderVersion :: Version -> Text
renderVersion (Version ns) = T.intercalate "." (map (T.pack . show) (NE.toList ns))

anyVersion :: VersionRange
anyVersion = AnyVersion

-- | The empty range @-none@.
noVersion :: VersionRange
noVersion = Earlier (Version (0 :| []))

thisVersion :: Version -> VersionRange
thisVersion = Equal

intersectRanges, unionRanges :: VersionRange -> VersionRange -> VersionRange
intersectRanges = Both
unionRanges = EitherRange

withinRange :: Version -> VersionRange -> Bool
withinRange v range = case range of
  AnyVersion -> True
  Equal w -> v == w
  Later w -> v > w
  Earlier w -> v < w
  AtLeast w -> v >= w
  AtMost w -> v <= w
  MajorBound w -> v >= w && v < majorUpperBound w
  Both a b -> withinRange v a && withinRange v b
  EitherRange a b -> withinRange v a || withinRange v b

-- | Parse a version range as Cabal-syntax does for the given specification version.
-- Unparenthesized @&&@ and @||@ associate to the right.
rangeParser :: Version -> Parser VersionRange
rangeParser spec = expr
  where
    expr = do
      space
      t <- term
      space
      (EitherRange t <$> (string "||" *> space *> expr)) <|> pure t
    term = do
      f <- factor
      space
      (Both f <$> (string "&&" *> space *> term)) <|> pure f
    factor = parens <|> prim
    parens = between (char '(' *> space) (char ')' *> space) (expr <* space)
    prim = do
      op <- takeWhile1P (Just "operator") isOpChar
      case op of
        "-" -> AnyVersion <$ string "any" <|> (string "none" *> none)
        "==" -> space *> (wildOrVersion <|> (versionSet >>= set Equal))
        "^>=" -> space *> (major <|> (versionSet >>= set MajorBound))
        _ -> do
          space
          (wild, v) <- versionOrWild
          when wild (fail ("wild-card version after non-== operator: " ++ T.unpack op))
          case op of
            ">=" -> pure (AtLeast v)
            "<" -> pure (Earlier v)
            "<=" -> pure (AtMost v)
            ">" -> pure (Later v)
            _ -> fail ("Unknown version operator " ++ T.unpack op)
    isOpChar c = c `elem` ("<=>^" :: String) || (c == '-' && spec < specVersion [3, 4])
    none
      | spec >= specVersion [1, 22] = pure noVersion
      | otherwise = fail "-none version range used"
    wildOrVersion = do
      (wild, v) <- versionOrWild
      pure (if wild then withinVersion v else Equal v)
    major = do
      (wild, v) <- versionOrWild
      when wild (fail "wild-card version after ^>= operator")
      if spec >= specVersion [2, 0] then pure (MajorBound v)
        else fail "major bounded version syntax (caret, ^>=) used"
    set constructor vs
      | spec >= specVersion [3, 0] = pure (foldr1 EitherRange (fmap constructor vs))
      | otherwise = fail "version set syntax used"
    versionSet = do
      _ <- char '{' *> space
      v <- plain <* space
      vs <- many (char ',' *> space *> plain <* space)
      _ <- char '}'
      pure (v :| vs)
    plain = Version . NE.fromList <$> versionDigits `sepBy1` char '.'
    versionOrWild = versionDigits >>= loop . pure
    loop acc = (char '.' *> ((versionDigits >>= loop . (: acc)) <|> ((True, done acc) <$ char '*')))
      <|> ((False, done acc) <$ tags)
    done = Version . NE.fromList . reverse

-- | The range @== v.*@.
withinVersion :: Version -> VersionRange
withinVersion v = Both (AtLeast v) (Earlier (wildcardUpperBound v))
wildcardUpperBound :: Version -> Version
wildcardUpperBound (Version ns) = Version (NE.fromList (NE.init ns ++ [NE.last ns + 1]))

majorUpperBound :: Version -> Version
majorUpperBound (Version ns) = Version $ case ns of
  x :| [] -> x :| [1]
  x :| (y:_) -> x :| [y + 1]

-- | Parse a version range. The parser accepts the syntax of all Cabal
-- format versions: @-any@, @-none@, @^>=@, and version sets.
parseVersionRange :: Text -> Either Text VersionRange
parseVersionRange = parseWith (rangeParser (specVersion [3, 0]))

renderVersionRange :: VersionRange -> Text
renderVersionRange r = case r of
  AnyVersion -> "-any"
  Equal v -> "== " <> renderVersion v
  Later v -> "> " <> renderVersion v
  Earlier v -> "< " <> renderVersion v
  AtLeast v -> ">= " <> renderVersion v
  AtMost v -> "<= " <> renderVersion v
  MajorBound v -> "^>= " <> renderVersion v
  Both a b -> "(" <> renderVersionRange a <> " && " <> renderVersionRange b <> ")"
  EitherRange a b -> "(" <> renderVersionRange a <> " || " <> renderVersionRange b <> ")"
