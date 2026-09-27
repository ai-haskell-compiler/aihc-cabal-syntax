{-# LANGUAGE OverloadedStrings #-}
module Aihc.Cabal.Version
  ( Version, versionNumbers, mkVersion, parseVersion, renderVersion
  , VersionRange (..), anyVersion, thisVersion, withinRange, intersectRanges
  , unionRanges, parseVersionRange, renderVersionRange, versionParser, rangeParser, conditionRangeParser
  ) where

import Control.Applicative (empty, (<|>))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, eof, errorBundlePretty, runParser, sepBy1, try, lookAhead)
import Text.Megaparsec.Char (char, space1, digitChar)
import qualified Text.Megaparsec.Char.Lexer as L

newtype Version = Version (NonEmpty Integer) deriving (Eq, Ord, Show)

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

space :: Parser ()
space = L.space space1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol space

versionParser :: Parser Version
versionParser = Version . NE.fromList <$> L.decimal `sepBy1` char '.'

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

rangeParser :: Parser VersionRange
rangeParser = rangeParserWith False

conditionRangeParser :: Parser VersionRange
conditionRangeParser = rangeParserWith True

rangeParserWith :: Bool -> Parser VersionRange
rangeParserWith leftAssociative = makeExprParser atom
  [ [operator (Both <$ symbol "&&")]
  , [operator (EitherRange <$ symbol "||")]
  ]
  where
    operator = if leftAssociative then InfixL else InfixR
    atom = between (symbol "(") (symbol ")") (rangeParserWith leftAssociative)
      <|> AnyVersion <$ symbol "-any"
      <|> Earlier (Version (0 :| [])) <$ symbol "-none"
      <|> (symbol "^>=" *> versions MajorBound)
      <|> try wildcard
      <|> (symbol "==" *> versions Equal)
      <|> (symbol ">=" *> (AtLeast <$> L.lexeme space versionParser))
      <|> (symbol "<=" *> (AtMost <$> L.lexeme space versionParser))
      <|> (symbol ">" *> (Later <$> L.lexeme space versionParser))
      <|> (symbol "<" *> (Earlier <$> L.lexeme space versionParser))
    versions constructor =
      (foldr1 EitherRange . map constructor <$> between (symbol "{") (symbol "}")
        (L.lexeme space versionParser `sepBy1` symbol ","))
      <|> (constructor <$> L.lexeme space versionParser)
    wildcard = do
      _ <- symbol "=="
      ns <- L.decimal `sepBy1` try (char '.' <* lookAhead digitChar)
      _ <- symbol ".*"
      let lower = Version (NE.fromList ns)
          upper = Version (NE.fromList (init ns ++ [last ns + 1]))
      pure (Both (AtLeast lower) (Earlier upper))

majorUpperBound :: Version -> Version
majorUpperBound (Version ns) = Version $ case ns of
  x :| [] -> x :| [1]
  x :| (y:_) -> x :| [y + 1]

parseVersionRange :: Text -> Either Text VersionRange
parseVersionRange = parseWith rangeParser

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
