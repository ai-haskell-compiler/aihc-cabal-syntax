{-# LANGUAGE OverloadedStrings #-}
-- | Parse a condition from the arguments of an @if@ or @elif@ section.
-- This module follows the condition parser of Cabal-syntax 3.12. The parser
-- reads section argument tokens, not text.
module Aihc.Cabal.Internal.Condition (parseCondition) where

import Control.Applicative (Alternative (..))
import Control.Monad (ap)
import Data.Char (isDigit, isAlphaNum)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (eof, runParser, satisfy, takeWhile1P)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char (char)
import Aihc.Cabal.Internal.Lexer (SectionArg (..))
import Aihc.Cabal.Internal.Types (Condition (..))
import Aihc.Cabal.Internal.Values (flagNameValue, identifier)
import Aihc.Cabal.Internal.Version

-- | A parser with Parsec semantics: an alternative runs only when the first
-- parser fails without input consumption.
newtype P a = P { runP :: [SectionArg] -> Reply a }

data Reply a = Ok a [SectionArg] Bool | Err Bool

instance Functor P where
  fmap f (P p) = P $ \s -> case p s of
    Ok a r c -> Ok (f a) r c
    Err c -> Err c

instance Applicative P where
  pure x = P (\s -> Ok x s False)
  (<*>) = ap

instance Monad P where
  P p >>= k = P $ \s -> case p s of
    Err c -> Err c
    Ok a r c -> case runP (k a) r of
      Ok b r' c' -> Ok b r' (c || c')
      Err c' -> Err (c || c')

instance Alternative P where
  empty = P (const (Err False))
  P p <|> P q = P $ \s -> case p s of
    Err False -> q s
    reply -> reply
  many p = some p <|> pure []
  some p = (:) <$> p <*> many p

instance MonadFail P where
  fail _ = empty

try :: P a -> P a
try (P p) = P $ \s -> case p s of
  Err _ -> Err False
  reply -> reply

tokenWith :: (SectionArg -> Maybe a) -> P a
tokenWith f = P $ \s -> case s of
  x : rest | Just a <- f x -> Ok a rest True
  _ -> Err False

name :: P Text
name = tokenWith $ \t -> case t of
  ArgName _ x -> Just x
  _ -> Nothing

word :: Text -> P ()
word w = tokenWith $ \t -> case t of
  ArgName _ x | x == w -> Just ()
  _ -> Nothing

oper :: Text -> P ()
oper o = tokenWith $ \t -> case t of
  ArgOther _ x | x == o -> Just ()
  _ -> Nothing

-- | Consume a name token and parse it completely. A parse error after the
-- token is an error with input consumption.
value :: Parser a -> P a
value p = do
  x <- name
  case runParser (p <* eof) "" x of
    Left _ -> P (const (Err True))
    Right a -> pure a

parens :: P a -> P a
parens p = oper "(" *> p <* oper ")"

sepBy1 :: P a -> P s -> P (NE.NonEmpty a)
sepBy1 p s = (NE.:|) <$> p <*> many (s *> p)

parseCondition :: [SectionArg] -> Maybe Condition
parseCondition args = case runP (condOr <* end) args of
  Ok c _ _ -> Just c
  Err _ -> Nothing
  where
    end = P $ \s -> if null s then Ok () s False else Err False
    condOr = foldl1 Or <$> sepBy1 condAnd (oper "||")
    condAnd = foldl1 And <$> sepBy1 cond (oper "&&")
    cond = boolean <|> parens condOr <|> (Not <$> (oper "!" *> cond))
      <|> (word "os" *> parens (OS <$> value identifier))
      <|> (word "arch" *> parens (Arch <$> value identifier))
      <|> (word "flag" *> parens (FlagValue <$> value flagNameValue))
      <|> (word "impl" *> parens (Impl <$> value compiler <*> (versionRange <|> pure anyVersion)))
    boolean = tokenWith $ \t -> case t of
      ArgName _ x
        | x `elem` ["True", "true"] -> Just (Literal True)
        | x `elem` ["False", "false"] -> Just (Literal False)
      _ -> Nothing
    compiler = do
      x <- takeWhile1P Nothing isAlphaNum
      if T.all isDigit x then fail "all digits compiler name" else pure x
    versionRange = expr
      where
        expr = foldl1 EitherRange <$> sepBy1 term (oper "||")
        term = foldl1 Both <$> sepBy1 factor (oper "&&")
        factor = parens expr
          <|> (AnyVersion <$ word "-any")
          <|> (noVersion <$ word "-none")
          <|> try (withinVersion <$> (oper "==" *> value versionStar) <* oper "*")
          <|> foldr1 (<|>) [try (f <$> (oper o *> value versionParser)) | (o, f) <- operators]
        operators =
          [ ("<", Earlier), ("<=", AtMost), (">", Later), (">=", AtLeast)
          , ("^>=", MajorBound), ("==", Equal) ]
        versionStar = specVersion <$> M.some (read <$> M.some (satisfy isDigit) <* char '.')
