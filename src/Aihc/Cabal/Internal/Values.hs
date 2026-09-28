{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Parsers for field values, and the free text rules. Each parser follows
-- the Cabal-syntax 3.12 parser for the same value.
module Aihc.Cabal.Internal.Values
  ( runValue, token, token', filePath, quoted, commaList, spaceList, optionList
  , componentName, moduleName, identifier, languageName, bool, buildTypeValue
  , dependency, exeDependency, legacyExeDependency, mixin, flagNameValue, specAtLeast, freeText
  ) where

import Control.Applicative (optional, (<|>))
import Control.Monad (void, when)
import Data.Char (chr, digitToInt, isAlpha, isAlphaNum, isDigit, isHexDigit, isOctDigit, isSpace, isUpper, toLower)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (between, choice, eof, errorBundlePretty, getInput, many, manyTill, option, runParser, satisfy, some, takeP, takeWhile1P, takeWhileP, try)
import Text.Megaparsec.Char (char, space, spaceChar, string)
import Aihc.Cabal.Internal.Types
import Aihc.Cabal.Internal.Version

specAtLeast :: [Integer] -> Version -> Bool
specAtLeast digits spec = spec >= specVersion digits

-- | Run a field parser on complete field text, as Cabal-syntax does.
runValue :: Parser a -> Text -> Either Text a
runValue p input = case runParser (space *> p <* space <* eof) "field" input of
  Left err -> Left (T.pack (errorBundlePretty err))
  Right x -> Right x

-- | A Haskell string literal.
haskellString :: Parser Text
haskellString = T.pack . catMaybes <$> (char '"' *> manyTill stringChar (char '"'))
  where
    stringChar = Just <$> satisfy (\c -> c /= '"' && c /= '\\' && c > '\026') <|> (char '\\' *> escape)
    escape = Nothing <$ (some (satisfy isSpace) *> char '\\')
      <|> Nothing <$ char '&'
      <|> Just <$> escapeCode
    escapeCode = choice (map (\(c, code) -> code <$ char c) (zip "abfnrtv\\\"'" "\a\b\f\n\r\t\v\\\"'"))
      <|> number 10 isDigit (pure ())
      <|> number 8 isOctDigit (void (char 'o'))
      <|> number 16 isHexDigit (void (char 'x'))
      <|> choice [try (code <$ string name) | (name, code) <- asciiCodes]
      <|> (char '^' *> ((\c -> chr (fromEnum c - fromEnum '@')) <$> satisfy (\c -> isUpper c || c == '@')))
    number :: Int -> (Char -> Bool) -> Parser () -> Parser Char
    number base valid prefix = do
      prefix
      ds <- some (satisfy valid)
      let n = foldl (\a d -> a * base + digitToInt d) 0 ds
      if n > 0x10FFFF then fail "out-of-range numeric escape sequence" else pure (chr n)
    asciiCodes = zip
      [ "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "DLE", "DC1", "DC2", "DC3", "DC4"
      , "NAK", "SYN", "ETB", "CAN", "SUB", "ESC", "DEL", "BS", "HT", "LF", "VT", "FF", "CR", "SO"
      , "SI", "EM", "FS", "GS", "RS", "US", "SP" ]
      "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\SUB\ESC\DEL\BS\HT\LF\VT\FF\CR\SO\SI\EM\FS\GS\RS\US\SP"

-- | A string or characters other than space and comma.
token :: Parser Text
token = haskellString <|> takeWhile1P (Just "identifier") (\c -> not (isSpace c) && c /= ',')

-- | A string or characters other than space.
token' :: Parser Text
token' = haskellString <|> takeWhile1P (Just "token") (not . isSpace)

-- | A token that is not empty.
filePath :: Parser Text
filePath = do
  x <- token
  when (T.null x) (fail "empty FilePath")
  pure x

quoted :: Parser a -> Parser a
quoted p = between (char '"') (char '"') p <|> p

comma :: Parser ()
comma = char ',' *> space

-- | The list format of @CommaVCat@ and @CommaFSep@ fields.
commaList :: Version -> Parser a -> Parser [a]
commaList spec p
  | specAtLeast [2, 2] spec = do
      c <- optional comma
      case c of
        Nothing -> sepEndBy1 item comma <|> pure []
        Just _ -> sepBy1 item comma
  | otherwise = sepBy item comma
  where item = p <* space

-- | The list format of @VCat@ and @FSep@ fields.
spaceList :: Version -> Parser a -> Parser [a]
spaceList spec p
  | specAtLeast [3, 0] spec = do
      c <- optional comma
      case c of
        Nothing -> start <|> pure []
        Just _ -> sepBy1 item comma
  | otherwise = sepBy item (optional comma)
  where
    item = p <* space
    start = do
      x <- item
      c <- optional comma
      case c of
        Nothing -> (x :) <$> many item
        Just _ -> (x :) <$> sepEndBy item comma

-- | The list format of @NoCommaFSep@ fields.
optionList :: Parser a -> Parser [a]
optionList p = many (p <* space)

sepBy :: Parser a -> Parser s -> Parser [a]
sepBy p s = sepBy1 p s <|> pure []

sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy1 p s = (:) <$> p <*> many (s *> p)

sepEndBy :: Parser a -> Parser s -> Parser [a]
sepEndBy p s = sepEndBy1 p s <|> pure []

sepEndBy1 :: Parser a -> Parser s -> Parser [a]
sepEndBy1 p s = do
  x <- p
  (s *> ((x :) <$> sepEndBy p s)) <|> pure [x]

-- | Take the longest prefix that the predicate accepts, if the check accepts
-- that prefix. Otherwise use the fallback parser. The result is a slice of the
-- input: a list of characters is not kept.
validPrefix :: (Char -> Bool) -> (Text -> Bool) -> Parser Text -> Parser Text
validPrefix allowed valid fallback = do
  input <- getInput
  let x = T.takeWhile allowed input
  if valid x then takeP Nothing (T.length x) else fallback

-- | A package or component name. Each part has a letter.
componentName :: Parser Text
componentName = validPrefix (\c -> isAlphaNum c || c == '-') valid (T.pack <$> state0 [])
  where
    valid x = not (T.null x) && all (\part -> not (T.null part) && not (T.all isDigit part)) (T.splitOn "-" x)
    ch = satisfy (\c -> isAlphaNum c || c == '-')
    state0 acc = do
      c <- ch
      if isDigit c then state0 (c : acc)
        else if isAlphaNum c then state1 (c : acc)
        else fail ("Empty component, after " ++ reverse acc)
    state1 acc = (do
      c <- ch
      if isAlphaNum c then state1 (c : acc) else state0 (c : acc)) <|> pure (reverse acc)

moduleName :: Parser Text
moduleName = validPrefix (\c -> isAlphaNum c || c == '_' || c == '\'' || c == '.') valid (T.pack <$> state0 [])
  where
    valid x = not (T.null x) && all (\part -> maybe False (isUpper . fst) (T.uncons part)) (T.splitOn "." x)
    state0 acc = do
      c <- satisfy isUpper
      state1 (c : acc)
    state1 acc = (do
      c <- satisfy (\c -> isAlphaNum c || c == '_' || c == '\'' || c == '.')
      if c == '.' then state0 (c : acc) else state1 (c : acc)) <|> pure (reverse acc)

-- | A name for an operating system or an architecture.
identifier :: Parser Text
identifier = T.cons <$> satisfy isAlpha <*> takeWhileP Nothing (\c -> isAlphaNum c || c == '_' || c == '-')

languageName :: Parser Text
languageName = takeWhile1P (Just "name") isAlphaNum

bool :: Parser Bool
bool = do
  x <- takeWhile1P (Just "boolean") isAlpha
  case T.toLower x of
    "true" -> pure True
    "false" -> pure False
    _ -> fail ("Not a boolean: " ++ T.unpack x)

-- | A build type. @Default@ is @Custom@ before cabal-version 1.20.
-- @Make@ is not permitted from cabal-version 3.18. @Hooks@ is permitted
-- from cabal-version 3.14.
buildTypeValue :: Version -> Parser Text
buildTypeValue spec = do
  x <- takeWhile1P (Just "build type") isAlphaNum
  case x of
    _ | x `elem` ["Simple", "Configure", "Custom"] -> pure x
    "Make" | not (specAtLeast [3, 18] spec) -> pure x
           | otherwise -> fail "build-type: 'Make'. This feature requires cabal-version <= 3.18."
    "Hooks" | specAtLeast [3, 14] spec -> pure x
            | otherwise -> fail "build-type: 'Hooks'. This feature requires cabal-version >= 3.14."
    "Default" | not (specAtLeast [1, 20] spec) -> pure "Custom"
    _ -> fail ("unknown build-type: '" ++ T.unpack x ++ "'")

flagNameValue :: Parser Text
flagNameValue = do
  c <- satisfy (\x -> isAlphaNum x || x == '_')
  rest <- takeWhileP Nothing (\x -> isAlphaNum x || x == '_' || x == '-')
  pure (T.map toLower (T.cons c rest))

dependency :: Version -> Parser Dependency
dependency spec = do
  name <- componentName
  libs <- optional $ do
    _ <- char ':'
    unless3 (fail "Sublibrary dependency syntax used")
    ((:| []) <$> library) <|> between (char '{' *> space) (space *> char '}') libraries
  space
  range <- optional (rangeParser spec)
  let normalize (NamedLibrary x) | x == name = MainLibrary
      normalize x = x
      -- Most dependencies have no library list. They share one value.
      !targets = maybe mainLibrary (fmap normalize) libs
  pure (Dependency name (fromMaybe anyVersion range) targets)
  where
    unless3 failure = when (not (specAtLeast [3, 0] spec)) failure
    library = NamedLibrary <$> componentName
    libraries = do
      x <- library <* space
      xs <- many (comma *> library <* space)
      pure (x :| xs)

mainLibrary :: NonEmpty LibraryTarget
mainLibrary = MainLibrary :| []
{-# NOINLINE mainLibrary #-}

exeDependency :: Version -> Parser ToolDependency
exeDependency spec = do
  pkg <- componentName
  _ <- char ':'
  exe <- componentName <* space
  range <- optional (rangeParser spec)
  pure (ToolDependency (Just pkg) exe (fromMaybe anyVersion range))

legacyExeDependency :: Version -> Parser ToolDependency
legacyExeDependency spec = do
  name <- quoted legacyName
  space
  range <- optional (quoted (rangeParser spec))
  pure (ToolDependency Nothing name (fromMaybe anyVersion range))
  where
    legacyName = T.intercalate "-" <$> sepBy1 part (char '-')
    part = do
      x <- takeWhile1P (Just "name") (\c -> isAlphaNum c || c == '+' || c == '_')
      if T.all isDigit x then fail "invalid component" else pure x

-- | A mixin: a package, an optional library, and module renamings.
mixin :: Version -> Parser Mixin
mixin spec = do
  pkg <- componentName
  lib <- option MainLibrary $ do
    _ <- char ':'
    when (not (specAtLeast [3, 4] spec)) (fail "Sublibrary mixin syntax used")
    NamedLibrary <$> componentName
  space
  provides <- renaming
  requires <- option DefaultRenaming (try (space *> string "requires" *> space *> renaming))
  let lib' = case lib of
        NamedLibrary x | x == pkg -> MainLibrary
        _ -> lib
  pure (Mixin pkg lib' provides requires)
  where
    lax = specAtLeast [3, 0] spec
    parens p
      | lax = between (char '(' *> space) (char ')' *> space) p
      | otherwise = between (char '(' *> optional (spaceChar *> fail "space after parenthesis")) (char ')') p
    listed = if lax then moduleName <* space else moduleName
    renaming = choice
      [ ModuleRenaming <$> parens (sepBy entry comma) <* space
      , HidingRenaming <$> (string "hiding" *> space *> parens (sepBy listed comma))
      , pure DefaultRenaming ]
    entry = do
      old <- moduleName <* space
      option (old, old) $ do
        _ <- string "as"
        _ <- some (satisfy isSpace)
        new <- moduleName <* space
        pure (old, new)

-- | Cabal-syntax gives free text with two sets of rules. From cabal-version
-- 3.0, it keeps blank lines and relative indentation.
freeText :: Version -> FieldValue -> Text
freeText spec (FieldValue pos ls) = case ls of
  [] -> ""
  _ | specAtLeast [3, 0] spec -> freeText3 pos ls
  [FieldLine _ "."] -> "."
  _ -> T.intercalate "\n" [if t == "." then "" else t | FieldLine _ x <- ls, let t = T.strip x]

freeText3 :: Position -> [FieldLine] -> Text
freeText3 _ [] = ""
freeText3 _ [FieldLine _ x] = x
freeText3 pos (FieldLine p1 x1 : rest@(FieldLine p2 _ : _))
  | positionRow pos == positionRow p1 = T.concat (x1 : lines' (minimum (column p1 : column p2 : map lineColumn rest)))
  | otherwise =
      let c = minimum (column p1 : map lineColumn rest)
      in T.concat (T.replicate (column p1 - c) " " : x1 : lines' c)
  where
    column = positionColumn
    lineColumn = column . fieldLinePosition
    lines' c = zipWith (line c) (p1 : map fieldLinePosition rest) rest
    line c previous (FieldLine q x) =
      T.replicate (positionRow q - positionRow previous) "\n" <> T.replicate (column q - c) " " <> x
