{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Read the outline of a Cabal file: fields, sections, and their positions.
-- This module follows the lexer and the outline parser of Cabal-syntax 3.12.
module Aihc.Cabal.Internal.Lexer
  ( Field (..), SectionArg (..), readFields, sectionArgText
  ) where

import Data.Char (isAsciiUpper, ord, toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Aihc.Cabal.Internal.Types (Diagnostic (..), FieldLine (..), Position (..))

data Field
  = Field !Position Text [FieldLine]
  | Section !Position Text [SectionArg] [Field]
  deriving Show

data SectionArg
  = ArgName !Position Text
  | ArgString !Position Text
  | ArgOther !Position Text
  deriving Show

sectionArgText :: SectionArg -> Text
sectionArgText (ArgName _ t) = t
sectionArgText (ArgString _ t) = t
sectionArgText (ArgOther _ t) = t

data Token
  = TokSym Text | TokStr Text | TokOther Text | Indent Int | TokFieldLine Text
  | Colon | OpenBrace | CloseBrace | EOF | LexicalError
  deriving (Eq, Show)

data Mode = BolSection | InSection | BolFieldLayout | InFieldLayout | BolFieldBraces | InFieldBraces
  deriving (Eq, Show)

-- | The state before a token. The token and the next state are computed
-- on demand, as in the Cabal-syntax parser.
data LexState = LexState
  { lexRow :: !Int
  , lexColumn :: !Int
  , lexInput :: !Text
  , lexMode :: !Mode
  }

data Stream = Stream LexState (Position, Token, Stream)

stream :: LexState -> Stream
stream st = Stream st (lexToken st)

setMode :: Mode -> Stream -> Stream
setMode m (Stream st _) = stream st {lexMode = m}

peek :: Stream -> (Position, Token)
peek (Stream _ (p, t, _)) = (p, t)

advance :: Stream -> Stream
advance (Stream _ (_, _, next)) = next

-- | The number of bytes in the UTF-8 encoding. Cabal-syntax counts columns in bytes.
width :: Char -> Int
width c
  | n < 0x80 = 1
  | n < 0x800 = 2
  | n < 0x10000 = 3
  | otherwise = 4
  where n = ord c

widthOf :: Text -> Int
widthOf = T.foldl' (\a c -> a + width c) 0

isSpaceTab :: Char -> Bool
isSpaceTab c = c == ' ' || c == '\t'

-- | Characters in the Cabal lexer class @$printable@.
isPrintable :: Char -> Bool
isPrintable c = c > '\x1f' && c /= '\x7f'

isSymbol' :: Char -> Bool
isSymbol' c = c `elem` (",=<>+*&|!$%^@#?/\\~" :: String)

isNameChar :: Char -> Bool
isNameChar c = isPrintable c && not (c `elem` (" :\"{}()[]" :: String)) && not (isSymbol' c)

isOpChar :: Char -> Bool
isOpChar c = isSymbol' c || c == '-' || c == '.'

isParen :: Char -> Bool
isParen c = c `elem` ("()[]" :: String)

-- | Split a line break at the start of the text: @\\n@, @\\r\\n@, or @\\r@.
newline :: Text -> Maybe Text
newline t = case T.uncons t of
  Just ('\n', rest) -> Just rest
  Just ('\r', rest) -> Just (fromMaybe rest (T.stripPrefix "\n" rest))
  _ -> Nothing

lexToken :: LexState -> (Position, Token, Stream)
lexToken st@(LexState row col input mode) = case mode of
  BolSection -> bol st $ \ws rest -> case T.uncons rest of
    Just ('{', r) | T.all isSpaceTab ws -> token col OpenBrace (LexState row (col + T.length ws + 1) r BolSection)
    Just ('}', r) | T.all isSpaceTab ws -> token col CloseBrace (LexState row (col + T.length ws + 1) r BolSection)
    _ -> indentation ws rest InSection
  BolFieldLayout -> bol st $ \ws rest -> indentation ws rest InFieldLayout
  BolFieldBraces -> bol st $ \_ _ -> lexToken st {lexMode = InFieldBraces}
  InSection -> case T.uncons input of
    Nothing -> token col EOF st
    Just (c, rest)
      | isSpaceTab c -> let (ws, r) = T.span isSpaceTab input in lexToken st {lexColumn = col + T.length ws, lexInput = r}
      | "--" `T.isPrefixOf` input ->
          let (comment, r) = T.span isCommentChar input
          in lexToken st {lexColumn = col + widthOf comment, lexInput = r}
      | Just r <- newline input -> lexToken (LexState (row + 1) 1 r BolSection)
      | c == ':' -> token col Colon st {lexColumn = col + 1, lexInput = rest}
      | c == '{' -> token col OpenBrace st {lexColumn = col + 1, lexInput = rest}
      | c == '}' -> token col CloseBrace st {lexColumn = col + 1, lexInput = rest}
      | c == '"' -> case stringToken rest of
          Just (s, r) -> token col (TokStr s) st {lexColumn = col + widthOf s + 2, lexInput = r}
          Nothing -> token col LexicalError st {lexInput = ""}
      | isParen c -> token col (TokOther (T.singleton c)) st {lexColumn = col + 1, lexInput = rest}
      | otherwise ->
          -- The longest match wins. A name wins a tie with an operator.
          let nameLen = T.length (T.takeWhile isNameChar input)
              opLen = T.length (T.takeWhile isOpChar input)
          in if nameLen == 0 && opLen == 0 then token col LexicalError st {lexInput = ""}
             else if nameLen >= opLen then word TokSym nameLen
             else word TokOther opLen
  InFieldLayout -> fieldLine (const True)
  InFieldBraces -> case T.uncons input of
    Just ('{', rest) -> token col OpenBrace st {lexColumn = col + 1, lexInput = rest}
    Just ('}', rest) -> token col CloseBrace st {lexColumn = col + 1, lexInput = rest}
    _ -> fieldLine (\c -> c /= '{' && c /= '}')
  where
    token c t next = (Position row c, t, stream next)
    word constructor n =
      let (w, rest) = T.splitAt n input
      in token col (constructor w) st {lexColumn = col + widthOf w, lexInput = rest}
    isCommentChar c = isPrintable c || c == '\t'
    -- Skip blank lines and comment lines at the start of a line.
    bol s k =
      let (ws, rest) = T.span (\c -> isSpaceTab c || c == '\xa0') (lexInput s)
      in case newline rest of
        Just r -> lexToken s {lexRow = lexRow s + 1, lexColumn = 1, lexInput = r}
        Nothing
          | "--" `T.isPrefixOf` T.dropWhile isSpaceTab (lexInput s) ->
              let (sp, afterSp) = T.span isSpaceTab (lexInput s)
                  (comment, r) = T.span isCommentChar afterSp
              in lexToken s {lexColumn = lexColumn s + T.length sp + widthOf comment, lexInput = r}
          | otherwise -> k ws rest
    indentation ws rest next
      | T.null rest = (Position row col, EOF, stream st)
      | otherwise =
          let n = T.length ws
          in token col (Indent n) (LexState row (col + n) rest next)
    fieldLine allowed = case T.uncons input of
      Nothing -> token col EOF st
      Just (c, _)
        | isSpaceTab c -> let (ws, r) = T.span isSpaceTab input in lexToken st {lexColumn = col + T.length ws, lexInput = r}
        | Just r <- newline input ->
            lexToken (LexState (row + 1) 1 r (if mode == InFieldLayout then BolFieldLayout else BolFieldBraces))
        | isPrintable c && allowed c ->
            let (line, r) = T.span (\x -> (isPrintable x || x == '\t') && allowed x) input
            in token col (TokFieldLine line) st {lexColumn = col + widthOf line, lexInput = r}
        | otherwise -> token col LexicalError st {lexInput = ""}

-- | The contents of a string token without its quotes. Escapes stay in the text.
-- The lexer uses the longest match: a quotation mark after a backslash can end
-- the string or continue it.
stringToken :: Text -> Maybe (Text, Text)
stringToken t = (\n -> (T.take n t, T.drop (n + 1) t)) <$> go Nothing ' ' 0 (T.unpack t)
  where
    go :: Maybe Int -> Char -> Int -> String -> Maybe Int
    go end _ _ [] = end
    go end previous !n (c : cs)
      | c == '"' = if previous == '\\' then go (Just n) c (n + 1) cs else Just n
      | isPrintable c = go end c (n + 1) cs
      | otherwise = end

type Parse a = Stream -> Either Diagnostic (a, Stream)

parseError :: Position -> Token -> Either Diagnostic a
parseError p t = Left (Diagnostic (Just p) ("Unexpected " <> describe t))
  where
    describe x = case x of
      TokSym s -> "symbol " <> T.pack (show s)
      TokStr s -> "string " <> T.pack (show s)
      TokOther s -> "operator " <> T.pack (show s)
      Indent _ -> "new line"
      TokFieldLine _ -> "field content"
      Colon -> "\":\""
      OpenBrace -> "\"{\""
      CloseBrace -> "\"}\""
      EOF -> "end of file"
      LexicalError -> "character in input"

lowerName :: Text -> Text
lowerName = T.map (\c -> if isAsciiUpper c then toLower c else c)

-- | Read the fields and sections of a file. The input has no byte order mark.
readFields :: Text -> Either Diagnostic [Field]
readFields input = do
  (fields, s) <- elements 0 (stream (LexState 1 1 input BolSection))
  case peek s of
    (_, EOF) -> Right fields
    (p, t) -> parseError p t

elements :: Int -> Parse [Field]
elements level = go []
  where
    go acc s = case peek s of
      (_, Indent j) | j >= level -> do
        let s1 = advance s
        case peek s1 of
          (p, TokSym n) -> do
            (f, s2) <- layoutElement (j + 1) p (lowerName n) (advance s1)
            go (f : acc) s2
          (p, t) -> parseError p t
      (p, TokSym n) -> do
        (f, s1) <- bracesElement p (lowerName n) (advance s)
        go (f : acc) s1
      _ -> Right (reverse acc, s)

sectionArgs :: Stream -> ([SectionArg], Stream)
sectionArgs = go []
  where
    go acc s = case peek s of
      (p, TokSym x) -> go (ArgName p x : acc) (advance s)
      (p, TokStr x) -> go (ArgString p x : acc) (advance s)
      (p, TokOther x) -> go (ArgOther p x : acc) (advance s)
      _ -> (reverse acc, s)

layoutElement :: Int -> Position -> Text -> Parse Field
layoutElement level p n s = case peek s of
  (_, Colon) -> fieldLayoutOrBraces level p n (advance s)
  _ -> do
    let (args, s1) = sectionArgs s
    case peek s1 of
      (_, OpenBrace) -> do
        (fields, s2) <- bracesBody (advance s1)
        Right (Section p n args fields, s2)
      _ -> do
        (fields, s2) <- elements level s1
        Right (Section p n args fields, s2)

bracesElement :: Position -> Text -> Parse Field
bracesElement p n s = case peek s of
  (_, Colon) -> do
    let s1 = advance s
    case peek s1 of
      (_, OpenBrace) -> fieldBraces p n (advance s1)
      _ -> do
        let s2 = setMode InFieldBraces s1
        case peek s2 of
          (q, TokFieldLine l) -> Right (Field p n [FieldLine q l], setMode InSection (advance s2))
          _ -> Right (Field p n [], setMode InSection s2)
  _ -> do
    let (args, s1) = sectionArgs s
    case peek s1 of
      (_, OpenBrace) -> do
        (fields, s2) <- bracesBody (advance s1)
        Right (Section p n args fields, s2)
      (q, t) -> parseError q t

bracesBody :: Parse [Field]
bracesBody s = do
  (fields, s1) <- elements 0 s
  let s2 = case peek s1 of
        (_, Indent _) -> advance s1
        _ -> s1
  case peek s2 of
    (_, CloseBrace) -> Right (fields, advance s2)
    (q, t) -> parseError q t

fieldLayoutOrBraces :: Int -> Position -> Text -> Parse Field
fieldLayoutOrBraces level p n s = case peek s of
  (_, OpenBrace) -> fieldBraces p n (advance s)
  _ -> do
    let s1 = setMode InFieldLayout s
        (first, s2) = case peek s1 of
          (q, TokFieldLine l) -> ([FieldLine q l], advance s1)
          _ -> ([], s1)
        rest acc st = case peek st of
          (_, Indent j) | j >= level -> case peek (advance st) of
            (q, TokFieldLine l) -> rest (FieldLine q l : acc) (advance (advance st))
            (q, t) -> parseError q t
          _ -> Right (reverse acc, st)
    (more, s3) <- rest [] s2
    Right (Field p n (first ++ more), setMode InSection s3)

fieldBraces :: Position -> Text -> Parse Field
fieldBraces p n s = do
  let go acc st = case peek st of
        (q, TokFieldLine l) -> go (FieldLine q l : acc) (advance st)
        _ -> (reverse acc, setMode InSection st)
      (ls, s1) = go [] (setMode InFieldBraces s)
  case peek s1 of
    (_, CloseBrace) -> Right (Field p n ls, advance s1)
    (q, t) -> parseError q t
