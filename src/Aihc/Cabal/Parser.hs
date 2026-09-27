{-# LANGUAGE OverloadedStrings #-}
module Aihc.Cabal.Parser (parsePackage, parseBuildInfo) where

import Control.Applicative (empty, optional, (<|>))
import Control.Monad (foldM, unless, when)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, isAscii, isLetter, isSpace)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, eof, errorBundlePretty, many, manyTill, runParser, satisfy, sepBy1, sepEndBy, some)
import Text.Megaparsec.Char (char, space1, string')
import qualified Text.Megaparsec.Char.Lexer as L
import Aihc.Cabal.Types
import Aihc.Cabal.Version

type Parser = Parsec Void Text
type Result a = Either Diagnostic a

data Line = Line Int Int Text deriving Show
data Node = Field Int Text Text | Section Int Text [Node] deriving Show

failure :: Int -> Text -> Result a
failure n = Left . Diagnostic n 1

space :: Parser ()
space = L.space space1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

value :: Int -> Parser a -> Text -> Result a
value n p input = case runParser (space *> p <* eof) "field" input of
  Left e -> failure n (T.pack (errorBundlePretty e))
  Right x -> Right x

name :: Parser Text
name = lexeme (T.pack <$> some (satisfy (\c -> isAscii c && (isAlphaNum c || c == '-' || c == '_'))))

packageNameParser :: Parser Text
packageNameParser = do
  x <- name
  unless (not ("_" `T.isInfixOf` x) && all (\part -> not (T.null part) && T.any isLetter part) (T.splitOn "-" x))
    (fail "Invalid package name")
  pure x

bool :: Parser Bool
bool = True <$ lexeme (string' "True") <|> False <$ lexeme (string' "False")

token :: Parser Text
token = lexeme (quoted <|> bare)
  where
    quoted = T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))
    bare = T.pack <$> some (satisfy (\c -> not (isSpace c) && c /= ',' && c /= '"'))

items :: Parser [Text]
items = optional (symbol ",") *> many (token <* optional (symbol ","))

conditionParser :: Parser Condition
conditionParser = makeExprParser atom
  [ [Prefix (Not <$ symbol "!")]
  , [InfixL (And <$ symbol "&&")]
  , [InfixL (Or <$ symbol "||")]
  ]
  where
    atom = between (symbol "(") (symbol ")") conditionParser
      <|> Literal <$> bool
      <|> call "os" (OS . T.toLower <$> name)
      <|> call "arch" (Arch . T.toLower <$> name)
      <|> call "flag" (FlagValue . T.toLower <$> name)
      <|> call "impl" (Impl . T.toLower <$> name <*> (fromMaybe anyVersion <$> optional rangeParser))
    call key p = symbol key *> between (symbol "(") (symbol ")") p

dependencyParser :: Parser Dependency
dependencyParser = do
  pkg <- packageNameParser
  libs <- optional (symbol ":" *> targets)
  range <- fromMaybe anyVersion <$> optional rangeParser
  pure (Dependency pkg range (fromMaybe (MainLibrary :| []) libs))
  where
    targets = (NE.fromList <$> between (symbol "{") (symbol "}") (target `sepBy1` symbol ","))
      <|> ((:| []) <$> target)
    target = NamedLibrary <$> name

toolParser :: Bool -> Parser ToolDependency
toolParser modern = do
  first <- if modern then packageNameParser else name
  exe <- if modern then Just <$> (symbol ":" *> name) else pure Nothing
  range <- fromMaybe anyVersion <$> optional rangeParser
  pure (case exe of
    Nothing -> ToolDependency Nothing first range
    Just x -> ToolDependency (Just first) x range)

-- | Read indentation before field values. A comment occupies a complete line.
layout :: BS.ByteString -> Result [Node]
layout bytes = do
  text <- case TE.decodeUtf8' bytes of
    Left _ -> failure 1 "Invalid UTF-8 input"
    Right x -> Right (T.dropWhile (== '\xfeff') x)
  lines' <- fmap concat (mapM prepare (zip [1..] (T.lines text)))
  case lines' of
    [] -> Right []
    Line n indent _ : _ | indent /= 0 -> failure n "Use column 1 for package fields"
    _ -> fst <$> block 0 lines'
  where
    prepare (n, raw) =
      let prefix = T.takeWhile (\c -> c == ' ' || c == '\t') raw
          content = T.stripEnd (T.drop (T.length prefix) raw)
      in if T.null content || "--" `T.isPrefixOf` content then Right []
         else if T.any (== '\t') prefix then failure n "Tabs in indentation are not supported"
         else Right [Line n (T.length prefix) content]
    block _ [] = Right ([], [])
    block indent allLines@(Line n current content : rest)
      | current < indent = Right ([], allLines)
      | current > indent = failure n "Invalid indentation"
      | otherwise = do
          let (children, after) = span (\(Line _ i _) -> i > indent) rest
              (key, suffix) = T.breakOn ":" content
              isField = not (T.null suffix) && not (T.null key)
                && T.all (\c -> isAlphaNum c || c == '-' || c == '_') key
          node <- if isField
            then Right (Field n (T.toLower key)
              (T.intercalate "\n" (T.strip (T.drop 1 suffix) : [t | Line _ _ t <- children])))
            else do
              when (T.any (`elem` ("{};" :: String)) content)
                (failure n "Explicit braces and semicolons are not supported")
              nested <- case children of
                [] -> failure n "A section must contain fields"
                Line _ i _ : _ -> do
                  (nestedNodes, remaining) <- block i children
                  case remaining of
                    Line m _ _ : _ -> failure m "Invalid indentation"
                    [] -> Right nestedNodes
              Right (Section n content nested)
          (nodes, remaining) <- block indent after
          Right (node : nodes, remaining)

report :: Result a -> ParseResult a
report = ParseResult [] . either (Left . (:| [])) Right

parsePackage :: BS.ByteString -> ParseResult Package
parsePackage bytes = report $ do
  nodes <- layout bytes
  let fields = [(n, k, v) | Field n k v <- nodes]
      sections = [(n, k, v) | Section n k v <- nodes]
  ensureUnique [(n, k) | (n,k,_) <- fields]
  (nameLine, pkgText) <- required "name" fields
  pkg <- value nameLine packageNameParser pkgText
  (versionLine, versionText) <- required "version" fields
  version <- value versionLine (lexeme versionParser) versionText
  (specLine, specText) <- required "cabal-version" fields
  spec <- value specLine (optional (symbol ">=") *> lexeme versionParser) specText
  exactFrom <- versionAt "2.2"
  when (">=" `T.isPrefixOf` T.strip specText && spec >= exactFrom)
    (failure specLine "Use an exact cabal-version from 2.2")
  lower <- versionAt "1.10"
  upper <- versionAt "3.14"
  unless (spec >= lower && spec <= upper)
    (failure specLine "Supported Cabal format versions are 1.10 through 3.14")
  let topFields = Map.fromList [(k,[v]) | (_,k,v) <- fields]
      hasSetup = any (\(_,k,_) -> T.toLower k == "custom-setup") sections
      bt = fromMaybe (if hasSetup then "Custom" else "Simple") (lookupField "build-type" fields)
  unless (bt `elem` ["Simple", "Configure", "Custom", "Make", "Hooks"])
    (failure 1 "Invalid build-type")
  (_, flags, components, repositories) <- foldM (section spec) (Map.empty, [], [], []) sections
  ensureUnique [(1, flagName f) | f <- flags]
  ensureUnique [(1, T.pack (show (componentKind c))) | c <- components]
  let knownFlags = map flagName flags
  mapM_ (checkFlags knownFlags . componentData) components
  let libraries = [x | Component (Library (Just x)) _ <- components]
      normalized = map (normalizeComponent pkg libraries) components
  pure (Package pkg version spec bt flags normalized topFields repositories)
  where
    versionAt t = either (failure 1) Right (parseVersion t)
    section spec (commons, flags, components, repositories) (n, header, body) = do
      let ws = case T.words header of { k:ks -> T.toLower k : ks; [] -> [] }
      case ws of
        ["common", key] -> do
          gate n spec "2.2" "common"
          when (Map.member key commons) (failure n "Duplicate common stanza")
          tree <- buildTree spec commons body
          pure (Map.insert key tree commons, flags, components, repositories)
        ["flag", key] -> do
          flag <- readFlag n key body
          pure (commons, flags ++ [flag], components, repositories)
        ["custom-setup"] -> pure (commons, flags, components, repositories)
        ["source-repository", kind] -> do
          repository <- readSourceRepository kind body
          pure (commons, flags, components, repositories ++ [repository])
        _ -> do
          kind <- case ws of
            ["library"] -> Right (Library Nothing)
            ["library", key] -> gate n spec "2.0" "named library" *> (Library . Just <$> value n name key)
            ["executable", key] -> Executable <$> value n name key
            ["test-suite", key] -> TestSuite <$> value n name key
            ["benchmark", key] -> Benchmark <$> value n name key
            ["foreign-library", key] -> ForeignLibrary <$> value n name key
            _ -> failure n ("Unsupported section: " <> header)
          tree <- buildTree spec commons body
          pure (commons, flags, components ++ [Component kind tree], repositories)

readSourceRepository :: Text -> [Node] -> Result SourceRepository
readSourceRepository kind body = SourceRepository kind <$> foldM field Map.empty body
  where
    field fields (Field _ key input) = Right (Map.insertWith (flip (++)) key [input] fields)
    field _ (Section n _ _) = failure n "A source repository cannot contain sections"

required :: Text -> [(Int, Text, Text)] -> Result (Int, Text)
required key xs = case [(n,v) | (n,k,v) <- xs, k == key] of
  x:_ -> Right x
  [] -> failure 1 ("Missing field: " <> key)

lookupField :: Text -> [(Int, Text, Text)] -> Maybe Text
lookupField key xs = case [v | (_,k,v) <- xs, k == key] of
  x:_ -> Just (T.strip x)
  [] -> Nothing

ensureUnique :: [(Int, Text)] -> Result ()
ensureUnique = go []
  where
    go _ [] = Right ()
    go seen ((n,k):rest)
      | k `elem` seen = failure n ("Duplicate name or field: " <> k)
      | otherwise = go (k:seen) rest

gate :: Int -> Version -> Text -> Text -> Result ()
gate n spec minimumVersion feature = case parseVersion minimumVersion of
  Right minimumSpec | spec >= minimumSpec -> Right ()
  _ -> failure n (feature <> " requires cabal-version " <> minimumVersion)

readFlag :: Int -> Text -> [Node] -> Result Flag
readFlag n key body = do
  key' <- T.toLower <$> value n name key
  let fields = [(i,k,v) | Field i k v <- body]
  unless (length fields == length body) (failure n "A flag cannot contain sections")
  ensureUnique [(i,k) | (i,k,_) <- fields]
  def <- maybe (Right True) (value n bool) (lookupField "default" fields)
  manual <- maybe (Right False) (value n bool) (lookupField "manual" fields)
  pure (Flag key' def manual)

buildTree :: Version -> Map.Map Text (Conditional BuildInfo) -> [Node] -> Result (Conditional BuildInfo)
buildTree spec commons = go False (Conditional emptyBuildInfo [])
  where
    go _ tree [] = Right tree
    go seen tree (Field n "import" input : rest) = do
      when seen (failure n "Put imports before other fields and conditions")
      gate n spec "2.2" "import"
      keys <- value n (name `sepBy1` symbol ",") input
      imported <- mapM (\key -> maybe (failure n ("Unknown common stanza: " <> key)) Right (Map.lookup key commons)) keys
      go False (foldl mergeTree tree imported) rest
    go _ tree (Field n key input : rest) = do
      info <- buildField spec n key input
      go True (tree {unconditional = mergeBuildInfo (unconditional tree) info}) rest
    go _ tree (Section n header body : rest)
      | Just expr <- T.stripPrefix "if " header = do
          cond <- value n conditionParser expr
          checkImportVersion n body
          yes <- buildTree spec commons body
          (no, after) <- case rest of
            Section m "else" other : remaining -> do
              checkImportVersion m other
              t <- buildTree spec commons other
              pure (Just t, remaining)
            _ -> pure (Nothing, rest)
          go True (tree {branches = branches tree ++ [Branch cond yes no]}) after
      | otherwise = failure n ("Unsupported conditional section: " <> header)
    checkImportVersion n body = when (any isImport body) (gate n spec "3.0" "conditional import")
    isImport (Field _ "import" _) = True
    isImport _ = False
    mergeTree (Conditional a bs) (Conditional b cs) = Conditional (mergeBuildInfo a b) (bs ++ cs)

buildField :: Version -> Int -> Text -> Text -> Result BuildInfo
buildField spec n key input
  | key `elem` ["signatures", "mixins", "reexported-modules"] = failure n ("Unsupported field: " <> key)
  | otherwise = case key of
      "buildable" -> (\x -> e {buildable = Just x}) <$> value n bool input
      "main-is" -> (\x -> e {mainIs = Just (T.unpack x)}) <$> value n token input
      "default-language" -> (\x -> e {defaultLanguage = Just x}) <$> value n name input
      "hs-source-dirs" -> paths (\x -> e {sourceDirs = x})
      "exposed-modules" -> modules (\x -> e {exposedModules = x})
      "other-modules" -> modules (\x -> e {otherModules = x})
      "autogen-modules" -> modules (\x -> e {autogenModules = x})
      "default-extensions" -> list (\x -> e {extensions = x})
      "extensions" -> removed "3.0" *> list (\x -> e {extensions = x})
      "build-depends" -> do
        rangeGates
        ds <- value n (optional (symbol ",") *> dependencyParser `sepEndBy` symbol ",") input
        when (any (any isNamed . dependencyLibraries) ds) (gate n spec "3.0" "library dependency targets")
        pure e {dependencies = ds}
      "build-tool-depends" -> do
        rangeGates
        gate n spec "2.0" "build-tool-depends"
        ds <- value n (optional (symbol ",") *> toolParser True `sepEndBy` symbol ",") input
        pure e {buildTools = ds}
      "build-tools" -> do
        removed "3.0"
        rangeGates
        ds <- value n (toolParser False `sepEndBy` symbol ",") input
        pure e {buildTools = ds}
      "c-sources" -> paths (\x -> e {cSources = x})
      "cxx-sources" -> paths (\x -> e {cxxSources = x})
      "include-dirs" -> paths (\x -> e {includeDirs = x})
      "install-includes" -> paths (\x -> e {installIncludes = x})
      "autogen-includes" -> paths (\x -> e {autogenIncludes = x})
      "cpp-options" -> opts (\x -> e {cppOptions = x})
      "cc-options" -> opts (\x -> e {ccOptions = x})
      "cxx-options" -> opts (\x -> e {cxxOptions = x})
      "ghc-options" -> opts (\x -> e {ghcOptions = x})
      _ -> pure e {extraFields = Map.singleton key [input]}
  where
    e = emptyBuildInfo
    removed v = case parseVersion v of
      Right limit | spec >= limit -> failure n (key <> " was removed in cabal-version " <> v)
      _ -> Right ()
    rangeGates = do
      when ("^>=" `T.isInfixOf` input) (gate n spec "2.0" "major version bounds")
      when ("{" `T.isInfixOf` input) (gate n spec "3.0" "set syntax")
      when ("," `T.isPrefixOf` T.strip input) (gate n spec "2.2" "leading comma")
    list set = set <$> value n items input
    paths set = list (set . map T.unpack)
    opts set = set <$> value n (many optionToken) input
    optionToken = lexeme (T.pack <$> (char '"' *> manyTill L.charLiteral (char '"')))
      <|> lexeme (T.pack <$> some (satisfy (\c -> not (isSpace c) && c /= '"')))
    modules set = do
      xs <- value n items input
      unless (all validModule xs) (failure n "Invalid module name")
      pure (set xs)
    validModule x = all validPart (T.splitOn "." x)
    validPart x = case T.uncons x of
      Just (c, cs) -> c >= 'A' && c <= 'Z' && T.all (\a -> isAlphaNum a || a == '_' || a == '\'') cs
      Nothing -> False
    isNamed MainLibrary = False
    isNamed _ = True

checkFlags :: [Text] -> Conditional BuildInfo -> Result ()
checkFlags known tree = mapM_ check (branches tree)
  where
    check (Branch c t e) = checkCondition c *> checkFlags known t *> mapM_ (checkFlags known) e
    checkCondition c = case c of
      FlagValue f | f `notElem` known -> failure 1 ("Unknown flag: " <> f)
      Not a -> checkCondition a
      And a b -> checkCondition a *> checkCondition b
      Or a b -> checkCondition a *> checkCondition b
      _ -> Right ()

normalizeComponent :: Text -> [Text] -> Component (Conditional BuildInfo) -> Component (Conditional BuildInfo)
normalizeComponent pkg libs (Component kind tree) = Component kind (go tree)
  where
    go (Conditional bi bs) = Conditional (bi {dependencies = map dep (dependencies bi)})
      [Branch c (go t) (go <$> e) | Branch c t e <- bs]
    dep d
      | dependencyPackage d `elem` libs && dependencyLibraries d == (MainLibrary :| []) =
          d {dependencyPackage = pkg, dependencyLibraries = NamedLibrary (dependencyPackage d) :| []}
      | otherwise = d {dependencyLibraries = fmap (target (dependencyPackage d)) (dependencyLibraries d)}
    target owner (NamedLibrary x) | x == owner = MainLibrary
    target _ x = x

parseBuildInfo :: BS.ByteString -> ParseResult BuildInfoFile
parseBuildInfo bytes = report $ do
  nodes <- layout bytes
  spec <- either (failure 1) Right (parseVersion "3.14")
  let (libNodes, rest) = break isExecutable nodes
  lib <- if null libNodes then pure Nothing else Just <$> fields spec libNodes
  exes <- groups spec rest
  ensureUnique [(1,k) | (k,_) <- exes]
  pure (BuildInfoFile lib (Map.fromList exes))
  where
    isExecutable (Field _ "executable" _) = True
    isExecutable _ = False
    fields spec = foldM (\acc node -> case node of
      Field n k v -> mergeBuildInfo acc <$> buildField spec n k v
      Section n _ _ -> failure n "Sections are not supported in buildinfo files") emptyBuildInfo
    groups _ [] = Right []
    groups spec (Field n "executable" key : rest) = do
      exe <- value n name key
      let (body, after) = break isExecutable rest
      bi <- fields spec body
      ((exe, bi) :) <$> groups spec after
    groups _ _ = failure 1 "Invalid executable buildinfo"
