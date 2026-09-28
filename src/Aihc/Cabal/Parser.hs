{-# LANGUAGE OverloadedStrings #-}
-- | Read package descriptions. The rules follow the package parser of
-- Cabal-syntax 3.12. Cabal format version 3.14 and build type @Hooks@ are
-- also accepted.
module Aihc.Cabal.Parser (parsePackage, parseBuildInfo) where

import Control.Monad (foldM, guard, unless, when)
import Data.Bits (shiftL, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isAlphaNum)
import Data.List (find, partition)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Megaparsec (eof, runParser, takeWhile1P, (<|>))
import Text.Megaparsec.Char (space)
import Aihc.Cabal.Condition (parseCondition)
import Aihc.Cabal.Fields (Field (..), SectionArg (..), readFields)
import Aihc.Cabal.Quirks (patchQuirks)
import Aihc.Cabal.Types
import Aihc.Cabal.Values
import Aihc.Cabal.Version

type Result a = Either Diagnostic a

type Fields = Map Text [FieldValue]

-- | A section inside a list of fields: position, name, arguments, and contents.
data SectionInfo = SectionInfo Position Text [SectionArg] [Field]

failAt :: Position -> Text -> Result a
failAt (Position r c) = Left . Diagnostic r c

zeroPosition :: Position
zeroPosition = Position 0 0

report :: Result a -> ParseResult a
report = ParseResult [] . either (Left . (:| [])) Right

-- | Decode UTF-8 text. For invalid input, use the Cabal-syntax decoder: it
-- replaces each invalid sequence with U+FFFD and continues.
decode :: BS.ByteString -> Text
decode bytes = either (const (T.pack (lenient (BS.unpack bytes)))) id (TE.decodeUtf8' bytes)
  where
    lenient [] = []
    lenient (c : cs)
      | c <= 0x7f = toEnum (fromIntegral c) : lenient cs
      | c <= 0xbf = replacement : lenient cs
      | c <= 0xdf = case cs of
          c1 : rest | c1 .&. 0xc0 == 0x80 ->
            let d = (fromIntegral (c .&. 0x1f) `shiftL` 6) .|. fromIntegral (c1 .&. 0x3f)
            in (if d >= 0x80 then toEnum d else replacement) : lenient rest
          _ -> replacement : lenient cs
      | c <= 0xef = more (3 :: Int) 0x800 cs (fromIntegral (c .&. 0xf))
      | c <= 0xf7 = more 4 0x10000 cs (fromIntegral (c .&. 0x7))
      | c <= 0xfb = more 5 0x200000 cs (fromIntegral (c .&. 0x3))
      | c <= 0xfd = more 6 0x4000000 cs (fromIntegral (c .&. 0x1))
      | otherwise = replacement : lenient cs
    more 1 overlong cs acc
      | overlong <= acc && acc <= 0x10ffff && (acc < 0xd800 || 0xdfff < acc) = toEnum acc : lenient cs
      | otherwise = replacement : lenient cs
    more n overlong (c : cs) acc
      | c .&. 0xc0 == 0x80 = more (n - 1) overlong cs ((acc `shiftL` 6) .|. fromIntegral (c .&. 0x3f))
    more _ _ cs _ = replacement : lenient cs
    replacement = '\xfffd' 

readInput :: BS.ByteString -> Result [Field]
readInput bytes = readFields (fromMaybe text (T.stripPrefix "\xfeff" text))
  where text = decode bytes

parseField :: Parser a -> FieldValue -> Result a
parseField p fv = either (failAt (fieldPosition fv)) Right (runValue p (fieldText fv))

-- | A singular field. The last value wins. As in Cabal-syntax, the first of
-- several values is not parsed.
singular :: (FieldValue -> Result a) -> [FieldValue] -> Result (Maybe a)
singular _ [] = Right Nothing
singular f [x] = Just <$> f x
singular f (_ : xs) = Just . last <$> mapM f xs

-- | A singular field that has no value when its text is empty.
optionalField :: Parser a -> [FieldValue] -> Result (Maybe a)
optionalField p = fmap (>>= id) . singular one
  where one fv = if null (fieldLines fv) then Right Nothing else Just <$> parseField p fv

monoidal :: Parser [a] -> [FieldValue] -> Result [a]
monoidal p = fmap concat . mapM (parseField p)

takeFields :: [Field] -> (Fields, [Field])
takeFields fields = (collect [(n, FieldValue p ls) | Field p n ls <- leading], rest)
  where (leading, rest) = span isField fields

isField :: Field -> Bool
isField Field {} = True
isField Section {} = False

collect :: [(Text, FieldValue)] -> Fields
collect xs = Map.fromListWith (flip (++)) [(k, [v]) | (k, v) <- xs]

-- | Separate fields from sections. Consecutive sections form one group.
partitionFields :: [Field] -> (Fields, [[SectionInfo]])
partitionFields fields = (collect [(n, FieldValue p ls) | Field p n ls <- fields], groups fields)
  where
    groups xs = case dropWhile isField xs of
      [] -> []
      ys -> let (ss, rest) = break isField ys
            in [SectionInfo p n a b | Section p n a b <- ss] : groups rest

-- | Fields of a section that cannot contain sections.
plainFields :: [Field] -> Result Fields
plainFields body = case sections of
  SectionInfo p n _ _ : _ -> failAt p ("invalid subsection " <> T.pack (show n))
  [] -> Right fs
  where (fs, sections) = fmap concat (partitionFields body)

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

-- | The Cabal specification version for version digits, or 'Nothing' for an
-- unknown version.
knownSpec :: NonEmpty Integer -> Maybe Version
knownSpec ds = case NE.toList ds of
  v | v `elem` [[3, 14], [3, 12], [3, 8], [3, 6], [3, 4], [3, 0], [2, 4], [2, 2], [2, 0]] -> Just (specVersion v)
    | v >= [1, 25] -> Nothing
    | otherwise -> specVersion . snd <$> find ((v >=) . fst) older
  where
    older =
      [ ([1, 23], [1, 24]), ([1, 21], [1, 22]), ([1, 19], [1, 20]), ([1, 17], [1, 18])
      , ([1, 11], [1, 12]), ([1, 9], [1, 10]), ([1, 7], [1, 8]), ([1, 5], [1, 6])
      , ([1, 3], [1, 4]), ([1, 1], [1, 2]), ([], [1, 0]) ]

-- | The value of a @cabal-version@ field: a version or a range.
specVersionParser :: Version -> Parser Version
specVersionParser spec = do
  v <- versionParser <|> range
  maybe (fail ("Unknown cabal spec version specified: " ++ T.unpack (renderVersion v))) pure (knownSpec (versionNumbers v))
  where
    range = do
      v <- lowestVersion <$> rangeParser spec
      when (v >= specVersion [2, 1]) (fail "cabal-version higher than 2.2 cannot be specified as a range")
      pure v

-- | The smallest lower bound of a range. An empty range gives version 0.
lowestVersion :: VersionRange -> Version
lowestVersion range = case [v | ((v, _), _) <- intervals range] of
  [] -> zero
  vs -> minimum vs
  where
    zero = specVersion [0]
    intervals r = case r of
      AnyVersion -> [((zero, True), Nothing)]
      Equal v -> [((v, True), Just (v, True))]
      Later v -> [((v, False), Nothing)]
      AtLeast v -> [((v, True), Nothing)]
      Earlier v -> nonEmpty ((zero, True), Just (v, False))
      AtMost v -> [((zero, True), Just (v, True))]
      MajorBound v -> intervals (Both (AtLeast v) (Earlier (majorUpper v)))
      Both a b -> concat [nonEmpty (maxLower l1 l2, minUpper u1 u2) | (l1, u1) <- intervals a, (l2, u2) <- intervals b]
      EitherRange a b -> intervals a ++ intervals b
    nonEmpty i@((l, li), u) = case u of
      Nothing -> [i]
      Just (h, hi) | l < h || (l == h && li && hi) -> [i]
                   | otherwise -> []
    maxLower a@(v, i) b@(w, j)
      | v /= w = if v > w then a else b
      | otherwise = (v, i && j)
    minUpper Nothing u = u
    minUpper u Nothing = u
    minUpper (Just a@(v, i)) (Just b@(w, j))
      | v /= w = Just (if v < w then a else b)
      | otherwise = Just (v, i && j)
    majorUpper v = case NE.toList (versionNumbers v) of
      [x] -> specVersion [x, 1]
      x : y : _ -> specVersion [x, y + 1]
      [] -> zero

-- | Read the version on the first line, as in @cabal-version: 3.0@.
scanSpecVersion :: BS.ByteString -> Maybe Version
scanSpecVersion bytes = do
  line : _ <- Just (BS8.lines bytes)
  let normalized = BS.map lower (BS.filter (/= 0x20) line)
  [key, text] <- Just (BS8.split ':' normalized)
  guard (key == "cabal-version")
  v <- either (const Nothing) Just (runParser (versionParser <* space <* eof) "" (decode text))
  guard (length (versionNumbers v) `elem` [2, 3])
  pure v
  where
    lower w = if w > 0x40 && w < 0x5b then w + 0x20 else w

-- | Change a file without sections to the section format.
sectionize :: [Field] -> [Field]
sectionize fields
  | not (all isField fields) = fields
  | otherwise = header ++ library ++ executables exes0
  where
    name (Field _ n _) = n
    name (Section _ n _ _) = n
    (header0, exes0) = break ((== "executable") . name) fields
    (header, libraryFields0) = partition ((`notElem` libraryFieldNames) . name) header0
    (deps, libraryFields) = partition ((== "build-depends") . name) libraryFields0
    library = case libraryFields of
      [] -> []
      f : _ -> [Section (fieldPos f) "library" [] (deps ++ libraryFields)]
    executables (Field p "executable" ls : rest) =
      let (body, after) = break ((== "executable") . name) rest
          exeName = T.dropWhile (== ' ') (T.dropWhileEnd (== ' ') (T.intercalate "\n" (map fieldLineText ls)))
      in Section p "executable" [ArgName p exeName] (deps ++ body) : executables after
    executables _ = []
    fieldPos (Field p _ _) = p
    fieldPos (Section p _ _ _) = p

-- | Fields of the build information grammar of Cabal-syntax.
buildInfoFieldNames :: [Text]
buildInfoFieldNames =
  [ "buildable", "build-tools", "build-tool-depends", "cpp-options", "asm-options", "cmm-options"
  , "cc-options", "cxx-options", "ld-options", "hsc2hs-options", "pkgconfig-depends", "frameworks"
  , "extra-framework-dirs", "asm-sources", "cmm-sources", "c-sources", "cxx-sources", "js-sources"
  , "hs-source-dirs", "hs-source-dir", "other-modules", "virtual-modules", "autogen-modules"
  , "default-language", "other-languages", "default-extensions", "other-extensions", "extensions"
  , "extra-libraries", "extra-libraries-static", "extra-ghci-libraries", "extra-bundled-libraries"
  , "extra-library-flavours", "extra-dynamic-library-flavours", "extra-lib-dirs"
  , "extra-lib-dirs-static", "include-dirs", "includes", "autogen-includes", "install-includes"
  , "ghc-options", "ghcjs-options", "jhc-options", "hugs-options", "nhc98-options"
  , "ghc-prof-options", "ghcjs-prof-options", "ghc-shared-options", "ghcjs-shared-options"
  , "build-depends", "mixins" ]

libraryFieldNames :: [Text]
libraryFieldNames = ["exposed-modules", "reexported-modules", "signatures", "exposed"] ++ buildInfoFieldNames

data Kind = CommonKind | LibraryKind | ExecutableKind | TestKind | BenchmarkKind | ForeignKind
  deriving Eq

type Commons = Map Text (Conditional BuildInfo)

mergeTree :: Conditional BuildInfo -> Conditional BuildInfo -> Conditional BuildInfo
mergeTree (Conditional a bs) (Conditional b cs) = Conditional (mergeBuildInfo a b) (bs ++ cs)

isImport :: Field -> Bool
isImport (Field _ "import" _) = True
isImport _ = False

-- | Read the imports at the start of a list of fields. Cabal-syntax ignores
-- the other imports with a warning.
imports :: Version -> Commons -> [Field] -> Result ([Conditional BuildInfo], [Field])
imports spec commons
  | specAtLeast [2, 2] spec = go []
  | otherwise = \fields -> Right ([], filter (not . isImport) fields)
  where
    go acc (Field p "import" ls : rest) = do
      names <- parseField (commaList spec token) (FieldValue p ls)
      trees <- mapM (\n -> maybe (failAt p ("Undefined common stanza imported: " <> n)) Right (Map.lookup n commons)) names
      go (acc ++ trees) rest
    go acc rest = Right (acc, filter (not . isImport) rest)

stanza :: Version -> Kind -> Commons -> [Field] -> Result (Conditional BuildInfo)
stanza spec kind commons fields = do
  (imported, rest) <- imports spec commons fields
  tree <- condTree spec kind commons rest
  pure (foldr mergeTree tree imported)

condTree :: Version -> Kind -> Commons -> [Field] -> Result (Conditional BuildInfo)
condTree spec kind commons fields0 = do
  (imported, fields) <- if specAtLeast [3, 0] spec
    then imports spec commons fields0
    else Right ([], filter (not . isImport) fields0)
  let (fs, groups) = partitionFields fields
  info <- buildInfoFields spec kind fs
  branches' <- concat <$> mapM ifs groups
  pure (foldr mergeTree (Conditional info branches') imported)
  where
    subtree = condTree spec kind commons
    ifs [] = Right []
    ifs (SectionInfo p "if" args body : rest) = do
      c <- conditionAt p args
      yes <- subtree body
      (no, rest') <- elses rest
      pure (Branch c yes no : rest')
    ifs (_ : rest) = ifs rest
    elses (SectionInfo p "else" args body : rest) = do
      unless (null args) (failAt p "`else` section has section arguments")
      no <- subtree body
      rest' <- ifs rest
      pure (Just no, rest')
    elses (SectionInfo p "elif" args body : rest)
      | specAtLeast [2, 2] spec = do
          c <- conditionAt p args
          yes <- subtree body
          (no, rest') <- elses rest
          pure (Just (Conditional emptyBuildInfo [Branch c yes no]), rest')
      | otherwise = (,) Nothing <$> ifs rest
    elses rest = (,) Nothing <$> ifs rest
    conditionAt p args = maybe (failAt p "Invalid condition") Right (parseCondition args)

-- | Parse the fields of one section level. Fields that the Cabal
-- specification version does not support are ignored.
buildInfoFields :: Version -> Kind -> Fields -> Result BuildInfo
buildInfoFields spec kind fs = do
  mapM_ removed [([3, 0], "hs-source-dir"), ([3, 0], "extensions"), ([3, 0], "build-tools")]
  buildable' <- singular (parseField bool) (get "buildable")
  dirs <- monoidal (spaceList spec sourceDir) (get "hs-source-dirs")
  oldDirs <- monoidal (spaceList spec sourceDir) (get "hs-source-dir")
  exposed <- if kind == LibraryKind then modules (get "exposed-modules") else pure []
  other <- modules (get "other-modules")
  autogen <- modules (since [2, 0] "autogen-modules")
  virtual <- modules (since [2, 2] "virtual-modules")
  main <- if kind `elem` [ExecutableKind, TestKind, BenchmarkKind]
    then optionalField filePath (get "main-is") else pure Nothing
  language <- optionalField (quoted languageName) (since [1, 10] "default-language")
  otherLanguages' <- names (since [1, 10] "other-languages")
  extensions' <- names (since [1, 10] "default-extensions")
  otherExtensions' <- names (get "other-extensions")
  legacy <- names (get "extensions")
  deps <- monoidal (commaList spec (dependency spec)) (get "build-depends")
  mixins' <- monoidal (commaList spec (mixin spec)) (since [2, 0] "mixins")
  legacyTools <- monoidal (commaList spec (legacyExeDependency spec)) (get "build-tools")
  tools <- monoidal (commaList spec (exeDependency spec)) (get "build-tool-depends")
  cs <- paths (get "c-sources")
  cxx <- paths (since [2, 2] "cxx-sources")
  asm <- paths (since [3, 0] "asm-sources")
  cmm <- paths (since [3, 0] "cmm-sources")
  js <- paths (get "js-sources")
  includeDirs' <- paths (get "include-dirs")
  includes' <- paths (get "includes")
  installIncludes' <- paths (get "install-includes")
  autogenIncludes' <- paths (since [3, 0] "autogen-includes")
  libDirs <- paths (get "extra-lib-dirs")
  staticLibDirs <- paths (since [3, 8] "extra-lib-dirs-static")
  frameworks' <- monoidal (spaceList spec token) (get "frameworks")
  frameworkDirs <- paths (get "extra-framework-dirs")
  cpp <- options (get "cpp-options")
  cc <- options (get "cc-options")
  cxxOpts <- options (since [2, 2] "cxx-options")
  ghc <- options (get "ghc-options")
  pure BuildInfo
    { buildable = buildable', sourceDirs = map T.unpack (dirs ++ oldDirs), exposedModules = exposed
    , otherModules = other, autogenModules = autogen, virtualModules = virtual
    , mainIs = T.unpack <$> main, defaultLanguage = language, otherLanguages = otherLanguages'
    , extensions = extensions', otherExtensions = otherExtensions', legacyExtensions = legacy
    , dependencies = deps, mixins = mixins', buildTools = legacyTools ++ tools, cSources = map T.unpack cs
    , cxxSources = map T.unpack cxx, asmSources = map T.unpack asm, cmmSources = map T.unpack cmm
    , jsSources = map T.unpack js, includeDirs = map T.unpack includeDirs'
    , includes = map T.unpack includes', installIncludes = map T.unpack installIncludes'
    , autogenIncludes = map T.unpack autogenIncludes', extraLibDirs = map T.unpack libDirs
    , extraLibDirsStatic = map T.unpack staticLibDirs, frameworks = frameworks'
    , extraFrameworkDirs = map T.unpack frameworkDirs, cppOptions = cpp, ccOptions = cc
    , cxxOptions = cxxOpts, ghcOptions = ghc, extraFields = rest
    }
  where
    get k = Map.findWithDefault [] k fs
    since v k = if specAtLeast v spec then get k else []
    removed (v, k) = case get k of
      fv : _ | specAtLeast v spec ->
        failAt (fieldPosition fv) ("The field " <> k <> " is removed in cabal-version " <> renderVersion (specVersion v))
      _ -> Right ()
    modules = monoidal (spaceList spec (quoted moduleName))
    names = monoidal (spaceList spec (quoted languageName))
    paths = monoidal (spaceList spec filePath)
    options = monoidal (optionList token')
    typed = typedFields ++ ["exposed-modules" | kind == LibraryKind]
      ++ ["main-is" | kind `elem` [ExecutableKind, TestKind, BenchmarkKind]]
    rest = Map.filterWithKey keep fs
    keep k _
      | k `elem` typed = False
      | kind == CommonKind = k `elem` buildInfoFieldNames || "x-" `T.isPrefixOf` k
      | otherwise = True

typedFields :: [Text]
typedFields =
  [ "buildable", "hs-source-dirs", "hs-source-dir", "other-modules", "autogen-modules"
  , "virtual-modules", "default-language", "other-languages", "default-extensions"
  , "other-extensions", "extensions", "build-depends", "mixins", "build-tools", "build-tool-depends"
  , "c-sources", "cxx-sources", "asm-sources", "cmm-sources", "js-sources", "include-dirs"
  , "includes", "install-includes", "autogen-includes", "extra-lib-dirs", "extra-lib-dirs-static"
  , "frameworks", "extra-framework-dirs", "cpp-options", "cc-options", "cxx-options", "ghc-options" ]

-- | The name argument of a component, common stanza, or flag section.
sectionName :: Position -> [SectionArg] -> Result Text
sectionName p args = case args of
  [ArgName _ x] -> Right x
  [ArgString _ x] -> Right x
  [] -> failAt p "name required"
  _ -> failAt p "Invalid name"

data State = State
  { stateCommons :: Commons
  , stateFlags :: [Flag]
  , stateComponents :: [Component (Conditional BuildInfo)]
  , stateRepositories :: [SourceRepository]
  , stateSetup :: Maybe [Dependency]
  }

-- | Parse a package description. First apply the Cabal-syntax patches for
-- known Hackage files. A patched file gets a warning, as in Cabal-syntax.
parsePackage :: BS.ByteString -> ParseResult Package
parsePackage input = case patchQuirks input of
  (patched, bytes) -> (report (parsePatched bytes))
    { parseWarnings = [Diagnostic 0 0 "Legacy cabal file" | patched] }

parsePatched :: BS.ByteString -> Result Package
parsePatched bytes = do
  fields0 <- readInput bytes
  let (top, sections) = takeFields (sectionize fields0)
      get k = Map.findWithDefault [] k top
  spec <- case scanSpecVersion bytes of
    Just v -> maybe (failAt zeroPosition "Unsupported cabal format version") Right (knownSpec (versionNumbers v))
    Nothing -> case get "cabal-version" of
      [] -> Right (specVersion [1, 0])
      values -> do
        v <- parseField (specVersionParser (specVersion [1, 24])) (last values)
        when (v >= specVersion [2, 2]) (failAt (fieldPosition (last values))
          "cabal-version should be at the beginning of the file starting with spec version 2.2")
        pure v
  parsedSpec <- optionalField (specVersionParser spec) (get "cabal-version")
  unless (fromMaybe (specVersion [1, 0]) parsedSpec == spec)
    (failAt zeroPosition "Scanned and parsed cabal-versions don't match")
  pkg <- required "name" componentName top
  version <- required "version" versionParser top
  rawBuildType <- optionalField (buildTypeValue spec) (get "build-type")
  State _ flags components repositories setup <- foldM (section spec) (State Map.empty [] [] [] Nothing) sections
  let bt = fromMaybe (if specAtLeast [2, 2] spec && not (isJust setup) then "Simple" else "Custom") rawBuildType
      knownFlags = map flagName flags
  when (bt == "Custom" && not (isJust setup) && specAtLeast [1, 24] spec)
    (failAt zeroPosition "Since cabal-version: 1.24 specifying custom-setup section is mandatory")
  mapM_ (checkFlags knownFlags . componentData) components
  let libraries = [x | not (specAtLeast [3, 4] spec), Component (Library (Just x)) _ <- components]
      internal = internalDependencies pkg libraries
      internalMixin m
        | mixinPackage m `elem` libraries, mixinLibrary m == MainLibrary =
            m {mixinPackage = pkg, mixinLibrary = if mixinPackage m == pkg then MainLibrary else NamedLibrary (mixinPackage m)}
        | otherwise = m
  pure Package
    { packageName = pkg, packageVersion = version, cabalVersion = spec, buildType = bt
    , packageFlags = flags
    , packageComponents =
        [ Component k (mapTree (\bi -> bi {dependencies = internal (dependencies bi), mixins = map internalMixin (mixins bi)}) t)
        | Component k t <- components ]
    , packageFields = top, packageSourceRepositories = repositories
    , packageSetupDependencies = internal <$> setup
    }

required :: Text -> Parser a -> Fields -> Result a
required key p fields = do
  value <- singular (parseField p) (Map.findWithDefault [] key fields)
  maybe (failAt zeroPosition (T.pack (show key) <> " field missing")) Right value

section :: Version -> State -> Field -> Result State
section _ st Field {} = Right st
section spec st (Section p name args body) = case name of
  "common"
    | not (specAtLeast [2, 2] spec) -> Right st
    | otherwise -> do
        key <- sectionName p args
        tree <- stanza spec CommonKind commons body
        when (Map.member key commons) (failAt p ("Duplicate common stanza: " <> key))
        Right st {stateCommons = Map.insert key tree commons}
  "library"
    | null args -> do
        when (any isMainLibrary (stateComponents st))
          (failAt p "Multiple main libraries; have you forgotten to specify a name for an internal library?")
        component (Library Nothing) LibraryKind
    | otherwise -> sectionName p args >>= \n -> component (Library (Just n)) LibraryKind
  "foreign-library" -> sectionName p args >>= \n -> component (ForeignLibrary n) ForeignKind
  "executable" -> sectionName p args >>= \n -> component (Executable n) ExecutableKind
  "test-suite" -> sectionName p args >>= \n -> component (TestSuite n) TestKind
  "benchmark" -> sectionName p args >>= \n -> component (Benchmark n) BenchmarkKind
  "flag" -> do
    n <- sectionName p args
    key <- either (failAt p) Right (runValue flagNameValue n)
    fields <- plainFields body
    let get k = Map.findWithDefault [] k fields
    def <- singular (parseField bool) (get "default")
    manual <- singular (parseField bool) (get "manual")
    let description = maybe "" (freeText spec . last) (nonEmpty (get "description"))
    Right st {stateFlags = stateFlags st ++ [Flag key (fromMaybe True def) (fromMaybe False manual) description]}
  "custom-setup" | null args -> do
    fields <- plainFields body
    deps <- monoidal (commaList spec (dependency spec)) (Map.findWithDefault [] "setup-depends" fields)
    Right st {stateSetup = Just deps}
  "source-repository" -> case args of
    [ArgName q kind] -> do
      kind' <- either (failAt q) Right (runValue (takeWhile1P Nothing (\c -> isAlphaNum c || c == '_' || c == '-')) kind)
      fields <- plainFields body
      Right st {stateRepositories = stateRepositories st ++ [SourceRepository kind' fields]}
    [] -> failAt p "'source-repository' requires exactly one argument"
    _ -> failAt p "Invalid source-repository kind"
  _ -> Right st
  where
    commons = stateCommons st
    component kind grammar = do
      tree <- stanza spec grammar commons body
      Right st {stateComponents = stateComponents st ++ [Component kind tree]}
    isMainLibrary (Component (Library Nothing) _) = True
    isMainLibrary _ = False
    nonEmpty [] = Nothing
    nonEmpty xs = Just xs

mapTree :: (BuildInfo -> BuildInfo) -> Conditional BuildInfo -> Conditional BuildInfo
mapTree f (Conditional bi bs) = Conditional (f bi) [Branch c (mapTree f t) (mapTree f <$> e) | Branch c t e <- bs]

-- | Before cabal-version 3.4, the name of an internal library refers to that
-- library of the same package. If the dependency also names other libraries,
-- Cabal-syntax 3.12 keeps the original dependency after the new one.
internalDependencies :: Text -> [Text] -> [Dependency] -> [Dependency]
internalDependencies pkg libraries = concatMap change
  where
    change d@(Dependency name range libs)
      | name `elem` libraries, MainLibrary `elem` libs =
          Dependency pkg range (NamedLibrary name :| []) : [d | any (/= MainLibrary) libs]
      | otherwise = [d]

checkFlags :: [Text] -> Conditional BuildInfo -> Result ()
checkFlags known tree = mapM_ check (branches tree)
  where
    check (Branch c t e) = checkCondition c *> checkFlags known t *> mapM_ (checkFlags known) e
    checkCondition c = case c of
      FlagValue f | f `notElem` known -> failAt zeroPosition ("These flags are used without having been defined: " <> f)
      Not a -> checkCondition a
      And a b -> checkCondition a *> checkCondition b
      Or a b -> checkCondition a *> checkCondition b
      _ -> Right ()

-- | Read a @.buildinfo@ file, as Cabal-syntax reads hooked build information.
parseBuildInfo :: BS.ByteString -> ParseResult BuildInfoFile
parseBuildInfo bytes = report $ do
  fields <- readInput bytes
  let (header, rest) = break isExecutable fields
  libraryFields <- plainFields header
  lib <- if Map.null libraryFields then pure Nothing else Just <$> buildInfoFields latestSpec CommonKind libraryFields
  exes <- groups rest
  ensureUnique (map fst exes)
  pure (BuildInfoFile lib (Map.fromList exes))
  where
    isExecutable (Field _ "executable" _) = True
    isExecutable _ = False
    groups (Field p "executable" ls : rest) = do
      exe <- parseField componentName (FieldValue p ls)
      let (body, after) = break isExecutable rest
      fs <- plainFields body
      bi <- buildInfoFields latestSpec CommonKind fs
      ((exe, bi) :) <$> groups after
    groups _ = Right []
    ensureUnique names = case [n | (i, n) <- zip [0 :: Int ..] names, n `elem` take i names] of
      n : _ -> failAt zeroPosition ("Duplicate executable: " <> n)
      [] -> Right ()
