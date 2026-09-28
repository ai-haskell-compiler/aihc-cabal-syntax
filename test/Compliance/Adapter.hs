{-# LANGUAGE OverloadedStrings #-}
module Compliance.Adapter (toCabal, runResult) where

import Control.Monad (foldM)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Aihc.Cabal as A
import qualified Distribution.CabalSpecVersion as C
import qualified Distribution.Compat.NonEmptySet as NES
import qualified Distribution.Compiler as C
import qualified Distribution.FieldGrammar.Parsec as C
import qualified Distribution.Fields.Field as C
import qualified Distribution.Fields.ParseResult as C
import qualified Distribution.PackageDescription as C
import qualified Distribution.PackageDescription.FieldGrammar as C
import qualified Distribution.Parsec as C
import qualified Distribution.Types.Version as C
import qualified Distribution.Types.VersionRange as C
import qualified Distribution.Utils.Path as C

-- Only fields retained as text use Cabal field grammars. Typed fields use
-- the project AST. This module cannot read the original file or reference AST.
toCabal :: A.Package -> Either String C.GenericPackageDescription
toCabal pkg = do
  spec <- maybe (Left "Unsupported Cabal format version") Right
    (C.cabalSpecFromVersionDigits (map fromInteger (NE.toList (A.versionNumbers (A.cabalVersion pkg)))))
  -- The typed name, version, and format version replace the retained text.
  let retained = foldr Map.delete (A.packageFields pkg) ["name", "version", "cabal-version"]
  pd <- fields spec C.packageDescriptionFieldGrammar
    (Map.insert "name" [text (A.packageName pkg)] (Map.insert "version" [text "0"] retained))
  repositories <- traverse (sourceRepository spec) (A.packageSourceRepositories pkg)
  flags <- traverse (flag spec) (A.packageFlags pkg)
  version <- convertVersion (A.packageVersion pkg)
  -- The grammar tells if the file sets a build type. The value is typed.
  buildType <- traverse (const (atom (A.buildType pkg))) (C.buildTypeRaw pd)
  setup <- traverse (fmap (\deps -> C.SetupBuildInfo deps False) . traverse convertDependency)
    (A.packageSetupDependencies pkg)
  let description = pd
        { C.package = C.PackageIdentifier (C.mkPackageName (T.unpack (A.packageName pkg))) version
        , C.specVersion = spec
        , C.buildTypeRaw = buildType
        , C.setupBuildInfo = setup
        , C.sourceRepos = repositories
        }
      initial = C.emptyGenericPackageDescription
        { C.packageDescription = description, C.genPackageFlags = flags }
  foldM (component spec) initial (A.packageComponents pkg)

flag :: C.CabalSpecVersion -> A.Flag -> Either String C.PackageFlag
flag spec f = do
  raw <- fields spec (C.flagFieldGrammar (C.mkFlagName (T.unpack (A.flagName f))))
    (Map.singleton "description" [text (A.flagDescription f)])
  pure raw { C.flagDefault = A.flagDefault f, C.flagManual = A.flagManual f }

sourceRepository :: C.CabalSpecVersion -> A.SourceRepository -> Either String C.SourceRepo
sourceRepository spec repository = do
  kind <- atom (A.sourceRepositoryKind repository)
  fields spec (C.sourceRepoFieldGrammar kind) (A.sourceRepositoryFields repository)

fields :: C.CabalSpecVersion -> C.ParsecFieldGrammar s a -> Map.Map Text [A.FieldValue] -> Either String a
fields spec grammar retained = either (Left . show) Right
  (snd (runResult (C.parseFieldGrammar spec (fieldMap retained) grammar)))
  where
    -- Keep each occurrence, each line, and each source position.
    fieldMap = Map.fromList . map (\(k, vs) -> (TE.encodeUtf8 k, map value vs)) . Map.toList
    value (A.FieldValue p ls) = C.MkNamelessField (position p)
      [C.FieldLine (position q) (TE.encodeUtf8 line) | A.FieldLine q line <- ls]
    position (A.Position row column) = C.Position row column

-- | Run a Cabal-syntax parser without a source name. The results keep the
-- error and warning format of Cabal-syntax 3.12.
runResult :: C.ParseResult () a
  -> ([C.PWarning], Either (Maybe C.Version, NonEmpty C.PError) a)
runResult result = case C.runParseResult result of
  (warnings, outcome) -> (map C.pwarning warnings, either (Left . fmap (fmap C.perror)) Right outcome)

-- | A field value for text that has no source position. Each line gets a
-- new row, so that Cabal free text rules give the same text.
text :: Text -> A.FieldValue
text value = A.FieldValue (A.Position 0 0)
  [A.FieldLine (A.Position row 1) line | not (T.null value), (row, line) <- zip [1 ..] (T.splitOn "\n" value)]

-- | Cabal-syntax makes these paths without checks or normalization.
path :: FilePath -> C.SymbolicPathX allowAbsolute from to
path = C.unsafeMakeSymbolicPath

atom :: C.Parsec a => Text -> Either String a
atom input = maybe (Left ("Cannot convert field value: " ++ T.unpack input)) Right (C.simpleParsec (T.unpack input))

convertVersion :: A.Version -> Either String C.Version
convertVersion v = do
  let digits = NE.toList (A.versionNumbers v)
  if any (> toInteger (maxBound :: Int)) digits
    then Left "Version component exceeds the Cabal integer limit"
    else Right (C.mkVersion (map fromInteger digits))

convertRange :: A.VersionRange -> Either String C.VersionRange
convertRange range = case range of
  A.AnyVersion -> Right C.anyVersion
  A.Equal v -> C.thisVersion <$> convertVersion v
  A.Later v -> C.laterVersion <$> convertVersion v
  A.Earlier v -> C.earlierVersion <$> convertVersion v
  A.AtLeast v -> C.orLaterVersion <$> convertVersion v
  A.AtMost v -> C.orEarlierVersion <$> convertVersion v
  A.MajorBound v -> C.majorBoundVersion <$> convertVersion v
  A.Both a b -> C.intersectVersionRanges <$> convertRange a <*> convertRange b
  A.EitherRange a b -> C.unionVersionRanges <$> convertRange a <*> convertRange b

convertDependency :: A.Dependency -> Either String C.Dependency
convertDependency dep = do
  range <- convertRange (A.dependencyRange dep)
  pure (C.Dependency (C.mkPackageName (T.unpack (A.dependencyPackage dep))) range
    (NES.fromNonEmpty (fmap target (A.dependencyLibraries dep))))
  where
    target A.MainLibrary = C.LMainLibName
    target (A.NamedLibrary name) = C.LSubLibName (C.mkUnqualComponentName (T.unpack name))

convertMixin :: A.Mixin -> Either String C.Mixin
convertMixin (A.Mixin pkg lib provides requires) = do
  provides' <- renaming provides
  requires' <- renaming requires
  pure (C.Mixin (C.mkPackageName (T.unpack pkg)) (library lib) (C.IncludeRenaming provides' requires'))
  where
    library A.MainLibrary = C.LMainLibName
    library (A.NamedLibrary name) = C.LSubLibName (C.mkUnqualComponentName (T.unpack name))
    renaming A.DefaultRenaming = Right C.DefaultRenaming
    renaming (A.ModuleRenaming pairs) = C.ModuleRenaming <$> traverse (\(a, b) -> (,) <$> atom a <*> atom b) pairs
    renaming (A.HidingRenaming names) = C.HidingRenaming <$> traverse atom names

convertBuildInfo :: C.BuildInfo -> A.BuildInfo -> Either String C.BuildInfo
convertBuildInfo raw bi = do
  other <- traverse atom (A.otherModules bi)
  autogen <- traverse atom (A.autogenModules bi)
  virtual <- traverse atom (A.virtualModules bi)
  language <- traverse atom (A.defaultLanguage bi)
  otherLanguages <- traverse atom (A.otherLanguages bi)
  extensions <- traverse atom (A.extensions bi)
  otherExtensions <- traverse atom (A.otherExtensions bi)
  legacyExtensions <- traverse atom (A.legacyExtensions bi)
  dependencies <- traverse convertDependency (A.dependencies bi)
  mixins <- traverse convertMixin (A.mixins bi)
  modern <- traverse modernTool [t | t <- A.buildTools bi, Just _ <- [A.toolPackage t]]
  legacy <- traverse legacyTool [t | t <- A.buildTools bi, Nothing <- [A.toolPackage t]]
  let C.PerCompilerFlavor _ ghcjs = C.options raw
  pure raw
    { C.buildable = fromMaybe True (A.buildable bi)
    , C.hsSourceDirs = map path (A.sourceDirs bi)
    , C.otherModules = other
    , C.autogenModules = autogen
    , C.virtualModules = virtual
    , C.defaultLanguage = language
    , C.otherLanguages = otherLanguages
    , C.defaultExtensions = extensions
    , C.otherExtensions = otherExtensions
    , C.oldExtensions = legacyExtensions
    , C.targetBuildDepends = dependencies
    , C.mixins = mixins
    , C.buildTools = legacy
    , C.buildToolDepends = modern
    , C.cSources = map path (A.cSources bi)
    , C.cxxSources = map path (A.cxxSources bi)
    , C.asmSources = map path (A.asmSources bi)
    , C.cmmSources = map path (A.cmmSources bi)
    , C.jsSources = map path (A.jsSources bi)
    , C.includeDirs = map path (A.includeDirs bi)
    , C.includes = map path (A.includes bi)
    , C.installIncludes = map path (A.installIncludes bi)
    , C.autogenIncludes = map path (A.autogenIncludes bi)
    , C.extraLibDirs = map path (A.extraLibDirs bi)
    , C.extraLibDirsStatic = map path (A.extraLibDirsStatic bi)
    , C.frameworks = map (path . T.unpack) (A.frameworks bi)
    , C.extraFrameworkDirs = map path (A.extraFrameworkDirs bi)
    , C.cppOptions = map T.unpack (A.cppOptions bi)
    , C.ccOptions = map T.unpack (A.ccOptions bi)
    , C.cxxOptions = map T.unpack (A.cxxOptions bi)
    , C.options = C.PerCompilerFlavor (map T.unpack (A.ghcOptions bi)) ghcjs
    }
  where
    modernTool tool = case A.toolPackage tool of
      Nothing -> Left "Missing build tool package"
      Just name -> C.ExeDependency (C.mkPackageName (T.unpack name))
        (C.mkUnqualComponentName (T.unpack (A.toolName tool))) <$> convertRange (A.toolRange tool)
    legacyTool tool = C.LegacyExeDependency (T.unpack (A.toolName tool)) <$> convertRange (A.toolRange tool)

convertCondition :: A.Condition -> Either String (C.Condition C.ConfVar)
convertCondition cond = case cond of
  A.Literal b -> Right (C.Lit b)
  A.OS name -> C.Var . C.OS <$> atom name
  A.Arch name -> C.Var . C.Arch <$> atom name
  A.Impl name range -> C.Var <$> (C.Impl <$> atom name <*> convertRange range)
  A.FlagValue name -> Right (C.Var (C.PackageFlag (C.mkFlagName (T.unpack name))))
  A.Not a -> C.CNot <$> convertCondition a
  A.And a b -> C.CAnd <$> convertCondition a <*> convertCondition b
  A.Or a b -> C.COr <$> convertCondition a <*> convertCondition b

convertTree :: (A.BuildInfo -> Either String a)
  -> A.Conditional A.BuildInfo -> Either String (C.CondTree C.ConfVar a)
convertTree convert (A.Conditional bi branches) = do
  value <- convert bi
  children <- traverse branch branches
  pure (C.CondNode value children)
  where
    branch (A.Branch cond yes no) = C.CondBranch <$> convertCondition cond
      <*> convertTree convert yes <*> traverse (convertTree convert) no

component :: C.CabalSpecVersion -> C.GenericPackageDescription
  -> A.Component (A.Conditional A.BuildInfo) -> Either String C.GenericPackageDescription
component spec gpd (A.Component kind tree) = case kind of
  A.Library target -> do
    let libName = case target of
          A.MainLibrary -> C.LMainLibName
          A.NamedLibrary n -> C.LSubLibName (componentName n)
    converted <- convertTree (library libName) tree
    pure $ case target of
      A.MainLibrary -> gpd { C.condLibrary = Just converted }
      A.NamedLibrary n -> gpd { C.condSubLibraries = C.condSubLibraries gpd ++ [(componentName n, converted)] }
  A.Executable name -> do
    converted <- convertTree (executable (componentName name)) tree
    pure gpd { C.condExecutables = C.condExecutables gpd ++ [(componentName name, converted)] }
  A.ForeignLibrary name -> do
    converted <- convertTree (foreignLibrary (componentName name)) tree
    pure gpd { C.condForeignLibs = C.condForeignLibs gpd ++ [(componentName name, converted)] }
  A.TestSuite name -> do
    converted <- convertTree testSuite tree
    pure gpd { C.condTestSuites = C.condTestSuites gpd ++ [(componentName name, converted)] }
  A.Benchmark name -> do
    converted <- convertTree benchmark tree
    pure gpd { C.condBenchmarks = C.condBenchmarks gpd ++ [(componentName name, converted)] }
  where
    componentName = C.mkUnqualComponentName . T.unpack
    library name bi = do
      raw <- fields spec (C.libraryFieldGrammar name) (A.extraFields bi)
      info <- convertBuildInfo (C.libBuildInfo raw) bi
      exposed <- traverse atom (A.exposedModules bi)
      pure raw { C.libBuildInfo = info, C.exposedModules = exposed }
    executable name bi = do
      raw <- fields spec (C.executableFieldGrammar name) (A.extraFields bi)
      info <- convertBuildInfo (C.buildInfo raw) bi
      pure raw { C.buildInfo = info, C.modulePath = path (fromMaybe "" (A.mainIs bi)) }
    foreignLibrary name bi = do
      raw <- fields spec (C.foreignLibFieldGrammar name) (A.extraFields bi)
      info <- convertBuildInfo (C.foreignLibBuildInfo raw) bi
      pure raw { C.foreignLibBuildInfo = info }
    testSuite bi = do
      raw <- fields spec C.testSuiteFieldGrammar (A.extraFields bi)
      info <- convertBuildInfo (C._testStanzaBuildInfo raw) bi
      result (C.validateTestSuite spec C.zeroPos raw
        { C._testStanzaBuildInfo = info, C._testStanzaMainIs = path <$> A.mainIs bi })
    benchmark bi = do
      raw <- fields spec C.benchmarkFieldGrammar (A.extraFields bi)
      info <- convertBuildInfo (C._benchmarkStanzaBuildInfo raw) bi
      result (C.validateBenchmark spec C.zeroPos raw
        { C._benchmarkStanzaBuildInfo = info, C._benchmarkStanzaMainIs = path <$> A.mainIs bi })
    result = either (Left . show) Right . snd . runResult
