{-# LANGUAGE OverloadedStrings #-}
module Compliance.Adapter (toCabal) where

import Control.Monad (foldM)
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
  let retained = Map.insert "name" [A.packageName pkg]
        $ Map.insert "version" [A.renderVersion (A.packageVersion pkg)]
        $ Map.insert "cabal-version" [A.renderVersion (A.cabalVersion pkg)]
        $ Map.adjust (const [A.buildType pkg]) "build-type" (A.packageFields pkg)
  pd <- fields spec C.packageDescriptionFieldGrammar retained
  repositories <- traverse (sourceRepository spec) (A.packageSourceRepositories pkg)
  version <- convertVersion (A.packageVersion pkg)
  buildType <- if Map.member "build-type" (A.packageFields pkg)
    then Just <$> atom (A.buildType pkg) else Right Nothing
  let description = pd
        { C.package = C.PackageIdentifier (C.mkPackageName (T.unpack (A.packageName pkg))) version
        , C.specVersion = spec
        , C.buildTypeRaw = buildType
        , C.sourceRepos = repositories
        }
      flags = [C.MkPackageFlag (C.mkFlagName (T.unpack (A.flagName f))) ""
                (A.flagDefault f) (A.flagManual f) | f <- A.packageFlags pkg]
      initial = C.emptyGenericPackageDescription
        { C.packageDescription = description, C.genPackageFlags = flags }
  foldM (component spec) initial (A.packageComponents pkg)

sourceRepository :: C.CabalSpecVersion -> A.SourceRepository -> Either String C.SourceRepo
sourceRepository spec repository = do
  kind <- atom (A.sourceRepositoryKind repository)
  fields spec (C.sourceRepoFieldGrammar kind) (A.sourceRepositoryFields repository)

fields :: C.CabalSpecVersion -> C.ParsecFieldGrammar s a -> Map.Map Text [Text] -> Either String a
fields spec grammar retained = either (Left . show) Right
  (snd (C.runParseResult (C.parseFieldGrammar spec (fieldMap retained) grammar)))
  where
    -- Keep each occurrence and each retained line separately.
    fieldMap = Map.fromList . map (\(k, vs) -> (TE.encodeUtf8 k, map value vs)) . Map.toList
    value text = C.MkNamelessField C.zeroPos
      [C.FieldLine C.zeroPos (TE.encodeUtf8 line) | line <- if T.null text then [] else T.splitOn "\n" text]

atom :: C.Parsec a => Text -> Either String a
atom text = maybe (Left ("Cannot convert field value: " ++ T.unpack text)) Right (C.simpleParsec (T.unpack text))

convertVersion :: A.Version -> Either String C.Version
convertVersion v = do
  let digits = NE.toList (A.versionNumbers v)
  if any (> toInteger (maxBound :: Int)) digits
    then Left "Version component exceeds the Cabal integer limit"
    else Right (C.mkVersion (map fromInteger digits))

convertRange :: A.VersionRange -> Either String C.VersionRange
convertRange range
  | range == A.anyVersion = Right C.anyVersion
  | otherwise = atom (A.renderVersionRange range)

convertDependency :: A.Dependency -> Either String C.Dependency
convertDependency dep = do
  range <- convertRange (A.dependencyRange dep)
  pure (C.Dependency (C.mkPackageName (T.unpack (A.dependencyPackage dep))) range
    (NES.fromNonEmpty (fmap target (A.dependencyLibraries dep))))
  where
    target A.MainLibrary = C.LMainLibName
    target (A.NamedLibrary name) = C.LSubLibName (C.mkUnqualComponentName (T.unpack name))

convertBuildInfo :: C.BuildInfo -> A.BuildInfo -> Either String C.BuildInfo
convertBuildInfo raw bi = do
  other <- traverse atom (A.otherModules bi)
  autogen <- traverse atom (A.autogenModules bi)
  language <- traverse atom (A.defaultLanguage bi)
  extensions <- traverse atom (A.extensions bi)
  dependencies <- traverse convertDependency (A.dependencies bi)
  modern <- traverse modernTool [t | t <- A.buildTools bi, Just _ <- [A.toolPackage t]]
  legacy <- traverse legacyTool [t | t <- A.buildTools bi, Nothing <- [A.toolPackage t]]
  let C.PerCompilerFlavor _ ghcjs = C.options raw
  pure raw
    { C.buildable = fromMaybe True (A.buildable bi)
    , C.hsSourceDirs = map C.unsafeMakeSymbolicPath (A.sourceDirs bi) ++ C.hsSourceDirs raw
    , C.otherModules = other
    , C.autogenModules = autogen
    , C.defaultLanguage = language
    , C.defaultExtensions = extensions
    , C.targetBuildDepends = dependencies
    , C.buildTools = legacy
    , C.buildToolDepends = modern
    , C.cSources = A.cSources bi
    , C.cxxSources = A.cxxSources bi
    , C.includeDirs = A.includeDirs bi
    , C.installIncludes = A.installIncludes bi
    , C.autogenIncludes = A.autogenIncludes bi
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

convertTree :: (A.BuildInfo -> Either String a) -> (a -> C.BuildInfo)
  -> A.Conditional A.BuildInfo -> Either String (C.CondTree C.ConfVar [C.Dependency] a)
convertTree convert getInfo (A.Conditional bi branches) = do
  value <- convert bi
  children <- traverse branch branches
  pure (C.CondNode value (C.targetBuildDepends (getInfo value)) children)
  where
    branch (A.Branch cond yes no) = C.CondBranch <$> convertCondition cond
      <*> convertTree convert getInfo yes <*> traverse (convertTree convert getInfo) no

component :: C.CabalSpecVersion -> C.GenericPackageDescription
  -> A.Component (A.Conditional A.BuildInfo) -> Either String C.GenericPackageDescription
component spec gpd (A.Component kind tree) = case kind of
  A.Library name -> do
    let libName = maybe C.LMainLibName (C.LSubLibName . componentName) name
    converted <- convertTree (library libName) C.libBuildInfo tree
    pure $ case name of
      Nothing -> gpd { C.condLibrary = Just converted }
      Just n -> gpd { C.condSubLibraries = C.condSubLibraries gpd ++ [(componentName n, converted)] }
  A.Executable name -> do
    converted <- convertTree (executable (componentName name)) C.buildInfo tree
    pure gpd { C.condExecutables = C.condExecutables gpd ++ [(componentName name, converted)] }
  A.ForeignLibrary name -> do
    converted <- convertTree (foreignLibrary (componentName name)) C.foreignLibBuildInfo tree
    pure gpd { C.condForeignLibs = C.condForeignLibs gpd ++ [(componentName name, converted)] }
  A.TestSuite name -> do
    converted <- convertTree testSuite C.testBuildInfo tree
    pure gpd { C.condTestSuites = C.condTestSuites gpd ++ [(componentName name, converted)] }
  A.Benchmark name -> do
    converted <- convertTree benchmark C.benchmarkBuildInfo tree
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
      pure raw { C.buildInfo = info, C.modulePath = fromMaybe "" (A.mainIs bi) }
    foreignLibrary name bi = do
      raw <- fields spec (C.foreignLibFieldGrammar name) (A.extraFields bi)
      info <- convertBuildInfo (C.foreignLibBuildInfo raw) bi
      pure raw { C.foreignLibBuildInfo = info }
    testSuite bi = do
      raw <- fields spec C.testSuiteFieldGrammar (A.extraFields bi)
      info <- convertBuildInfo (C._testStanzaBuildInfo raw) bi
      result (C.validateTestSuite spec C.zeroPos raw
        { C._testStanzaBuildInfo = info, C._testStanzaMainIs = A.mainIs bi })
    benchmark bi = do
      raw <- fields spec C.benchmarkFieldGrammar (A.extraFields bi)
      info <- convertBuildInfo (C._benchmarkStanzaBuildInfo raw) bi
      result (C.validateBenchmark spec C.zeroPos raw
        { C._benchmarkStanzaBuildInfo = info, C._benchmarkStanzaMainIs = A.mainIs bi })
    result = either (Left . show) Right . snd . C.runParseResult
