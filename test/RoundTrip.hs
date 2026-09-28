{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Make random package descriptions with Hedgehog. Write each description
-- with the Cabal-syntax pretty-printer. Then read the text with the parser
-- and with Cabal-syntax. The two results must be equal. The Cabal-syntax
-- result must also be equal to the random description, so that the test
-- finds data that the text does not keep.
module Main (main) where

import Control.Monad (unless)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Config as Config
import qualified Hedgehog.Internal.Property as Property
import qualified Hedgehog.Internal.Report as Report
import qualified Hedgehog.Internal.Runner as Runner
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Range as Range
import System.Exit (exitFailure)
import qualified Aihc.Cabal as A
import Compliance.Adapter (runResult, toCabal)
import qualified Distribution.CabalSpecVersion as C
import qualified Distribution.Compat.NonEmptySet as NES
import qualified Distribution.Compiler as C
import qualified Distribution.License as L
import qualified Language.Haskell.Extension as C
import qualified Distribution.ModuleName as C
import qualified Distribution.PackageDescription as C
import qualified Distribution.PackageDescription.Parsec as C
import qualified Distribution.PackageDescription.PrettyPrint as C
import qualified Distribution.Parsec as C
import qualified Distribution.SPDX as SPDX
import qualified Distribution.System as C
import qualified Distribution.Types.Version as C
import qualified Distribution.Types.VersionRange as C
import qualified Distribution.Utils.Path as C
import qualified Distribution.Utils.ShortText as C

-- | The number of random packages in each test run.
testCount :: TestLimit
testCount = 2000

-- | A fixed seed makes each run test the same packages.
seed :: Seed.Seed
seed = Seed.from 20260928

main :: IO ()
main = do
  report <- Runner.checkReport (Property.propertyConfig prop) 0 seed (Property.propertyTest prop) (const (pure ()))
  output <- Report.renderResult Config.DisableColor (Just "pretty-printer round trip") report
  putStrLn output
  unless (Report.reportStatus report == Report.OK) exitFailure
  where
    prop = withTests testCount prop_roundTrip

prop_roundTrip :: Property
prop_roundTrip = property $ do
  gpd <- forAllWith C.showGenericPackageDescription genPackage
  let pd = C.packageDescription gpd
      spec = C.specVersion pd
  cover 15 "format version before 2.0" (spec < C.CabalSpecV2_0)
  cover 15 "format version 3.0 or later" (spec >= C.CabalSpecV3_0)
  cover 50 "conditional branches" (hasBranches gpd)
  cover 20 "multi-line description" ('\n' `elem` C.fromShortText (C.description pd))
  cover 30 "sub-libraries" (not (null (C.condSubLibraries gpd)))
  let bytes = TE.encodeUtf8 (T.pack (C.showGenericPackageDescription gpd))
  reference <- evalEither (snd (runResult (C.parseGenericPackageDescription bytes)))
  ours <- evalEither (A.parseValue (A.parsePackage bytes))
  converted <- evalEither (toCabal ours)
  converted === reference
  reference === gpd

-- | True if a component has a conditional branch.
hasBranches :: C.GenericPackageDescription -> Bool
hasBranches gpd = or
  [ maybe False branches (C.condLibrary gpd)
  , any (branches . snd) (C.condSubLibraries gpd)
  , any (branches . snd) (C.condExecutables gpd)
  , any (branches . snd) (C.condForeignLibs gpd)
  , any (branches . snd) (C.condTestSuites gpd)
  , any (branches . snd) (C.condBenchmarks gpd)
  ]
  where
    branches :: Tree a -> Bool
    branches = not . null . C.condTreeComponents

-- Names and atoms

-- | Parse a generated atom with Cabal-syntax. The generators make only
-- valid text, so an error is a generator bug.
atom :: C.Parsec a => String -> a
atom input = fromMaybe (error ("Generator made an invalid value: " ++ input)) (C.simpleParsec input)

lowerWord :: Gen String
lowerWord = (:) <$> Gen.lower <*> Gen.string (Range.linear 0 6) (Gen.frequency [(5, Gen.lower), (1, Gen.digit)])

upperWord :: Gen String
upperWord = (:) <$> Gen.upper <*> Gen.string (Range.linear 0 6) Gen.alphaNum

-- | A package or component name: words with hyphens between them.
genName :: Gen String
genName = concatWith '-' <$> Gen.list (Range.linear 1 3) lowerWord

concatWith :: Char -> [String] -> String
concatWith c = foldr1 (\a b -> a ++ c : b)

genModuleName :: Gen C.ModuleName
genModuleName = atom . concatWith '.' <$> Gen.list (Range.linear 1 3) upperWord

genModules :: Gen [C.ModuleName]
genModules = Gen.list (Range.linear 0 3) genModuleName

genVersion :: Gen C.Version
genVersion = C.mkVersion <$> Gen.list (Range.linear 1 4) (Gen.int (Range.linear 0 20))

-- | A relative path. Some paths contain spaces, so the printer must use
-- quotation marks.
genPath :: Gen FilePath
genPath = concatWith '/' <$> Gen.list (Range.linear 1 3) segment
  where
    segment = Gen.frequency
      [ (8, lowerWord)
      , (1, (\a b -> a ++ " " ++ b) <$> lowerWord <*> lowerWord)
      , (1, (++ ".hs") <$> upperWord)
      ]

genPaths :: Gen [C.SymbolicPathX allowAbsolute from to]
genPaths = map C.unsafeMakeSymbolicPath . nub <$> Gen.list (Range.linear 0 3) genPath

-- | A command line option. Some options contain spaces or commas. The
-- printer does not escape quotation marks, so the options do not contain them.
genOption :: Gen String
genOption = Gen.frequency
  [ (6, ('-' :) <$> Gen.string (Range.linear 1 10) (Gen.element ("abcDEFghi0123=-_.:/" :: String)))
  , (1, (\a b -> "-D" ++ a ++ "=" ++ b) <$> upperWord <*> lowerWord)
  , (1, (\a b -> a ++ " " ++ b) <$> lowerWord <*> lowerWord)
  , (1, (\a b -> a ++ "," ++ b) <$> lowerWord <*> lowerWord)
  ]

genOptions :: Gen [String]
genOptions = Gen.list (Range.linear 0 3) genOption

-- | One line of free text without leading or trailing spaces.
genLine :: Gen String
genLine = unwords <$> Gen.list (Range.linear 1 5)
  (Gen.frequency [(6, lowerWord), (1, upperWord), (1, Gen.element ["(a)", "x,y", "a--b", "é", "*"])])

genShortText :: Gen C.ShortText
genShortText = C.toShortText <$> Gen.frequency [(2, pure ""), (3, genLine)]

-- | Free text with one or more lines.
genFreeText :: Gen String
genFreeText = Gen.frequency
  [ (2, pure "")
  , (3, genLine)
  , (2, concatWith '\n' <$> Gen.list (Range.linear 2 4) genLine)
  ]

-- Versions and dependencies

-- | A version range. The printer does not write parentheses around an
-- operand with the same operator. In a dependency, the parser groups such
-- operators to the right. Thus an operand at the left of an operator never
-- uses the same operator. The printer does not write a range that contains
-- all versions, so such a range becomes 'C.anyVersion'.
genRange :: C.CabalSpecVersion -> Gen C.VersionRange
genRange = genRangeWith foldr1

-- | A version range in an impl condition. In a condition, the parser groups
-- operators to the left.
genConditionRange :: C.CabalSpecVersion -> Gen C.VersionRange
genConditionRange = genRangeWith foldl1

genRangeWith :: (forall a. (a -> a -> a) -> [a] -> a) -> C.CabalSpecVersion -> Gen C.VersionRange
genRangeWith fold spec = Gen.frequency [(2, pure C.anyVersion), (5, anyVersion <$> go (2 :: Int))]
  where
    anyVersion range = if C.isAnyVersion range then C.anyVersion else range
    go depth = fold C.unionVersionRanges <$> Gen.list (Range.linear 1 3) (conjunction depth)
    -- A union in parentheses occurs only as an operand of an intersection.
    conjunction depth = Gen.choice
      [ simple
      , fold C.intersectVersionRanges <$> Gen.list (Range.linear 2 3) (term depth)
      ]
    term depth
      | depth == 0 = simple
      | otherwise = Gen.frequency
          [ (4, simple)
          , (1, fold C.unionVersionRanges <$> Gen.list (Range.linear 2 3) (conjunction (depth - 1)))
          ]
    simple = Gen.element constructors <*> genVersion
    constructors = [C.thisVersion, C.laterVersion, C.earlierVersion, C.orLaterVersion, C.orEarlierVersion]
      ++ [C.majorBoundVersion | spec >= C.CabalSpecV2_0]

-- | A package name that no component of the generated package uses.
genPackageName :: Gen C.PackageName
genPackageName = C.mkPackageName <$> Gen.frequency
  [ (3, Gen.element ["base", "containers", "text", "bytestring", "mtl"])
  , (1, ("ext-" ++) <$> genName)
  ]

genDependency :: C.CabalSpecVersion -> Gen C.Dependency
genDependency spec = do
  name <- genPackageName
  range <- genRange spec
  libraries <- if spec >= C.CabalSpecV3_0
    then Gen.frequency
      [ (4, pure (NES.singleton C.LMainLibName))
      , (1, NES.fromNonEmpty <$> Gen.nonEmpty (Range.linear 1 2) genLibraryName)
      ]
    else pure (NES.singleton C.LMainLibName)
  pure (C.Dependency name range libraries)
  where
    genLibraryName = Gen.frequency
      [ (1, pure C.LMainLibName)
      , (2, C.LSubLibName . C.mkUnqualComponentName . ("lib-" ++) <$> genName)
      ]

genMixin :: C.CabalSpecVersion -> Gen C.Mixin
genMixin spec = do
  name <- genPackageName
  library <- if spec >= C.CabalSpecV3_4
    then Gen.frequency [(3, pure C.LMainLibName), (1, C.LSubLibName . C.mkUnqualComponentName . ("lib-" ++) <$> genName)]
    else pure C.LMainLibName
  provides <- genRenaming
  requires <- genRenaming
  pure (C.mkMixin name library (C.IncludeRenaming provides requires))
  where
    genRenaming = Gen.choice
      [ pure C.DefaultRenaming
      , C.ModuleRenaming <$> Gen.list (Range.linear 0 2) ((,) <$> genModuleName <*> genModuleName)
      , C.HidingRenaming <$> Gen.list (Range.linear 0 2) genModuleName
      ]

genToolDependency :: C.CabalSpecVersion -> Gen C.ExeDependency
genToolDependency spec = C.ExeDependency <$> genPackageName
  <*> (C.mkUnqualComponentName <$> genName) <*> genRange spec

genLegacyTool :: C.CabalSpecVersion -> Gen C.LegacyExeDependency
genLegacyTool spec = C.LegacyExeDependency <$> Gen.element ["happy", "alex", "c2hs", "ext-tool"] <*> genRange spec

-- Build information

-- | Build information for the given Cabal format version. A field that the
-- format version does not support stays empty.
genBuildInfo :: C.CabalSpecVersion -> Gen C.BuildInfo
genBuildInfo spec = do
  buildable <- Gen.frequency [(4, pure True), (1, pure False)]
  sourceDirs <- genPaths
  other <- genModules
  autogen <- since C.CabalSpecV2_0 genModules
  virtual <- since C.CabalSpecV2_2 genModules
  language <- if spec >= C.CabalSpecV1_10
    then Gen.maybe (Gen.element [C.Haskell98, C.Haskell2010, C.GHC2021])
    else pure Nothing
  otherLanguages <- since C.CabalSpecV1_10 (Gen.list (Range.linear 0 2) (Gen.element [C.Haskell98, C.Haskell2010]))
  extensions <- since C.CabalSpecV1_10 genExtensions
  otherExtensions <- since C.CabalSpecV1_10 genExtensions
  oldExtensions <- if spec < C.CabalSpecV3_0 then genExtensions else pure []
  dependencies <- Gen.list (Range.linear 0 4) (genDependency spec)
  mixins <- since C.CabalSpecV2_0 (Gen.list (Range.linear 0 2) (genMixin spec))
  tools <- since C.CabalSpecV2_0 (Gen.list (Range.linear 0 2) (genToolDependency spec))
  legacyTools <- if spec < C.CabalSpecV3_0 then Gen.list (Range.linear 0 2) (genLegacyTool spec) else pure []
  cSources <- genPaths
  cxxSources <- since C.CabalSpecV2_2 genPaths
  asmSources <- since C.CabalSpecV3_0 genPaths
  cmmSources <- since C.CabalSpecV3_0 genPaths
  jsSources <- genPaths
  includeDirs <- genPaths
  includes <- genPaths
  installIncludes <- genPaths
  autogenIncludes <- since C.CabalSpecV3_0 genPaths
  extraLibDirs <- genPaths
  extraLibDirsStatic <- since C.CabalSpecV3_8 genPaths
  frameworks <- genPaths
  frameworkDirs <- genPaths
  cppOptions <- genOptions
  ccOptions <- genOptions
  cxxOptions <- since C.CabalSpecV2_2 genOptions
  ldOptions <- genOptions
  asmOptions <- since C.CabalSpecV3_0 genOptions
  hsc2hsOptions <- since C.CabalSpecV3_6 genOptions
  ghcOptions <- genOptions
  ghcjsOptions <- genOptions
  profOptions <- genOptions
  sharedOptions <- genOptions
  extraLibraries <- Gen.list (Range.linear 0 2) lowerWord
  pkgconfig <- Gen.list (Range.linear 0 2)
    (C.PkgconfigDependency <$> (atom <$> genName) <*> pure C.anyPkgconfigVersion)
  custom <- genCustomFields
  pure C.emptyBuildInfo
    { C.buildable = buildable
    , C.hsSourceDirs = sourceDirs
    , C.otherModules = other
    , C.autogenModules = autogen
    , C.virtualModules = virtual
    , C.defaultLanguage = language
    , C.otherLanguages = otherLanguages
    , C.defaultExtensions = extensions
    , C.otherExtensions = otherExtensions
    , C.oldExtensions = oldExtensions
    , C.targetBuildDepends = dependencies
    , C.mixins = mixins
    , C.buildToolDepends = tools
    , C.buildTools = legacyTools
    , C.cSources = cSources
    , C.cxxSources = cxxSources
    , C.asmSources = asmSources
    , C.cmmSources = cmmSources
    , C.jsSources = jsSources
    , C.includeDirs = includeDirs
    , C.includes = includes
    , C.installIncludes = installIncludes
    , C.autogenIncludes = autogenIncludes
    , C.extraLibDirs = extraLibDirs
    , C.extraLibDirsStatic = extraLibDirsStatic
    , C.frameworks = frameworks
    , C.extraFrameworkDirs = frameworkDirs
    , C.cppOptions = cppOptions
    , C.ccOptions = ccOptions
    , C.cxxOptions = cxxOptions
    , C.ldOptions = ldOptions
    , C.asmOptions = asmOptions
    , C.hsc2hsOptions = hsc2hsOptions
    , C.options = C.PerCompilerFlavor ghcOptions ghcjsOptions
    , C.profOptions = C.PerCompilerFlavor profOptions []
    , C.sharedOptions = C.PerCompilerFlavor sharedOptions []
    , C.extraLibs = extraLibraries
    , C.pkgconfigDepends = pkgconfig
    , C.customFieldsBI = custom
    }
  where
    since version gen = if spec >= version then gen else pure []
    genExtensions = Gen.list (Range.linear 0 3) (atom <$> Gen.element
      ["CPP", "OverloadedStrings", "NoImplicitPrelude", "GADTs", "RankNTypes", "Unknown"])

-- | Fields with an @x-@ prefix. The names are unique.
genCustomFields :: Gen [(String, String)]
genCustomFields = do
  names <- nub <$> Gen.list (Range.linear 0 2) (("x-" ++) <$> genName)
  traverse (\name -> (,) name <$> genLine) names

-- Components

type Tree a = C.CondTree C.ConfVar a

-- | A condition tree. The generator makes the data of each node from new
-- build information.
genTree :: C.CabalSpecVersion -> [C.FlagName] -> (C.BuildInfo -> Gen a) -> Gen (Tree a)
genTree spec flags make = go (2 :: Int)
  where
    go depth = do
      value <- genBuildInfo spec >>= make
      children <- if depth == 0 then pure [] else Gen.list (Range.linear 0 2) (branch (depth - 1))
      pure (C.CondNode value children)
    branch depth = C.CondBranch <$> genCondition spec flags <*> go depth <*> Gen.maybe (go depth)

genCondition :: C.CabalSpecVersion -> [C.FlagName] -> Gen (C.Condition C.ConfVar)
genCondition spec flags = Gen.recursive Gen.choice leaves
  [ Gen.subterm (genCondition spec flags) negation
  , Gen.subterm2 (genCondition spec flags) (genCondition spec flags) C.CAnd
  , Gen.subterm2 (genCondition spec flags) (genCondition spec flags) C.COr
  ]
  where
    -- The printer writes two negations as one "!!" token, which Cabal-syntax
    -- rejects.
    negation c@(C.CNot _) = c
    negation c = C.CNot c
    leaves =
      [ C.Lit <$> Gen.bool
      , C.Var . C.OS <$> Gen.element C.knownOSs
      , C.Var . C.Arch <$> Gen.element C.knownArches
      , C.Var <$> (C.Impl <$> Gen.element [C.GHC, C.GHCJS] <*> genConditionRange spec)
      ] ++ [C.Var . C.PackageFlag <$> Gen.element flags | not (null flags)]

genLibrary :: C.CabalSpecVersion -> C.LibraryName -> C.BuildInfo -> Gen C.Library
genLibrary spec name bi = do
  exposed <- genModules
  signatures <- if spec >= C.CabalSpecV2_0 then genModules else pure []
  visibility <- case name of
    C.LSubLibName _ | spec >= C.CabalSpecV3_0 -> Gen.element [C.LibraryVisibilityPrivate, C.LibraryVisibilityPublic]
    C.LSubLibName _ -> pure C.LibraryVisibilityPrivate
    C.LMainLibName -> pure C.LibraryVisibilityPublic
  isExposed <- Gen.bool
  pure C.emptyLibrary
    { C.libName = name, C.exposedModules = exposed, C.signatures = signatures
    , C.libExposed = isExposed, C.libVisibility = visibility, C.libBuildInfo = bi }

genExecutable :: C.CabalSpecVersion -> C.UnqualComponentName -> C.BuildInfo -> Gen C.Executable
genExecutable spec name bi = do
  mainIs <- genPath
  scope <- if spec >= C.CabalSpecV2_0
    then Gen.element [C.ExecutablePublic, C.ExecutablePrivate]
    else pure C.ExecutablePublic
  pure C.emptyExecutable
    { C.exeName = name, C.modulePath = C.unsafeMakeSymbolicPath mainIs
    , C.exeScope = scope, C.buildInfo = bi }

genForeignLibrary :: C.UnqualComponentName -> C.BuildInfo -> Gen C.ForeignLib
genForeignLibrary name bi = do
  kind <- Gen.element [C.ForeignLibNativeShared, C.ForeignLibNativeStatic]
  options <- Gen.element [[], [C.ForeignLibStandalone]]
  versionInfo <- Gen.maybe (C.mkLibVersionInfo <$> ((,,) <$> small <*> small <*> small))
  versionLinux <- Gen.maybe genVersion
  pure C.emptyForeignLib
    { C.foreignLibName = name, C.foreignLibType = kind, C.foreignLibOptions = options
    , C.foreignLibVersionInfo = versionInfo, C.foreignLibVersionLinux = versionLinux
    , C.foreignLibBuildInfo = bi }
  where
    small = Gen.int (Range.linear 0 5)

-- | A test suite. Only the top node has an interface. The branch nodes
-- have the value that Cabal-syntax gives to a branch without a type field.
-- Cabal-syntax does not keep the name in the test suite value.
genTestSuite :: Bool -> C.BuildInfo -> Gen C.TestSuite
genTestSuite top bi = do
  interface <- if top
    then Gen.choice
      [ C.TestSuiteExeV10 (C.mkVersion [1, 0]) . C.unsafeMakeSymbolicPath <$> genPath
      , C.TestSuiteLibV09 (C.mkVersion [0, 9]) <$> genModuleName
      ]
    else pure (C.TestSuiteUnsupported (C.TestTypeUnknown "" C.nullVersion))
  pure C.emptyTestSuite { C.testInterface = interface, C.testBuildInfo = bi }

genBenchmark :: Bool -> C.BuildInfo -> Gen C.Benchmark
genBenchmark top bi = do
  interface <- if top
    then C.BenchmarkExeV10 (C.mkVersion [1, 0]) . C.unsafeMakeSymbolicPath <$> genPath
    else pure (C.BenchmarkUnsupported (C.BenchmarkTypeUnknown "" C.nullVersion))
  pure C.emptyBenchmark { C.benchmarkInterface = interface, C.benchmarkBuildInfo = bi }

-- | A tree whose top node differs from the branch nodes.
genTopTree :: C.CabalSpecVersion -> [C.FlagName] -> (Bool -> C.BuildInfo -> Gen a) -> Gen (Tree a)
genTopTree spec flags make = do
  C.CondNode _ children <- genTree spec flags (make False)
  value <- genBuildInfo spec >>= make True
  pure (C.CondNode value children)

-- | Unique component names for one kind of component.
genComponentNames :: String -> Int -> Gen [C.UnqualComponentName]
genComponentNames prefix count =
  map (C.mkUnqualComponentName . (prefix ++)) . nub <$> Gen.list (Range.linear 0 count) genName

-- Package

genSpec :: Gen C.CabalSpecVersion
genSpec = Gen.element
  [ C.CabalSpecV1_10, C.CabalSpecV1_12, C.CabalSpecV1_18, C.CabalSpecV1_20
  , C.CabalSpecV1_22, C.CabalSpecV1_24, C.CabalSpecV2_0, C.CabalSpecV2_2
  , C.CabalSpecV2_4, C.CabalSpecV3_0, C.CabalSpecV3_4, C.CabalSpecV3_6
  , C.CabalSpecV3_8, C.CabalSpecV3_12, C.CabalSpecV3_14
  ]

genLicense :: C.CabalSpecVersion -> Gen (Either SPDX.License L.License)
genLicense spec
  | spec >= C.CabalSpecV2_2 = Left <$> Gen.element
      [ SPDX.NONE
      , simple SPDX.MIT
      , simple SPDX.BSD_3_Clause
      , SPDX.License (SPDX.EOr (SPDX.ELicense (SPDX.ELicenseId SPDX.MIT) Nothing)
          (SPDX.ELicense (SPDX.ELicenseId SPDX.Apache_2_0) Nothing))
      ]
  | otherwise = Right <$> Gen.element
      [L.BSD3, L.MIT, L.GPL (Just (C.mkVersion [3])), L.PublicDomain, L.AllRightsReserved]
  where
    simple license = SPDX.License (SPDX.ELicense (SPDX.ELicenseId license) Nothing)

genSourceRepo :: Gen C.SourceRepo
genSourceRepo = do
  kind <- Gen.element [C.RepoHead, C.RepoThis]
  kindOfRepo <- Gen.maybe (Gen.element [C.KnownRepoType C.Git, C.KnownRepoType C.Darcs, C.KnownRepoType C.Mercurial])
  location <- Gen.maybe (("https://example.com/" ++) <$> genName)
  branch <- Gen.maybe lowerWord
  tag <- Gen.maybe lowerWord
  subdir <- Gen.maybe genPath
  pure (C.emptySourceRepo kind)
    { C.repoType = kindOfRepo, C.repoLocation = location, C.repoBranch = branch
    , C.repoTag = tag, C.repoSubdir = subdir }

-- | Without a build-type field, the build type is Custom before format
-- version 2.2. From format version 1.24, a Custom build type needs a
-- custom-setup section.
genBuildType :: C.CabalSpecVersion -> Gen (Maybe C.BuildType)
genBuildType spec
  | spec >= C.CabalSpecV1_24 && spec < C.CabalSpecV2_2 = Just <$> types
  | otherwise = Gen.maybe types
  where
    types = Gen.element [C.Simple, C.Configure, C.Make, C.Custom]

genFlag :: C.FlagName -> Gen C.PackageFlag
genFlag name = C.MkPackageFlag name <$> genFreeText <*> Gen.bool <*> Gen.bool

genPackage :: Gen C.GenericPackageDescription
genPackage = do
  spec <- genSpec
  name <- C.mkPackageName <$> genName
  version <- genVersion
  flagNames <- map C.mkFlagName . nub <$> Gen.list (Range.linear 0 3) lowerWord
  flags <- traverse genFlag flagNames
  license <- genLicense spec
  copyright <- genShortText
  maintainer <- genShortText
  author <- genShortText
  stability <- genShortText
  homepage <- genShortText
  bugReports <- genShortText
  synopsis <- genShortText
  category <- genShortText
  description <- genFreeText
  testedWith <- Gen.list (Range.linear 0 2) ((,) C.GHC <$> genRange spec)
  buildType <- genBuildType spec
  setup <- if buildType == Just C.Custom && spec >= C.CabalSpecV1_24
    then Just . (`C.SetupBuildInfo` False) <$> Gen.list (Range.linear 0 3) (genDependency spec)
    else pure Nothing
  repositories <- Gen.list (Range.linear 0 2) genSourceRepo
  custom <- genCustomFields
  extraSources <- genPaths
  extraDocs <- genPaths
  dataFiles <- genPaths
  library <- Gen.maybe (genTree spec flagNames (genLibrary spec C.LMainLibName))
  libraryNames <- genComponentNames "lib-" 2
  libraries <- traverse (\n -> (,) n <$> genTree spec flagNames (genLibrary spec (C.LSubLibName n))) libraryNames
  exeNames <- genComponentNames "exe-" 2
  executables <- traverse (\n -> (,) n <$> genTree spec flagNames (genExecutable spec n)) exeNames
  foreignNames <- genComponentNames "foreign-" 1
  foreignLibraries <- traverse (\n -> (,) n <$> genTree spec flagNames (genForeignLibrary n)) foreignNames
  testNames <- genComponentNames "test-" 2
  tests <- traverse (\n -> (,) n <$> genTopTree spec flagNames genTestSuite) testNames
  benchNames <- genComponentNames "bench-" 1
  benchmarks <- traverse (\n -> (,) n <$> genTopTree spec flagNames genBenchmark) benchNames
  let pd = C.emptyPackageDescription
        { C.specVersion = spec
        , C.package = C.PackageIdentifier name version
        , C.licenseRaw = license
        , C.copyright = copyright
        , C.maintainer = maintainer
        , C.author = author
        , C.stability = stability
        , C.homepage = homepage
        , C.bugReports = bugReports
        , C.synopsis = synopsis
        , C.category = category
        , C.description = C.toShortText description
        , C.testedWith = testedWith
        , C.buildTypeRaw = buildType
        , C.setupBuildInfo = setup
        , C.sourceRepos = repositories
        , C.customFieldsPD = custom
        , C.extraSrcFiles = extraSources
        , C.extraDocFiles = extraDocs
        , C.dataFiles = dataFiles
        }
  pure C.emptyGenericPackageDescription
    { C.packageDescription = pd
    , C.genPackageFlags = flags
    , C.condLibrary = library
    , C.condSubLibraries = libraries
    , C.condExecutables = executables
    , C.condForeignLibs = foreignLibraries
    , C.condTestSuites = tests
    , C.condBenchmarks = benchmarks
    }
