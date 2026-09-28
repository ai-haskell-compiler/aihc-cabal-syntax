{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Make random package descriptions with Hedgehog. Write each description
-- with the Cabal-syntax pretty-printer. Then read the text with the parser
-- and with Cabal-syntax. The two results must be equal. The Cabal-syntax
-- result must also be equal to the random description, so that the test
-- finds data that the text does not keep.
--
-- The generator makes all values that the printer can write correctly.
-- A comment identifies each value that the generator does not make, and
-- gives the reason.
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
import qualified Language.Haskell.Extension as C

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
      version = C.specVersion pd
  cover 10 "format version before 1.10" (version < C.CabalSpecV1_10)
  cover 5 "format version 3.16 or later" (version >= C.CabalSpecV3_16)
  cover 50 "conditional branches" (hasBranches gpd)
  cover 20 "multi-line description" ('\n' `elem` C.fromShortText (C.description pd))
  cover 30 "sub-libraries" (not (null (C.condSubLibraries gpd)))
  cover 10 "custom-setup section" (C.setupBuildInfo pd /= Nothing)
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

-- | The data that the generators of one package share.
data Context = Context
  { contextSpec :: C.CabalSpecVersion
  , packageName :: C.PackageName
  , subLibraries :: [C.UnqualComponentName]
  , flagNames :: [C.FlagName]
  }

since :: Context -> C.CabalSpecVersion -> Gen [a] -> Gen [a]
since ctx version gen = if contextSpec ctx >= version then gen else pure []

-- Names and atoms

-- | Parse a generated atom with Cabal-syntax. The generators make only
-- valid text, so an error is a generator bug.
atom :: C.Parsec a => String -> a
atom input = fromMaybe (error ("Generator made an invalid value: " ++ input)) (C.simpleParsec input)

-- | A letter. Some letters are not ASCII.
letter :: Gen Char
letter = Gen.frequency [(20, Gen.lower), (3, Gen.upper), (1, Gen.element ("éüßøλжé" :: String))]

lowerWord :: Gen String
lowerWord = (:) <$> Gen.lower <*> Gen.string (Range.linear 0 6) (Gen.frequency [(5, Gen.lower), (1, Gen.digit)])

upperWord :: Gen String
upperWord = (:) <$> Gen.upper <*> Gen.string (Range.linear 0 6)
  (Gen.frequency [(10, Gen.alphaNum), (1, Gen.element ("_'éλ" :: String))])

-- | A package or component name: parts with hyphens between them. Each
-- part contains a letter. Letters can be upper case or not ASCII.
genName :: Gen String
genName = concatWith '-' <$> Gen.list (Range.linear 1 3) part
  where
    part = do
      before <- Gen.string (Range.linear 0 2) Gen.digit
      first <- letter
      after <- Gen.string (Range.linear 0 5) (Gen.frequency [(4, letter), (1, Gen.digit)])
      pure (before ++ first : after)

concatWith :: Char -> [String] -> String
concatWith c = foldr1 (\a b -> a ++ c : b)

genModuleName :: Gen C.ModuleName
genModuleName = atom . concatWith '.' <$> Gen.list (Range.linear 1 3) upperWord

genModules :: Gen [C.ModuleName]
genModules = Gen.list (Range.linear 0 3) genModuleName

genVersion :: Gen C.Version
genVersion = C.mkVersion <$> Gen.list (Range.linear 1 4)
  (Gen.frequency [(10, Gen.int (Range.linear 0 20)), (1, Gen.int (Range.linear 0 999999999))])

-- | A path. Some paths contain spaces, characters that are not ASCII, or
-- glob characters. Some paths are absolute or start with "..".
genPath :: Gen FilePath
genPath = do
  start <- Gen.frequency [(10, pure ""), (1, pure "/"), (1, pure "../"), (1, pure "./")]
  parts <- Gen.list (Range.linear 1 3) segment
  pure (start ++ concatWith '/' parts)
  where
    segment = Gen.frequency
      [ (8, Gen.string (Range.linear 1 8) (Gen.frequency [(10, letter), (2, Gen.digit), (2, Gen.element ("-_." :: String))]))
      , (1, (\a b -> a ++ " " ++ b) <$> lowerWord <*> lowerWord)
      , (1, (++ ".hs") <$> upperWord)
      , (1, ("*." ++) <$> lowerWord)
      ]

-- | A relative path. Paths with a leading slash are absolute.
genRelativePath :: Gen FilePath
genRelativePath = Gen.filter (\p -> take 1 p /= "/") genPath

genPaths :: Gen [C.SymbolicPathX allowAbsolute from to]
genPaths = map C.unsafeMakeSymbolicPath . nub <$> Gen.list (Range.linear 0 3) genPath

genRelativePaths :: Gen [C.SymbolicPathX allowAbsolute from to]
genRelativePaths = map C.unsafeMakeSymbolicPath . nub <$> Gen.list (Range.linear 0 3) genRelativePath

-- | A command line option. Some options contain spaces, commas, or
-- characters that are not ASCII. The printer does not escape quotation
-- marks, so the options do not contain them.
genOption :: Gen String
genOption = Gen.frequency
  [ (6, ('-' :) <$> Gen.string (Range.linear 1 10) (Gen.frequency [(5, letter), (3, Gen.element ("0123=-_.:/+@#$%^&*()[]{}<>;!?~|\\" :: String))]))
  , (1, (\a b -> "-D" ++ a ++ "=" ++ b) <$> upperWord <*> lowerWord)
  , (1, (\a b -> a ++ " " ++ b) <$> lowerWord <*> lowerWord)
  , (1, (\a b -> a ++ "," ++ b) <$> lowerWord <*> lowerWord)
  ]

genOptions :: Gen [String]
genOptions = Gen.list (Range.linear 0 3) genOption

-- | A token without spaces, for example a library name.
genToken :: Gen String
genToken = Gen.string (Range.linear 1 8) (Gen.frequency [(10, letter), (2, Gen.digit), (1, Gen.element ("-_.+" :: String))])

-- | One line of free text without leading or trailing spaces. A line does
-- not start with "--", because Cabal reads such a line as a comment. The
-- text does not contain braces, because Cabal reads them as layout.
genLine :: Gen String
genLine = unwords <$> Gen.list (Range.linear 1 5) word
  where
    word = Gen.frequency
      [ (6, lowerWord)
      , (1, upperWord)
      , (1, Gen.element ["(a)", "x,y", "a--b", "é", "*", "λ", "a:b", "\"q\"", "<p>", "100%", "a;b", "#", "!"])
      ]

genShortText :: Gen C.ShortText
genShortText = C.toShortText <$> Gen.frequency [(2, pure ""), (3, genLine)]

-- | Free text with one or more lines. From format version 3.0, some lines
-- are empty or indented. The first line and the last line are not empty or
-- indented, because Cabal-syntax removes them. Before format version 3.0, Cabal-syntax removes
-- the indentation and ignores empty lines.
genFreeText :: C.CabalSpecVersion -> Gen String
genFreeText version = Gen.frequency
  [ (2, pure "")
  , (3, genLine)
  , (2, concatWith '\n' <$> Gen.list (Range.linear 2 4) genLine)
  , (if version >= C.CabalSpecV3_0 then 2 else 0, do
      first <- genLine
      middle <- Gen.list (Range.linear 1 4) (Gen.frequency
        [ (3, genLine)
        , (1, pure "")
        , (1, (++) <$> Gen.string (Range.linear 1 4) (pure ' ') <*> genLine)
        ])
      final <- genLine
      pure (concatWith '\n' (first : middle ++ [final])))
  ]

-- Versions and dependencies

-- | A version range. The printer does not write parentheses around an
-- operand with the same operator. In a dependency, the parser groups such
-- operators to the right. Thus an operand at the left of an operator never
-- uses the same operator. The printer does not write a range that contains
-- all versions, so such a range becomes 'C.anyVersion'.
genRange :: Context -> Gen C.VersionRange
genRange = genRangeWith foldr1

-- | A version range in an impl condition. In a condition, the parser groups
-- operators to the left.
genConditionRange :: Context -> Gen C.VersionRange
genConditionRange = genRangeWith foldl1

genRangeWith :: (forall a. (a -> a -> a) -> [a] -> a) -> Context -> Gen C.VersionRange
genRangeWith fold ctx = Gen.frequency [(2, pure C.anyVersion), (5, anyVersion <$> go (2 :: Int))]
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
      ++ [C.majorBoundVersion | contextSpec ctx >= C.CabalSpecV2_0]

-- | The name of a package that is not this package. Before format version
-- 3.4, the name of a sub-library refers to the sub-library, so the name
-- is not the name of a sub-library.
genOtherPackage :: Context -> Gen C.PackageName
genOtherPackage ctx = Gen.filter allowed (Gen.frequency
  [ (3, C.mkPackageName <$> Gen.element ["base", "containers", "text", "bytestring", "mtl"])
  , (2, C.mkPackageName <$> genName)
  , (if null (subLibraries ctx) then 0 else 1, C.unqualComponentNameToPackageName <$> Gen.element (subLibraries ctx))
  ])
  where
    allowed name = name /= packageName ctx && (contextSpec ctx >= C.CabalSpecV3_4
      || C.packageNameToUnqualComponentName name `notElem` subLibraries ctx)

genLibraryName :: Gen C.LibraryName
genLibraryName = Gen.frequency
  [ (1, pure C.LMainLibName)
  , (2, C.LSubLibName . C.mkUnqualComponentName <$> genName)
  ]

genDependency :: Context -> Gen C.Dependency
genDependency ctx = Gen.frequency
  [ (4, C.Dependency <$> genOtherPackage ctx <*> genRange ctx <*> otherLibraries)
  , (1, C.Dependency (packageName ctx) <$> genRange ctx <*> ownLibraries)
  ]
  where
    -- The syntax for library sets starts in format version 3.0.
    otherLibraries
      | contextSpec ctx >= C.CabalSpecV3_0 = Gen.frequency
          [ (4, pure (NES.singleton C.LMainLibName))
          , (1, NES.fromNonEmpty <$> Gen.nonEmpty (Range.linear 1 3) genLibraryName)
          ]
      | otherwise = pure (NES.singleton C.LMainLibName)
    -- Before format version 3.0, the printer writes a dependency on a
    -- sub-library of this package with the name of the sub-library.
    ownLibraries
      | contextSpec ctx >= C.CabalSpecV3_0 = otherLibraries
      | otherwise = NES.singleton <$> Gen.element
          (C.LMainLibName : map C.LSubLibName (subLibraries ctx))

genMixin :: Context -> Gen C.Mixin
genMixin ctx = do
  (name, library) <- Gen.frequency
    [ (4, (,) <$> genOtherPackage ctx <*> otherLibrary)
    , (if null (subLibraries ctx) && contextSpec ctx < C.CabalSpecV3_4 then 0 else 1, (,) (packageName ctx) <$> ownLibrary)
    ]
  provides <- genRenaming
  requires <- genRenaming
  pure (C.mkMixin name library (C.IncludeRenaming provides requires))
  where
    -- The syntax for a sub-library in a mixin starts in format version 3.4.
    otherLibrary = if contextSpec ctx >= C.CabalSpecV3_4 then genLibraryName else pure C.LMainLibName
    -- Before format version 3.4, the printer writes a mixin of a
    -- sub-library of this package with the name of the sub-library.
    ownLibrary = if contextSpec ctx >= C.CabalSpecV3_4
      then genLibraryName
      else C.LSubLibName <$> Gen.element (subLibraries ctx)
    genRenaming = Gen.choice
      [ pure C.DefaultRenaming
      , C.ModuleRenaming <$> Gen.list (Range.linear 0 3) ((,) <$> genModuleName <*> genModuleName)
      , C.HidingRenaming <$> Gen.list (Range.linear 0 3) genModuleName
      ]

genToolDependency :: Context -> Gen C.ExeDependency
genToolDependency ctx = C.ExeDependency
  <$> Gen.choice [genOtherPackage ctx, pure (packageName ctx)]
  <*> (C.mkUnqualComponentName <$> genName) <*> genRange ctx

genLegacyTool :: Context -> Gen C.LegacyExeDependency
genLegacyTool ctx = C.LegacyExeDependency
  <$> Gen.frequency [(1, Gen.element ["happy", "alex", "c2hs", "hsc2hs", "cpphs"]), (2, genName)]
  <*> genRange ctx

-- | A pkg-config dependency. Versions with letters start in format version 3.0.
genPkgconfigDependency :: Context -> Gen C.PkgconfigDependency
genPkgconfigDependency ctx = do
  name <- genToken
  range <- Gen.element (["", " >= 1.2", " < 3 || > 4.1", " >= 1 && < 2"]
    ++ [" == 2.0.1a" | contextSpec ctx >= C.CabalSpecV3_0])
  let input = name ++ range
  pure (fromMaybe (error ("Generator made an invalid value: " ++ input)) (C.simpleParsec' (contextSpec ctx) input))

-- | An extension. The name of an unknown extension contains only letters
-- and digits.
genExtension :: Gen C.Extension
genExtension = Gen.frequency
  [ (10, C.EnableExtension <$> Gen.enumBounded)
  , (3, C.DisableExtension <$> Gen.enumBounded)
  , (1, C.UnknownExtension . ("Unknown" ++) <$> Gen.string (Range.linear 1 6) Gen.alphaNum)
  ]

genLanguage :: Gen C.Language
genLanguage = Gen.frequency
  [ (4, Gen.element C.knownLanguages)
  , (1, C.UnknownLanguage . ("Unknown" ++) <$> Gen.string (Range.linear 1 6) Gen.alphaNum)
  ]

-- Build information

-- | Build information for the format version of the context. A field that
-- the format version does not support stays empty. The grammar has no field
-- for 'C.staticOptions', so that value stays empty.
genBuildInfo :: Context -> Gen C.BuildInfo
genBuildInfo ctx = do
  buildable <- Gen.frequency [(4, pure True), (1, pure False)]
  sourceDirs <- genPaths
  other <- genModules
  autogen <- since' C.CabalSpecV2_0 genModules
  virtual <- since' C.CabalSpecV2_2 genModules
  language <- if contextSpec ctx >= C.CabalSpecV1_10 then Gen.maybe genLanguage else pure Nothing
  otherLanguages <- since' C.CabalSpecV1_10 (Gen.list (Range.linear 0 2) genLanguage)
  extensions <- since' C.CabalSpecV1_10 genExtensions
  otherExtensions <- since' C.CabalSpecV1_10 genExtensions
  -- The extensions field is not available from format version 3.0.
  oldExtensions <- if contextSpec ctx < C.CabalSpecV3_0 then genExtensions else pure []
  dependencies <- Gen.list (Range.linear 0 4) (genDependency ctx)
  mixins <- since' C.CabalSpecV2_0 (Gen.list (Range.linear 0 2) (genMixin ctx))
  tools <- Gen.list (Range.linear 0 2) (genToolDependency ctx)
  -- The build-tools field is not available from format version 3.0.
  legacyTools <- if contextSpec ctx < C.CabalSpecV3_0 then Gen.list (Range.linear 0 2) (genLegacyTool ctx) else pure []
  cSources <- genPaths
  cxxSources <- since' C.CabalSpecV2_2 genPaths
  asmSources <- since' C.CabalSpecV3_0 genPaths
  cmmSources <- since' C.CabalSpecV3_0 genPaths
  jsSources <- genPaths
  includeDirs <- genPaths
  includes <- genPaths
  installIncludes <- genRelativePaths
  autogenIncludes <- since' C.CabalSpecV3_0 genRelativePaths
  extraLibDirs <- genPaths
  extraLibDirsStatic <- since' C.CabalSpecV3_8 genPaths
  frameworks <- genRelativePaths
  frameworkDirs <- genPaths
  cppOptions <- genOptions
  ccOptions <- genOptions
  cxxOptions <- since' C.CabalSpecV2_2 genOptions
  jsppOptions <- since' C.CabalSpecV3_16 genOptions
  ldOptions <- genOptions
  asmOptions <- since' C.CabalSpecV3_0 genOptions
  cmmOptions <- since' C.CabalSpecV3_0 genOptions
  hsc2hsOptions <- since' C.CabalSpecV3_6 genOptions
  ghcOptions <- genOptions
  ghcjsOptions <- genOptions
  profOptions <- genOptions
  profjsOptions <- genOptions
  sharedOptions <- genOptions
  sharedjsOptions <- genOptions
  profSharedOptions <- since' C.CabalSpecV3_14 genOptions
  profSharedjsOptions <- since' C.CabalSpecV3_14 genOptions
  extraLibraries <- tokens
  extraLibrariesStatic <- since' C.CabalSpecV3_8 tokens
  extraGHCiLibraries <- tokens
  extraBundledLibraries <- tokens
  extraLibraryFlavours <- tokens
  extraDynamicFlavours <- since' C.CabalSpecV3_0 tokens
  pkgconfig <- Gen.list (Range.linear 0 2) (genPkgconfigDependency ctx)
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
    , C.jsppOptions = jsppOptions
    , C.ldOptions = ldOptions
    , C.asmOptions = asmOptions
    , C.cmmOptions = cmmOptions
    , C.hsc2hsOptions = hsc2hsOptions
    , C.options = C.PerCompilerFlavor ghcOptions ghcjsOptions
    , C.profOptions = C.PerCompilerFlavor profOptions profjsOptions
    , C.sharedOptions = C.PerCompilerFlavor sharedOptions sharedjsOptions
    , C.profSharedOptions = C.PerCompilerFlavor profSharedOptions profSharedjsOptions
    , C.extraLibs = extraLibraries
    , C.extraLibsStatic = extraLibrariesStatic
    , C.extraGHCiLibs = extraGHCiLibraries
    , C.extraBundledLibs = extraBundledLibraries
    , C.extraLibFlavours = extraLibraryFlavours
    , C.extraDynLibFlavours = extraDynamicFlavours
    , C.pkgconfigDepends = pkgconfig
    , C.customFieldsBI = custom
    }
  where
    since' = since ctx
    genExtensions = Gen.list (Range.linear 0 3) genExtension
    tokens = Gen.list (Range.linear 0 2) genToken

-- | Fields with an @x-@ prefix. The names are unique. A value can have
-- more than one line. A field name contains only ASCII letters, digits,
-- hyphens, and underscores. Cabal-syntax changes field names to lower case.
genCustomFields :: Gen [(String, String)]
genCustomFields = do
  names <- nub <$> Gen.list (Range.linear 0 2) (("x-" ++) <$> Gen.string (Range.linear 1 10)
    (Gen.frequency [(10, Gen.lower), (2, Gen.digit), (1, Gen.element ("-_" :: String))]))
  traverse (\name -> (,) name <$> value) names
  where
    -- Cabal-syntax ignores empty lines and indentation in these values.
    value = concatWith '\n' <$> Gen.list (Range.linear 1 4) genLine

-- Components

type Tree a = C.CondTree C.ConfVar a

-- | A condition tree. Each node gets its data from the given generator.
-- The flag tells if the node is the top node.
genTree :: Context -> (Bool -> C.BuildInfo -> Gen a) -> Gen (Tree a)
genTree ctx make = go True (2 :: Int)
  where
    go top depth = do
      value <- genBuildInfo ctx >>= make top
      children <- if depth == 0 then pure [] else Gen.list (Range.linear 0 2) (branch (depth - 1))
      pure (C.CondNode value children)
    branch depth = C.CondBranch <$> genCondition ctx <*> go False depth <*> Gen.maybe (go False depth)

genCondition :: Context -> Gen (C.Condition C.ConfVar)
genCondition ctx = Gen.recursive Gen.choice leaves
  [ Gen.subterm (genCondition ctx) negation
  , Gen.subterm2 (genCondition ctx) (genCondition ctx) C.CAnd
  , Gen.subterm2 (genCondition ctx) (genCondition ctx) C.COr
  ]
  where
    -- The printer writes two negations as one "!!" token, which Cabal-syntax
    -- rejects.
    negation c@(C.CNot _) = c
    negation c = C.CNot c
    leaves =
      [ C.Lit <$> Gen.bool
      , C.Var . C.OS <$> Gen.frequency
          [(4, Gen.element C.knownOSs), (1, C.OtherOS . ("other" ++) <$> lowerWord)]
      , C.Var . C.Arch <$> Gen.frequency
          [(4, Gen.element C.knownArches), (1, C.OtherArch . ("other" ++) <$> lowerWord)]
      , C.Var <$> (C.Impl <$> genCompiler <*> genConditionRange ctx)
      ] ++ [C.Var . C.PackageFlag <$> Gen.element (flagNames ctx) | not (null (flagNames ctx))]

genCompiler :: Gen C.CompilerFlavor
genCompiler = Gen.frequency
  [ (4, Gen.element C.knownCompilerFlavors)
  , (1, C.OtherCompiler . ("other" ++) <$> lowerWord)
  ]

genLibrary :: Context -> C.LibraryName -> Bool -> C.BuildInfo -> Gen C.Library
genLibrary ctx name _ bi = do
  exposed <- genModules
  reexported <- Gen.list (Range.linear 0 2) genReexport
  signatures <- since ctx C.CabalSpecV2_0 genModules
  -- Only a sub-library has a visibility field.
  visibility <- case name of
    C.LSubLibName _ | contextSpec ctx >= C.CabalSpecV3_0 ->
      Gen.element [C.LibraryVisibilityPrivate, C.LibraryVisibilityPublic]
    C.LSubLibName _ -> pure C.LibraryVisibilityPrivate
    C.LMainLibName -> pure C.LibraryVisibilityPublic
  isExposed <- Gen.bool
  pure C.emptyLibrary
    { C.libName = name, C.exposedModules = exposed, C.reexportedModules = reexported
    , C.signatures = signatures, C.libExposed = isExposed, C.libVisibility = visibility
    , C.libBuildInfo = bi }
  where
    genReexport = C.ModuleReexport
      <$> Gen.maybe (Gen.choice [genOtherPackage ctx, pure (packageName ctx)])
      <*> genModuleName <*> genModuleName

genExecutable :: Context -> C.UnqualComponentName -> Bool -> C.BuildInfo -> Gen C.Executable
genExecutable ctx name _ bi = do
  mainIs <- Gen.frequency [(1, pure ""), (4, genRelativePath)]
  scope <- if contextSpec ctx >= C.CabalSpecV2_0
    then Gen.element [C.ExecutablePublic, C.ExecutablePrivate]
    else pure C.ExecutablePublic
  pure C.emptyExecutable
    { C.exeName = name, C.modulePath = C.unsafeMakeSymbolicPath mainIs
    , C.exeScope = scope, C.buildInfo = bi }

genForeignLibrary :: C.UnqualComponentName -> Bool -> C.BuildInfo -> Gen C.ForeignLib
genForeignLibrary name _ bi = do
  kind <- Gen.element C.knownForeignLibTypes
  options <- Gen.element [[], [C.ForeignLibStandalone]]
  versionInfo <- Gen.maybe (C.mkLibVersionInfo <$> ((,,) <$> small <*> small <*> small))
  versionLinux <- Gen.maybe genVersion
  modDefFiles <- genRelativePaths
  pure C.emptyForeignLib
    { C.foreignLibName = name, C.foreignLibType = kind, C.foreignLibOptions = options
    , C.foreignLibVersionInfo = versionInfo, C.foreignLibVersionLinux = versionLinux
    , C.foreignLibModDefFile = modDefFiles, C.foreignLibBuildInfo = bi }
  where
    small = Gen.int (Range.linear 0 100)

-- | A test suite. The top node always has an interface. A branch node can
-- have an interface or the value that Cabal-syntax gives to a branch
-- without a type field. Cabal-syntax does not keep the name in the test
-- suite value.
genTestSuite :: Context -> Bool -> C.BuildInfo -> Gen C.TestSuite
genTestSuite ctx top bi = do
  interface <- Gen.frequency
    [ (if top then 0 else 2, pure (C.TestSuiteUnsupported (C.TestTypeUnknown "" C.nullVersion)))
    , (2, C.TestSuiteExeV10 (C.mkVersion [1, 0]) . C.unsafeMakeSymbolicPath <$> genRelativePath)
    , (1, C.TestSuiteLibV09 (C.mkVersion [0, 9]) <$> genModuleName)
    ]
  generators <- since ctx C.CabalSpecV3_8 (Gen.list (Range.linear 0 2) genToken)
  pure C.emptyTestSuite
    { C.testInterface = interface, C.testBuildInfo = bi, C.testCodeGenerators = generators }

genBenchmark :: Bool -> C.BuildInfo -> Gen C.Benchmark
genBenchmark top bi = do
  interface <- Gen.frequency
    [ (if top then 0 else 2, pure (C.BenchmarkUnsupported (C.BenchmarkTypeUnknown "" C.nullVersion)))
    , (2, C.BenchmarkExeV10 (C.mkVersion [1, 0]) . C.unsafeMakeSymbolicPath <$> genRelativePath)
    ]
  pure C.emptyBenchmark { C.benchmarkInterface = interface, C.benchmarkBuildInfo = bi }

-- | Unique component names for one kind of component.
genComponentNames :: Int -> Gen [C.UnqualComponentName]
genComponentNames count = map C.mkUnqualComponentName . nub <$> Gen.list (Range.linear 0 count) genName

-- Package

genLicense :: C.CabalSpecVersion -> Gen (Either SPDX.License L.License)
genLicense version
  | version >= C.CabalSpecV2_2 = Left <$> Gen.frequency
      [ (1, pure SPDX.NONE)
      , (4, SPDX.License <$> expression (2 :: Int))
      ]
  | otherwise = Right <$> Gen.frequency
      [ (4, Gen.element L.knownLicenses)
      , (1, L.GPL . Just <$> genVersion)
      , (1, L.UnknownLicense . ("Unknown" ++) <$> Gen.string (Range.linear 1 6) Gen.alphaNum)
      ]
  where
    list = SPDX.cabalSpecVersionToSPDXListVersion version
    -- The printer writes an operand with the same operator without
    -- parentheses, and the parser groups the operators to the right.
    expression depth = foldr1 SPDX.EOr <$> Gen.list (Range.linear 1 3) (conjunction depth)
    -- A disjunction in parentheses occurs only as an operand of a conjunction.
    conjunction depth = Gen.choice
      [ simple
      , foldr1 SPDX.EAnd <$> Gen.list (Range.linear 2 3) (term depth)
      ]
    term depth
      | depth == 0 = simple
      | otherwise = Gen.frequency
          [ (4, simple)
          , (1, foldr1 SPDX.EOr <$> Gen.list (Range.linear 2 3) (conjunction (depth - 1)))
          ]
    simple = SPDX.ELicense <$> simpleLicense <*> Gen.frequency
      [(4, pure Nothing), (1, Just <$> Gen.element (SPDX.licenseExceptionIdList list))]
    simpleLicense = Gen.frequency
      [ (6, SPDX.ELicenseId <$> Gen.element (SPDX.licenseIdList list))
      , (1, SPDX.ELicenseIdPlus <$> Gen.element (SPDX.licenseIdList list))
      , (1, SPDX.ELicenseRef <$> (SPDX.mkLicenseRef' <$> Gen.maybe genToken <*> genToken))
      ]

genSourceRepo :: Gen C.SourceRepo
genSourceRepo = do
  kind <- Gen.frequency
    [(4, Gen.element [C.RepoHead, C.RepoThis]), (1, C.RepoKindUnknown . ("other" ++) <$> lowerWord)]
  kindOfRepo <- Gen.maybe (Gen.frequency
    [ (4, C.KnownRepoType <$> Gen.element C.knownRepoTypes)
    , (1, C.OtherRepoType . ("other" ++) <$> lowerWord)
    ])
  location <- Gen.maybe (Gen.frequency [(4, ("https://example.com/" ++) <$> genName), (1, genLine)])
  repoModule <- Gen.maybe genToken
  branch <- Gen.maybe genToken
  tag <- Gen.maybe genToken
  subdir <- Gen.maybe genPath
  pure (C.emptySourceRepo kind)
    { C.repoType = kindOfRepo, C.repoLocation = location, C.repoModule = repoModule
    , C.repoBranch = branch, C.repoTag = tag, C.repoSubdir = subdir }

-- | A build type and a custom-setup section.
--
-- * The custom-setup section starts in format version 1.24.
-- * Without a build-type field, the build type is Custom before format
--   version 2.2. From format version 2.2, the build type is Custom if a
--   custom-setup section is present, and Simple otherwise.
-- * From format version 1.24, a Custom build type needs a custom-setup
--   section.
-- * The Hooks build type starts in format version 3.14 and needs a
--   custom-setup section.
-- * The Make build type stops in format version 3.18.
genSetup :: Context -> Gen (Maybe C.BuildType, Maybe C.SetupBuildInfo)
genSetup ctx = do
  buildType <- Gen.maybe (Gen.element types)
  let effective = fromMaybe (if v >= C.CabalSpecV2_2 then C.Simple else C.Custom) buildType
      required = (effective == C.Custom && v >= C.CabalSpecV1_24) || effective == C.Hooks
  present <- if required then pure True else if v >= C.CabalSpecV1_24 then Gen.bool else pure False
  setup <- if present
    then Just . (`C.SetupBuildInfo` False) <$> Gen.list (Range.linear 0 3) (genDependency ctx)
    else pure Nothing
  -- A custom-setup section changes the default build type from format
  -- version 2.2, so the value must be explicit.
  let explicit = case buildType of
        Nothing | present && v >= C.CabalSpecV2_2 -> Just C.Custom
        _ -> buildType
  pure (explicit, setup)
  where
    v = contextSpec ctx
    types = [C.Simple, C.Configure, C.Custom]
      ++ [C.Make | v < C.CabalSpecV3_18] ++ [C.Hooks | v >= C.CabalSpecV3_14]

genFlag :: C.CabalSpecVersion -> C.FlagName -> Gen C.PackageFlag
genFlag version name = C.MkPackageFlag name <$> genFreeText version <*> Gen.bool <*> Gen.bool

-- | A flag name. Cabal-syntax changes flag names to lower case.
genFlagName :: Gen C.FlagName
genFlagName = C.mkFlagName <$> Gen.filter (\n -> take 1 n /= "-") (Gen.string (Range.linear 1 8)
  (Gen.frequency [(10, Gen.lower), (2, Gen.digit), (1, Gen.element ("-_" :: String))]))

genPackage :: Gen C.GenericPackageDescription
genPackage = do
  version <- Gen.enumBounded
  name <- C.mkPackageName <$> genName
  flags <- nub <$> Gen.list (Range.linear 0 3) genFlagName
  subLibraryNames <- filter ((/= name) . C.unqualComponentNameToPackageName) <$> genComponentNames 2
  let ctx = Context version name subLibraryNames flags
  packageVersion <- genVersion
  flagDeclarations <- traverse (genFlag version) flags
  license <- genLicense version
  licenseFiles <- genRelativePaths
  copyright <- genShortText
  maintainer <- genShortText
  author <- genShortText
  stability <- genShortText
  homepage <- genShortText
  packageUrl <- genShortText
  bugReports <- genShortText
  synopsis <- genShortText
  category <- genShortText
  description <- genFreeText version
  testedWith <- Gen.list (Range.linear 0 2) ((,) <$> genCompiler <*> genRange ctx)
  (buildType, setup) <- genSetup ctx
  repositories <- Gen.list (Range.linear 0 2) genSourceRepo
  custom <- genCustomFields
  extraSources <- genRelativePaths
  extraDocs <- genRelativePaths
  extraTemporary <- genRelativePaths
  extraFiles <- since ctx C.CabalSpecV3_14 genRelativePaths
  dataFiles <- genRelativePaths
  dataDir <- Gen.frequency [(3, pure C.sameDirectory), (1, C.unsafeMakeSymbolicPath <$> genPath)]
  library <- Gen.maybe (genTree ctx (genLibrary ctx C.LMainLibName))
  libraries <- traverse (\n -> (,) n <$> genTree ctx (genLibrary ctx (C.LSubLibName n))) subLibraryNames
  exeNames <- genComponentNames 2
  executables <- traverse (\n -> (,) n <$> genTree ctx (genExecutable ctx n)) exeNames
  foreignNames <- genComponentNames 1
  foreignLibraries <- traverse (\n -> (,) n <$> genTree ctx (genForeignLibrary n)) foreignNames
  testNames <- genComponentNames 2
  tests <- traverse (\n -> (,) n <$> genTree ctx (genTestSuite ctx)) testNames
  benchNames <- genComponentNames 1
  benchmarks <- traverse (\n -> (,) n <$> genTree ctx genBenchmark) benchNames
  let pd = C.emptyPackageDescription
        { C.specVersion = version
        , C.package = C.PackageIdentifier name packageVersion
        , C.licenseRaw = license
        , C.licenseFiles = licenseFiles
        , C.copyright = copyright
        , C.maintainer = maintainer
        , C.author = author
        , C.stability = stability
        , C.homepage = homepage
        , C.pkgUrl = packageUrl
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
        , C.extraTmpFiles = extraTemporary
        , C.extraFiles = extraFiles
        , C.dataFiles = dataFiles
        , C.dataDir = dataDir
        }
  pure C.emptyGenericPackageDescription
    { C.packageDescription = pd
    , C.genPackageFlags = flagDeclarations
    , C.condLibrary = library
    , C.condSubLibraries = libraries
    , C.condExecutables = executables
    , C.condForeignLibs = foreignLibraries
    , C.condTestSuites = tests
    , C.condBenchmarks = benchmarks
    }
