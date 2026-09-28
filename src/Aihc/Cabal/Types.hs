{-# LANGUAGE OverloadedStrings #-}
module Aihc.Cabal.Types where

import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Aihc.Cabal.Version (Version, VersionRange)

data Diagnostic = Diagnostic
  { diagnosticLine :: Int
  , diagnosticColumn :: Int
  , diagnosticMessage :: Text
  } deriving (Eq, Show)

-- | A source position. Rows start at 1. Columns start at 1 and count UTF-8
-- bytes, as in Cabal-syntax.
data Position = Position
  { positionRow :: !Int
  , positionColumn :: !Int
  } deriving (Eq, Ord, Show)

-- | One line of a field value. The text has no leading spaces or line break.
data FieldLine = FieldLine
  { fieldLinePosition :: !Position
  , fieldLineText :: !Text
  } deriving (Eq, Show)

-- | A field value as the source file gives it. The position is the position
-- of the field name. The lines are in source order. Comment lines and blank
-- lines are not included.
data FieldValue = FieldValue
  { fieldPosition :: !Position
  , fieldLines :: [FieldLine]
  } deriving (Eq, Show)

-- | The lines of a field value, joined with line breaks.
fieldText :: FieldValue -> Text
fieldText = T.intercalate "\n" . map fieldLineText . fieldLines

data ParseResult a = ParseResult
  { parseWarnings :: [Diagnostic]
  , parseValue :: Either (NonEmpty Diagnostic) a
  } deriving (Eq, Show)

data LibraryTarget = MainLibrary | NamedLibrary Text deriving (Eq, Ord, Show)

data Dependency = Dependency
  { dependencyPackage :: Text
  , dependencyRange :: VersionRange
  , dependencyLibraries :: NonEmpty LibraryTarget
  } deriving (Eq, Show)

-- | Module visibility for a mixin or a package dependency.
data ModuleRenaming
  = DefaultRenaming
  -- ^ All exposed modules with their names.
  | ModuleRenaming [(Text, Text)]
  -- ^ Only the given modules, each with a new name.
  | HidingRenaming [Text]
  -- ^ All exposed modules except the given modules.
  deriving (Eq, Show)

-- | A Backpack mixin from a @mixins@ field.
data Mixin = Mixin
  { mixinPackage :: Text
  , mixinLibrary :: LibraryTarget
  , mixinProvides :: ModuleRenaming
  , mixinRequires :: ModuleRenaming
  } deriving (Eq, Show)

data ToolDependency = ToolDependency
  { toolPackage :: Maybe Text
  , toolName :: Text
  , toolRange :: VersionRange
  } deriving (Eq, Show)

data Flag = Flag
  { flagName :: Text
  , flagDefault :: Bool
  , flagManual :: Bool
  , flagDescription :: Text
  } deriving (Eq, Show)

type FlagAssignment = Map Text Bool

data Condition
  = Literal Bool | OS Text | Arch Text | Impl Text VersionRange | FlagValue Text
  | Not Condition | And Condition Condition | Or Condition Condition
  deriving (Eq, Show)

data Conditional a = Conditional
  { unconditional :: a
  , branches :: [Branch a]
  } deriving (Eq, Show)

data Branch a = Branch
  { condition :: Condition
  , whenTrue :: Conditional a
  , whenFalse :: Maybe (Conditional a)
  } deriving (Eq, Show)

data ComponentKind
  = Library (Maybe Text) | Executable Text | TestSuite Text
  | Benchmark Text | ForeignLibrary Text
  deriving (Eq, Ord, Show)

data Component a = Component
  { componentKind :: ComponentKind
  , componentData :: a
  } deriving (Eq, Show)

-- | Fields before defaults are applied. Lists keep their source order.
-- A field that the Cabal specification version does not support is absent.
data BuildInfo = BuildInfo
  { buildable :: Maybe Bool
  , sourceDirs :: [FilePath]
  , exposedModules :: [Text]
  , otherModules :: [Text]
  , autogenModules :: [Text]
  , virtualModules :: [Text]
  , mainIs :: Maybe FilePath
  , defaultLanguage :: Maybe Text
  , otherLanguages :: [Text]
  , extensions :: [Text]
  , otherExtensions :: [Text]
  , legacyExtensions :: [Text]
  , dependencies :: [Dependency]
  , mixins :: [Mixin]
  , buildTools :: [ToolDependency]
  , cSources :: [FilePath]
  , cxxSources :: [FilePath]
  , asmSources :: [FilePath]
  , cmmSources :: [FilePath]
  , jsSources :: [FilePath]
  , includeDirs :: [FilePath]
  , includes :: [FilePath]
  , installIncludes :: [FilePath]
  , autogenIncludes :: [FilePath]
  , extraLibDirs :: [FilePath]
  , extraLibDirsStatic :: [FilePath]
  , frameworks :: [Text]
  , extraFrameworkDirs :: [FilePath]
  , cppOptions :: [Text]
  , ccOptions :: [Text]
  , cxxOptions :: [Text]
  , ghcOptions :: [Text]
  , extraFields :: Map Text [FieldValue]
  } deriving (Eq, Show)

emptyBuildInfo :: BuildInfo
emptyBuildInfo = BuildInfo
  { buildable = Nothing, sourceDirs = [], exposedModules = [], otherModules = []
  , autogenModules = [], virtualModules = [], mainIs = Nothing, defaultLanguage = Nothing
  , otherLanguages = [], extensions = [], otherExtensions = [], legacyExtensions = []
  , dependencies = [], mixins = [], buildTools = [], cSources = [], cxxSources = [], asmSources = []
  , cmmSources = [], jsSources = [], includeDirs = [], includes = [], installIncludes = []
  , autogenIncludes = [], extraLibDirs = [], extraLibDirsStatic = [], frameworks = []
  , extraFrameworkDirs = [], cppOptions = [], ccOptions = [], cxxOptions = []
  , ghcOptions = [], extraFields = Map.empty
  }

-- | Merge the fields of two parts, as Cabal-syntax merges build information.
-- Use it for common stanza imports and for active conditional branches.
-- Values in the second part come after values in the first part. Some lists
-- do not keep duplicate values. A scalar value in the second part replaces the
-- scalar value in the first part. Apply defaults after the merge.
mergeBuildInfo :: BuildInfo -> BuildInfo -> BuildInfo
mergeBuildInfo a b = BuildInfo
  { buildable = case (buildable a, buildable b) of
      (Nothing, y) -> y
      (x, Nothing) -> x
      (Just x, Just y) -> Just (x && y)
  , sourceDirs = unique sourceDirs
  , exposedModules = both exposedModules
  , otherModules = unique otherModules
  , autogenModules = unique autogenModules
  , virtualModules = unique virtualModules
  , mainIs = prefer (mainIs a) (mainIs b)
  , defaultLanguage = prefer (defaultLanguage a) (defaultLanguage b)
  , otherLanguages = unique otherLanguages
  , extensions = unique extensions
  , otherExtensions = unique otherExtensions
  , legacyExtensions = unique legacyExtensions
  , dependencies = unique dependencies
  , mixins = both mixins
  , buildTools = both buildTools
  , cSources = unique cSources
  , cxxSources = unique cxxSources
  , asmSources = unique asmSources
  , cmmSources = unique cmmSources
  , jsSources = unique jsSources
  , includeDirs = unique includeDirs
  , includes = unique includes
  , installIncludes = unique installIncludes
  , autogenIncludes = unique autogenIncludes
  , extraLibDirs = unique extraLibDirs
  , extraLibDirsStatic = unique extraLibDirsStatic
  , frameworks = unique frameworks
  , extraFrameworkDirs = unique extraFrameworkDirs
  , cppOptions = both cppOptions
  , ccOptions = both ccOptions
  , cxxOptions = both cxxOptions
  , ghcOptions = both ghcOptions
  , extraFields = Map.unionWith (++) (extraFields a) (extraFields b)
  }
  where
    both f = f a ++ f b
    unique f = nub (both f)
    prefer x Nothing = x
    prefer _ y = y

-- | Repeated repository fields keep their values in source order.
data SourceRepository = SourceRepository
  { sourceRepositoryKind :: Text
  , sourceRepositoryFields :: Map Text [FieldValue]
  } deriving (Eq, Show)

data Package = Package
  { packageName :: Text
  , packageVersion :: Version
  , cabalVersion :: Version
  , buildType :: Text
  , packageFlags :: [Flag]
  , packageComponents :: [Component (Conditional BuildInfo)]
  , packageFields :: Map Text [FieldValue]
  , packageSourceRepositories :: [SourceRepository]
  , packageSetupDependencies :: Maybe [Dependency]
  } deriving (Eq, Show)

data Environment = Environment
  { targetOS :: Text
  , targetArch :: Text
  , compilerVersions :: Map Text Version
  } deriving (Eq, Show)

data ResolvedPackage = ResolvedPackage
  { resolvedName :: Text
  , resolvedVersion :: Version
  , resolvedFlags :: FlagAssignment
  , resolvedComponents :: [Component BuildInfo]
  } deriving (Eq, Show)

data BuildInfoFile = BuildInfoFile
  { libraryBuildInfo :: Maybe BuildInfo
  , executableBuildInfo :: Map Text BuildInfo
  } deriving (Eq, Show)
