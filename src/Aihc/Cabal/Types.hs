{-# LANGUAGE OverloadedStrings #-}
module Aihc.Cabal.Types where

import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Aihc.Cabal.Version (Version, VersionRange)

data Diagnostic = Diagnostic
  { diagnosticLine :: Int
  , diagnosticColumn :: Int
  , diagnosticMessage :: Text
  } deriving (Eq, Show)

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

-- | Fields before defaults are applied. Lists retain their source order.
data BuildInfo = BuildInfo
  { buildable :: Maybe Bool
  , sourceDirs :: [FilePath]
  , exposedModules :: [Text]
  , otherModules :: [Text]
  , autogenModules :: [Text]
  , mainIs :: Maybe FilePath
  , defaultLanguage :: Maybe Text
  , extensions :: [Text]
  , dependencies :: [Dependency]
  , buildTools :: [ToolDependency]
  , cSources :: [FilePath]
  , cxxSources :: [FilePath]
  , includeDirs :: [FilePath]
  , installIncludes :: [FilePath]
  , autogenIncludes :: [FilePath]
  , cppOptions :: [Text]
  , ccOptions :: [Text]
  , cxxOptions :: [Text]
  , ghcOptions :: [Text]
  , extraFields :: Map Text [Text]
  } deriving (Eq, Show)

emptyBuildInfo :: BuildInfo
emptyBuildInfo = BuildInfo Nothing [] [] [] [] Nothing Nothing [] [] [] [] [] [] [] [] [] [] [] [] Map.empty

-- | Merge explicit fields. Apply defaults after all active branches are merged.
mergeBuildInfo :: BuildInfo -> BuildInfo -> BuildInfo
mergeBuildInfo a b = BuildInfo
  { buildable = case (buildable a, buildable b) of
      (Nothing, y) -> y
      (x, Nothing) -> x
      (Just x, Just y) -> Just (x && y)
  , sourceDirs = nub (sourceDirs a ++ sourceDirs b)
  , exposedModules = exposedModules a ++ exposedModules b
  , otherModules = nub (otherModules a ++ otherModules b)
  , autogenModules = nub (autogenModules a ++ autogenModules b)
  , mainIs = prefer (mainIs a) (mainIs b)
  , defaultLanguage = prefer (defaultLanguage a) (defaultLanguage b)
  , extensions = nub (extensions a ++ extensions b)
  , dependencies = nub (dependencies a ++ dependencies b)
  , buildTools = buildTools a ++ buildTools b
  , cSources = nub (cSources a ++ cSources b)
  , cxxSources = nub (cxxSources a ++ cxxSources b)
  , includeDirs = nub (includeDirs a ++ includeDirs b)
  , installIncludes = nub (installIncludes a ++ installIncludes b)
  , autogenIncludes = nub (autogenIncludes a ++ autogenIncludes b)
  , cppOptions = cppOptions a ++ cppOptions b
  , ccOptions = ccOptions a ++ ccOptions b
  , cxxOptions = cxxOptions a ++ cxxOptions b
  , ghcOptions = ghcOptions a ++ ghcOptions b
  , extraFields = Map.unionWith (++) (extraFields a) (extraFields b)
  }
  where
    prefer x Nothing = x
    prefer _ y = y

-- | Repeated repository fields keep their values in source order.
data SourceRepository = SourceRepository
  { sourceRepositoryKind :: Text
  , sourceRepositoryFields :: Map Text [Text]
  } deriving (Eq, Show)

data Package = Package
  { packageName :: Text
  , packageVersion :: Version
  , cabalVersion :: Version
  , buildType :: Text
  , packageFlags :: [Flag]
  , packageComponents :: [Component (Conditional BuildInfo)]
  , packageFields :: Map Text [Text]
  , packageSourceRepositories :: [SourceRepository]
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
