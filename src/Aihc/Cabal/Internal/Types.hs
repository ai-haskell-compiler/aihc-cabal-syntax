{-# LANGUAGE OverloadedStrings #-}
-- | The data types of the library. "Aihc.Cabal" exports all of them.
--
-- The types keep the data of a Cabal file as the file gives it. Names,
-- modules, and options are 'Text'. Paths are 'FilePath', because the caller
-- joins them with directories. A scalar field that is absent is 'Nothing'.
-- A list field that is absent is @[]@. Defaults are applied by
-- 'Aihc.Cabal.resolvePackage', not by the parser.
module Aihc.Cabal.Internal.Types where

import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Aihc.Cabal.Internal.Version (Version, VersionRange)

-- * Positions and diagnostics

-- | A source position. Rows start at 1. Columns start at 1 and count UTF-8
-- bytes, as in Cabal-syntax.
data Position = Position
  { positionRow :: !Int
  , positionColumn :: !Int
  } deriving (Eq, Ord, Show)

-- | An error or a warning from the parser. A check of the complete package
-- has no position.
data Diagnostic = Diagnostic
  { diagnosticPosition :: Maybe Position
  , diagnosticMessage :: Text
  } deriving (Eq, Show)

-- | The result of a parse. The parser stops at the first error, so the
-- error side holds one diagnostic. Warnings are present with an error and
-- with a value. The parser gives one warning: @Legacy cabal file@ for a
-- file that gets a Cabal-syntax patch.
data ParseResult a = ParseResult
  { parseWarnings :: [Diagnostic]
  , parseValue :: Either Diagnostic a
  } deriving (Eq, Show)

-- * Field values

-- | One line of a field value. The text has no leading spaces or line break.
data FieldLine = FieldLine
  { fieldLinePosition :: !Position
  , fieldLineText :: !Text
  } deriving (Eq, Show)

-- | A field value as the source file gives it. The position is the position
-- of the field name. The lines are in source order. Comment lines and blank
-- lines are not included.
--
-- Field values occur in 'packageFields', 'extraFields', and
-- 'sourceRepositoryFields'. The parser does not check these values.
data FieldValue = FieldValue
  { fieldPosition :: !Position
  , fieldLines :: [FieldLine]
  } deriving (Eq, Show)

-- | The lines of a field value, joined with line breaks. For a free text
-- field, such as @description@, use 'Aihc.Cabal.packageFieldText'.
fieldText :: FieldValue -> Text
fieldText = T.intercalate "\n" . map fieldLineText . fieldLines

-- * Dependencies

-- | A library of a package. 'MainLibrary' is the library without a name.
data LibraryTarget = MainLibrary | NamedLibrary Text deriving (Eq, Ord, Show)

-- | One entry of a @build-depends@ or @setup-depends@ field.
--
-- Before @cabal-version@ 3.4, a dependency on the name of an internal
-- library of the same package refers to that library. The parser changes
-- such a dependency to the package name and the 'NamedLibrary' target.
data Dependency = Dependency
  { dependencyPackage :: Text
  , dependencyRange :: VersionRange
  -- ^ 'Aihc.Cabal.anyVersion' when the field gives no range.
  , dependencyLibraries :: NonEmpty LibraryTarget
  -- ^ The libraries in @pkg:{a, b}@ syntax. Without that syntax, the main
  -- library.
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

-- | One entry of a @build-tool-depends@ or a legacy @build-tools@ field.
data ToolDependency = ToolDependency
  { toolPackage :: Maybe Text
  -- ^ The package in @pkg:exe@ syntax. 'Nothing' for a @build-tools@ entry,
  -- which names only the tool.
  , toolName :: Text
  , toolRange :: VersionRange
  -- ^ 'Aihc.Cabal.anyVersion' when the field gives no range.
  } deriving (Eq, Show)

-- * Flags and conditions

-- | A @flag@ section. Flag names are lower case.
data Flag = Flag
  { flagName :: Text
  , flagDefault :: Bool
  -- ^ The @default@ field. 'True' when the field is absent.
  , flagManual :: Bool
  -- ^ The @manual@ field. 'False' when the field is absent.
  , flagDescription :: Text
  -- ^ The @description@ field after the Cabal free text rules. @\"\"@ when
  -- the field is absent.
  } deriving (Eq, Show)

-- | Flag values by flag name.
type FlagAssignment = Map Text Bool

-- | The condition of an @if@ or @elif@ section.
data Condition
  = Literal Bool
  | OS Text
  -- ^ @os(name)@. The comparison ignores case. The parser changes an alias
  -- to its canonical name, as Cabal-syntax does. For example, @darwin@
  -- becomes @osx@, and @mingw32@ becomes @windows@.
  | Arch Text
  -- ^ @arch(name)@. The comparison ignores case.
  | Impl Text VersionRange
  -- ^ @impl(compiler range)@. Without a range, 'Aihc.Cabal.anyVersion'.
  -- The comparison of the compiler name ignores case.
  | FlagValue Text
  -- ^ @flag(name)@. The name is lower case.
  | Not Condition
  | And Condition Condition
  | Or Condition Condition
  deriving (Eq, Show)

-- | Data with conditional parts, as one section of a Cabal file gives it.
-- 'unconditional' holds the fields outside @if@ sections. 'branches' holds
-- the @if@ sections in source order.
data Conditional a = Conditional
  { unconditional :: a
  , branches :: [Branch a]
  } deriving (Eq, Show)

-- | An @if@ section with its optional @else@ or @elif@ part. An @elif@ part
-- becomes an @else@ part with one branch.
data Branch a = Branch
  { condition :: Condition
  , whenTrue :: Conditional a
  , whenFalse :: Maybe (Conditional a)
  } deriving (Eq, Show)

-- * Components

-- | The kind and name of a component section.
data ComponentKind
  = Library LibraryTarget
  | Executable Text
  | TestSuite Text
  | Benchmark Text
  | ForeignLibrary Text
  deriving (Eq, Ord, Show)

-- | A component of a package. In a t'Package', the data is a
-- @t'Conditional' t'BuildInfo'@. In a t'ResolvedPackage', the data is a
-- t'BuildInfo'.
data Component a = Component
  { componentKind :: ComponentKind
  , componentData :: a
  } deriving (Eq, Show)

-- | The build fields of one section level, before defaults. Lists keep
-- their source order. A field that the Cabal format version of the file
-- does not support is absent.
--
-- The 'Semigroup' instance merges two parts as Cabal-syntax merges build
-- information. Values in the second part come after values in the first
-- part. Some lists do not keep duplicate values. A scalar value in the
-- second part replaces the scalar value in the first part. The parser uses
-- the merge for common stanza imports. 'Aihc.Cabal.resolvePackage' uses it
-- for active conditional branches.
data BuildInfo = BuildInfo
  { buildable :: Maybe Bool
  -- ^ @buildable@. Two values merge with 'Bool' and.
  , sourceDirs :: [FilePath]
  -- ^ @hs-source-dirs@, then the older @hs-source-dir@ values.
  , exposedModules :: [Text]
  -- ^ @exposed-modules@. Only a library has this field.
  , otherModules :: [Text]
  -- ^ @other-modules@.
  , autogenModules :: [Text]
  -- ^ @autogen-modules@, from @cabal-version@ 2.0.
  , virtualModules :: [Text]
  -- ^ @virtual-modules@, from @cabal-version@ 2.2.
  , mainIs :: Maybe FilePath
  -- ^ @main-is@. Only an executable, a test suite, or a benchmark has this
  -- field.
  , defaultLanguage :: Maybe Text
  -- ^ @default-language@, from @cabal-version@ 1.10. 'Nothing' means the
  -- Haskell98 default of Cabal.
  , otherLanguages :: [Text]
  -- ^ @other-languages@, from @cabal-version@ 1.10.
  , extensions :: [Text]
  -- ^ @default-extensions@, from @cabal-version@ 1.10.
  , otherExtensions :: [Text]
  -- ^ @other-extensions@.
  , legacyExtensions :: [Text]
  -- ^ The older @extensions@ field. It is an error from @cabal-version@ 3.0.
  -- Use it with 'extensions' when you select compiler extensions.
  , dependencies :: [Dependency]
  -- ^ @build-depends@.
  , mixins :: [Mixin]
  -- ^ @mixins@, from @cabal-version@ 2.0.
  , buildTools :: [ToolDependency]
  -- ^ The legacy @build-tools@ values, then the @build-tool-depends@ values.
  , cSources :: [FilePath]
  -- ^ @c-sources@.
  , cxxSources :: [FilePath]
  -- ^ @cxx-sources@, from @cabal-version@ 2.2.
  , asmSources :: [FilePath]
  -- ^ @asm-sources@, from @cabal-version@ 3.0.
  , cmmSources :: [FilePath]
  -- ^ @cmm-sources@, from @cabal-version@ 3.0.
  , jsSources :: [FilePath]
  -- ^ @js-sources@.
  , includeDirs :: [FilePath]
  -- ^ @include-dirs@.
  , includes :: [FilePath]
  -- ^ @includes@.
  , installIncludes :: [FilePath]
  -- ^ @install-includes@.
  , autogenIncludes :: [FilePath]
  -- ^ @autogen-includes@, from @cabal-version@ 3.0.
  , extraLibDirs :: [FilePath]
  -- ^ @extra-lib-dirs@.
  , extraLibDirsStatic :: [FilePath]
  -- ^ @extra-lib-dirs-static@, from @cabal-version@ 3.8.
  , frameworks :: [Text]
  -- ^ @frameworks@.
  , extraFrameworkDirs :: [FilePath]
  -- ^ @extra-framework-dirs@.
  , cppOptions :: [Text]
  -- ^ @cpp-options@. Options keep all values, also duplicates.
  , ccOptions :: [Text]
  -- ^ @cc-options@.
  , cxxOptions :: [Text]
  -- ^ @cxx-options@, from @cabal-version@ 2.2.
  , ghcOptions :: [Text]
  -- ^ @ghc-options@.
  , extraFields :: Map Text [FieldValue]
  -- ^ All other fields of the section, by lower case name, with their values
  -- in source order. This includes @x-@ fields, for example
  -- @x-aihc-lir-sources@. Use 'fieldText' to read a value.
  } deriving (Eq, Show)

-- | A t'BuildInfo' without fields. Use it with record syntax to make a value.
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

-- | Merge two parts. See the 'Semigroup' instance of t'BuildInfo'.
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

instance Semigroup BuildInfo where
  (<>) = mergeBuildInfo

instance Monoid BuildInfo where
  mempty = emptyBuildInfo

-- * Packages

-- | A @source-repository@ section. Repeated fields keep their values in
-- source order. The values keep quotation marks.
data SourceRepository = SourceRepository
  { sourceRepositoryKind :: Text
  -- ^ The section argument, for example @head@ or @this@.
  , sourceRepositoryFields :: Map Text [FieldValue]
  } deriving (Eq, Show)

-- | A parsed package description. Conditions are not evaluated. Use
-- 'Aihc.Cabal.resolvePackage' to get a t'ResolvedPackage'.
data Package = Package
  { packageName :: Text
  , packageVersion :: Version
  , cabalVersion :: Version
  -- ^ The Cabal format version that Cabal-syntax uses for the file. A range
  -- such as @>=1.9@ gives the version that Cabal-syntax selects, here 1.10.
  -- A file without a @cabal-version@ field has version 1.0.
  , buildType :: Text
  -- ^ The build type that Cabal-syntax uses: @Simple@, @Configure@,
  -- @Custom@, @Make@, or @Hooks@. Without a @build-type@ field, the value is
  -- @Simple@ from @cabal-version@ 2.2 and @Custom@ before it. A
  -- @custom-setup@ section gives @Custom@.
  , packageFlags :: [Flag]
  -- ^ The @flag@ sections in source order.
  , packageComponents :: [Component (Conditional BuildInfo)]
  -- ^ The component sections in source order.
  , packageFields :: Map Text [FieldValue]
  -- ^ All fields before the first section, by lower case name, with their
  -- values in source order. This includes @name@, @version@,
  -- @cabal-version@, and @build-type@. Use 'Aihc.Cabal.packageFieldText'
  -- to read a value.
  , packageSourceRepositories :: [SourceRepository]
  -- ^ The @source-repository@ sections in source order.
  , packageSetupDependencies :: Maybe [Dependency]
  -- ^ The @setup-depends@ values of a @custom-setup@ section. 'Nothing'
  -- without that section.
  } deriving (Eq, Show)

-- | The target that 'Aihc.Cabal.evaluateCondition' compares with. The
-- library does not read the host platform or an installed compiler.
data Environment = Environment
  { targetOS :: Text
  -- ^ For @os(...)@, for example @linux@. Case does not matter. The
  -- aliases of Cabal-syntax for host names apply, so @darwin@ is @osx@.
  , targetArch :: Text
  -- ^ For @arch(...)@, for example @x86_64@. Case does not matter. The
  -- aliases of Cabal-syntax for host names apply, so @arm64@ is @aarch64@.
  , compiler :: Text
  -- ^ For @impl(...)@, for example @ghc@. Case does not matter.
  , compilerVersion :: Version
  -- ^ The version that the compiler reports.
  } deriving (Eq, Show)

-- | A package after condition evaluation and defaults. See
-- 'Aihc.Cabal.resolvePackage'.
data ResolvedPackage = ResolvedPackage
  { resolvedFlags :: FlagAssignment
  -- ^ The value of each declared flag.
  , resolvedComponents :: [Component BuildInfo]
  -- ^ All components, in source order. This includes components with
  -- @buildable: False@. The caller selects the components to build.
  } deriving (Eq, Show)

-- | The contents of a @.buildinfo@ file, as a configure script writes it.
data HookedBuildInfo = HookedBuildInfo
  { hookedLibrary :: Maybe BuildInfo
  -- ^ The fields before the first @executable@ field.
  , hookedExecutables :: Map Text BuildInfo
  -- ^ The fields after each @executable: name@ field.
  } deriving (Eq, Show)
