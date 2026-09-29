-- | Parse Cabal package descriptions for the aihc compiler.
--
-- This is the only public module of the package. It has no dependency on
-- Cabal or Cabal-syntax. The parser follows the package parser of
-- Cabal-syntax 3.18.1.0 and accepts format versions 1.0 through 3.18.
--
-- The record field names are short, for example 'buildable' and
-- 'dependencies'. Import the module qualified to keep them out of your
-- namespace.
--
-- = Use
--
-- Read a package, evaluate its conditions for one target, and read the
-- fields of the main library:
--
-- @
-- import qualified Aihc.Cabal as Cabal
-- import qualified Data.ByteString as BS
-- import qualified Data.Map.Strict as Map
--
-- main :: IO ()
-- main = do
--   bytes <- BS.readFile \"text.cabal\"
--   package <- either (fail . show) pure (Cabal.parseValue (Cabal.parsePackage bytes))
--   ghc <- either (fail . show) pure (Cabal.parseVersion \"9.12.2\")
--   let environment = Cabal.Environment \"linux\" \"x86_64\" \"ghc\" ghc
--       overrides = Map.fromList [(\"simdutf\", False)]
--       resolved = Cabal.resolvePackage environment overrides package
--   case [bi | Cabal.Component (Cabal.Library Cabal.MainLibrary) bi <- Cabal.resolvedComponents resolved] of
--     [library] -> do
--       print (Cabal.exposedModules library)
--       print (map Cabal.dependencyPackage (Cabal.dependencies library))
--       print (map Cabal.fieldText (Map.findWithDefault [] \"x-aihc-lir-sources\" (Cabal.extraFields library)))
--     _ -> fail \"No main library\"
-- @
--
-- 'parsePackage' keeps conditions. 'resolvePackage' evaluates them for an
-- t'Environment' and a 'FlagAssignment', merges the active parts, and applies
-- defaults. The caller selects the components to build. A dependency solver
-- can inspect the t'Conditional' values of a t'Package' before it selects
-- flags.
--
-- = Limits
--
-- * Package fields other than @name@, @version@, @cabal-version@, and
--   @build-type@ stay in 'packageFields' as text. Component fields without
--   a t'BuildInfo' field stay in 'extraFields' as text. The parser does not
--   check these values.
-- * The parser stops at the first error.
-- * There is no version range simplifier and no package printer.
-- * The library does not solve dependencies, find source files, or run
--   configure scripts.
module Aihc.Cabal
  ( -- * Parsing
    parsePackage
  , parseHookedBuildInfo
  , parseDependency
  , parsePackageIdentifier
  , ParseResult (..)
  , Diagnostic (..)
  , renderDiagnostic
  , Position (..)
    -- * Packages
  , Package (..)
  , packageFieldText
  , Flag (..)
  , SourceRepository (..)
  , FieldValue (..)
  , FieldLine (..)
  , fieldText
    -- * Components
  , Component (..)
  , ComponentKind (..)
  , LibraryTarget (..)
  , Conditional (..)
  , Branch (..)
  , Condition (..)
    -- * Build information
  , BuildInfo (..)
  , emptyBuildInfo
  , mergeBuildInfo
  , Dependency (..)
  , ToolDependency (..)
  , Mixin (..)
  , ModuleRenaming (..)
  , HookedBuildInfo (..)
    -- * Resolution
  , Environment (..)
  , FlagAssignment
  , evaluateCondition
  , resolvePackage
  , ResolvedPackage (..)
    -- * Versions
  , Version
  , mkVersion
  , versionNumbers
  , parseVersion
  , renderVersion
    -- * Version ranges
  , VersionRange (..)
  , anyVersion
  , noVersion
  , thisVersion
  , withinVersion
  , intersectRanges
  , unionRanges
  , withinRange
  , parseVersionRange
  , renderVersionRange
  ) where

import Aihc.Cabal.Internal.Parser
import Aihc.Cabal.Internal.Resolve
import Aihc.Cabal.Internal.Types
import Aihc.Cabal.Internal.Values (parseDependency, parsePackageIdentifier)
import Aihc.Cabal.Internal.Version
