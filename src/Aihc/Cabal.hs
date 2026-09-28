{-# LANGUAGE OverloadedStrings #-}
module Aihc.Cabal
  ( module Aihc.Cabal.Types, module Aihc.Cabal.Version
  , parsePackage, parseBuildInfo, resolvePackage, evaluateCondition
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Aihc.Cabal.Types
import Aihc.Cabal.Version hiding (Parser, versionParser, versionDigits, rangeParser)
import Aihc.Cabal.Parser

evaluateCondition :: Environment -> FlagAssignment -> Condition -> Bool
evaluateCondition env flags cond = case cond of
  Literal b -> b
  OS x -> T.toLower x == T.toLower (targetOS env)
  Arch x -> T.toLower x == T.toLower (targetArch env)
  Impl x range -> maybe False (`withinRange` range) (Map.lookup (T.toLower x) (compilerVersions env))
  FlagValue x -> Map.findWithDefault False x flags
  Not a -> not (evaluateCondition env flags a)
  And a b -> evaluateCondition env flags a && evaluateCondition env flags b
  Or a b -> evaluateCondition env flags a || evaluateCondition env flags b

-- | Resolve all components. Component selection belongs to the caller.
resolvePackage :: Environment -> FlagAssignment -> Package -> Either (NonEmpty Diagnostic) ResolvedPackage
resolvePackage env overrides pkg = case unknown of
  x:_ -> Left (Diagnostic 1 1 ("Unknown flag: " <> x) :| [])
  [] -> Right (ResolvedPackage (packageName pkg) (packageVersion pkg) flags components)
  where
    defaults = Map.fromList [(flagName f, flagDefault f) | f <- packageFlags pkg]
    unknown = Map.keys (Map.difference overrides defaults)
    flags = Map.union overrides defaults
    components = [Component k (finish (resolve t)) | Component k t <- packageComponents pkg]
    resolve (Conditional info bs) = foldl mergeBuildInfo info (concatMap branch bs)
    branch (Branch c t e)
      | evaluateCondition env flags c = [resolve t]
      | otherwise = maybe [] (pure . resolve) e
    finish bi = bi
      { buildable = Just (fromMaybe True (buildable bi))
      , sourceDirs = if null (sourceDirs bi) then ["."] else sourceDirs bi
      }
