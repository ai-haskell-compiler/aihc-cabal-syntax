{-# LANGUAGE OverloadedStrings #-}
-- | Condition evaluation and flag resolution.
module Aihc.Cabal.Internal.Resolve (evaluateCondition, resolvePackage, packageFieldText) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Aihc.Cabal.Internal.Platform (Strictness (..), canonicalArch, canonicalOS)
import Aihc.Cabal.Internal.Types
import Aihc.Cabal.Internal.Values (freeText)
import Aihc.Cabal.Internal.Version (withinRange)

-- | Evaluate a condition for a target and a flag assignment. A flag that is
-- not in the assignment is 'False'. Names of operating systems,
-- architectures, and compilers compare without case.
--
-- The names get the aliases of Cabal-syntax before the comparison. A name in
-- @os(...)@ uses the aliases for conditions, so @os(darwin)@ is true for the
-- target @osx@. A name in @arch(...)@ has no aliases, so @arch(arm64)@ is
-- false for the target @aarch64@, as in Cabal. The target names use the
-- aliases for host names, so the target @darwin@ is @osx@ and the target
-- @arm64@ is @aarch64@.
evaluateCondition :: Environment -> FlagAssignment -> Condition -> Bool
evaluateCondition env flags cond = case cond of
  Literal b -> b
  OS x -> sameName (canonicalOS Compat x) (canonicalOS Permissive (targetOS env))
  Arch x -> sameName (canonicalArch Strict x) (canonicalArch Permissive (targetArch env))
  Impl x range -> T.toLower x == T.toLower (compiler env) && withinRange (compilerVersion env) range
  FlagValue x -> Map.findWithDefault False x flags
  Not a -> not (evaluateCondition env flags a)
  And a b -> evaluateCondition env flags a && evaluateCondition env flags b
  Or a b -> evaluateCondition env flags a || evaluateCondition env flags b
  where
    sameName a b = T.toLower a == T.toLower b

-- | Evaluate the conditions of all components and apply defaults.
--
-- The flag assignment is the given assignment over the flag defaults of the
-- package. A given flag that the package does not declare has no effect.
-- The active parts of each component merge in source order with the
-- 'Semigroup' instance of t'BuildInfo'. Then an absent 'buildable' becomes
-- 'True', and an empty 'sourceDirs' becomes @[\".\"]@. An absent
-- 'defaultLanguage' stays 'Nothing'.
--
-- The result has all components. The caller selects the components to
-- build.
resolvePackage :: Environment -> FlagAssignment -> Package -> ResolvedPackage
resolvePackage env overrides pkg = ResolvedPackage flags components
  where
    defaults = Map.fromList [(flagName f, flagDefault f) | f <- packageFlags pkg]
    flags = Map.union (Map.intersection overrides defaults) defaults
    components = [Component k (finish (resolve t)) | Component k t <- packageComponents pkg]
    resolve (Conditional info bs) = mconcat (info : concatMap branch bs)
    branch (Branch c t e)
      | evaluateCondition env flags c = [resolve t]
      | otherwise = maybe [] (pure . resolve) e
    finish bi = bi
      { buildable = Just (fromMaybe True (buildable bi))
      , sourceDirs = if null (sourceDirs bi) then ["."] else sourceDirs bi
      }

-- | The text of a package field, such as @description@ or @synopsis@, after
-- the Cabal free text rules of the file's format version. If the field
-- occurs more than one time, the last value wins, as in Cabal-syntax. The
-- result is 'Nothing' for an absent field.
--
-- Before @cabal-version@ 3.0, each line loses its leading and trailing
-- spaces, and a line with one dot becomes an empty line. From
-- @cabal-version@ 3.0, the text keeps blank lines and relative indentation.
packageFieldText :: Package -> Text -> Maybe Text
packageFieldText pkg name = case Map.findWithDefault [] (T.toLower name) (packageFields pkg) of
  [] -> Nothing
  values -> Just (freeText (cabalVersion pkg) (last values))
