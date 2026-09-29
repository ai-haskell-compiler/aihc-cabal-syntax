{-# LANGUAGE OverloadedStrings #-}
-- | Operating system and architecture names, with the aliases of
-- Cabal-syntax 3.18.1.0 (@Distribution.System@).
--
-- Cabal-syntax uses three alias tables. The names in @os(...)@ conditions
-- use the Compat table. The names in @arch(...)@ conditions use the Strict
-- table, which has no aliases. The names of the host platform use the
-- Permissive table.
module Aihc.Cabal.Internal.Platform
  ( Strictness (..)
  , canonicalOS
  , canonicalArch
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | The alias table that Cabal-syntax uses for a name.
data Strictness = Strict | Compat | Permissive
  deriving (Eq, Show)

-- | The canonical name of an operating system, for example @osx@ for
-- @darwin@. A known name becomes lower case. An unknown name does not change.
canonicalOS :: Strictness -> Text -> Text
canonicalOS strictness name =
  case lookup (T.toLower name) table of
    Just canonical -> canonical
    Nothing -> name
  where
    table = [(alias, os) | os <- knownOSs, alias <- os : osAliases strictness os]

-- | The canonical name of an architecture, for example @aarch64@ for
-- @arm64@. A known name becomes lower case. An unknown name does not change.
canonicalArch :: Strictness -> Text -> Text
canonicalArch strictness name =
  case lookup (T.toLower name) table of
    Just canonical -> canonical
    Nothing -> name
  where
    table = [(alias, arch) | arch <- knownArches, alias <- arch : archAliases strictness arch]

knownOSs :: [Text]
knownOSs =
  [ "linux", "windows", "osx", "freebsd", "openbsd", "netbsd", "dragonfly", "solaris", "aix", "hpux"
  , "irix", "halvm", "hurd", "ios", "android", "ghcjs", "wasi", "haiku" ]

osAliases :: Strictness -> Text -> [Text]
osAliases strictness os = case (strictness, os) of
  (Permissive, "windows") -> ["mingw32", "win32", "cygwin32"]
  (Compat, "windows") -> ["mingw32", "win32"]
  (_, "osx") -> ["darwin"]
  (_, "hurd") -> ["gnu"]
  (Permissive, "freebsd") -> ["kfreebsdgnu"]
  (Compat, "freebsd") -> ["kfreebsdgnu"]
  (Permissive, "solaris") -> ["solaris2"]
  (Compat, "solaris") -> ["solaris2"]
  (Permissive, "android") -> ["linux-android", "linux-androideabi", "linux-androideabihf"]
  (Compat, "android") -> ["linux-android"]
  _ -> []

knownArches :: [Text]
knownArches =
  [ "i386", "x86_64", "ppc", "ppc64", "ppc64le", "sparc", "sparc64", "arm", "aarch64", "mips", "sh"
  , "ia64", "s390", "s390x", "alpha", "hppa", "rs6000", "m68k", "vax", "riscv64", "loongarch64"
  , "javascript", "wasm32" ]

archAliases :: Strictness -> Text -> [Text]
archAliases strictness arch = case (strictness, arch) of
  (Permissive, "ppc") -> ["powerpc"]
  (Permissive, "ppc64") -> ["powerpc64"]
  (Permissive, "ppc64le") -> ["powerpc64le"]
  (Permissive, "mips") -> ["mipsel", "mipseb"]
  (Permissive, "arm") -> ["armeb", "armel"]
  (Permissive, "aarch64") -> ["arm64"]
  _ -> []
