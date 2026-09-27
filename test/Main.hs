{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Aihc.Cabal
import qualified Distribution.PackageDescription as C
import qualified Distribution.PackageDescription.Parsec as C
import qualified Distribution.Parsec as C
import qualified Distribution.Pretty as C
import qualified Distribution.Types.Version as C
import qualified Distribution.Types.VersionRange as C
import qualified Distribution.Utils.Path as C

assert :: (Eq a, Show a) => String -> a -> a -> IO ()
assert label expected actual = unless (expected == actual)
  (fail (label ++ "\nExpected: " ++ show expected ++ "\nActual: " ++ show actual))

right :: Show e => Either e a -> IO a
right = either (fail . show) pure

parse :: BSC.ByteString -> IO Package
parse = right . parseValue . parsePackage

version :: Text -> Version
version = either (error . T.unpack) id . parseVersion

environment :: Environment
environment = Environment "linux" "x86_64" (Map.singleton "ghc" (version "9.12.2"))

resolve :: FlagAssignment -> Package -> IO [Component BuildInfo]
resolve flags pkg = resolvedComponents <$> right (resolvePackage environment flags pkg)

header :: [String]
header = ["cabal-version: 3.0", "name: sample", "version: 1.2.3"]

fixture :: BSC.ByteString
fixture = BSC.unlines (map BSC.pack (header ++
  [ "flag fast", "  default: True", "  manual: False"
  , "common shared", "  hs-source-dirs: src", "  default-language: Haskell2010"
  , "  build-depends: base >=4.16 && <5"
  , "  cpp-options: -DROOT"
  , "  if flag(fast)", "    cpp-options: -DFAST"
  , "common native", "  import: shared", "  c-sources: cbits/a.c"
  , "  cxx-sources: cbits/b.cpp", "  include-dirs: include", "  install-includes: api.h"
  , "  autogen-includes: config.h", "  cc-options: -std=c11", "  cxx-options: -std=c++17"
  , "library", "  import: native", "  exposed-modules: Sample"
  , "  other-modules: Sample.Internal", "  autogen-modules: Paths_sample"
  , "  default-extensions: CPP, OverloadedStrings"
  , "  ghc-options: -F -pgmFtrhsx -optP-DPAIR=1,2"
  , "  x-aihc-lir-sources: lir/a.lir lir/b.lir"
  , "  build-depends: sample:internal, containers ^>=0.7"
  , "  build-tool-depends: alex:alex >=3.2"
  , "  if os(linux) && impl(ghc >=9.10) && flag(fast)"
  , "    exposed-modules: Sample.Fast", "    cpp-options: -DSELECTED", "    hs-source-dirs: src", "    other-modules: Sample.Internal", "    default-extensions: CPP"
  , "    if arch(x86_64)", "      other-modules: Sample.X86"
  , "  else", "    exposed-modules: Sample.Slow", "    buildable: False"
  , "library internal", "  exposed-modules: Internal", "  build-depends: base"
  , "executable sample-tool", "  main-is: Main.hs", "  build-depends: internal"
  , "test-suite tests", "  type: exitcode-stdio-1.0", "  main-is: Tests.hs"
  , "  build-depends: sample, base"
  , "benchmark bench", "  type: exitcode-stdio-1.0", "  main-is: Bench.hs"
  , "foreign-library native-lib", "  type: native-shared", "  build-tool-depends: hsc2hs:hsc2hs"
  ]))

main :: IO ()
main = do
  testVersions
  testPackage
  testConsumers
  testDefaults
  testLegacy
  testSourceRepositories
  testBuildInfo
  testErrors
  putStrLn "All parser checks passed"

testVersions :: IO ()
testVersions = do
  forM_ ["0", "0.0", "1", "1.2", "1.2.0", "1.2.3", "1.3", "2", "2.0", "4.18", "9.12.2"] $ \v ->
    assert "Version round trip" (Right (version v)) (parseVersion (renderVersion (version v)))
  forM_ ["-any", "-none", "==1.2", "==1.2.*", ">=1.2 && <2", "^>=1.2.3", "^>=1", "=={1.2,2.3}", "^>={1.2,2.3}", "<=1.2 || >2", "(>=1 && <2) || ==3"] $ \input -> do
    range <- right (parseVersionRange input)
    ref <- case input of
      "-any" -> pure C.anyVersion
      "-none" -> pure C.noVersion
      _ -> maybe (fail ("Reference range parse failed: " ++ T.unpack input)) pure (C.simpleParsec (T.unpack input) :: Maybe C.VersionRange)
    forM_ [[0], [1], [1,0], [1,2], [1,2,0], [1,2,3], [1,2,4], [1,3], [2], [2,0], [3], [4,18]] $ \ns -> do
      v <- maybe (fail "Invalid test version") pure (mkVersion (NE.fromList (map toInteger ns)))
      assert ("Range membership: " ++ T.unpack input ++ " " ++ show ns)
        (C.withinRange (C.mkVersion ns) ref) (withinRange v range)
    parsed <- right (parseVersionRange (renderVersionRange range))
    assert "Range round trip" range parsed
  assert "Version ordering" True (version "1.2" < version "1.2.0")
  assert "Negative version" Nothing (mkVersion ((-1) :| []))
  forM_ ["", "1.", "-1", "1..2", "1a", "1.2 trailing"] $ \input ->
    case parseVersion input of
      Left _ -> pure ()
      Right _ -> fail "Invalid version accepted"
  forM_ ["==", ">=1 trailing", "==1.*.*", "<1 &&", "1.2"] $ \input ->
    case parseVersionRange input of
      Left _ -> pure ()
      Right _ -> fail "Invalid range accepted"

testPackage :: IO ()
testPackage = do
  pkg <- parse fixture
  ref <- right (snd (C.runParseResult (C.parseGenericPackageDescription fixture)))
  assert "Package name" "sample" (packageName pkg)
  assert "Component count" 6 (length (packageComponents pkg))
  assert "Flag declarations" [Flag "fast" True False ""] (packageFlags pkg)
  forM_ [True, False] $ \fast -> do
    cs <- resolve (Map.singleton "fast" fast) pkg
    bi <- case cs of Component (Library Nothing) b:_ -> pure b; _ -> fail "Missing library"
    tree <- maybe (fail "Missing reference library") pure (C.condLibrary ref)
    let selected = collect fast tree
        cbi = mconcat (map C.libBuildInfo selected)
    assert "Buildable" (Just (C.buildable cbi)) (buildable bi)
    assert "Source directories" (map C.getSymbolicPath (C.hsSourceDirs cbi)) (sourceDirs bi)
    assert "Exposed modules" (map (T.pack . C.prettyShow) (concatMap C.exposedModules selected)) (exposedModules bi)
    assert "Other modules" (map (T.pack . C.prettyShow) (C.otherModules cbi)) (otherModules bi)
    assert "Generated modules" (map (T.pack . C.prettyShow) (C.autogenModules cbi)) (autogenModules bi)
    assert "Language" (T.pack . C.prettyShow <$> C.defaultLanguage cbi) (defaultLanguage bi)
    assert "Extensions" (map (T.pack . C.prettyShow) (C.defaultExtensions cbi)) (extensions bi)
    assert "CPP options" (map T.pack (C.cppOptions cbi)) (cppOptions bi)
    assert "C sources" (C.cSources cbi) (cSources bi)
    assert "C++ sources" (C.cxxSources cbi) (cxxSources bi)
    assert "C options" (map T.pack (C.ccOptions cbi)) (ccOptions bi)
    assert "C++ options" (map T.pack (C.cxxOptions cbi)) (cxxOptions bi)
    assert "Include directories" (C.includeDirs cbi) (includeDirs bi)
    assert "Public headers" (C.installIncludes cbi) (installIncludes bi)
    assert "Generated headers" (C.autogenIncludes cbi) (autogenIncludes bi)
    assert "Option commas" ["-F", "-pgmFtrhsx", "-optP-DPAIR=1,2"] (ghcOptions bi)
    assert "Custom fields" (Just ["lir/a.lir lir/b.lir"]) (Map.lookup "x-aihc-lir-sources" (extraFields bi))
    assert "Dependency names" ["base", "sample", "containers"] (map dependencyPackage (dependencies bi))
    assert "Named dependency" (NamedLibrary "internal" :| []) (dependencyLibraries (dependencies bi !! 1))
    assert "Build tools" ["alex"] (map toolName (buildTools bi))
    assert "Tool packages" [Just "alex"] (map toolPackage (buildTools bi))
    let exe = componentData (cs !! 2)
    assert "Legacy internal dependency" ["sample"] (map dependencyPackage (dependencies exe))
    assert "Legacy internal target" [NamedLibrary "internal" :| []] (map dependencyLibraries (dependencies exe))
  where
    collect fast (C.CondNode a _ bs) = a : concatMap branch bs
      where
        branch (C.CondBranch c t e) = if eval c then collect fast t else maybe [] (collect fast) e
        eval c = case c of
          C.Lit b -> b
          C.Var (C.PackageFlag _) -> fast
          C.Var (C.OS os) -> C.prettyShow os == "linux"
          C.Var (C.Arch arch) -> C.prettyShow arch == "x86_64"
          C.Var (C.Impl compiler range) -> C.prettyShow compiler == "ghc" && C.withinRange (C.mkVersion [9,12,2]) range
          C.CNot a' -> not (eval a')
          C.CAnd a' b -> eval a' && eval b
          C.COr a' b -> eval a' || eval b

testConsumers :: IO ()
testConsumers = forM_ ["aihc-hackage", "aihc-package-plan", "aihc-haddock"] $ \pkgName -> do
  input <- BSC.readFile ("test/fixtures/" ++ pkgName ++ ".cabal")
  pkg <- parse input
  ref <- right (snd (C.runParseResult (C.parseGenericPackageDescription input)))
  components <- resolve Map.empty pkg
  assert "Consumer package name" (T.pack pkgName) (packageName pkg)
  bi <- case components of
    Component (Library Nothing) b:_ -> pure b
    _ -> fail "Missing consumer library"
  library <- maybe (fail "Missing reference library") (pure . C.condTreeData) (C.condLibrary ref)
  let cbi = C.libBuildInfo library
  assert "Consumer modules" (map (T.pack . C.prettyShow) (C.exposedModules library)) (exposedModules bi)
  assert "Consumer source directories" (map C.getSymbolicPath (C.hsSourceDirs cbi)) (sourceDirs bi)
  assert "Consumer language" (T.pack . C.prettyShow <$> C.defaultLanguage cbi) (defaultLanguage bi)
  assert "Consumer dependency count" (length (C.targetBuildDepends cbi)) (length (dependencies bi))

testDefaults :: IO ()
testDefaults = do
  pkg <- parse (BSC.unlines (map BSC.pack (header ++
    ["flag chosen", "  default: False", "library", "  if !flag(chosen)", "    hs-source-dirs: generated", "    default-language: Haskell2010", "  else", "    buildable: False"])))
  [Component _ bi] <- resolve Map.empty pkg
  assert "Branch directory excludes default" ["generated"] (sourceDirs bi)
  assert "Branch language" (Just "Haskell2010") (defaultLanguage bi)
  assert "Buildable default" (Just True) (buildable bi)
  [Component _ other] <- resolve (Map.singleton "chosen" True) pkg
  assert "Default directory" ["."] (sourceDirs other)
  assert "Absent language stays absent" Nothing (defaultLanguage other)
  assert "Buildable branch" (Just False) (buildable other)
  case resolvePackage environment (Map.singleton "missing" True) pkg of
    Left _ -> pure ()
    Right _ -> fail "Unknown override accepted"
  quoted <- parse (BSC.unlines (map BSC.pack (header ++ ["library", "  hs-source-dirs: \"source files\"", "  cpp-options: \"-DNAME=hello world\"", "  buildable: False", "  if True", "    buildable: True"])))
  [Component _ q] <- resolve Map.empty quoted
  assert "Quoted path" ["source files"] (sourceDirs q)
  assert "Quoted option" ["-DNAME=hello world"] (cppOptions q)
  assert "Buildable conjunction" (Just False) (buildable q)

testLegacy :: IO ()
testLegacy = do
  let input = "cabal-version: >=1.10\nname: legacy\nversion: 1\nlibrary\n  extensions: CPP\n  build-tools: happy >=1.20\n"
  pkg <- parse input
  _ <- right (snd (C.runParseResult (C.parseGenericPackageDescription input)))
  [Component _ bi] <- resolve Map.empty pkg
  assert "Legacy extensions" ["CPP"] (extensions bi)
  assert "Legacy tool name" ["happy"] (map toolName (buildTools bi))
  assert "Legacy tool package" [Nothing] (map toolPackage (buildTools bi))
  let setInput = BSC.unlines (map BSC.pack (header ++
        ["library", "  build-depends: , base:{base} >=4, sample:{one,two} ^>={1.2,2.3}"]))
  sets <- parse setInput
  _ <- right (snd (C.runParseResult (C.parseGenericPackageDescription setInput)))
  [Component _ setInfo] <- resolve Map.empty sets
  assert "Main library target" (MainLibrary :| []) (dependencyLibraries (dependencies setInfo !! 0))
  assert "Library target set" (NamedLibrary "one" :| [NamedLibrary "two"])
    (dependencyLibraries (dependencies setInfo !! 1))

testSourceRepositories :: IO ()
testSourceRepositories = do
  let input = BSC.unlines (map BSC.pack (header ++
        [ "source-repository head", "  type: git"
        , "  location: https://example.com/first", "  location: https://example.com/second"
        , "  x-note: first", "    second"
        , "library", "  exposed-modules: Sample"
        , "source-repository this", "  type: git", "  tag: v1.2.3"
        , "  subdir: \"source files\""
        , "source-repository head", "  type: darcs", "  location: https://example.com/third"
        ]))
  pkg <- parse input
  assert "Keep repository sections in source order"
    [ SourceRepository "head" (Map.fromList
        [ ("type", ["git"])
        , ("location", ["https://example.com/first", "https://example.com/second"])
        , ("x-note", ["first\nsecond"])
        ])
    , SourceRepository "this" (Map.fromList
        [("type", ["git"]), ("tag", ["v1.2.3"]), ("subdir", ["\"source files\""])])
    , SourceRepository "head" (Map.fromList
        [("type", ["darcs"]), ("location", ["https://example.com/third"])])
    ] (packageSourceRepositories pkg)
  empty <- parse (BSC.unlines (map BSC.pack header))
  assert "Absent repositories" [] (packageSourceRepositories empty)

testBuildInfo :: IO ()
testBuildInfo = do
  let input = "cc-options: -DHOOKED\ncpp-options: -DHOOKED_HS\ninclude-dirs: generated\nc-sources: generated.c\nexecutable: sample-tool\ncpp-options: -DEXE\n"
  ours <- right (parseValue (parseBuildInfo input))
  (lib, exes) <- right (snd (C.runParseResult (C.parseHookedBuildInfo input)))
  assert "Buildinfo C options" (map T.pack . C.ccOptions <$> lib) (ccOptions <$> libraryBuildInfo ours)
  assert "Buildinfo executable count" (length exes) (Map.size (executableBuildInfo ours))
  assert "Buildinfo executable options" (Just ["-DEXE"]) (cppOptions <$> Map.lookup "sample-tool" (executableBuildInfo ours))
  empty <- right (parseValue (parseBuildInfo ""))
  assert "Empty buildinfo" (BuildInfoFile Nothing Map.empty) empty

testErrors :: IO ()
testErrors = do
  forM_
    [ ["library", "    buildable: True", "  other-modules: Lost"]
    , ["library", "  build-tools: happy"]
    , ["library", "  signatures: Hole"]
    , ["library", "  mixins: base hiding (Prelude)"]
    , ["library", "  reexported-modules: Other"]
    , ["library", "  if flag(missing)", "    buildable: False"]
    , ["library", "  import: missing"]
    , ["library", "  buildable: maybe"]
    , ["library", "  build-depends: base >="]
    , ["library", "  exposed-modules: lower"]
    , ["library { exposed-modules: Sample }"]
    , ["library", "\tbuildable: True"]
    , ["library", "  else", "    buildable: False"]
    , ["common a", "  import: a", "library", "  import: a"]
    , ["library", "  other-modules: Sample", "  import: a"]
    , ["library", "  buildable: True", "library", "  buildable: True"]
    , ["source-repository head", "  if True", "    type: git"]
    ] $ \body -> reject (BSC.unlines (map BSC.pack (header ++ body)))
  reject "name: sample\nversion: 1\n"
  reject "cabal-version: 99\nname: sample\nversion: 1\n"
  reject (BS.pack [255,254])
  let bad = parsePackage (BSC.unlines (map BSC.pack (header ++ ["library", "  buildable: invalid"])))
  case parseValue bad of
    Left (d :| _) -> assert "Error source line" 5 (diagnosticLine d)
    Right _ -> fail "Invalid input accepted"
  where
    reject bytes = case parseValue (parsePackage bytes) of
      Left _ -> pure ()
      Right _ -> fail ("Invalid input accepted: " ++ BSC.unpack bytes)
