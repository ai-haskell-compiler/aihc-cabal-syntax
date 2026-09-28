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
import qualified Data.Text.Encoding as TE
import Aihc.Cabal
import qualified Distribution.Fields.ParseResult as C
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

-- | Run a Cabal-syntax parser without a source name. The results keep the
-- error and warning format of Cabal-syntax 3.12.
runResult :: C.ParseResult () a
  -> ([C.PWarning], Either (Maybe C.Version, NonEmpty C.PError) a)
runResult result = case C.runParseResult result of
  (warnings, outcome) -> (map C.pwarning warnings, either (Left . fmap (fmap C.perror)) Right outcome)

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
  testEmptySections
  testLegacy
  testSourceRepositories
  testBuildInfo
  testConditionalSpelling
  testElif
  testImportCommas
  testInternalLibraryNames
  testRetainedFields
  testOlderToolDependencies
  testRepeatedPackageFields
  testNewFormatVersions
  testErrors
  putStrLn "All parser checks passed"

testVersions :: IO ()
testVersions = do
  forM_ ["0", "0.0", "1", "1.2", "1.2.0", "1.2.3", "1.3", "2", "2.0", "4.18", "9.12.2"] $ \v ->
    assert "Version round trip" (Right (version v)) (parseVersion (renderVersion (version v)))
  forM_ ["-any", "-none", "==1.2", "==1.2.*", ">=1.2 && <2", "^>=1.2.3", "^>=1", "^>=0.0.3", ">=1 && <3 && >1.2", "==1 || ==2 || ==3", "=={1.2,2.3}", "^>={1.2,2.3}", "<=1.2 || >2", "(>=1 && <2) || ==3"] $ \input -> do
    range <- right (parseVersionRange input)
    ref <- case input of
      "-any" -> pure C.anyVersion
      "-none" -> pure C.noVersion
      _ -> maybe (fail ("Reference range parse failed: " ++ T.unpack input)) pure (C.simpleParsec (T.unpack input) :: Maybe C.VersionRange)
    forM_ [[0], [0,0,3], [0,1], [1], [1,0], [1,1], [1,2], [1,2,0], [1,2,3], [1,2,4], [1,3], [2], [2,0], [3], [4,18]] $ \ns -> do
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
  ref <- right (snd (runResult (C.parseGenericPackageDescription fixture)))
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
    assert "C sources" (map C.getSymbolicPath (C.cSources cbi)) (cSources bi)
    assert "C++ sources" (map C.getSymbolicPath (C.cxxSources cbi)) (cxxSources bi)
    assert "C options" (map T.pack (C.ccOptions cbi)) (ccOptions bi)
    assert "C++ options" (map T.pack (C.cxxOptions cbi)) (cxxOptions bi)
    assert "Include directories" (map C.getSymbolicPath (C.includeDirs cbi)) (includeDirs bi)
    assert "Public headers" (map C.getSymbolicPath (C.installIncludes cbi)) (installIncludes bi)
    assert "Generated headers" (map C.getSymbolicPath (C.autogenIncludes cbi)) (autogenIncludes bi)
    assert "Option commas" ["-F", "-pgmFtrhsx", "-optP-DPAIR=1,2"] (ghcOptions bi)
    assert "Custom fields" (Just ["lir/a.lir lir/b.lir"]) (map fieldText <$> Map.lookup "x-aihc-lir-sources" (extraFields bi))
    assert "Dependency names" ["base", "sample", "containers"] (map dependencyPackage (dependencies bi))
    assert "Named dependency" (NamedLibrary "internal" :| []) (dependencyLibraries (dependencies bi !! 1))
    assert "Build tools" ["alex"] (map toolName (buildTools bi))
    assert "Tool packages" [Just "alex"] (map toolPackage (buildTools bi))
    let exe = componentData (cs !! 2)
    assert "Legacy internal dependency" ["sample"] (map dependencyPackage (dependencies exe))
    assert "Legacy internal target" [NamedLibrary "internal" :| []] (map dependencyLibraries (dependencies exe))
  where
    collect fast (C.CondNode a bs) = a : concatMap branch bs
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
  ref <- right (snd (runResult (C.parseGenericPackageDescription input)))
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

testEmptySections :: IO ()
testEmptySections = do
  pkg <- parse "cabal-version: 3.0\nname: sample\nversion: 1\nflag fast\ncommon shared\nlibrary\n  import: shared\n  if flag(fast)\n  else\n    cpp-options: -DSLOW\n  ghc-options: -Wall\nexecutable tool\n"
  assert "Empty flag defaults" [Flag "fast" True False ""] (packageFlags pkg)
  assert "Keep empty sections and branch boundaries"
    [ Component (Library Nothing) (Conditional
        (emptyBuildInfo { ghcOptions = ["-Wall"] })
        [Branch (FlagValue "fast") (Conditional emptyBuildInfo [])
          (Just (Conditional (emptyBuildInfo { cppOptions = ["-DSLOW"] }) []))])
    , Component (Executable "tool") (Conditional emptyBuildInfo [])
    ] (packageComponents pkg)
  forM_ [True, False] $ \fast -> do
    components <- resolve (Map.singleton "fast" fast) pkg
    info <- case components of
      Component (Library Nothing) bi : _ -> pure bi
      _ -> fail "Missing library"
    assert "Select an empty branch" (if fast then [] else ["-DSLOW"]) (cppOptions info)
    assert "Keep fields after an empty branch" ["-Wall"] (ghcOptions info)

testLegacy :: IO ()
testLegacy = do
  forM_ ["1.0", "1.2", "1.4", "1.6", "1.8"] $ \spec -> do
    older <- parse ("cabal-version: >=" <> TE.encodeUtf8 spec
      <> "\nname: sample\nversion: 1\nlibrary\n  exposed-modules: Sample\n")
    assert "Keep the older format version" (version spec) (cabalVersion older)
    assert "Keep the older library modules" [["Sample"]]
      (map (exposedModules . unconditional . componentData) (packageComponents older))
  let input = "cabal-version: >=1.10\nname: legacy\nversion: 1\nlibrary\n  extensions: CPP\n  build-tools: happy >=1.20\n"
  pkg <- parse input
  _ <- right (snd (runResult (C.parseGenericPackageDescription input)))
  [Component _ bi] <- resolve Map.empty pkg
  assert "Legacy extensions" ["CPP"] (legacyExtensions bi)
  assert "Default extension field" [] (extensions bi)
  let extensionInput = "cabal-version: 2.2\nname: sample\nversion: 1\ncommon shared\n  extensions: CPP\n  default-extensions: OverloadedStrings\nlibrary\n  import: shared\n  if os(linux)\n    extensions: ForeignFunctionInterface\n    default-extensions: BangPatterns\n"
  extensionPackage <- parse extensionInput
  [Component _ extensionInfo] <- resolve Map.empty extensionPackage
  assert "Merge older extensions" ["CPP", "ForeignFunctionInterface"] (legacyExtensions extensionInfo)
  assert "Merge default extensions" ["OverloadedStrings", "BangPatterns"] (extensions extensionInfo)
  assert "Legacy tool name" ["happy"] (map toolName (buildTools bi))
  assert "Legacy tool package" [Nothing] (map toolPackage (buildTools bi))
  let setInput = BSC.unlines (map BSC.pack (header ++
        ["library", "  build-depends: , base:{base} >=4, sample:{one,two} ^>={1.2,2.3}"]))
  sets <- parse setInput
  _ <- right (snd (runResult (C.parseGenericPackageDescription setInput)))
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
    [ ("head", Map.fromList
        [ ("type", ["git"])
        , ("location", ["https://example.com/first", "https://example.com/second"])
        , ("x-note", ["first\nsecond"])
        ])
    , ("this", Map.fromList
        [("type", ["git"]), ("tag", ["v1.2.3"]), ("subdir", ["\"source files\""])])
    , ("head", Map.fromList
        [("type", ["darcs"]), ("location", ["https://example.com/third"])])
    ] [(k, Map.map (map fieldText) fs) | SourceRepository k fs <- packageSourceRepositories pkg]
  assert "Keep field positions"
    [Just [FieldValue (Position 8 3) [FieldLine (Position 8 11) "first", FieldLine (Position 9 5) "second"]]]
    (take 1 [Map.lookup "x-note" fs | SourceRepository _ fs <- packageSourceRepositories pkg])
  empty <- parse (BSC.unlines (map BSC.pack header))
  assert "Absent repositories" [] (packageSourceRepositories empty)

testBuildInfo :: IO ()
testBuildInfo = do
  let input = "cc-options: -DHOOKED\ncpp-options: -DHOOKED_HS\ninclude-dirs: generated\nc-sources: generated.c\nexecutable: sample-tool\ncpp-options: -DEXE\n"
  ours <- right (parseValue (parseBuildInfo input))
  (lib, exes) <- right (snd (runResult (C.parseHookedBuildInfo input)))
  assert "Buildinfo C options" (map T.pack . C.ccOptions <$> lib) (ccOptions <$> libraryBuildInfo ours)
  assert "Buildinfo executable count" (length exes) (Map.size (executableBuildInfo ours))
  assert "Buildinfo executable options" (Just ["-DEXE"]) (cppOptions <$> Map.lookup "sample-tool" (executableBuildInfo ours))
  empty <- right (parseValue (parseBuildInfo ""))
  assert "Empty buildinfo" (BuildInfoFile Nothing Map.empty) empty

-- | Cabal does not make a difference between upper case and lower case
-- in section keywords. A parenthesis can follow the keyword directly.
testConditionalSpelling :: IO ()
testConditionalSpelling = do
  pkg <- parse (BSC.unlines (map BSC.pack (header ++
    [ "flag fast", "  default: False", "library"
    , "  If flag(fast)", "    cpp-options: -DFAST"
    , "  Else", "    cpp-options: -DSLOW"
    , "  if(os(linux))", "    cpp-options: -DLINUX"
    ])))
  [Component _ bi] <- resolve Map.empty pkg
  assert "Keyword spelling" ["-DSLOW", "-DLINUX"] (cppOptions bi)
  [Component _ fast] <- resolve (Map.singleton "fast" True) pkg
  assert "Keyword spelling with a flag" ["-DFAST", "-DLINUX"] (cppOptions fast)

testElif :: IO ()
testElif = do
  let input = BSC.unlines (map BSC.pack (header ++
        [ "library"
        , "  if arch(wasm32)", "    hs-source-dirs: wasm"
        , "  elif os(osx)", "    hs-source-dirs: darwin"
        , "  elif os(linux)", "    hs-source-dirs: linux"
        , "  else", "    hs-source-dirs: other"
        ]))
  pkg <- parse input
  _ <- right (snd (runResult (C.parseGenericPackageDescription input)))
  [Component _ bi] <- resolve Map.empty pkg
  assert "Select an elif branch" ["linux"] (sourceDirs bi)
  let at os arch = do
        resolved <- right (resolvePackage (Environment os arch Map.empty) Map.empty pkg)
        pure [sourceDirs b | Component _ b <- resolvedComponents resolved]
  darwin <- at "osx" "aarch64"
  assert "Select the first elif branch" [["darwin"]] darwin
  wasm <- at "wasi" "wasm32"
  assert "Select the if branch" [["wasm"]] wasm
  other <- at "windows" "x86_64"
  assert "Select the else branch" [["other"]] other
  -- Before cabal-version 2.2, Cabal ignores elif and gives a warning. It also
  -- ignores the else section after it, because no if section comes before it.
  older <- parse "cabal-version: 2.0\nname: sample\nversion: 1\nbuild-type: Simple\nlibrary\n  if os(osx)\n    cpp-options: -DOSX\n  elif os(linux)\n    cpp-options: -DLINUX\n  else\n    cpp-options: -DOTHER\n"
  [Component _ olderInfo] <- resolve Map.empty older
  assert "Ignore elif before 2.2" [] (cppOptions olderInfo)

testImportCommas :: IO ()
testImportCommas = do
  let input = BSC.unlines (map BSC.pack (header ++
        [ "common one", "  cpp-options: -DONE", "common two", "  cpp-options: -DTWO"
        , "library", "  import:", "    , one", "    , two"
        ]))
  pkg <- parse input
  _ <- right (snd (runResult (C.parseGenericPackageDescription input)))
  [Component _ bi] <- resolve Map.empty pkg
  assert "Import list with leading commas" ["-DONE", "-DTWO"] (cppOptions bi)

-- | Before cabal-version 3.4, an internal library name hides a package
-- with the same name. From 3.4, a dependency name always identifies a package.
testInternalLibraryNames :: IO ()
testInternalLibraryNames = forM_ [("3.0", ("sample", NamedLibrary "mtl")), ("3.4", ("mtl", MainLibrary))] $ \(spec, expected) -> do
  let input = BSC.unlines (map BSC.pack
        [ "cabal-version: " ++ spec, "name: sample", "version: 1"
        , "library", "  build-depends: mtl", "library mtl", "  build-depends: base"
        ])
  pkg <- parse input
  ref <- right (snd (runResult (C.parseGenericPackageDescription input)))
  library <- maybe (fail "Missing reference library") (pure . C.libBuildInfo . C.condTreeData) (C.condLibrary ref)
  (Component _ bi : _) <- resolve Map.empty pkg
  assert ("Dependency name for " ++ spec) [expected]
    [(dependencyPackage d, NE.head (dependencyLibraries d)) | d <- dependencies bi]
  assert ("Reference dependency name for " ++ spec) [fst expected]
    [T.pack (C.prettyShow (C.depPkgName d)) | d <- C.targetBuildDepends library]

-- | The parser keeps fields that it does not interpret.
testRetainedFields :: IO ()
testRetainedFields = do
  pkg <- parse (BSC.unlines (map BSC.pack (header ++
    [ "library", "  reexported-modules: Data.Other, Data.Alias as Alias"
    , "  mixins: base hiding (Prelude), containers (A as B) requires (C)", "  signatures: Hole"
    ])))
  [Component _ bi] <- resolve Map.empty pkg
  assert "Module reexports" (Just ["Data.Other, Data.Alias as Alias"]) (map fieldText <$> Map.lookup "reexported-modules" (extraFields bi))
  assert "Mixins"
    [ Mixin "base" MainLibrary (HidingRenaming ["Prelude"]) DefaultRenaming
    , Mixin "containers" MainLibrary (ModuleRenaming [("A", "B")]) (ModuleRenaming [("C", "C")]) ]
    (mixins bi)
  assert "Signatures" (Just ["Hole"]) (map fieldText <$> Map.lookup "signatures" (extraFields bi))

-- | Before cabal-version 2.0, Cabal keeps build-tool-depends and gives a warning.
testOlderToolDependencies :: IO ()
testOlderToolDependencies = do
  let input = "cabal-version: >=1.10\nname: sample\nversion: 1\nlibrary\n  build-tool-depends: hspec-discover:hspec-discover\n  build-tools: happy\n"
  pkg <- parse input
  ref <- right (snd (runResult (C.parseGenericPackageDescription input)))
  library <- maybe (fail "Missing reference library") (pure . C.libBuildInfo . C.condTreeData) (C.condLibrary ref)
  [Component _ bi] <- resolve Map.empty pkg
  assert "Reference keeps the field" 1 (length (C.buildToolDepends library))
  assert "Keep build-tool-depends after build-tools" [Nothing, Just "hspec-discover"] (map toolPackage (buildTools bi))

-- | Cabal accepts repeated package fields with a warning. For a field with
-- one value, the last value wins.
testRepeatedPackageFields :: IO ()
testRepeatedPackageFields = do
  pkg <- parse (BSC.unlines (map BSC.pack (header ++
    ["extra-source-files: a.txt", "tested-with: GHC == 9.10", "extra-source-files: b.txt"])))
  assert "Keep each value in source order" (Just ["a.txt", "b.txt"]) (map fieldText <$> Map.lookup "extra-source-files" (packageFields pkg))
  renamed <- parse (BSC.unlines (map BSC.pack (header ++ ["name: other", "version: 2"])))
  assert "Last name wins" "other" (packageName renamed)
  assert "Last version wins" (version "2") (packageVersion renamed)
  forM_ ["cabal-version: 2.2", "version: 1..2"] $ \field ->
    case parseValue (parsePackage (BSC.unlines (map BSC.pack (header ++ [field])))) of
      Left _ -> pure ()
      Right _ -> fail ("Invalid repeated field accepted: " ++ field)

-- | Format versions 3.16 and 3.18, the build type rules of Cabal-syntax
-- 3.18, and absolute source directories.
testNewFormatVersions :: IO ()
testNewFormatVersions = do
  forM_
    [ ("3.16", "build-type: Simple", True)
    , ("3.18", "build-type: Simple", True)
    , ("3.16", "build-type: Make", True)
    , ("3.18", "build-type: Make", False)
    , ("3.12", "build-type: Hooks\ncustom-setup\n  setup-depends: base", False)
    , ("3.14", "build-type: Hooks\ncustom-setup\n  setup-depends: base", True)
    , ("3.18", "build-type: Hooks\ncustom-setup\n  setup-depends: base", True)
    , ("3.14", "build-type: Hooks", False)
    , ("3.18", "library\n  hs-source-dirs: /absolute", True)
    ] $ \(spec, body, accepted) -> do
      let input = BSC.pack ("cabal-version: " ++ spec ++ "\nname: sample\nversion: 1\n" ++ body ++ "\n")
          ours = either (const False) (const True) (parseValue (parsePackage input))
          reference = either (const False) (const True) (snd (runResult (C.parseGenericPackageDescription input)))
      assert ("Reference acceptance: " ++ spec ++ " " ++ body) accepted reference
      assert ("Acceptance: " ++ spec ++ " " ++ body) accepted ours
  pkg <- parse "cabal-version: 3.18\nname: sample\nversion: 1\n"
  assert "Newest format version" (version "3.18") (cabalVersion pkg)

testErrors :: IO ()
testErrors = do
  forM_
    [ ["library", "  build-tools: happy"]
    , ["library", "  if flag(missing)", "    buildable: False"]
    , ["library", "  import: missing"]
    , ["library", "  buildable: maybe"]
    , ["library", "  build-depends: base >="]
    , ["library", "  exposed-modules: lower"]
    , ["library { exposed-modules: Sample"]
    , ["common a", "  import: a", "library", "  import: a"]
    , ["library", "  buildable: True", "library", "  buildable: True"]
    , ["source-repository head", "  if True", "    type: git"]
    , ["library", "  if"]
    , ["library", "  if flag(missing)"]
    , ["library", "  if os(linux) &&", "    buildable: False"]
    , ["flag bad name"]
    , ["library", "  build-depends: base:"]
    ] $ \body -> reject (BSC.unlines (map BSC.pack (header ++ body)))
  reject "version: 1\n"
  reject "cabal-version: 99\nname: sample\nversion: 1\n"
  reject "cabal-version: 3.10\nname: sample\nversion: 1\n"
  reject "name: sample\nversion: 1\ncabal-version: 2.2\n"
  reject (BS.pack [255,254])
  let bad = parsePackage (BSC.unlines (map BSC.pack (header ++ ["library", "  buildable: invalid"])))
  case parseValue bad of
    Left (d :| _) -> assert "Error source line" 5 (diagnosticLine d)
    Right _ -> fail "Invalid input accepted"
  where
    reject bytes = case parseValue (parsePackage bytes) of
      Left _ -> pure ()
      Right _ -> fail ("Invalid input accepted: " ++ BSC.unpack bytes)
