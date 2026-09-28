{-# LANGUAGE OverloadedStrings #-}
-- | Patches for known Hackage files. Cabal-syntax applies the same patches
-- before it parses a file. The data comes from
-- @Distribution.PackageDescription.Quirks@ in Cabal-syntax 3.18.1.0.
module Aihc.Cabal.Internal.Quirks (patchQuirks) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Map.Strict as Map
import Foreign.Ptr (castPtr)
import GHC.Fingerprint (Fingerprint (..), fingerprintData)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | A change to the file bytes. Each change applies to the first match only.
data Edit
  = Replace BS.ByteString BS.ByteString
  -- ^ Replace the first match. If there is no match, keep the bytes.
  | Remove BS.ByteString
  -- ^ Remove the first match.
  | RemoveFrom BS.ByteString
  -- ^ Remove the first match and all bytes after it.

-- | Patch the bytes of a known file. 'True' shows that the bytes changed.
-- A patch applies only if the MD5 hash of the input and the MD5 hash of the
-- result are the expected values.
patchQuirks :: BS.ByteString -> (Bool, BS.ByteString)
patchQuirks bytes = case Map.lookup (md5 bytes) patches of
  Just (expected, edits)
    | md5 output == expected -> (True, output)
    where output = foldl (flip edit) bytes edits
  _ -> (False, bytes)

edit :: Edit -> BS.ByteString -> BS.ByteString
edit change bytes = case change of
  Replace needle new
    | BS.null after -> bytes
    | otherwise -> BS.concat [before, new, BS.drop (BS.length needle) after]
    where (before, after) = BS.breakSubstring needle bytes
  Remove needle -> case BS.breakSubstring needle bytes of
    (before, after) -> before <> BS.drop (BS.length needle) after
  RemoveFrom needle -> fst (BS.breakSubstring needle bytes)

md5 :: BS.ByteString -> Fingerprint
md5 bytes = unsafeDupablePerformIO $ BSU.unsafeUseAsCStringLen bytes $ \(ptr, size) ->
  fingerprintData (castPtr ptr) size

entry :: Fingerprint -> Fingerprint -> [Edit] -> (Fingerprint, (Fingerprint, [Edit]))
entry input output edits = (input, (output, edits))

-- | Each entry has the input hash, the result hash, and the changes in order.
patches :: Map.Map Fingerprint (Fingerprint, [Edit])
patches = Map.fromList
  [ -- unicode-transforms 0.3.3
    entry (Fingerprint 15958160436627155571 10318709190730872881) (Fingerprint 11008465475756725834 13815629925116264363)
      [Remove "  other-modules:\n      .\n"]
  , -- DSTM 0.1.2
    entry (Fingerprint 6919263071548559054 9050746360708965827) (Fingerprint 17015177514298962556 11943164891661867280)
      [Replace "Other modules:" "-- "]
  , -- DSTM 0.1.1
    entry (Fingerprint 17313105789069667153 9610429408495338584) (Fingerprint 17250946493484671738 17629939328766863497)
      [Replace "Other modules:" "-- "]
  , -- DSTM 0.1
    entry (Fingerprint 10502599650530614586 16424112934471063115) (Fingerprint 13562014713536696107 17899511905611879358)
      [Replace "Other modules:" "-- "]
  , -- control-monad-exception-mtl 0.10.3
    entry (Fingerprint 18274748422558568404 4043538769550834851) (Fingerprint 11395257416101232635 4303318131190196308)
      [Replace " default- extensions:" "unknown-section"]
  , -- vacuum-opengl 0.0
    entry (Fingerprint 5946760521961682577 16933361639326309422) (Fingerprint 14034745101467101555 14024175957788447824)
      [Remove "\DEL"]
  , -- vacuum-opengl 0.0.1
    entry (Fingerprint 10790950110330119503 1309560249972452700) (Fingerprint 1565743557025952928 13645502325715033593)
      [Remove "\DEL"]
  , -- ixset 1.0.4
    entry (Fingerprint 11886092342440414185 4150518943472101551) (Fingerprint 5731367240051983879 17473925006273577821)
      [RemoveFrom "{-"]
  , -- ds-kanren 0.2.0.0
    entry (Fingerprint 2804006762382336875 9677726932108735838) (Fingerprint 9830506174094917897 12812107316777006473)
      [Replace "Test-Suite test-list-ops:" "Test-Suite \"test-list-ops:\"", Replace "Test-Suite test-unify:" "Test-Suite \"test-unify:\""]
  , -- ds-kanren 0.2.0.1
    entry (Fingerprint 9130259649220396193 2155671144384738932) (Fingerprint 1847988234352024240 4597789823227580457)
      [Replace "Test-Suite test-list-ops:" "Test-Suite \"test-list-ops:\"", Replace "Test-Suite test-unify:" "Test-Suite \"test-unify:\""]
  , -- metric 0.1.4
    entry (Fingerprint 6150019278861565482 3066802658031228162) (Fingerprint 9124826020564520548 15629704249829132420)
      [Replace "test-suite metric-tests:" "test-suite \"metric-tests:\""]
  , -- metric 0.2.0
    entry (Fingerprint 4639805967994715694 7859317050376284551) (Fingerprint 5566222290622325231 873197212916959151)
      [Replace "test-suite metric-tests:" "test-suite \"metric-tests:\""]
  , -- phasechange 0.1
    entry (Fingerprint 10546509771395401582 245508422312751943) (Fingerprint 5169853482576003304 7247091607933993833)
      [Replace "impl(ghc >= 7.6):" "erroneous-section", Replace "impl(ghc >= 7.4):" "erroneous-section"]
  , -- smartword 0.0.0.5
    entry (Fingerprint 7803544783533485151 10807347873998191750) (Fingerprint 1665635316718752601 16212378357991151549)
      [Replace "build depends:" "--"]
  , -- shelltestrunner 1.3
    entry (Fingerprint 4403237110790078829 15392625961066653722) (Fingerprint 10218887328390239431 4644205837817510221)
      [Replace "other modules:" "--"]
  , -- hblas 0.2.0.0
    entry (Fingerprint 8570120150072467041 18315524331351505945) (Fingerprint 10838007242302656005 16026440017674974175)
      [Replace "&&!" "&& !"]
  , -- hblas 0.3.0.0
    entry (Fingerprint 5262875856214215155 10846626274067555320) (Fingerprint 3022954285783401045 13395975869915955260)
      [Replace "&&!" "&& !"]
  , -- hblas 0.3.0.1
    entry (Fingerprint 54222628930951453 5526514916844166577) (Fingerprint 1749630806887010665 8607076506606977549)
      [Replace "&&!" "&& !"]
  , -- hblas 0.3.1.0
    entry (Fingerprint 6817250511240350300 15278852712000783849) (Fingerprint 15757717081429529536 15542551865099640223)
      [Replace "&&!" "&& !"]
  , -- hblas 0.3.1.1
    entry (Fingerprint 8310050400349211976 201317952074418615) (Fingerprint 10283381191257209624 4231947623042413334)
      [Replace "&&!" "&& !"]
  , -- hblas 0.3.2.1
    entry (Fingerprint 7010988292906098371 11591884496857936132) (Fingerprint 6158672440010710301 6419743768695725095)
      [Replace "&&!" "&& !"]
  , -- hblas 0.3.2.1, revision 1
    entry (Fingerprint 2076850805659055833 16615160726215879467) (Fingerprint 10634706281258477722 5285812379517916984)
      [Replace "&&!" "&& !"]
  , -- hblas 0.3.2.1, revision 2
    entry (Fingerprint 11850020631622781099 11956481969231030830) (Fingerprint 13702868780337762025 13383526367149067158)
      [Replace "&&!" "&& !"]
  , -- hblas 0.4.0.0
    entry (Fingerprint 13690322768477779172 19704059263540994) (Fingerprint 11189374824645442376 8363528115442591078)
      [Replace "&&!" "&& !"]
  , -- brainheck 0.1.0.2
    entry (Fingerprint 6910727116443152200 15401634478524888973) (Fingerprint 16551412117098094368 16260377389127603629)
      [Replace "flag(llvm-fast)" "False"]
  , -- brainheck 0.1.0.2, revision 1
    entry (Fingerprint 14320987921316832277 10031098243571536929) (Fingerprint 7959395602414037224 13279941216182213050)
      [Replace "flag(llvm-fast)" "False"]
  , -- brainheck 0.1.0.2, revision 2
    entry (Fingerprint 3809078390223299128 10796026010775813741) (Fingerprint 1127231189459220796 12088367524333209349)
      [Replace "flag(llvm-fast)" "False"]
  , -- brainheck 0.1.0.2, revision 3
    entry (Fingerprint 13860013038089410950 12479824176801390651) (Fingerprint 4687484721703340391 8013395164515771785)
      [Replace "flag(llvm-fast)" "False"]
  , -- wordchoice 0.1.0.1
    entry (Fingerprint 16215911397419608203 15594928482155652475) (Fingerprint 15120681510314491047 2666192399775157359)
      [Replace "flag(llvm-fast)" "False"]
  , -- wordchoice 0.1.0.1, revision 1
    entry (Fingerprint 16593139224723441188 4052919014346212001) (Fingerprint 3577381082410411593 11481899387780544641)
      [Replace "flag(llvm-fast)" "False"]
  , -- wordchoice 0.1.0.2
    entry (Fingerprint 9321301260802539374 1316392715016096607) (Fingerprint 3784628652257760949 12662640594755291035)
      [Replace "flag(llvm-fast)" "False"]
  , -- wordchoice 0.1.0.2, revision 1
    entry (Fingerprint 2546901804824433337 2059732715322561176) (Fingerprint 8082068680348326500 615008613291421947)
      [Replace "flag(llvm-fast)" "False"]
  , -- wordchoice 0.1.0.3
    entry (Fingerprint 2282380737467965407 12457554753171662424) (Fingerprint 17324757216926991616 17172911843227482125)
      [Replace "flag(llvm-fast)" "False"]
  , -- wordchoice 0.1.0.3, revision 1
    entry (Fingerprint 12907988890480595481 11078473638628359710) (Fingerprint 13246185333368731848 4663060731847518614)
      [Replace "flag(llvm-fast)" "False"]
  , -- hw-prim-bits 0.1.0.0
    entry (Fingerprint 12386777729082870356 17414156731912743711) (Fingerprint 3452290353395041602 14102887112483033720)
      [Replace "flag(sse42)" "False"]
  , -- hw-prim-bits 0.1.0.1
    entry (Fingerprint 6870520675313101180 14553457351296240636) (Fingerprint 12481021059537696455 14711088786769892762)
      [Replace "flag(sse42)" "False"]
  , -- Sit 0.2017.2.26
    entry (Fingerprint 8458530898096910998 3228538743646501413) (Fingerprint 14470502514907936793 17514354054641875371)
      [Replace "0.2017.02.26" "0.2017.2.26"]
  , -- Sit 0.2017.5.1
    entry (Fingerprint 1450130849535097473 11742099607098860444) (Fingerprint 16679762943850814021 4253724355613883542)
      [Replace "0.2017.05.01" "0.2017.5.1"]
  , -- Sit 0.2017.5.2
    entry (Fingerprint 297248532398492441 17322625167861324800) (Fingerprint 634812045126693280 1755581866539318862)
      [Replace "0.2017.05.02" "0.2017.5.2"]
  , -- Sit 0.2017.5.2, revision 1
    entry (Fingerprint 3697869560530373941 3942982281026987312) (Fingerprint 14344526114710295386 16386400353475114712)
      [Replace "0.2017.5.02" "0.2017.5.2"]
  , -- MiniAgda 0.2017.2.18
    entry (Fingerprint 17167128953451088679 4300350537748753465) (Fingerprint 12402236925293025673 7715084875284020606)
      [Replace "0.2017.02.18" "0.2017.2.18"]
  , -- fast-downward 0.1.0.0
    entry (Fingerprint 11256076039027887363 6867903407496243216) (Fingerprint 12159816716813155434 5278015399212299853)
      [Replace "1.2.03.0" "1.2.3.0"]
  , -- fast-downward 0.1.0.0, revision 1
    entry (Fingerprint 9216193973149680231 893446343655828508) (Fingerprint 10020169545407746427 1828336750379510675)
      [Replace "1.2.03.0" "1.2.3.0"]
  , -- fast-downward 0.1.0.1
    entry (Fingerprint 9899886602574848632 5980433644983783334) (Fingerprint 12007469255857289958 8321466548645225439)
      [Replace "1.2.03.0" "1.2.3.0"]
  , -- fast-downward 0.1.1.0
    entry (Fingerprint 12694656661460787751 1902242956706735615) (Fingerprint 15433152131513403849 2284712791516353264)
      [Replace "1.2.03.0" "1.2.3.0"]
  , -- SGplus 1.1
    entry (Fingerprint 17735649550442248029 11493772714725351354) (Fingerprint 9565458801063261772 15955773698774721052)
      [Replace "1000000000" "100000000"]
  , -- control-dotdotdot 0.1.0.1
    entry (Fingerprint 1514257173776509942 7756050823377346485) (Fingerprint 14082092642045505999 18415918653404121035)
      [Replace "9223372036854775807" "5"]
  , -- data-foldapp 0.1.1.0
    entry (Fingerprint 4511234156311243251 11701153011544112556) (Fingerprint 11820542702491924189 4902231447612406724)
      [Replace "9223372036854775807" "999", Replace "9223372036854775807" "999"]
  , -- data-list-zigzag 0.1.1.1
    entry (Fingerprint 12475837388692175691 18053834261188158945) (Fingerprint 16279938253437334942 15753349540193002309)
      [Replace "9223372036854775807" "999"]
  , -- nat 0.1
    entry (Fingerprint 9222512268705577108 13085311382746579495) (Fingerprint 17468921266614378430 13221316288008291892)
      [Replace "\xf6" "\xc3\xb6"]
  , -- streaming-bracketed 0.1.0.0
    entry (Fingerprint 14670044663153191927 1427497586294143829) (Fingerprint 9233007756654759985 6571998449003682006)
      [Replace "cabal-version:       2" "cabal-version: 2.0"]
  , -- streaming-bracketed 0.1.0.1
    entry (Fingerprint 7298738862909203815 10141693276062967842) (Fingerprint 1349949738792220441 3593683359695349293)
      [Replace "cabal-version:       2" "cabal-version: 2.0"]
  , -- zsyntax 0.2.0.0
    entry (Fingerprint 17812331267506881875 3005293725141563863) (Fingerprint 3445957263137759540 12472369104312474458)
      [Replace "cabal-version:  2" "cabal-version: 2.0"]
  , -- wai-middleware-hmac-client 0.1.0.1
    entry (Fingerprint 3112606538775065787 11984607507024462091) (Fingerprint 6916432989977230500 6621389616675138128)
      [Replace "\"\"" "."]
  , -- wai-middleware-hmac-client 0.1.0.2
    entry (Fingerprint 12566783342663020458 17562089389615949789) (Fingerprint 15745683452603944938 10556498036622072844)
      [Replace "\"\"" "."]
  , -- reheat 0.1.4
    entry (Fingerprint 9155400339287317061 14812953666990892802) (Fingerprint 7687053346032173923 15384472501136606592)
      [Replace "/home/palo/dev/haskell-workspace/playground/reheat/gpl-3.0.txt" ""]
  , -- reheat 0.1.5
    entry (Fingerprint 2984391146441073709 11728234882049907993) (Fingerprint 12058479081855347701 14017937756688869826)
      [Replace "/home/palo/dev/haskell-workspace/playground/reheat/gpl-3.0.txt" ""]
  ]
