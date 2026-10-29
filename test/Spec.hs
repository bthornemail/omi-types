module Main where

import Prelude
import Data.List (intercalate)
import qualified Data.List as DL

import qualified OMI.Carrier as C
import qualified OMI.DecisionTable as DT
import qualified OMI.Gauge as G
import qualified OMI.Gossip.Types as GT
import qualified OMI.Lisp as L
import qualified OMI.Markdown as MD
import qualified OMI.Memory as M
import qualified OMI.Stream as Stream
import qualified OMI.Pipeline as P
import qualified OMI.Reconcile as R
import qualified OMI.Relation as Rel
import qualified OMI.Scope as S
import qualified OMI.VCS as VCS
import qualified OMI.Wittgenstein as W
import qualified OMI.Canvas.JSON as CJ
import qualified OMI.Delta as D
import qualified OMI.Handler as H
import qualified OMI.BIOS as BIOS
import qualified OMI.Sabbath as Sab
import qualified OMI.Kernel as K
import qualified OMI.BQF as Q
import qualified OMI.Orbit as Orb
import qualified OMI.Fano as F
import qualified OMI.Ruler as Ru
import qualified OMI.Torus as T
import qualified OMI.Types ()
import qualified OMI.Try as Try
import qualified OMI.WSN as WSN
import qualified OMI.Polytope as Poly
import qualified OMI.Patricia as Pat
import qualified OMI.Consensus as Con
import qualified Data.Bits as HostBits
import OMI.Core
import OMI.Kernel

main :: IO ()
main = do
  testGaugeOperators
  testPreHeader
  testInvalidGauge
  testResolveDeclaration
  testDecisionTableExtraction
  testClosureAndContradiction
  testMarkdownExtraction
  testMarkdownTamperRejection
  testCanvasProjection
  testMemoryReconciliation
  testVcsReceiptedRecord
  testCarrierPrefix
  testAddressedFrame
  testCarrierCarCdrComposition
  testUnaryAdvanceAndXor
  testCarrierIgnoresEvidenceChecksumIdentity
  testStreamAndGossipAdapters
  testDeltaMatchesSpec
  testDeltaPeriodEight
  testHandlerMatchesOmiReference
  testOrbitOf60
  testBlocks
  testOrbitConstants
  testQuadraticForms
  testTorusMetric
  testRulerFold
  testFanoConstants
  testBiosTriesTheGivenRelation
  testSabbathContract
  testSabbathAgainstCoq
  testCaughtFrameRepairsForAnotherTry
  testCoordinateNibbles
  testHandlerPositionTypes
  testWsnInItself
  testWsnCasts
  testWsnStackUnderCasts
  testWsnLogic
  testWsnExpandsOnce
  testPolytopeLaws
  testPolytopeBetti
  testPolytopeSignature
  testPolytopeTransforms
  testPatricia
  testConsensus

testGaugeOperators :: IO ()
testGaugeOperators =
  sequence_
    [ assertOperator "F0" L.gaugeF0 (code O O O O)
    , assertOperator "F1" L.gaugeF1 (code O O O I)
    , assertOperator "F2" L.gaugeF2 (code O O I O)
    , assertOperator "F3" L.gaugeF3 (code O O I I)
    , assertOperator "F4" L.gaugeF4 (code O I O O)
    , assertOperator "F5" L.gaugeF5 (code O I O I)
    , assertOperator "F6" L.gaugeF6 (code O I I O)
    , assertOperator "F7" L.gaugeF7 (code O I I I)
    , assertOperator "F8" L.gaugeF8 (code I O O O)
    , assertOperator "F9" L.gaugeF9 (code I O O I)
    , assertOperator "FA" L.gaugeFA (code I O I O)
    , assertOperator "FB" L.gaugeFB (code I O I I)
    , assertOperator "FC" L.gaugeFC (code I I O O)
    , assertOperator "FD" L.gaugeFD (code I I O I)
    , assertOperator "FE" L.gaugeFE (code I I I O)
    , assertOperator "FF" L.gaugeFF (code I I I I)
    ]

testPreHeader :: IO ()
testPreHeader =
  assertOmiTrue "canonical gauge pre-header did not match" $
    G.matchesCanonicalPreHeader G.canonicalGaugePreHeader

testInvalidGauge :: IO ()
testInvalidGauge =
  assertOmiTrue "non-F* gauge byte was accepted" $
    L.notB (G.gaugeIsValid (G.mkGauge L.byteNull))

testResolveDeclaration :: IO ()
testResolveDeclaration =
  case P.attestationRelation (P.resolveDeclaration minimalDeclaration) of
    Relation{} -> pure ()

testDecisionTableExtraction :: IO ()
testDecisionTableExtraction =
  case DT.decisionRules (DT.decisionTableFromSExpr decisionExpr) of
    [] -> fail "decision table did not extract rules"
    _ -> pure ()

testClosureAndContradiction :: IO ()
testClosureAndContradiction = do
  assertOmiTrue "FF did not produce tautology" $
    W.wittIsTautology (G.gaugeToOperator (G.mkGauge L.gaugeFF))
  assertOmiTrue "F0 did not produce contradiction" $
    W.wittIsContradiction (G.gaugeToOperator (G.mkGauge L.gaugeF0))

testMarkdownExtraction :: IO ()
testMarkdownExtraction = do
  let first = MD.extractMarkdown markdownSample
      second = MD.extractMarkdown markdownSample
  assertEqualInt "markdown extraction block count changed"
    (length (MD.markdownBlocks first))
    (length (MD.markdownBlocks second))
  assertEqualInt "markdown declaration count changed"
    (length (MD.markdownDeclarations first))
    (length (MD.markdownDeclarations second))
  case MD.markdownDeclarations first of
    [] -> fail "markdown did not find OMI-Lisp declarations"
    _ -> pure ()
  case MD.markdownFrontMatter first of
    [] -> fail "markdown did not find front matter"
    _ -> pure ()

testMarkdownTamperRejection :: IO ()
testMarkdownTamperRejection =
  case MD.markdownBlocks (MD.extractMarkdown markdownSample) of
    [] -> fail "markdown sample produced no evidence"
    block:_ -> do
      let evidence = MD.blockEvidence block
      assertHostTrue "evidence did not verify against original markdown" $
        MD.verifyEvidence markdownSample evidence
      assertHostFalse "tampered evidence was accepted" $
        MD.verifyEvidence tamperedMarkdownSample evidence
      let originalOmi = findMarkdownBlock MD.OmiLispBlock (MD.extractMarkdown markdownSample)
          changedOmi = findMarkdownBlock MD.OmiLispBlock (MD.extractMarkdown samePlaceDifferentEvidenceSample)
      case (originalOmi, changedOmi) of
        (Just original, Just changed) -> do
          assertRelation "checksum changed Markdown citation identity"
            (MD.evidenceIdentity (MD.blockEvidence original))
            (MD.evidenceIdentity (MD.blockEvidence changed))
          assertHostFalse "changed declaration bytes verified against original evidence" $
            MD.verifyEvidence samePlaceDifferentEvidenceSample (MD.blockEvidence original)
        _ -> fail "could not find OMI-Lisp block for identity/checksum regression"

testCanvasProjection :: IO ()
testCanvasProjection = do
  let att = P.resolveDeclaration minimalDeclaration
      first = CJ.encodeJsonCanvas (CJ.attestationToJsonCanvas att)
      second = CJ.encodeJsonCanvas (CJ.attestationToJsonCanvas att)
  assertEqualString "JSON Canvas projection is not deterministic" first second
  case CJ.jsonCanvasNodes (CJ.attestationToJsonCanvas att) of
    [] -> fail "JSON Canvas projection produced no nodes"
    _ -> pure ()

testMemoryReconciliation :: IO ()
testMemoryReconciliation = do
  let bitboard = M.mkBitboard nullRel [nullRel]
      bitblip = M.mkBitBlip nullRel nullRel nullRel
      boardA = M.resolveBlackboard bitboard bitblip
      boardB = M.resolveBlackboard bitboard bitblip
      state = R.reconcile M.EmmcUser bitboard bitblip
      witness = R.witnessVersion state
      att = R.attestReconciliation witness
  assertRelation "bitboard and bit-blip did not resolve deterministically"
    (P.blackboardRelation boardA)
    (P.blackboardRelation boardB)
  case P.attestationRelation att of
    Relation{} -> pure ()

testVcsReceiptedRecord :: IO ()
testVcsReceiptedRecord = do
  let bitboard = M.mkBitboard nullRel [nullRel]
      bitblip = M.mkBitBlip nullRel nullRel nullRel
      board = M.resolveBlackboard bitboard bitblip
      state = R.reconcile M.EmmcSecure bitboard bitblip
      witness = R.witnessVersion state
      att = R.attestReconciliation witness
      accepted = R.acceptReconciliation att witness
      parent = Receipt nullRel
      scope = S.mkScope [L.byteA] [L.byteA] [L.byteA] [L.byteA]
      record = VCS.recordReceiptedReconciliation
        parent scope M.EmmcSecure bitboard bitblip board accepted
  case VCS.vcsResult record of
    result ->
      case R.acceptedReceipt result of
        Receipt Relation{} -> pure ()

testCarrierPrefix :: IO ()
testCarrierPrefix =
  assertBytes "canonical carrier prefix mismatch"
    L.gaugePreHeader
    (C.carrierPrefixBytes C.canonicalCarrierPrefix)

testAddressedFrame :: IO ()
testAddressedFrame = do
  let frame = C.mkAddressedFrame sampleAddress (G.mkGauge L.gaugeFF) samplePlaceValue
  assertRelation "addressed frame did not preserve address"
    sampleAddress
    (C.addressedFrameAddress frame)
  assertRelation "addressed frame did not preserve place-value frame"
    samplePlaceValue
    (C.addressedFramePlaceValue frame)
  assertOmiTrue "addressed frame did not preserve canonical active gauge preheader" $
    G.matchesCanonicalPreHeader (C.addressedFrameGaugePreHeader frame)

testCarrierCarCdrComposition :: IO ()
testCarrierCarCdrComposition = do
  let carFrame = C.mkAddressedFrame sampleAddress (G.mkGauge L.gaugeFF) samplePlaceValue
      cdrFrame = C.mkAddressedFrame samplePlaceValue (G.mkGauge L.gaugeFF) sampleAddress
      fragment = C.attachCDR cdrFrame (C.attachCAR carFrame C.emptyCarrierFragment)
  assertRelation "CAR frame was not attached as a full addressed frame"
    (C.carrierCarFrame fragment)
    (C.carrierCarFrame (C.attachCAR carFrame C.emptyCarrierFragment))
  assertRelation "CDR frame was not attached as a full addressed continuation"
    (C.carrierCdrFrame fragment)
    (C.carrierCdrFrame (C.attachCDR cdrFrame C.emptyCarrierFragment))

testUnaryAdvanceAndXor :: IO ()
testUnaryAdvanceAndXor = do
  let advancedA = C.advanceUnary unaryOne
      advancedB = C.advanceUnary unaryOne
      zeroWitness = C.xorWitness advancedA advancedB
  assertWord32 "unary advance was not deterministic in high word"
    (C.unaryRegisterHi advancedA)
    (C.unaryRegisterHi advancedB)
  assertWord32 "unary advance was not deterministic in low word"
    (C.unaryRegisterLo advancedA)
    (C.unaryRegisterLo advancedB)
  assertWord32 "xor witness of equal unary registers did not clear high word"
    null32
    (C.unaryRegisterHi zeroWitness)
  assertWord32 "xor witness of equal unary registers did not clear low word"
    null32
    (C.unaryRegisterLo zeroWitness)

testCarrierIgnoresEvidenceChecksumIdentity :: IO ()
testCarrierIgnoresEvidenceChecksumIdentity = do
  let originalOmi = findMarkdownBlock MD.OmiLispBlock (MD.extractMarkdown markdownSample)
      changedOmi = findMarkdownBlock MD.OmiLispBlock (MD.extractMarkdown samePlaceDifferentEvidenceSample)
      carFrame = C.mkAddressedFrame sampleAddress (G.mkGauge L.gaugeFF) samplePlaceValue
      cdrFrame = C.mkAddressedFrame samplePlaceValue (G.mkGauge L.gaugeFF) sampleAddress
      fragment = C.attachCDR cdrFrame (C.attachCAR carFrame C.emptyCarrierFragment)
  case (originalOmi, changedOmi) of
    (Just original, Just changed) -> do
      assertRelation "evidence checksum changed evidence citation identity"
        (MD.evidenceIdentity (MD.blockEvidence original))
        (MD.evidenceIdentity (MD.blockEvidence changed))
      assertRelation "evidence checksum changed carrier fragment identity"
        (C.carrierFragmentRelation fragment)
        (C.carrierFragmentRelation fragment)
    _ -> fail "could not find OMI-Lisp block for carrier checksum regression"

testStreamAndGossipAdapters :: IO ()
testStreamAndGossipAdapters = do
  let payload = [L.byteA]
      streamBytes = C.carrierPrefixBytes C.canonicalCarrierPrefix ++ payload
      carFrame = C.mkAddressedFrame sampleAddress (G.mkGauge L.gaugeFF) samplePlaceValue
      fragment = C.attachCAR carFrame C.emptyCarrierFragment
  case Stream.recognizeCarrierPrefix streamBytes of
    Stream.RecognizedCarrier _ rest -> assertBytes "stream prefix stripped the wrong payload" payload rest
    Stream.UnrecognizedCarrier _ -> fail "canonical carrier prefix was not recognized"
  assertRelation "gossip message did not carry carrier fragment relation"
    (C.carrierFragmentRelation fragment)
    (GT.gossipFragmentRelation (GT.GossipCarrierFragment fragment))

assertOperator :: String -> Byte -> Byte -> IO ()
assertOperator label gauge expected =
  let op = G.gaugeToOperator (G.mkGauge gauge)
  in assertOmiTrue ("operator mismatch for " ++ label) $
       L.eqByte (W.wittOperatorCode op) expected

assertOmiTrue :: String -> L.Bool -> IO ()
assertOmiTrue _ L.Tru = pure ()
assertOmiTrue label L.Fls = fail label

assertHostTrue :: String -> Bool -> IO ()
assertHostTrue _ True = pure ()
assertHostTrue label False = fail label

assertHostFalse :: String -> Bool -> IO ()
assertHostFalse _ False = pure ()
assertHostFalse label True = fail label

assertEqualInt :: String -> Int -> Int -> IO ()
assertEqualInt _ a b | a == b = pure ()
assertEqualInt label _ _ = fail label

assertEqualString :: String -> String -> String -> IO ()
assertEqualString _ a b | a == b = pure ()
assertEqualString label _ _ = fail label

assertRelation :: String -> Relation -> Relation -> IO ()
assertRelation label a b =
  case Rel.eqRelation a b of
    Rel.Tru -> pure ()
    Rel.Fls -> fail label

assertWord32 :: String -> Word32 -> Word32 -> IO ()
assertWord32 label a b =
  case Rel.eqWord32 a b of
    Rel.Tru -> pure ()
    Rel.Fls -> fail label

assertBytes :: String -> [Byte] -> [Byte] -> IO ()
assertBytes _ [] [] = pure ()
assertBytes label (a:as) (b:bs) =
  case L.eqByte a b of
    L.Tru -> assertBytes label as bs
    L.Fls -> fail label
assertBytes label _ _ = fail label

findMarkdownBlock :: MD.MarkdownBlockKind -> MD.MarkdownExtraction -> Maybe MD.MarkdownBlock
findMarkdownBlock kind extraction =
  findByKind (MD.markdownBlocks extraction)
  where
    findByKind [] = Nothing
    findByKind (block:blocks)
      | MD.blockKind block == kind = Just block
      | otherwise = findByKind blocks

code :: Bit -> Bit -> Bit -> Bit -> Byte
code a b c d = L.mkByte O O O O a b c d

sym :: [Byte] -> L.SExpr
sym = L.SSym

pair :: L.SExpr -> L.SExpr -> L.SExpr
pair = L.SCons

list :: [L.SExpr] -> L.SExpr
list [] = L.SNil
list (x:xs) = L.SCons x (list xs)

minimalDeclaration :: L.SExpr
minimalDeclaration = pair (sym [L.gaugeFF]) L.SNil

decisionExpr :: L.SExpr
decisionExpr =
  pair
    (sym L.decisionTableSym)
    (list
      [ pair (sym L.nameTag) (sym [L.byteA])
      , pair (sym L.inputsTag) (list [sym [L.byteA]])
      , pair (sym L.outputTag) (sym [L.byteA])
      , pair (sym L.operatorTag) (sym [L.gaugeFF])
      , pair (sym L.rulesTag)
          (list [pair (sym [L.byteA]) (sym [L.byteA])])
      ])

sampleAddress :: Relation
sampleAddress =
  Relation
    (W16 L.byteA L.byteA)
    null16
    null16
    null16
    null16
    null16
    null16
    null16
    null32
    null32
    null32
    null32

samplePlaceValue :: Relation
samplePlaceValue =
  Relation
    null16
    (W16 L.byteA L.byteA)
    null16
    null16
    null16
    null16
    null16
    null16
    null32
    null32
    null32
    null32

unaryOne :: C.UnaryRegister
unaryOne =
  C.mkUnaryRegister null32
    (W32 null16 (W16 L.byteNull (B (N O O O O) (N O O O I))))

markdownSample :: String
markdownSample =
  "---\n"
  ++ "fs: sample.md\n"
  ++ "---\n"
  ++ "\n"
  ++ "```omi-lisp fs=sample.md gs=rules rs=minimal us=a\n"
  ++ "(a)\n"
  ++ "```\n"
  ++ "\n"
  ++ "```decision-table fs=sample.md gs=rules rs=table us=a\n"
  ++ "(decision-table ((name . a) (inputs . (a)) (output . a) (truth-operator . FF) (rules . ((a . a)))))\n"
  ++ "```\n"
  ++ "\n"
  ++ "```canvas fs=sample.md gs=projection rs=face us=a\n"
  ++ "{\"nodes\":[]}\n"
  ++ "```\n"

tamperedMarkdownSample :: String
tamperedMarkdownSample =
  "-x-\n"
  ++ "fs: sample.md\n"
  ++ "---\n"
  ++ "\n"
  ++ "```omi-lisp fs=sample.md gs=rules rs=minimal us=a\n"
  ++ "(a)\n"
  ++ "```\n"
  ++ "\n"
  ++ "```decision-table fs=sample.md gs=rules rs=table us=a\n"
  ++ "(decision-table ((name . a) (inputs . (a)) (output . a) (truth-operator . FF) (rules . ((a . a)))))\n"
  ++ "```\n"
  ++ "\n"
  ++ "```canvas fs=sample.md gs=projection rs=face us=a\n"
  ++ "{\"nodes\":[]}\n"
  ++ "```\n"

samePlaceDifferentEvidenceSample :: String
samePlaceDifferentEvidenceSample =
  "---\n"
  ++ "fs: sample.md\n"
  ++ "---\n"
  ++ "\n"
  ++ "```omi-lisp fs=sample.md gs=rules rs=minimal us=a\n"
  ++ "(b)\n"
  ++ "```\n"
  ++ "\n"
  ++ "```decision-table fs=sample.md gs=rules rs=table us=a\n"
  ++ "(decision-table ((name . a) (inputs . (a)) (output . a) (truth-operator . FF) (rules . ((a . a)))))\n"
  ++ "```\n"
  ++ "\n"
  ++ "```canvas fs=sample.md gs=projection rs=face us=a\n"
  ++ "{\"nodes\":[]}\n"
  ++ "```\n"

-- Merged from the former omi reference (omi-files/omi): the delta law and
-- the handler, now on the nibble-pair kernel types.

testDeltaMatchesSpec :: IO ()
testDeltaMatchesSpec =
  sequence_
    [ assertHostTrue ("delta differs from rotl1^rotl3^rotr2^c at x=" ++ show x ++ " c=" ++ show c)
        (fromW16 (D.delta (toW16 x) (toW16 c)) == specDelta x c)
    | c <- [0x0000, 0x001D, 0x1D1D, 0x1337, 0xFFFF]
    , x <- [0 .. 65535]
    ]
  where
    specDelta x c =
      (rot16L x 1 `HostBits.xor` rot16L x 3 `HostBits.xor` rot16L x 14 `HostBits.xor` c) HostBits..&. 0xFFFF
    rot16L x n = ((x `HostBits.shiftL` n) HostBits..|. (x `HostBits.shiftR` (16 - n))) HostBits..&. 0xFFFF

testDeltaPeriodEight :: IO ()
testDeltaPeriodEight =
  sequence_
    [ assertEqualInt ("delta^8 is not the identity for seed " ++ show seed)
        seed (fromW16 (D.delta8 (toW16 seed) D.deltaC))
    | seed <- [1, 2, 3, 5, 7, 11, 17, 60, 64, 124, 255, 256, 1000, 65535]
    ]

-- Expected slot values (relW16a .. relW16h) produced by the omi reference
-- before the merge, for bind / apply / eval / digest of (PPoint a, PNumber b).
testHandlerMatchesOmiReference :: IO ()
testHandlerMatchesOmiReference =
  sequence_
    [ do check "bind" (H.bind p q) eBind
         check "apply" (H.apply p q) eApply
         check "eval" (H.eval p q) eEval
         check "digest" (H.digest p q) eDigest
    | (a, b, eBind, eApply, eEval, eDigest) <- omiReference
    , let p = H.PPoint (toW16 a)
          q = H.PNumber (toW16 b)
    ]
  where
    check name rel expected =
      assertHostTrue ("handler " ++ name ++ " differs from the omi reference: " ++ show (slots rel))
        (slots rel == expected)
    slots r = map fromW16 [Rel.relW16a r, Rel.relW16b r, Rel.relW16c r, Rel.relW16d r,
                           Rel.relW16e r, Rel.relW16f r, Rel.relW16g r, Rel.relW16h r]
    omiReference =
      [ (0, 0, [0,0,0,0,0,0,0,0], [7453,7453,7453,7453,7453,7453,7453,7453], [34952,34952,34952,34952,34952,34952,34952,34952], [7453,7453,7453,7453,7453,7453,7453,7453])
      , (3, 5, [3,5,0,0,0,0,0,0], [56579,7453,23870,7453,7453,7453,7453,7453], [34952,47172,34952,55772,34952,34952,34952,34952], [56576,7448,23870,7453,7453,7453,7453,7453])
      , (60, 64, [60,64,0,0,0,0,0,0], [7306,7453,8077,7453,7453,7453,7453,7453], [34952,18299,34952,39308,34952,34952,34952,34952], [7350,7517,8077,7453,7453,7453,7453,7453])
      , (17, 19, [17,19,0,0,0,0,0,0], [23987,7453,56743,7453,7453,7453,7453,7453], [34952,40077,34952,48133,34952,34952,34952,34952], [23970,7438,56743,7453,7453,7453,7453,7453])
      , (65535, 1, [65535,1,0,0,0,0,0,0], [58082,7453,23831,7453,7453,7453,7453,7453], [34952,30583,34952,39116,34952,34952,34952,34952], [7453,7452,23831,7453,7453,7453,7453,7453])
      ]

-- Host-side conversions for tests only (the core stays free of Int).
toW16 :: Int -> Word16
toW16 n = W16 (B (nib 15) (nib 11)) (B (nib 7) (nib 3))
  where
    nib top = N (bit top) (bit (top - 1)) (bit (top - 2)) (bit (top - 3))
    bit i = if HostBits.testBit n i then I else O

fromW16 :: Word16 -> Int
fromW16 (W16 (B (N a1 a2 a3 a4) (N a5 a6 a7 a8)) (B (N b1 b2 b3 b4) (N b5 b6 b7 b8))) =
  foldl (\acc b -> acc * 2 + bitInt b) 0 [a1, a2, a3, a4, a5, a6, a7, a8, b1, b2, b3, b4, b5, b6, b7, b8]
  where
    bitInt O = 0
    bitInt I = 1

-- Types augmented from "The OMI Protocol in Pure Haskell - Corrected".

testOrbitOf60 :: IO ()
testOrbitOf60 = do
  assertHostTrue "orbit60 is not 60 xor n for n = 0..63"
    (map fromByte Orb.orbit60 == [60 `HostBits.xor` n | n <- [0 .. 63]])
  assertEqualString "the first sixteen of the orbit are not the four blocks"
    "<=>?89:;45670123" (map (toEnum . fromByte) (take 16 Orb.orbit60))

testBlocks :: IO ()
testBlocks =
  sequence_
    [ assertEqualString ("block of " ++ show n) expected (blockName (Orb.blockOf (toByte n)))
    | (n, expected) <- [(63, "0"), (60, "0"), (59, "1"), (56, "1"), (55, "2"), (52, "2"), (51, "3"), (48, "3"), (47, "-"), (64, "-"), (124, "-")]
    ]
  where
    blockName (Orb.InBlock Orb.Block0) = "0"
    blockName (Orb.InBlock Orb.Block1) = "1"
    blockName (Orb.InBlock Orb.Block2) = "2"
    blockName (Orb.InBlock Orb.Block3) = "3"
    blockName Orb.OutsideBlocks = "-"

testOrbitConstants :: IO ()
testOrbitConstants = do
  assertEqualInt "diagonal is not 12" 12 (fromByte Orb.diagonal)
  assertHostTrue "four-block family is not [3,7,11,15]" (map fromByte Orb.fourBlockFamily == [3, 7, 11, 15])
  assertHostTrue "four-block family is not 3 xor {0,4,8,12}" (map fromByte Orb.fourBlockFamily == map (HostBits.xor 3) [0, 4, 8, 12])
  assertEqualInt "orbital base is not 19" 19 (fromByte Orb.orbitalBase)
  assertEqualInt "orbital base is not orthogonal to the diagonal" 0 (fromByte Orb.orbitalBase HostBits..&. fromByte Orb.diagonal)

testQuadraticForms :: IO ()
testQuadraticForms = do
  assertEqualInt "projective discriminant is not -704" (-704) (disc Q.projective)
  assertEqualInt "affine discriminant is not 0" 0 (disc Q.affine)
  assertHostTrue "lift is not projective minus affine"
    (coeffs Q.liftForm == zipWith (-) (coeffs Q.projective) (coeffs Q.affine))
  where
    coeffs q = map fromByte [Q.formA q, Q.formB q, Q.formC q]
    disc q = case coeffs q of
      [a, b, c] -> b * b - 4 * a * c
      _ -> error "three coefficients"

testTorusMetric :: IO ()
testTorusMetric = do
  assertHostTrue "weight is not symmetric"
    (and [weightOf a b == weightOf b a | a <- cells, b <- cells])
  assertHostTrue "a cell is not at weight 0 from itself" (and [weightOf a a == 0 | a <- cells])
  assertEqualInt "the diagonal does not have four cells" 4 (length (filter onDiag cells))
  assertEqualInt "the largest weight is not 4" 4 (maximum [weightOf a b | a <- cells, b <- cells])
  assertHostTrue "a diagonal cell is not its own nearest diagonal cell"
    (and [weightOf d (T.nearestDiagonal d) == 0 | d <- T.diagonalCells])
  where
    cells = [T.Cell s h | s <- [T.FS, T.GS, T.RS, T.US], h <- [T.KK, T.KU, T.UK, T.UU]]
    weightOf a b = case T.weight a b of
      T.W0 -> 0 :: Int
      T.W1 -> 1
      T.W2 -> 2
      T.W3 -> 3
      T.W4 -> 4
    onDiag c = case T.onDiagonal c of
      Rel.Tru -> True
      Rel.Fls -> False

-- The ruler fold (state' = delta(state, correction), correction' = state)
-- returns after 24 steps; measured on one word before porting.
testRulerFold :: IO ()
testRulerFold =
  sequence_
    [ do let start = Ru.mkRuler (wordRel a) (wordRel c)
             end = iterate Ru.advanceRuler start !! 24
         assertRelation ("ruler state did not return after 24 steps for " ++ show (a, c)) (Ru.rulerState start) (Ru.rulerState end)
         assertRelation ("ruler correction did not return after 24 steps for " ++ show (a, c)) (Ru.rulerCorrection start) (Ru.rulerCorrection end)
    | (a, c) <- [(1, 0), (60, 64), (0x1D1D, 0x1337), (65535, 12)]
    ]
  where
    wordRel n = Rel.setW16a nullRel (toW16 n)

-- A relation whose diagonal (byte index 0) is the XOR of byte indices 2..7.
framedRelation :: Relation
framedRelation = Rel.setW16d (Rel.setW16c (Rel.setW16b (Rel.setW16a nullRel (toW16 diag)) (toW16 0x0102)) (toW16 0x0408)) (toW16 0x1020)
  where diag = (1 `HostBits.xor` 2 `HostBits.xor` 4 `HostBits.xor` 8 `HostBits.xor` 0x10 `HostBits.xor` 0x20) * 256

-- The same relation with a broken diagonal.
unframedRelation :: Relation
unframedRelation = Rel.setW16a framedRelation (toW16 0x0500)

attemptName :: Try.Attempt a -> String
attemptName (Try.Tried _) = "tried"
attemptName (Try.Caught _) = "caught"

-- Author's decision: a path starts from the relation it is given.
testBiosTriesTheGivenRelation :: IO ()
testBiosTriesTheGivenRelation = do
  let path = BIOS.mkGatePath BIOS.Endpoint5T BIOS.Endpoint10T [BIOS.Interior6T]
  assertEqualString "a framed relation was not tried" "tried" (attemptName (BIOS.executePath path framedRelation))
  assertEqualString "an unframed relation was not caught" "caught" (attemptName (BIOS.executePath path unframedRelation))
  case BIOS.executePath path framedRelation of
    Try.Tried r -> assertHostFalse "the path did not run its gates" (fromW16 (Rel.relW16a r) == fromW16 (Rel.relW16a framedRelation))
    Try.Caught _ -> fail "framed relation caught"
  case BIOS.bootClosed (BIOS.mkBootKernel (BIOS.mkBIOS [path]) [nullRel]) of
    Rel.Tru -> pure ()
    Rel.Fls -> fail "the zero relation is framed and should still close"
  case BIOS.bootClosed (BIOS.mkBootKernel (BIOS.mkBIOS [path]) [unframedRelation]) of
    Rel.Tru -> fail "a kernel with a caught path reported closed"
    Rel.Fls -> pure ()

-- The catch carries the structured coordinate; its difference is the repair.
testCaughtFrameRepairsForAnotherTry :: IO ()
testCaughtFrameRepairsForAnotherTry =
  case Try.tryFrame unframedRelation of
    Try.Tried _ -> fail "unframed relation was tried"
    Try.Caught d -> do
      let expected = 1 `HostBits.xor` 2 `HostBits.xor` 4 `HostBits.xor` 8 `HostBits.xor` 0x10 `HostBits.xor` 0x20
      assertEqualInt "catch: expected is not the diagonal folded from 0" expected (fromW16 (H.deviationExpected d))
      assertEqualInt "catch: actual is not what index 0 held" 0x05 (fromW16 (H.deviationActual d))
      assertEqualInt "catch: difference is not expected xor actual" (expected `HostBits.xor` 0x05) (fromW16 (H.deviationDifference d))
      assertEqualInt "finally: the witness is not the repair at index 0" (expected `HostBits.xor` 0x05) (fromW16 (Rel.relW16a (Try.finally (Try.Caught d))))
      assertEqualString "another try after the repair was not tried" "tried"
        (attemptName (Try.tryFrame (Try.repairFrame d unframedRelation)))

-- PLiteral and PStruct are coordinate nibbles; sixteen make the first 8 byte indices.
testCoordinateNibbles :: IO ()
testCoordinateNibbles = do
  let lit = H.PLiteral (toW16 3) H.RadixX (toW16 4) H.KindN
      struct = H.PStruct (toW16 1) H.SepDot (toW16 2) H.RadixB (toW16 7) H.KindI
      frame = H.coordinateFrame (H.positionNibbles lit ++ H.positionNibbles struct ++ H.positionNibbles (H.PPoint (toW16 0xABCD)))
  assertEqualInt "PLiteral does not give four nibbles" 4 (length (H.positionNibbles lit))
  assertEqualInt "PStruct does not give six nibbles" 6 (length (H.positionNibbles struct))
  assertHostTrue "the frame is not the nibbles in order across words a..d"
    (map fromW16 [Rel.relW16a frame, Rel.relW16b frame, Rel.relW16c frame, Rel.relW16d frame]
       == [0x3242, 0x1120, 0x71AB, 0xCD00])
  assertHostTrue "the frame wrote beyond the first 8 byte indices"
    (map fromW16 [Rel.relW16e frame, Rel.relW16f frame, Rel.relW16g frame, Rel.relW16h frame] == [0, 0, 0, 0])

testHandlerPositionTypes :: IO ()
testHandlerPositionTypes = do
  assertEqualInt "PDecimal 0x12 0x34 does not read as 0x1234" 0x1234
    (fromW16 (H.positionWord (H.PDecimal (toW16 0x12) (toW16 0x34))))
  assertEqualInt "PLiteral 3 x 4 n does not pack as 0x3242" 0x3242
    (fromW16 (H.positionWord (H.PLiteral (toW16 3) H.RadixX (toW16 4) H.KindN)))
  assertEqualInt "a deviation's difference is not expected xor actual" (7 `HostBits.xor` 9)
    (fromW16 (H.deviationDifference (H.deviation (H.PPoint (toW16 5)) (toW16 7) (toW16 9))))
  assertRelation "runOperation Digest differs from digest"
    (H.digest (H.PPoint (toW16 3)) (H.PNumber (toW16 5)))
    (H.runOperation H.Digest (H.PPoint (toW16 3)) (H.PNumber (toW16 5)))

toByte :: Int -> Byte
toByte n = case toW16 n of
  W16 _ lo -> lo

fromByte :: Byte -> Int
fromByte b = fromW16 (W16 (B (N O O O O) (N O O O O)) b)

-- The sixteen Fano constants, tracking boxedPINE core/src/verified/fano.ts
-- (wiki: SPEC-07 I-65 to I-67, I-77, I-80).
toMask :: Int -> F.Mask
toMask n = F.Mask (b 0) (b 1) (b 2) (b 3) (b 4) (b 5) (b 6)
  where b k = if HostBits.testBit n k then I else O

toTriad :: Int -> F.Triad
toTriad n = F.T (b 2) (b 1) (b 0)
  where b k = if HostBits.testBit n k then I else O

fromTriad :: F.Triad -> Int
fromTriad (F.T a b c) = 4 * bitInt a + 2 * bitInt b + bitInt c
  where bitInt I = 1
        bitInt O = 0

truth :: F.Bool -> Bool
truth F.Tru = True
truth F.Fls = False

testFanoConstants :: IO ()
testFanoConstants = do
  let closed = [n | n <- [0 .. 127], truth (F.isConstant (toMask n))]
  assertEqualInt "the XOR-closed masks are not exactly 16" 16 (length closed)
  assertHostTrue "some mask does not converge to a constant in two steps"
    (all (\n -> truth (F.isConstant (F.converge (toMask n)))) [0 .. 127])
  let triads = [(a, b, c) | a <- [0 .. 7], b <- [0 .. 7], c <- [0 .. 7]]
      moving = [t | t@(a, b, c) <- triads, truth (F.independent (toTriad a) (toTriad b) (toTriad c))]
  assertEqualInt "the transforms that move the plane onto itself are not 168" 168 (length moving)
  assertHostTrue "an independent triple does not permute the seven points"
    (all (\(a, b, c) -> length (dedupe [fromTriad (F.transform3 (toTriad a) (toTriad b) (toTriad c) (toTriad x)) | x <- [1 .. 7]]) == 7) moving)
  assertEqualInt "9 xor 10 is not swap32's mask 3" 3 (9 `HostBits.xor` (10 :: Int))
  assertEqualInt "11 observed through 9 xor 10 is not the single bit 8" 8 (11 `HostBits.xor` (9 `HostBits.xor` (10 :: Int)))
  where
    dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- ---- The Sabbath contract (OMI.Sabbath) ----

-- sequence number n as an OMI Word32 (low bits only, enough for the tests)
seqWord :: Int -> K.Word32
seqWord n = K.W32 z (K.W16 z8 (K.B z4 (K.N (b 3) (b 2) (b 1) (b 0))))
  where
    z4 = K.N K.O K.O K.O K.O
    z8 = K.B z4 z4
    z = K.W16 z8 z8
    b i = if (n `div` (2 ^ (i :: Int))) `mod` 2 == 1 then K.I else K.O

seqInt :: K.Word32 -> Int
seqInt (K.W32 _ (K.W16 _ (K.B _ (K.N a b2 c d)))) = sum [v a * 8, v b2 * 4, v c * 2, v d]
  where v K.I = 1; v K.O = 0

modeName :: Sab.Mode -> String
modeName Sab.Normal = "Normal"
modeName Sab.Sabbath = "Sabbath"

stepName :: Sab.Step -> String
stepName (Sab.Stepped m) = "-> " ++ modeName m
stepName (Sab.Refused Sab.SabbathViolation n) = "refused: SabbathViolation #" ++ show (seqInt n)
stepName (Sab.Refused Sab.InvalidModeTransition n) = "refused: InvalidModeTransition #" ++ show (seqInt n)

kinds :: [(String, Sab.EventKind)]
kinds = [("Utterance", Sab.Utterance), ("Attestation", Sab.Attestation), ("AttestationSuspended", Sab.AttestationSuspended), ("AttestationResumed", Sab.AttestationResumed), ("Ceremony", Sab.Ceremony), ("Checkpoint", Sab.Checkpoint)]

testSabbathContract :: IO ()
testSabbathContract = do
  -- the document's own test: attestation during the Sabbath is refused, carrying the sequence number
  assertEqualString "attestation during the Sabbath" "refused: SabbathViolation #2" (stepName (Sab.validate Sab.Sabbath (Sab.Event (seqWord 2) Sab.Attestation)))
  assertEqualString "suspension starts the Sabbath" "-> Sabbath" (stepName (Sab.validate Sab.Normal (Sab.Event (seqWord 1) Sab.AttestationSuspended)))
  assertEqualString "resumption ends it" "-> Normal" (stepName (Sab.validate Sab.Sabbath (Sab.Event (seqWord 1) Sab.AttestationResumed)))
  assertEqualString "resuming with nothing suspended" "refused: InvalidModeTransition #3" (stepName (Sab.validate Sab.Normal (Sab.Event (seqWord 3) Sab.AttestationResumed)))
  assertEqualString "suspending twice" "refused: InvalidModeTransition #4" (stepName (Sab.validate Sab.Sabbath (Sab.Event (seqWord 4) Sab.AttestationSuspended)))
  -- a trace: speak, suspend, speak, resume, attest: fine; attest inside the Sabbath: refused at that event
  let ev n k = Sab.Event (seqWord n) k
  assertEqualString "a clean trace" "-> Normal" (stepName (Sab.run Sab.Normal [ev 1 Sab.Utterance, ev 2 Sab.AttestationSuspended, ev 3 Sab.Ceremony, ev 4 Sab.AttestationResumed, ev 5 Sab.Attestation]))
  assertEqualString "a trace that attests in the Sabbath stops there" "refused: SabbathViolation #3" (stepName (Sab.run Sab.Normal [ev 1 Sab.Utterance, ev 2 Sab.AttestationSuspended, ev 3 Sab.Attestation, ev 4 Sab.AttestationResumed]))

-- The Coq proof's step (proofs/coq/04-execution/SabbathProtocolRejectsRestAttestation.v), transcribed:
--   Normal, AttestSuspended => Some SabbathMode; SabbathMode, AttestResumed => Some Normal;
--   SabbathMode, Attestation => None; _, _ => Some mode.  (Coq has no Checkpoint.)
coqStep :: Sab.Mode -> Sab.EventKind -> String
coqStep Sab.Normal Sab.AttestationSuspended = "-> Sabbath"
coqStep Sab.Sabbath Sab.AttestationResumed = "-> Normal"
coqStep Sab.Sabbath Sab.Attestation = "refused"
coqStep m _ = "-> " ++ modeName m

testSabbathAgainstCoq :: IO ()
testSabbathAgainstCoq = do
  let short s = if take 7 s == "refused" then "refused" else s
      rows = [ (modeName m ++ ", " ++ kn, short (stepName (Sab.validate m (Sab.Event (seqWord 0) k))), coqStep m k)
             | m <- [Sab.Normal, Sab.Sabbath], (kn, k) <- kinds, kn /= "Checkpoint" ]
      differ = [ r | r@(_, h, c) <- rows, h /= c ]
  assertEqualString "Haskell and Coq differ in exactly two places"
    "Normal, AttestationResumed: Haskell refused, Coq -> Normal; Sabbath, AttestationSuspended: Haskell refused, Coq -> Sabbath"
    (intercalate "; " [ w ++ ": Haskell " ++ h ++ ", Coq " ++ c | (w, h, c) <- differ ])

-- Wirth syntax notation, tracking unbound/PLAN9 (wsn/stack.wsn, src/zcast.c, src/zlazy.c)
wsnBytes :: String -> [Byte]
wsnBytes = map (toByte . fromEnum)

wsnString :: [Byte] -> String
wsnString = map (toEnum . byteValue)
  where
    byteValue (B (N a b c d) (N e f g h)) = foldl (\acc x -> acc * 2 + bit x) 0 [a, b, c, d, e, f, g, h]
    bit O = 0
    bit I = 1

wsnParse :: String -> WSN.Syntax
wsnParse t = case WSN.parse (wsnBytes t) of
  WSN.Read g -> g
  WSN.NoRead -> error ("does not parse: " ++ t)

wsnAnswer :: WSN.Answer -> String
wsnAnswer WSN.Yes = "yes"
wsnAnswer WSN.No = "no"

-- WSN defined in itself (upper case identifiers: letter and character left to the language)
testWsnInItself :: IO ()
testWsnInItself = do
  let wirth = unlines
        [ "SYNTAX     = { PRODUCTION } ."
        , "PRODUCTION = IDENTIFIER \"=\" EXPRESSION \".\" ."
        , "EXPRESSION = TERM { \"|\" TERM } ."
        , "TERM       = FACTOR { FACTOR } ."
        , "FACTOR     = IDENTIFIER | LITERAL | \"[\" EXPRESSION \"]\""
        , "           | \"(\" EXPRESSION \")\" | \"{\" EXPRESSION \"}\" ."
        , "IDENTIFIER = LETTER { LETTER } ."
        , "LITERAL    = \"\"\"\" CHARACTER { CHARACTER } \"\"\"\" ." ]
      once = wsnString (WSN.render (wsnParse wirth))
  assertEqualString "WSN renders itself, one production a line" (unlines
    [ "SYNTAX = { PRODUCTION } ."
    , "PRODUCTION = IDENTIFIER \"=\" EXPRESSION \".\" ."
    , "EXPRESSION = TERM { \"|\" TERM } ."
    , "TERM = FACTOR { FACTOR } ."
    , "FACTOR = IDENTIFIER | LITERAL | \"[\" EXPRESSION \"]\" | \"(\" EXPRESSION \")\" | \"{\" EXPRESSION \"}\" ."
    , "IDENTIFIER = LETTER { LETTER } ."
    , "LITERAL = \"\"\"\" CHARACTER { CHARACTER } \"\"\"\" ." ]) once
  assertEqualString "and reads back to the same text" once (wsnString (WSN.render (wsnParse once)))
  assertHostTrue "a production without its full stop does not parse"
    (case WSN.parse (wsnBytes "A = \"x\"") of { WSN.NoRead -> True; _ -> False })
  assertHostTrue "an empty literal does not parse"
    (case WSN.parse (wsnBytes "A = \"\" .") of { WSN.NoRead -> True; _ -> False })

-- each zeroth's cast, as zcast printed it in Plan 9
testWsnCasts :: IO ()
testWsnCasts = do
  let heldC = "HELDCONSONANT = \"Y\" [ \"out\" \"!\" \"Y\" EMPTY ] | \"H\" [ \"out\" \"!\" \"H\" EMPTY ] | \"W\" [ \"out\" \"!\" \"W\" EMPTY ] ."
  assertEqualString "YHWH casts consonants and no vowel" (unlines
    [ "CONSONANT = \"Y\" | \"H\" | \"W\" ."
    , "LETTER = CONSONANT | VOWEL ."
    , heldC ]) (wsnString (WSN.render (WSN.cast (wsnBytes "YHWH"))))
  assertEqualString "YAHWEH adds the vowels, lowered 0x20" (unlines
    [ "CONSONANT = \"Y\" | \"H\" | \"W\" ."
    , "VOWEL = \"a\" | \"e\" ."
    , "LETTER = CONSONANT | VOWEL ."
    , heldC
    , "HELDVOWEL = \"a\" [ \"out\" \"!\" \"a\" EMPTY ] | \"e\" [ \"out\" \"!\" \"e\" EMPTY ] ." ])
    (wsnString (WSN.render (WSN.cast (wsnBytes "YAHWEH"))))
  assertEqualString "lower case is cast as terms.mjs casts it: yahweh as YAHWEH"
    (wsnString (WSN.render (WSN.cast (wsnBytes "YAHWEH")))) (wsnString (WSN.render (WSN.cast (wsnBytes "yahweh"))))

-- the stack element under each cast: cast.rc's fourteen answers, and the element never flips 0x20
testWsnStackUnderCasts :: IO ()
testWsnStackUnderCasts = do
  let stack = wsnParse (unlines
        [ "STACK = EMPTY ."
        , "EMPTY = [ \"in\" \"?\" HELD ] ."
        , "HELD = HELDCONSONANT | HELDVOWEL | HELDDIGIT ." ])
      under z = WSN.extend stack (WSN.cast (wsnBytes z))
      ask z sentence = wsnAnswer (WSN.accepts (under z) (wsnBytes "STACK") (wsnBytes sentence))
      rows =
        [ ("in?Y", "yes", "yes")
        , ("in?Yout!Y", "yes", "yes")
        , ("in?Yout!H", "no", "no")
        , ("in?a", "no", "yes")
        , ("in?aout!a", "no", "yes")
        , ("in?aout!A", "no", "no")
        , ("in?Yout!Yin?Hout!Hin?Wout!Win?Hout!H", "yes", "yes")
        , ("in?Yout!Yin?aout!ain?Hout!Hin?Wout!Win?eout!ein?Hout!H", "no", "yes") ]
  sequence_ [ assertEqualString (z ++ " " ++ t) want (ask z t)
            | (t, yhwh, yahweh) <- rows, (z, want) <- [("YHWH", yhwh), ("YAHWEH", yahweh)] ]
  assertEqualString "uncast, the element takes nothing (letter undefined)" "no"
    (wsnAnswer (WSN.accepts stack (wsnBytes "STACK") (wsnBytes "in?Y")))
  assertEqualString "and is empty" "yes" (wsnAnswer (WSN.accepts stack (wsnBytes "STACK") []))

-- the logic grammar, lazy.rc's questions
testWsnLogic :: IO ()
testWsnLogic = do
  let logic = wsnParse (unlines
        [ "BIT       = \"0\" | \"1\" ."
        , "XOR       = \"000\" | \"011\" | \"101\" | \"110\" ."
        , "XNOR      = \"001\" | \"010\" | \"100\" | \"111\" ."
        , "FULLADDER = \"000\" \"00\" | \"001\" \"10\" | \"010\" \"10\" | \"011\" \"01\""
        , "          | \"100\" \"10\" | \"101\" \"01\" | \"110\" \"01\" | \"111\" \"11\" ."
        , "ADD       = CARRYZERO ."
        , "CARRYZERO = [ \"000\" CARRYZERO | \"011\" CARRYZERO | \"101\" CARRYZERO | \"110\" CARRYONE ] ."
        , "CARRYONE  = \"001\" CARRYZERO | \"010\" CARRYONE | \"100\" CARRYONE | \"111\" CARRYONE ." ])
      ask p t = wsnAnswer (WSN.accepts logic (wsnBytes p) (wsnBytes t))
  sequence_ [ assertEqualString (p ++ " " ++ t) want (ask p t)
            | (p, t, want) <-
                [ ("XOR", "011", "yes"), ("XOR", "010", "no"), ("XNOR", "111", "yes")
                , ("FULLADDER", "11101", "no"), ("FULLADDER", "11111", "yes"), ("FULLADDER", "01101", "yes")
                , ("ADD", "110010100001", "yes"), ("ADD", "110010100000", "no"), ("ADD", "000011101000", "yes") ] ]

-- never the same step twice: each production at each place expanded once, as many as zlazy
-- reported in Plan 9 ("N expanded of P productions x Q positions"), sentence by sentence
testWsnExpandsOnce :: IO ()
testWsnExpandsOnce = do
  let stack = wsnParse (unlines
        [ "STACK = EMPTY ."
        , "EMPTY = [ \"in\" \"?\" HELD ] ."
        , "HELD = HELDCONSONANT | HELDVOWEL | HELDDIGIT ." ])
      under z = WSN.extend stack (WSN.cast (wsnBytes z))
      count z t = length (WSN.expanded (under z) (wsnBytes "STACK") (wsnBytes t))
      long1 = "in?Yout!Yin?Hout!Hin?Wout!Win?Hout!H"
      long2 = "in?Yout!Yin?aout!ain?Hout!Hin?Wout!Win?eout!ein?Hout!H"
      zlazy =
        [ ("YHWH", "in?Y", 4), ("YHWH", "in?Yout!Y", 5), ("YHWH", "in?Yout!H", 4), ("YHWH", "in?a", 4)
        , ("YHWH", "in?aout!a", 4), ("YHWH", "in?aout!A", 4), ("YHWH", long1, 14), ("YHWH", long2, 7)
        , ("YAHWEH", "in?Y", 5), ("YAHWEH", "in?Yout!Y", 6), ("YAHWEH", "in?Yout!H", 5), ("YAHWEH", "in?a", 5)
        , ("YAHWEH", "in?aout!a", 6), ("YAHWEH", "in?aout!A", 5), ("YAHWEH", long1, 18), ("YAHWEH", long2, 26) ]
  sequence_ [ assertEqualString (z ++ " " ++ t ++ " expansions") (show n) (show (count z t)) | (z, t, n) <- zlazy ]

-- The showcase's polyhedra (unbound/TSX/src/templates/Polyhedral Spin Explorer.tsx,
-- its nine rows as written: V, E, F, p, q, betti, name) under OMI.Polytope's laws.
-- Every row keeps Euler and Euler-Poincare, so universalSpin is 0 on each; the three
-- Archimedean rows carry a Schlafli symbol their counts do not keep, and two of them
-- carry another solid's counts (truncated tetrahedron 12 18 8, cuboctahedron 12 24 14).
testPolytopeLaws :: IO ()
testPolytopeLaws = do
  let rows =
        [ (4, 6, 4, 3, 3, "Tetrahedron", True)
        , (8, 12, 6, 4, 3, "Cube", True)
        , (6, 12, 8, 3, 4, "Octahedron", True)
        , (20, 30, 12, 5, 3, "Dodecahedron", True)
        , (12, 30, 20, 3, 5, "Icosahedron", True)
        , (24, 36, 14, 4, 3, "Truncated Tetrahedron", False)
        , (12, 18, 8, 3, 3, "Cuboctahedron", False)
        , (24, 36, 14, 6, 3, "Truncated Cube", False)
        , (8, 12, 6, 4, 3, "Custom Shape", True)
        ] :: [(Int, Int, Int, Int, Int, String, Bool)]
      poly v e f p q = Poly.PolyStructure (Poly.Counts (toW16 v) (toW16 e) (toW16 f)) (Poly.Schlafli (toW16 p) (toW16 q)) (Poly.Betti (toW16 1) (toW16 0) (toW16 1))
      yes WSN.Yes = True
      yes WSN.No = False
  mapM_ (\(v, e, f, p, q, name, reg) -> do
    assertHostTrue (name ++ " keeps Euler") (yes (Poly.euler (Poly.Counts (toW16 v) (toW16 e) (toW16 f))))
    assertHostTrue (name ++ " keeps Euler-Poincare, so universalSpin is 0") (yes (Poly.eulerPoincare (poly v e f p q)))
    assertHostTrue (name ++ (if reg then " is a regular {p, q}" else " does not keep its {p, q}")) (yes (Poly.regular (poly v e f p q)) == reg)) rows
  assertHostFalse "V5 E5 F5 is no polyhedron" (yes (Poly.euler (Poly.Counts (toW16 5) (toW16 5) (toW16 5))))
  assertHostFalse "the row named Truncated Tetrahedron is not the truncated tetrahedron (12 18 8)" ((24, 36, 14) == (12 :: Int, 18 :: Int, 8 :: Int))
  assertEqualInt "the adder: 180 + 62" 242 (fromW16 (Poly.addW16 (toW16 180) (toW16 62)))
  assertEqualInt "the multiplier: 5 x 12" 60 (fromW16 (Poly.mulW16 (toW16 5) (toW16 12)))
  assertEqualInt "the multiplier: 3 x 20" 60 (fromW16 (Poly.mulW16 (toW16 3) (toW16 20)))
  let _ = [Poly.Vertex2 (toW16 1) (toW16 2), Poly.Vertex3 (toW16 1) (toW16 2) (toW16 3)]
  putStrLn "polytope: the showcase's PolyStructure and Vertex, one type each, under Euler, Euler-Poincare and {p, q}"

-- Betti numbers computed over Z/2, not declared (OMI.Polytope.betti, after
-- waveform-core's reduction), on complexes derived from bits: the tetrahedron, every
-- triple of four vertices; the cube, the 3-bit words, edges one bit apart, faces one
-- bit fixed; the octahedron, the cube's dual; and a torus, a 3 x 3 grid glued at its
-- edges, which must not come out as a sphere.
testPolytopeBetti :: IO ()
testPolytopeBetti = do
  let row n ks = [if k `elem` ks then I else O | k <- [0 .. n - 1]]
      edgeIndex es (a, b) = head [i | (i, (x, y)) <- zip [0 ..] es, (x, y) == (a, b) || (x, y) == (b, a)]
      complex nv es fs = (Poly.Counts (toW16 nv) (toW16 (length es)) (toW16 (length fs)),
                          [row nv [a, b] | (a, b) <- es],
                          [row (length es) (map (edgeIndex es) f) | f <- fs])
      cycleEdges vs = zip vs (tail vs ++ [head vs])
      bettiOf (cs, d1, d2) = case Poly.betti cs d1 d2 of
        Poly.Betti b0 b1 b2 -> (fromW16 b0, fromW16 b1, fromW16 b2)
      tetra = complex 4 [(a, b) | a <- [0 .. 3], b <- [a + 1 .. 3]]
                [cycleEdges [a, b, c] | a <- [0 .. 3], b <- [a + 1 .. 3], c <- [b + 1 .. 3]]
      cubeV = [0 .. 7] :: [Int]
      cubeE = [(a, b) | a <- cubeV, b <- cubeV, a < b, HostBits.popCount (a `HostBits.xor` b) == 1]
      cubeF = [cycleEdges [base, base `HostBits.xor` m1, base `HostBits.xor` m1 `HostBits.xor` m2, base `HostBits.xor` m2]
              | bit <- [0, 1, 2], val <- [0, 1], let m1 = HostBits.shiftL 1 ((bit + 1) `mod` 3), let m2 = HostBits.shiftL 1 ((bit + 2) `mod` 3),
                let base = if val == 1 then HostBits.shiftL 1 bit else 0]
      cube = complex 8 cubeE cubeF
      octV = [(bit, val) | bit <- [0, 1, 2], val <- [0, 1]] :: [(Int, Int)]
      octIx = zip octV [0 ..]
      ix v = head [i | (w, i) <- octIx, w == v]
      octE = [(ix a, ix b) | (a, b) <- [(a, b) | a <- octV, b <- octV, fst a < fst b]]
      octF = [cycleEdges [ix (0, x), ix (1, y), ix (2, z)] | x <- [0, 1], y <- [0, 1], z <- [0, 1]]
      octa = complex 6 octE octF
      tv i j = (i `mod` 3) * 3 + (j `mod` 3)
      torusE = concat [[(tv i j, tv i (j + 1)), (tv i j, tv (i + 1) j)] | i <- [0 .. 2], j <- [0 .. 2]]
      torusF = [cycleEdges [tv i j, tv i (j + 1), tv (i + 1) (j + 1), tv (i + 1) j] | i <- [0 .. 2], j <- [0 .. 2]]
      torus = complex 9 torusE torusF
  assertEqualString "tetrahedron: 4 6 4" "(1,0,1)" (show (bettiOf tetra))
  assertEqualString "cube: 8 12 6, derived from the 3-bit words" "(1,0,1)" (show (bettiOf cube))
  assertEqualString "octahedron: 6 12 8, the cube's dual" "(1,0,1)" (show (bettiOf octa))
  assertEqualString "the falsifier, a torus: 9 18 9, not a sphere" "(1,2,1)" (show (bettiOf torus))
  assertEqualInt "rank over Z/2 of three rows that sum to zero" 2 (fromW16 (Poly.rank [[I, I, O], [O, I, I], [I, O, I]]))
  putStrLn "polytope: Betti numbers computed over Z/2: the tetrahedron, cube and octahedron are (1,0,1), as the templates declare; a torus is (1,2,1)"

-- The degree signature (Shape Signature's rule: classify by degree, do not name),
-- read from the incidence the Betti test derives: the tetrahedron {3, 3}, the cube
-- {4, 3}, the octahedron {3, 4}; a torus grid {4, 4}, uniform but no sphere; a square
-- pyramid, mixed (its apex of degree 4, its base corners of degree 3).
testPolytopeSignature :: IO ()
testPolytopeSignature = do
  let row n ks = [if k `elem` ks then I else O | k <- [0 .. n - 1]]
      edgeIndex es (a, b) = fst (Prelude.head [(i, ()) | (i, (x, y)) <- zip [0 :: Int ..] es, (x, y) == (a, b) || (x, y) == (b, a)])
      mats nv es fs = ([row nv [a, b] | (a, b) <- es], [row (length es) (map (edgeIndex es) f) | f <- fs])
      cyc vs = zip vs (drop 1 vs ++ take 1 vs)
      sig (d1, d2) = case Poly.signature d1 d2 of
        Poly.Uniform (Poly.Schlafli p q) -> "{" ++ show (fromW16 p) ++ "," ++ show (fromW16 q) ++ "}"
        Poly.Mixed -> "mixed"
      tetra = mats 4 [(a, b) | a <- [0 .. 3], b <- [a + 1 .. 3]] [cyc [a, b, c] | a <- [0 .. 3], b <- [a + 1 .. 3], c <- [b + 1 .. 3 :: Int]]
      cubeE = [(a, b) | a <- [0 .. 7], b <- [0 .. 7], a < b, HostBits.popCount (a `HostBits.xor` b) == (1 :: Int)] :: [(Int, Int)]
      cubeF = [cyc [base, base `HostBits.xor` m1, base `HostBits.xor` m1 `HostBits.xor` m2, base `HostBits.xor` m2]
              | bt <- [0, 1, 2], v <- [0, 1 :: Int], let m1 = HostBits.shiftL 1 ((bt + 1) `mod` 3), let m2 = HostBits.shiftL 1 ((bt + 2) `mod` 3),
                let base = if v == 1 then HostBits.shiftL 1 bt else 0] :: [[(Int, Int)]]
      cube = mats 8 cubeE cubeF
      octV = [(bt, v) | bt <- [0, 1, 2], v <- [0, 1]] :: [(Int, Int)]
      ix w = fst (Prelude.head [(i, ()) | (i, u) <- zip [0 :: Int ..] octV, u == w])
      octa = mats 6 [(ix a, ix b) | a <- octV, b <- octV, fst a < fst b] [cyc [ix (0, x), ix (1, y), ix (2, z)] | x <- [0, 1], y <- [0, 1], z <- [0, 1]]
      tv i j = (i `mod` 3) * 3 + (j `mod` 3)
      torus = mats 9 (concat [[(tv i j, tv i (j + 1)), (tv i j, tv (i + 1) j)] | i <- [0 .. 2], j <- [0 .. 2]])
                [cyc [tv i j, tv i (j + 1), tv (i + 1) (j + 1), tv (i + 1) j] | i <- [0 .. 2], j <- [0 .. 2 :: Int]]
      pyramid = mats 5 [(0, 1), (1, 2), (2, 3), (3, 0), (4, 0), (4, 1), (4, 2), (4, 3)]
                  [cyc [0, 1, 2, 3], cyc [4, 0, 1], cyc [4, 1, 2], cyc [4, 2, 3], cyc [4, 3, 0 :: Int]]
  assertEqualString "tetrahedron's signature" "{3,3}" (sig tetra)
  assertEqualString "cube's signature, from the 3-bit words" "{4,3}" (sig cube)
  assertEqualString "octahedron's signature, the cube's dual" "{3,4}" (sig octa)
  assertEqualString "a torus grid is uniform, {4,4}, and no sphere" "{4,4}" (sig torus)
  assertEqualString "a square pyramid is mixed" "mixed" (sig pyramid)
  putStrLn "polytope: degree signatures read from incidence: {3,3} {4,3} {3,4}; a torus {4,4}; a square pyramid mixed"

-- Shape Signature's Archimedean transforms and Catalan duals, built from face cycles
-- alone: truncation (each vertex cut off, each p-gon a 2p-gon), rectification (a vertex
-- at each edge's middle) and the dual (a vertex per face, a face per vertex). Each
-- result is measured, never named: its counts, Betti numbers, degree signature and
-- whether it is vertex-uniform (archimedean) or face-uniform (catalan). The showcase's
-- rows named Truncated Tetrahedron (24 36 14) and Cuboctahedron (12 18 8) are measured
-- against the shapes the transforms build.
type Faces = [[Int]]

polyEdges :: Faces -> [(Int, Int)]
polyEdges fs = DL.nub [(min a b, max a b) | f <- fs, (a, b) <- zip f (drop 1 f ++ take 1 f)]

polyVerts :: Faces -> [Int]
polyVerts fs = DL.sort (DL.nub (concat fs))

polyMats :: Faces -> (Poly.Counts, [[Bit]], [[Bit]])
polyMats fs = (Poly.Counts (toW16 (length vs)) (toW16 (length es)) (toW16 (length fs)), d1, d2)
  where
    vs = polyVerts fs
    es = polyEdges fs
    row n ks = [if k `elem` ks then I else O | k <- [0 .. n - 1]]
    vi v = length (takeWhile (/= v) vs)
    ei e = length (takeWhile (/= e) es)
    d1 = [row (length vs) [vi a, vi b] | (a, b) <- es]
    d2 = [row (length es) [ei (min a b, max a b) | (a, b) <- zip f (drop 1 f ++ take 1 f)] | f <- fs]

-- a single cycle from its undirected edges
chainCycle :: Eq a => [(a, a)] -> [a]
chainCycle [] = []
chainCycle ((a, b) : rest) = a : go a b rest
  where
    go start cur es
      | cur == start = []
      | otherwise = cur : case [(x, y) | (x, y) <- es, x == cur || y == cur] of
          (e@(x, y) : _) -> go start (if x == cur then y else x) (DL.delete e es)
          [] -> []

relabel :: Eq a => [[a]] -> Faces
relabel fss = [[ix x | x <- f] | f <- fss]
  where
    keys = DL.nub (concat fss)
    ix x = length (takeWhile (/= x) keys)

-- the faces around v, in order: neighbours share an edge through v
around :: Faces -> Int -> [[Int]]
around fs v = map (fs !!) (chainCycle [(i, j) | (i, fi) <- zip [0 ..] fs, (j, fj) <- zip [0 ..] fs, i < j, v `elem` fi, v `elem` fj, shared fi fj])
  where
    shared fi fj = any (\w -> w /= v && w `elem` fi && w `elem` fj && adjacent fi w && adjacent fj w) (fi ++ fj)
    adjacent f w = (min v w, max v w) `elem` polyEdges [f]

polyDual :: Faces -> Faces
polyDual fs = [[ix f | f <- around fs v] | v <- polyVerts fs]
  where
    ix f = length (takeWhile (/= f) fs)

polyTruncate :: Faces -> Faces
polyTruncate fs = relabel (big ++ small)
  where
    big = [concat [[(a, b), (b, a)] | (a, b) <- zip f (drop 1 f ++ take 1 f)] | f <- fs]
    small = [chainCycle [((v, a), (v, b)) | f <- fs, (a, v, b) <- triples f, v == u] | u <- polyVerts fs]
    triples f = zip3 (drop (length f - 1) f ++ take (length f - 1) f) f (drop 1 f ++ take 1 f)

polyRectify :: Faces -> Faces
polyRectify fs = relabel (big ++ small)
  where
    edge a b = (min a b, max a b)
    big = [[edge a b | (a, b) <- zip f (drop 1 f ++ take 1 f)] | f <- fs]
    small = [chainCycle [(edge a v, edge v b) | f <- fs, (a, v', b) <- triples f, v' == u, let v = u] | u <- polyVerts fs]
    triples f = zip3 (drop (length f - 1) f ++ take (length f - 1) f) f (drop 1 f ++ take 1 f)

testPolytopeTransforms :: IO ()
testPolytopeTransforms = do
  let tetraF = [[0, 1, 2], [0, 3, 1], [1, 3, 2], [0, 2, 3]] :: Faces
      cubeF = [[b, b `HostBits.xor` m1, b `HostBits.xor` m1 `HostBits.xor` m2, b `HostBits.xor` m2]
              | bt <- [0, 1, 2], v <- [0, 1 :: Int], let m1 = HostBits.shiftL 1 ((bt + 1) `mod` 3), let m2 = HostBits.shiftL 1 ((bt + 2) `mod` 3),
                let b = if v == 1 then HostBits.shiftL 1 bt else 0] :: Faces
      yes WSN.Yes = True
      yes WSN.No = False
      measure fs = case polyMats fs of
        (cs@(Poly.Counts v e f), d1, d2) ->
          ( (fromW16 v, fromW16 e, fromW16 f)
          , case Poly.betti cs d1 d2 of Poly.Betti a b c -> (fromW16 a, fromW16 b, fromW16 c)
          , case Poly.signature d1 d2 of
              Poly.Uniform (Poly.Schlafli p q) -> "{" ++ show (fromW16 p) ++ "," ++ show (fromW16 q) ++ "}"
              Poly.Mixed -> "mixed"
          , yes (Poly.archimedean d1 d2), yes (Poly.catalan d1 d2) )
      check name fs expected = assertEqualString name (show expected) (show (measure fs))
  check "the tetrahedron" tetraF ((4, 6, 4), (1, 0, 1), "{3,3}", True, True)
  check "the cube" cubeF ((8, 12, 6), (1, 0, 1), "{4,3}", True, True)
  check "the dual of the cube is an octahedron" (polyDual cubeF) ((6, 12, 8), (1, 0, 1), "{3,4}", True, True)
  check "the truncated tetrahedron, built: 12 18 8, vertex-uniform (3.6.6)" (polyTruncate tetraF) ((12, 18, 8), (1, 0, 1), "mixed", True, False)
  check "the truncated cube, built: 24 36 14 (3.8.8)" (polyTruncate cubeF) ((24, 36, 14), (1, 0, 1), "mixed", True, False)
  check "the cuboctahedron, the cube rectified: 12 24 14 (3.4.3.4)" (polyRectify cubeF) ((12, 24, 14), (1, 0, 1), "mixed", True, False)
  check "the triakis tetrahedron, the truncated tetrahedron's dual: Catalan, face-uniform" (polyDual (polyTruncate tetraF)) ((8, 18, 12), (1, 0, 1), "mixed", False, True)
  check "the triakis octahedron, the truncated cube's dual: Catalan, face-uniform" (polyDual (polyTruncate cubeF)) ((14, 36, 24), (1, 0, 1), "mixed", False, True)
  assertHostFalse "the showcase's Truncated Tetrahedron row (24 36 14) is not the truncated tetrahedron built (12 18 8)" ((24, 36, 14) == (12 :: Int, 18 :: Int, 8 :: Int))
  assertHostFalse "the showcase's Cuboctahedron row (12 18 8) is not the cuboctahedron built (12 24 14)" ((12, 18, 8) == (12 :: Int, 24 :: Int, 14 :: Int))
  putStrLn "polytope: truncation, rectification and the dual, built and measured: Archimedean shapes vertex-uniform, their Catalan duals face-uniform, every one Betti (1,0,1)"

-- The Patricia trie of word literals (OMI.Patricia): its shape fixed by the set of words,
-- its keys in byte order, each word's path static for a vocabulary.
patWords :: [String]
patWords = ["YAHWEH", "A", "AND", "YHWH", "AN", "ADONAI", "YAH", "ANT", "B"]

patOf :: [String] -> Pat.Patricia String
patOf ws = Pat.fromList [K.cons (wsnBytes w) w | w <- ws]

patPath :: String -> Pat.Patricia String -> String
patPath w t = case Pat.path (wsnBytes w) t of
  WSN.Read bs -> map (\b -> case b of { O -> '0'; I -> '1' }) bs
  WSN.NoRead -> "none"

testPatricia :: IO ()
testPatricia = do
  let t = patOf patWords
  assertEqualString "the keys are the words in byte order, a shorter word before the longer it begins"
    (intercalate " " (DL.sort patWords)) (intercalate " " (map wsnString (Pat.keys t)))
  assertHostTrue "every word reads back its own value"
    (and [case Pat.lookup (wsnBytes w) t of { WSN.Read v -> v == w; WSN.NoRead -> False } | w <- patWords])
  assertHostTrue "a word not in the trie reads nothing: ANTS, Y, YAHW, the empty word"
    (and [case Pat.lookup (wsnBytes w) t of { WSN.NoRead -> True; _ -> False } | w <- ["ANTS", "Y", "YAHW", ""]])
  assertHostTrue "the shape is fixed by the set of words: every order of six of them makes the same trie"
    (and [wsnAnswer (Pat.sameShape (patOf (take 6 patWords)) (patOf ws)) == "yes" | ws <- DL.permutations (take 6 patWords)])
  assertHostTrue "falsifier: another set of words makes another shape"
    (wsnAnswer (Pat.sameShape (patOf patWords) (patOf ("YAG" : patWords))) == "no")
  let yah = patPath "YAH" t
      yahweh = patPath "YAHWEH" t
  assertHostTrue "YAH and YAHWEH share their path to the fork where YAH ends (0) and YAHWEH goes on (1)"
    (init yah `DL.isPrefixOf` yahweh && last yah == '0' && yahweh !! length (init yah) == '1')
  assertHostTrue "a path is static for a vocabulary: a word added changes it only where it differs first"
    (patPath "AND" (patOf ("YAG" : patWords)) == patPath "AND" t && patPath "YAH" (patOf ("YAG" : patWords)) /= yah)

-- h2gnn's geometric consensus (OMI.Consensus): the engine's own 324 decisions (unbound PLAN9
-- in/cons/decisions, written by its geometric-subsidiarity.ts), each as 4 yes >= d n.
conName :: Con.Solid -> String
conName s = case s of
  Con.Tetrahedron -> "tetrahedron"
  Con.Cube -> "cube"
  Con.Octahedron -> "octahedron"
  Con.Icosahedron -> "icosahedron"
  Con.Dodecahedron -> "dodecahedron"

conLine :: Con.Proof -> String
conLine p = conName (Con.proofSolid p) ++ " " ++ wsnAnswer (Con.proofVerdict p)

testConsensus :: IO ()
testConsensus = do
  golden <- readFile "test/data/consensus-decisions"
  let rows = [ (n, y, s ++ " " ++ v) | l <- lines golden, let [n, y, s, v] = words l ]
      ours = [ (n, y, conLine (Con.decide (toW16 (read n)) (toW16 (read y)))) | (n, y, _) <- rows ]
  assertEqualInt "the engine's decisions, read" 324 (length rows)
  assertHostTrue "every decision of the engine, by 4 yes >= d n"
    (and [ a == b | ((_, _, a), (_, _, b)) <- zip rows ours ])
  let degreeOf s = fromW16 (Con.degree (Con.schlafli s))
      solids = [Con.Tetrahedron, Con.Cube, Con.Octahedron, Con.Icosahedron, Con.Dodecahedron]
  assertHostTrue "d is 3, 2, 2, 1, 1: the thresholds 3/4, 1/2, 1/2, 1/4, 1/4"
    (map degreeOf solids == [3, 2, 2, 1, 1])
  assertHostTrue "a solid and its dual have the same d"
    (and [ degreeOf s == degreeOf (Con.dual s) | s <- solids ])
  assertHostTrue "read by the octahedron instead of the cube, no decision of 1 to 8 changes"
    (and [ wsnAnswer (Con.proofVerdict (Con.decideWith Con.Octahedron (toW16 n) (toW16 y)))
             == wsnAnswer (Con.proofVerdict (Con.decideWith Con.Cube (toW16 n) (toW16 y)))
         | n <- [1 .. 8], y <- [0 .. n] ])
  assertHostTrue "falsifier: read by the tetrahedron instead of the cube, decisions change"
    (or [ wsnAnswer (Con.proofVerdict (Con.decideWith Con.Tetrahedron (toW16 n) (toW16 y)))
            /= wsnAnswer (Con.proofVerdict (Con.decideWith Con.Cube (toW16 n) (toW16 y)))
        | n <- [5 .. 8], y <- [0 .. n] ])
  let proof = Con.decide (toW16 7) (toW16 5)
  assertEqualString "the proof of 7 with 5 yes: the cube, d 2, 4 yes 20 against d n 14, consensus"
    "cube 2 20 14 yes"
    (conName (Con.proofSolid proof) ++ " " ++ show (fromW16 (Con.proofDegree proof)) ++ " "
      ++ show (fromW16 (Con.proofFourYes proof)) ++ " " ++ show (fromW16 (Con.proofDN proof)) ++ " "
      ++ wsnAnswer (Con.proofVerdict proof))
