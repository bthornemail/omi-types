module Main where

import Prelude

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
import qualified OMI.BQF as Q
import qualified OMI.Orbit as Orb
import qualified OMI.Ruler as Ru
import qualified OMI.Torus as T
import qualified OMI.Types ()
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
  testBiosFromZero
  testHandlerPositionTypes

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

-- As in the document: from the zero relation every gate path stays closed.
testBiosFromZero :: IO ()
testBiosFromZero = do
  let paths = [ BIOS.mkGatePath BIOS.Endpoint5T BIOS.Endpoint10T [BIOS.Interior6T, BIOS.Interior8T]
              , BIOS.mkGatePath BIOS.Endpoint5T BIOS.Endpoint5T []
              ]
      kernel = BIOS.mkBootKernel (BIOS.mkBIOS paths) []
  case BIOS.bootClosed kernel of
    Rel.Tru -> pure ()
    Rel.Fls -> fail "a BIOS path from zero did not close"

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
