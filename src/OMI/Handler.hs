{-# LANGUAGE NoImplicitPrelude #-}

-- | The four handler operations over positions, merged from the former
--   @omi@ reference and augmented with the types of "The OMI Protocol in
--   Pure Haskell - Corrected", Part VI. Positions carry 'Word16' indices,
--   not host integers (indices, not values), and the document's @Char@
--   fields are the small enums 'Radix', 'Kind' and 'Separator'.
--
--   The document's string @Grammar@ is not ported: in this package the
--   'Position' constructors are the grammar (every value is admissible by
--   construction, as the document says), and the string patterns live in
--   @rosetta/src/grammar/grammar.ts@.
--
--   This is a reference reading of bind / apply / eval / digest, not an
--   authority layer: acceptance still goes through "OMI.Pipeline".
module OMI.Handler
  ( Position(..)
  , Radix(..)
  , Kind(..)
  , Separator(..)
  , positionWord
  , positionNibbles
  , coordinateFrame
  , Deviation
  , deviation
  , deviationPosition
  , deviationExpected
  , deviationActual
  , deviationDifference
  , Operation(..)
  , allOperations
  , runOperation
  , bind
  , apply
  , eval
  , digest
  , relationXor
  , relationClosed
  ) where

import OMI.Bits
import OMI.Core
import OMI.Delta
import OMI.Kernel
import OMI.Relation

-- | The radix letters of @[boxd]@.
data Radix = RadixB | RadixO | RadixX | RadixD

-- | The literal kinds of @[pin]@.
data Kind = KindP | KindI | KindN

-- | The separators of @[e.]@ in a STRUCT wordform.
data Separator = SepE | SepDot

data Position
  = PPoint Word16
  | PIndex Word16
  | PNumber Word16
  | PExponent Word16
  | PBinary Word16
  | POctal Word16
  | PHex Word16
  | PDecimal Word16 Word16
  | PLiteral Word16 Radix Word16 Kind
  | PStruct Word16 Separator Word16 Radix Word16 Kind

-- | The 16-bit reading of a position.
--
--   Simple positions read their own word. @PDecimal a b@ reads as the low
--   byte of @a@ then the low byte of @b@ (the document's @a * 256 + b@).
--
--   @PLiteral@ and @PStruct@ are computed coordinate nibbles (author,
--   2026-10-07): each field is an index 0..15 into the first 16 indices of
--   the buffer. Their word is their first four nibbles; see
--   'positionNibbles' for all of them.
positionWord :: Position -> Word16
positionWord (PPoint w) = w
positionWord (PIndex w) = w
positionWord (PNumber w) = w
positionWord (PExponent w) = w
positionWord (PBinary w) = w
positionWord (POctal w) = w
positionWord (PHex w) = w
positionWord (PDecimal a b) = W16 (lowByte a) (lowByte b)
positionWord (PLiteral a r b k) = W16 (B (lowNibble a) (radixNibble r)) (B (lowNibble b) (kindNibble k))
positionWord (PStruct a s b r _ _) = W16 (B (lowNibble a) (separatorNibble s)) (B (lowNibble b) (radixNibble r))

-- | The coordinate nibbles of a position. Simple positions give the four
--   nibbles of their word; @PLiteral@ gives four (a, radix, b, kind) and
--   @PStruct@ six (a, separator, b, radix, e, kind).
positionNibbles :: Position -> [Nibble]
positionNibbles (PLiteral a r b k) = [lowNibble a, radixNibble r, lowNibble b, kindNibble k]
positionNibbles (PStruct a s b r e k) =
  [lowNibble a, separatorNibble s, lowNibble b, radixNibble r, lowNibble e, kindNibble k]
positionNibbles p = case positionWord p of
  W16 (B n1 n2) (B n3 n4) -> [n1, n2, n3, n4]

-- | The first 16 coordinate nibbles make the first 8 byte indices of the
--   buffer: relation words a..d, in order. Missing nibbles are zero; any
--   beyond sixteen are not part of the frame.
--   ⟦PLACEHOLDER⟧ Which positions are laid into one frame, and in what
--   order, is the author's to define.
coordinateFrame :: [Nibble] -> Relation
coordinateFrame ns = case take16 ns of
  [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15] ->
    setW16d (setW16c (setW16b (setW16a nullRel (W16 (B n0 n1) (B n2 n3)))
      (W16 (B n4 n5) (B n6 n7))) (W16 (B n8 n9) (B n10 n11))) (W16 (B n12 n13) (B n14 n15))
  _ -> nullRel
  where
    take16 xs = go sixteen xs
    go [] _ = []
    go (_ : ks) [] = N O O O O : go ks []
    go (_ : ks) (y : ys) = y : go ks ys
    sixteen = [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O]

lowByte :: Word16 -> Byte
lowByte (W16 _ lo) = lo

lowNibble :: Word16 -> Nibble
lowNibble (W16 _ (B _ lo)) = lo

radixNibble :: Radix -> Nibble
radixNibble RadixB = N O O O O
radixNibble RadixO = N O O O I
radixNibble RadixX = N O O I O
radixNibble RadixD = N O O I I

kindNibble :: Kind -> Nibble
kindNibble KindP = N O O O O
kindNibble KindI = N O O O I
kindNibble KindN = N O O I O

separatorNibble :: Separator -> Nibble
separatorNibble SepE = N O O O O
separatorNibble SepDot = N O O O I

-- | A failed compare-exchange: where, what was expected, what was there,
--   and the difference (expected XOR actual), which is also the repair.
data Deviation = Deviation Position Word16 Word16 Word16

deviation :: Position -> Word16 -> Word16 -> Deviation
deviation p expected actual = Deviation p expected actual (xorWord16 expected actual)

deviationPosition :: Deviation -> Position
deviationPosition (Deviation p _ _ _) = p

deviationExpected :: Deviation -> Word16
deviationExpected (Deviation _ e _ _) = e

deviationActual :: Deviation -> Word16
deviationActual (Deviation _ _ a _) = a

deviationDifference :: Deviation -> Word16
deviationDifference (Deviation _ _ _ d) = d

data Operation = Bind | Apply | Eval | Digest

allOperations :: [Operation]
allOperations = [Bind, Apply, Eval, Digest]

runOperation :: Operation -> Position -> Position -> Relation
runOperation Bind = bind
runOperation Apply = apply
runOperation Eval = eval
runOperation Digest = digest

-- | Bind: the two positions in the first two slots.
bind :: Position -> Position -> Relation
bind p1 p2 = setW16b (setW16a nullRel (positionWord p1)) (positionWord p2)

-- | Apply: the positions in slots a and c, advanced one delta step.
apply :: Position -> Position -> Relation
apply p1 p2 = advanceRelation (setW16c (setW16a nullRel (positionWord p1)) (positionWord p2))

-- | Eval: the positions in slots b and d, advanced two delta steps.
eval :: Position -> Position -> Relation
eval p1 p2 = advanceRelation (advanceRelation (setW16d (setW16b nullRel (positionWord p1)) (positionWord p2)))

-- | Digest: the XOR of bind and apply.
digest :: Position -> Position -> Relation
digest p1 p2 = relationXor (bind p1 p2) (apply p1 p2)

-- | Slot-by-slot XOR of two relations.
relationXor :: Relation -> Relation -> Relation
relationXor r s =
  setW32d (setW32c (setW32b (setW32a (setW16h (setW16g (setW16f (setW16e (setW16d (setW16c (setW16b (setW16a r
    (xorWord16 (relW16a r) (relW16a s))) (xorWord16 (relW16b r) (relW16b s)))
    (xorWord16 (relW16c r) (relW16c s))) (xorWord16 (relW16d r) (relW16d s)))
    (xorWord16 (relW16e r) (relW16e s))) (xorWord16 (relW16f r) (relW16f s)))
    (xorWord16 (relW16g r) (relW16g s))) (xorWord16 (relW16h r) (relW16h s)))
    (xorWord32 (relW32a r) (relW32a s))) (xorWord32 (relW32b r) (relW32b s)))
    (xorWord32 (relW32c r) (relW32c s))) (xorWord32 (relW32d r) (relW32d s))

-- | A relation is closed when every slot is zero.
relationClosed :: Relation -> Bool
relationClosed r = eqRelation r nullRel
