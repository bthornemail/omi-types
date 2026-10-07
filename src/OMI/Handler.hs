{-# LANGUAGE NoImplicitPrelude #-}

-- | The four handler operations over positions, merged from the former
--   @omi@ reference. Positions carry a 'Word16' index, not a host integer
--   (indices, not values).
--
--   This is a reference reading of bind / apply / eval / digest, not an
--   authority layer: acceptance still goes through "OMI.Pipeline".
module OMI.Handler
  ( Position(..)
  , positionWord
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

data Position
  = PPoint Word16
  | PIndex Word16
  | PNumber Word16
  | PExponent Word16

positionWord :: Position -> Word16
positionWord (PPoint w) = w
positionWord (PIndex w) = w
positionWord (PNumber w) = w
positionWord (PExponent w) = w

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
