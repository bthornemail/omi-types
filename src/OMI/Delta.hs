{-# LANGUAGE NoImplicitPrelude #-}

-- | The bit-level delta law, merged from the former @omi@ reference:
--
-- > delta(x, c) = rotl(x, 1) XOR rotl(x, 3) XOR rotr(x, 2) XOR c
--
--   Exact period 8 on 16-bit words (proved in omi-axioms,
--   @Delta16HasExactPeriodEight.v@). The protocol has since moved to the
--   swap permutations (swap16, swap32, swap64); this is the earlier form
--   the proof and the specification still use.
module OMI.Delta
  ( deltaC
  , delta
  , delta8
  , advanceRelation
  ) where

import OMI.Bits
import OMI.Kernel
import OMI.Relation

-- | The delta constant, 0x1D1D.
deltaC :: Word16
deltaC = W16 b1D b1D
  where
    b1D = B (N O O O I) (N I I O I)

-- | One delta step.
delta :: Word16 -> Word16 -> Word16
delta x c = xorWord16 (xorWord16 (xorWord16 (rotl1 x) (rotl3 x)) (rotr2 x)) c

-- | Eight delta steps: the identity, by the period-8 law.
delta8 :: Word16 -> Word16 -> Word16
delta8 x c = step (step (step (step (step (step (step (step x)))))))
  where
    step w = delta w c

-- | Advance every 16-bit word of a Relation by one delta step with 'deltaC'.
--   The four 32-bit words are carried unchanged.
advanceRelation :: Relation -> Relation
advanceRelation r =
  setW16h (setW16g (setW16f (setW16e (setW16d (setW16c (setW16b (setW16a r
    (d (relW16a r))) (d (relW16b r))) (d (relW16c r))) (d (relW16d r)))
    (d (relW16e r))) (d (relW16f r))) (d (relW16g r))) (d (relW16h r))
  where
    d w = delta w deltaC
