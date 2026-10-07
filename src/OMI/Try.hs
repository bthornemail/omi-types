{-# LANGUAGE NoImplicitPrelude #-}

-- | try / catch / finally, as total values.
--
--   From the Fano Fold conversation (boxedPINE/_archive): "try — the fold
--   runs; catch — if the fold fails, the deviation is the output; finally —
--   the attestation is witnessed". The catch "carries the structured
--   coordinate (position, expected, actual, difference)", and the
--   difference is also the repair, so a caught attempt is open for another
--   try. Nothing here throws: an 'Attempt' is the coproduct of a tried value
--   and a caught 'Deviation', and a chain of attempts stops at the first
--   catch.
--
--   The structure checked is the frame condition (rev1 §5.4): index 0, the
--   diagonal, must equal the six spatial indices 2..7 folded by XOR from 0.
--   The first 8 byte indices are the first 16 coordinate nibbles of the
--   buffer, which are relation words a..d.
module OMI.Try
  ( Attempt(..)
  , andThen
  , finally
  , frameDiagonal
  , tryFrame
  , repairFrame
  ) where

import OMI.Bits (xorByte)
import OMI.Core (null16, nullRel)
import OMI.Handler
import OMI.Kernel
import OMI.Relation

-- | A tried value, or the deviation that was caught.
data Attempt a = Tried a | Caught Deviation

-- | Chain a second step after a first: a catch stops the chain.
andThen :: Attempt a -> (a -> Attempt b) -> Attempt b
andThen (Tried a) next = next a
andThen (Caught d) _ = Caught d

-- | The witness that always runs. A tried relation is witnessed as itself;
--   a caught deviation is witnessed as the repair it carries: a relation
--   whose index 0 holds the difference, everything else zero.
finally :: Attempt Relation -> Relation
finally (Tried r) = r
finally (Caught d) = setW16a nullRel (deviationDifference d)

-- | The diagonal the frame should have: indices 2..7 folded by XOR from 0.
frameDiagonal :: Relation -> Byte
frameDiagonal r =
  case (relW16b r, relW16c r, relW16d r) of
    (W16 i2 i3, W16 i4 i5, W16 i6 i7) ->
      xorByte (xorByte (xorByte (xorByte (xorByte (xorByte zeroByte i2) i3) i4) i5) i6) i7

-- | One compare-exchange on index 0: expected the folded diagonal, actual
--   what the relation holds. Tried if they agree; otherwise caught, with the
--   structured coordinate of index 0.
tryFrame :: Relation -> Attempt Relation
tryFrame r =
  case relW16a r of
    W16 actual _ ->
      let expected = frameDiagonal r
      in case eqWordByte expected actual of
           Tru -> Tried r
           Fls -> Caught (deviation (PIndex null16) (W16 zeroByte expected) (W16 zeroByte actual))

-- | Apply a caught deviation's repair to index 0, ready for another try.
repairFrame :: Deviation -> Relation -> Relation
repairFrame d r =
  case (relW16a r, deviationDifference d) of
    (W16 actual size, W16 _ difference) -> setW16a r (W16 (xorByte actual difference) size)

zeroByte :: Byte
zeroByte = B (N O O O O) (N O O O O)
