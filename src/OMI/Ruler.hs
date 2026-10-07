{-# LANGUAGE NoImplicitPrelude #-}

-- | The 16-byte ruler: two relation halves that trade places each step.
--   From "The OMI Protocol in Pure Haskell - Corrected", Part III
--   (@Ruler@, @advanceRuler@).
--
--   Each step, every 16-bit word of the state is advanced by the delta
--   law using the matching word of the correction; the old state becomes
--   the new correction. The four 32-bit words of the state are carried.
module OMI.Ruler
  ( Ruler
  , mkRuler
  , rulerState
  , rulerCorrection
  , advanceRuler
  ) where

import OMI.Delta (delta)
import OMI.Kernel
import OMI.Relation

data Ruler = Ruler Relation Relation

mkRuler :: Relation -> Relation -> Ruler
mkRuler = Ruler

rulerState :: Ruler -> Relation
rulerState (Ruler s _) = s

rulerCorrection :: Ruler -> Relation
rulerCorrection (Ruler _ c) = c

advanceRuler :: Ruler -> Ruler
advanceRuler (Ruler s c) = Ruler (foldDelta s c) s

foldDelta :: Relation -> Relation -> Relation
foldDelta s c =
  setW16h (setW16g (setW16f (setW16e (setW16d (setW16c (setW16b (setW16a s
    (delta (relW16a s) (relW16a c))) (delta (relW16b s) (relW16b c)))
    (delta (relW16c s) (relW16c c))) (delta (relW16d s) (relW16d c)))
    (delta (relW16e s) (relW16e c))) (delta (relW16f s) (relW16f c)))
    (delta (relW16g s) (relW16g c))) (delta (relW16h s) (relW16h c))
