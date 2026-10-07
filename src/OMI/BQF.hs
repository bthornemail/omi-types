{-# LANGUAGE NoImplicitPrelude #-}

-- | The binary quadratic forms as declared coefficients.
--   From "The OMI Protocol in Pure Haskell - Corrected", Part IV.
--
--   The Corrected document evaluated the forms on host integers. Under the
--   index rule a form is a selector over indices, and evaluating it or
--   taking its discriminant is a value read, so this module only declares
--   the coefficients (a, b, c) of a·x² + b·xy + c·y². Evaluation belongs to
--   adapters and tests.
module OMI.BQF
  ( QuadraticForm
  , formA
  , formB
  , formC
  , projective
  , affine
  , liftForm
  ) where

import OMI.Kernel

data QuadraticForm = QuadraticForm Byte Byte Byte

formA :: QuadraticForm -> Byte
formA (QuadraticForm a _ _) = a

formB :: QuadraticForm -> Byte
formB (QuadraticForm _ b _) = b

formC :: QuadraticForm -> Byte
formC (QuadraticForm _ _ c) = c

-- | Q60(x, y) = 60x² + 16xy + 4y²  (discriminant −704).
projective :: QuadraticForm
projective = QuadraticForm (B (N O O I I) (N I I O O)) sixteen four

-- | Q16(x, y) = 16x² + 16xy + 4y² = (4x + 2y)²  (discriminant 0).
affine :: QuadraticForm
affine = QuadraticForm sixteen sixteen four

-- | The lift Q60 − Q16 = 44x².
liftForm :: QuadraticForm
liftForm = QuadraticForm (B (N O O I O) (N I I O O)) zero zero

sixteen :: Byte
sixteen = B (N O O O I) (N O O O O)

four :: Byte
four = B (N O O O O) (N O I O O)

zero :: Byte
zero = B (N O O O O) (N O O O O)
