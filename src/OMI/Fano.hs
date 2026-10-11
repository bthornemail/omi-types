{-# LANGUAGE NoImplicitPrelude #-}

-- | The sixteen Fano constants, as gates.
--   Tracks boxedPINE core/src/verified/fano.ts (wiki: SPEC-16, SPEC-07 I-65 to I-67, I-77).
--
--   A constant is seven bits, one per point of the Fano plane (the nonzero
--   3-bit indices 1..7). Exactly 16 of the 128 masks are closed under XOR:
--   the empty mask, the 7 points, the 7 lines, the full mask. 'step' is the
--   gate netlist: each point turns on when two other points of a line through
--   it are on, one OR of three ANDs per point. Two steps converge any mask.
--   No integers: points are 'Triad's of bits, masks are seven 'Bit's.
module OMI.Fano
  ( Bool(..)
  , Triad(..)
  , Mask(..)
  , xorTriad
  , step
  , converge
  , isConstant
  , transform3
  , independent
  ) where

import OMI.Kernel

data Bool = Fls | Tru

-- | A point of the plane as three bits, high to low: T b2 b1 b0.
data Triad = T Bit Bit Bit

-- | One bit per point, points 1 to 7.
data Mask = Mask Bit Bit Bit Bit Bit Bit Bit

andBit :: Bit -> Bit -> Bit
andBit I I = I
andBit _ _ = O

orBit :: Bit -> Bit -> Bit
orBit O O = O
orBit _ _ = I

xorBit :: Bit -> Bit -> Bit
xorBit O O = O
xorBit I I = O
xorBit _ _ = I

eqBit :: Bit -> Bit -> Bool
eqBit O O = Tru
eqBit I I = Tru
eqBit _ _ = Fls

andB :: Bool -> Bool -> Bool
andB Tru Tru = Tru
andB _ _ = Fls

notB :: Bool -> Bool
notB Tru = Fls
notB Fls = Tru

xorTriad :: Triad -> Triad -> Triad
xorTriad (T a b c) (T x y z) = T (xorBit a x) (xorBit b y) (xorBit c z)

-- | One convergence step. The seven lines are {1,2,3} {1,4,5} {1,6,7}
--   {2,4,6} {2,5,7} {3,4,7} {3,5,6}; each output is its own bit OR the three
--   pairs that complete a line through it.
step :: Mask -> Mask
step (Mask a1 a2 a3 a4 a5 a6 a7) =
  Mask (gate a1 a2 a3 a4 a5 a6 a7)
       (gate a2 a1 a3 a4 a6 a5 a7)
       (gate a3 a1 a2 a4 a7 a5 a6)
       (gate a4 a1 a5 a2 a6 a3 a7)
       (gate a5 a1 a4 a2 a7 a3 a6)
       (gate a6 a1 a7 a2 a4 a3 a5)
       (gate a7 a1 a6 a2 a5 a3 a4)
  where
    gate self p q r s u v = orBit self (orBit (andBit p q) (orBit (andBit r s) (andBit u v)))

-- | Two steps reach the closed mask from any input (checked over all 128).
converge :: Mask -> Mask
converge m = step (step m)

eqMask :: Mask -> Mask -> Bool
eqMask (Mask a b c d e f g) (Mask p q r s t u v) =
  andB (eqBit a p) (andB (eqBit b q) (andB (eqBit c r) (andB (eqBit d s)
    (andB (eqBit e t) (andB (eqBit f u) (eqBit g v))))))

-- | A constant is a mask the step leaves unchanged.
isConstant :: Mask -> Bool
isConstant m = eqMask (step m) m

-- | Any three points as the columns of a 3 x 3 bit matrix: a point goes
--   through it by XOR of the columns its bits select.
transform3 :: Triad -> Triad -> Triad -> Triad -> Triad
transform3 a b c (T x2 x1 x0) = xorTriad (select x0 a) (xorTriad (select x1 b) (select x2 c))
  where
    select O _ = T O O O
    select I t = t

eqTriad :: Triad -> Triad -> Bool
eqTriad (T a b c) (T x y z) = andB (eqBit a x) (andB (eqBit b y) (eqBit c z))

isZero :: Triad -> Bool
isZero t = eqTriad t (T O O O)

-- | The three columns move the plane onto itself exactly when none is zero,
--   no two are equal, and the third is not the XOR of the first two.
--   There are 168 such ordered triples: the plane's symmetry group.
independent :: Triad -> Triad -> Triad -> Bool
independent a b c =
  andB (notB (isZero a)) (andB (notB (isZero b)) (andB (notB (eqTriad a b))
    (andB (notB (isZero c)) (andB (notB (eqTriad c a)) (andB (notB (eqTriad c b))
      (notB (eqTriad c (xorTriad a b))))))))
