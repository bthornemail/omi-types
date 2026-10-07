{-# LANGUAGE NoImplicitPrelude #-}

-- | The iExtant torus: two cyclic coordinates of four values, sixteen cells.
--   From "The OMI Protocol in Pure Haskell - Corrected", Part IX, and rev1
--   Part X.
--
--   The Corrected document measured distance with host integers. Here a
--   distance around a 4-cycle is one of three steps, and a weight (the sum
--   of two distances) is one of five.
module OMI.Torus
  ( Scope(..)
  , Shape(..)
  , Cell(..)
  , Distance(..)
  , Weight(..)
  , scopeDistance
  , shapeDistance
  , weight
  , diagonalCells
  , onDiagonal
  , nearestDiagonal
  ) where

import OMI.Relation (Bool(..))

data Scope = FS | GS | RS | US

data Shape = KK | KU | UK | UU

data Cell = Cell Scope Shape

data Distance = D0 | D1 | D2

data Weight = W0 | W1 | W2 | W3 | W4

-- The four places on a cycle.
data Place = P0 | P1 | P2 | P3

scopePlace :: Scope -> Place
scopePlace FS = P0
scopePlace GS = P1
scopePlace RS = P2
scopePlace US = P3

shapePlace :: Shape -> Place
shapePlace KK = P0
shapePlace KU = P1
shapePlace UK = P2
shapePlace UU = P3

placeDistance :: Place -> Place -> Distance
placeDistance a b = fromSteps (stepsFrom a b)
  where
    fromSteps S0 = D0
    fromSteps S1 = D1
    fromSteps S2 = D2
    fromSteps S3 = D1

-- Steps forward from one place to another.
data Steps = S0 | S1 | S2 | S3

stepsFrom :: Place -> Place -> Steps
stepsFrom P0 P0 = S0
stepsFrom P0 P1 = S1
stepsFrom P0 P2 = S2
stepsFrom P0 P3 = S3
stepsFrom P1 P1 = S0
stepsFrom P1 P2 = S1
stepsFrom P1 P3 = S2
stepsFrom P1 P0 = S3
stepsFrom P2 P2 = S0
stepsFrom P2 P3 = S1
stepsFrom P2 P0 = S2
stepsFrom P2 P1 = S3
stepsFrom P3 P3 = S0
stepsFrom P3 P0 = S1
stepsFrom P3 P1 = S2
stepsFrom P3 P2 = S3

scopeDistance :: Scope -> Scope -> Distance
scopeDistance a b = placeDistance (scopePlace a) (scopePlace b)

shapeDistance :: Shape -> Shape -> Distance
shapeDistance a b = placeDistance (shapePlace a) (shapePlace b)

-- | The weight between two cells: scope distance plus shape distance.
--   W0 means the two peers agree.
weight :: Cell -> Cell -> Weight
weight (Cell s1 h1) (Cell s2 h2) = addDistances (scopeDistance s1 s2) (shapeDistance h1 h2)

addDistances :: Distance -> Distance -> Weight
addDistances D0 D0 = W0
addDistances D0 D1 = W1
addDistances D1 D0 = W1
addDistances D0 D2 = W2
addDistances D2 D0 = W2
addDistances D1 D1 = W2
addDistances D1 D2 = W3
addDistances D2 D1 = W3
addDistances D2 D2 = W4

-- | The closing diagonal: (FS, KK) ⊕ (GS, KU) ⊕ (RS, UK) ⊕ (US, UU) = 0.
diagonalCells :: [Cell]
diagonalCells = [Cell FS KK, Cell GS KU, Cell RS UK, Cell US UU]

-- | A cell is on the diagonal when its scope and shape sit at the same place.
onDiagonal :: Cell -> Bool
onDiagonal (Cell s h) = samePlace (stepsFrom (scopePlace s) (shapePlace h))
  where
    samePlace S0 = Tru
    samePlace _ = Fls

-- | The diagonal cell with the least weight from a cell; ties go to the
--   earlier diagonal cell, as in the Corrected document.
nearestDiagonal :: Cell -> Cell
nearestDiagonal c = pick diagonalCells
  where
    pick [d] = d
    pick (d : ds) = closer d (pick ds)
    pick [] = Cell FS KK
    closer a b = atMost (weight c a) (weight c b) a b

-- | @atMost x y a b@ is @a@ when x ≤ y, otherwise @b@.
atMost :: Weight -> Weight -> c -> c -> c
atMost x y a b = case lessOrEqual x y of
  Tru -> a
  Fls -> b

lessOrEqual :: Weight -> Weight -> Bool
lessOrEqual W0 _ = Tru
lessOrEqual W1 W0 = Fls
lessOrEqual W1 _ = Tru
lessOrEqual W2 W0 = Fls
lessOrEqual W2 W1 = Fls
lessOrEqual W2 _ = Tru
lessOrEqual W3 W3 = Tru
lessOrEqual W3 W4 = Tru
lessOrEqual W3 _ = Fls
lessOrEqual W4 W4 = Tru
lessOrEqual W4 _ = Fls
