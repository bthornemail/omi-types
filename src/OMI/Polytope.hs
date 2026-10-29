{-# LANGUAGE NoImplicitPrelude #-}

-- | Polyhedra as the TSX showcase types them, each type once.
--   Tracks unbound/TSX/src/templates: Polyhedral Spin Explorer and SCGNN VM
--   both declare PolyStructure (vertices, edges, faces, p, q, betti, name),
--   field for field; Incidence Hypergraph Visualization and Tetrahedral Graph
--   Visualization both declare Vertex, one in two coordinates and one in three,
--   so here it is one union. A name, an id, a label and a colour are host
--   strings and stay outside the core (AGENTS.md).
--
--   The laws a polyhedron's counts must keep, over the kernel's own bits (a
--   ripple-carry adder and shift-and-add, on 'Word16'):
--
--   > euler          V + F = E + 2
--   > eulerPoincare  V - E + F = b0 - b1 + b2, as V + F + b1 = E + b0 + b2
--   > regular        p F = 2 E = q V, the Schlafli symbol {p, q}
--
--   And the Betti numbers computed, not declared, over Z/2 as
--   waveform-core computes them (Waveform/Topology/Persistence.hs: a column a
--   set of rows, two columns added by symmetric difference, XOR), on a solid's
--   cell complex: 'betti' takes the counts and the boundary matrices (an edge's
--   row over the vertices, a face's row over the edges) and gives
--   b_k = dim C_k - rank d_k - rank d_(k+1), with 'rank' by elimination on bits.
--
--   Both templates' universalSpin multiplies by the sign of V - E + F minus
--   b0 - b1 + b2, which 'eulerPoincare' says is zero for every polyhedron: it
--   is nonzero only for counts that are not one.
module OMI.Polytope
  ( Counts(..)
  , Schlafli(..)
  , Betti(..)
  , PolyStructure(..)
  , Vertex(..)
  , betti
  , rank
  , euler
  , eulerPoincare
  , regular
  , addW16
  , mulW16
  , eqW16
  ) where

import OMI.Kernel
import OMI.WSN (Answer(..))

-- | V, E, F.
data Counts = Counts Word16 Word16 Word16

-- | {p, q}: p-gons, q at each vertex.
data Schlafli = Schlafli Word16 Word16

-- | b0, b1, b2.
data Betti = Betti Word16 Word16 Word16

data PolyStructure = PolyStructure Counts Schlafli Betti

-- | The two templates' Vertex: in the plane, or in space.
data Vertex
  = Vertex2 Word16 Word16
  | Vertex3 Word16 Word16 Word16

-- bits, most significant first, as the kernel writes them
bits :: Word16 -> [Bit]
bits (W16 (B (N a1 a2 a3 a4) (N a5 a6 a7 a8)) (B (N b1 b2 b3 b4) (N b5 b6 b7 b8))) =
  [a1, a2, a3, a4, a5, a6, a7, a8, b1, b2, b3, b4, b5, b6, b7, b8]

word :: [Bit] -> Word16
word [a1, a2, a3, a4, a5, a6, a7, a8, b1, b2, b3, b4, b5, b6, b7, b8] =
  W16 (B (N a1 a2 a3 a4) (N a5 a6 a7 a8)) (B (N b1 b2 b3 b4) (N b5 b6 b7 b8))
word _ = word [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O]

rev :: [a] -> [a]
rev = go []
  where
    go acc [] = acc
    go acc (x : xs) = go (x : acc) xs

xorBit :: Bit -> Bit -> Bit
xorBit O O = O
xorBit I I = O
xorBit _ _ = I

-- | The carry of three bits: at least two of them set.
majority :: Bit -> Bit -> Bit -> Bit
majority I I _ = I
majority I _ I = I
majority _ I I = I
majority _ _ _ = O

-- | Ripple-carry addition, modulo 2^16.
addW16 :: Word16 -> Word16 -> Word16
addW16 a b = word (rev (go (rev (bits a)) (rev (bits b)) O))
  where
    go (x : xs) (y : ys) c = xorBit (xorBit x y) c : go xs ys (majority x y c)
    go _ _ _ = []

-- | Shift and add, modulo 2^16.
mulW16 :: Word16 -> Word16 -> Word16
mulW16 a b = go zero (bits b)
  where
    zero = word []
    shl w = case bits w of
      (_ : rest) -> word (rest `app` [O])
      [] -> w
    go acc [] = acc
    go acc (I : rest) = go (addW16 (shl acc) a) rest
    go acc (O : rest) = go (shl acc) rest
    app [] ys = ys
    app (x : xs) ys = x : app xs ys

eqW16 :: Word16 -> Word16 -> Answer
eqW16 a b = go (bits a) (bits b)
  where
    go (I : xs) (I : ys) = go xs ys
    go (O : xs) (O : ys) = go xs ys
    go [] [] = Yes
    go _ _ = No

both :: Answer -> Answer -> Answer
both Yes Yes = Yes
both _ _ = No

two :: Word16
two = word [O, O, O, O, O, O, O, O, O, O, O, O, O, O, I, O]

-- | V + F = E + 2.
euler :: Counts -> Answer
euler (Counts v e f) = eqW16 (addW16 v f) (addW16 e two)

-- | V - E + F = b0 - b1 + b2, without subtraction: V + F + b1 = E + b0 + b2.
eulerPoincare :: PolyStructure -> Answer
eulerPoincare (PolyStructure (Counts v e f) _ (Betti b0 b1 b2)) =
  eqW16 (addW16 (addW16 v f) b1) (addW16 (addW16 e b0) b2)

-- | p F = 2 E = q V: the counts of a regular {p, q}.
regular :: PolyStructure -> Answer
regular (PolyStructure (Counts v e f) (Schlafli p q) _) =
  both (eqW16 (mulW16 p f) (addW16 e e)) (eqW16 (mulW16 q v) (addW16 e e))

-- | Rank over Z/2 of rows of bits, by elimination: a row with I in the first
--   column is the pivot, XORed into every other such row; then the column goes.
rank :: [[Bit]] -> Word16
rank rows = case dropZero rows of
  [] -> word []
  live -> go live
  where
    go rs = case pivot rs of
      Found p others -> addW16 (word one) (rank (map tl (eliminate p others)))
      None -> rank (map tl rs)
    one = [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, I]
    eliminate p (r@(I : _) : rest) = xorRow p r : eliminate p rest
    eliminate p (r : rest) = r : eliminate p rest
    eliminate _ [] = []
    tl (_ : xs) = xs
    tl [] = []
    map f (x : xs) = f x : map f xs
    map _ [] = []

data Pivot = Found [Bit] [[Bit]] | None

pivot :: [[Bit]] -> Pivot
pivot (r@(I : _) : rest) = Found r rest
pivot (r : rest) = case pivot rest of
  Found p others -> Found p (r : others)
  None -> None
pivot [] = None

xorRow :: [Bit] -> [Bit] -> [Bit]
xorRow (a : as) (b : bs) = xorBit a b : xorRow as bs
xorRow _ _ = []

dropZero :: [[Bit]] -> [[Bit]]
dropZero (r : rest) = case zero r of
  Yes -> dropZero rest
  No -> r : dropZero rest
  where
    zero (I : _) = No
    zero (O : xs) = zero xs
    zero [] = Yes
dropZero [] = []

-- | a - b, modulo 2^16: a plus the complement of b plus one.
subW16 :: Word16 -> Word16 -> Word16
subW16 a b = addW16 a (addW16 (word (complement (bits b))) (word [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, I]))
  where
    complement (I : xs) = O : complement xs
    complement (O : xs) = I : complement xs
    complement [] = []

-- | b0, b1, b2 of a 2-dimensional cell complex from its counts and its
--   boundary matrices: d1, one row per edge over the vertices; d2, one row
--   per face over the edges.
betti :: Counts -> [[Bit]] -> [[Bit]] -> Betti
betti (Counts v e f) d1 d2 = Betti (subW16 v r1) (subW16 (subW16 e r1) r2) (subW16 f r2)
  where
    r1 = rank d1
    r2 = rank d2
