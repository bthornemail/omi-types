{-# LANGUAGE NoImplicitPrelude #-}

-- | h2gnn's geometric consensus (~/devops/h2gnn-enhanced
--   src/core/geometric-subsidiarity.ts), each decision carried with its proof.
--   The engine's rule is threshold p/V for a solid {p,q}. Euler gives
--   E d = 2pq and V = 4p/d with d = 4 - (p-2)(q-2), so p/V = d/4, and
--   consensus, yes/n >= p/V, is 4 yes >= d n: one integer per solid, d = 3
--   for the tetrahedron, 2 for the cube and the octahedron, 1 for the
--   icosahedron and the dodecahedron. d is symmetric in p and q, so a solid and
--   its dual decide alike: the dual reading changes the solid, not one
--   decision (the author: the 8-dot encapsulation of the 6-dot "changes the
--   interpretation without changing the dots"). A group of n takes the first of
--   tetrahedron, cube, icosahedron, dodecahedron with V >= n, that is
--   4p >= d n, the dodecahedron past 20 (the engine's mapToGeometric, which by
--   count never takes the octahedron). No division and no ratio: Word16 only.
--   Tracks unbound/PLAN9 src/zcons.c.
module OMI.Consensus
  ( Solid(..)
  , Proof(..)
  , schlafli
  , dual
  , degree
  , solidFor
  , decide
  , decideWith
  ) where

import OMI.Kernel
import OMI.Polytope (Schlafli(..), mulW16, subW16, eqW16, leW16)
import OMI.WSN (Answer(..))

data Solid = Tetrahedron | Cube | Octahedron | Icosahedron | Dodecahedron

-- | A decision and its proof: the solid read, its {p,q}, d, then 4 yes against d n, and the verdict.
data Proof = Proof
  { proofSolid :: Solid
  , proofSchlafli :: Schlafli
  , proofDegree :: Word16
  , proofFourYes :: Word16
  , proofDN :: Word16
  , proofVerdict :: Answer
  }

small :: Nibble -> Word16
small n = W16 (B (N O O O O) (N O O O O)) (B (N O O O O) n)

two, three, four, five :: Word16
two = small (N O O I O)
three = small (N O O I I)
four = small (N O I O O)
five = small (N O I O I)

zero :: Word16
zero = small (N O O O O)

schlafli :: Solid -> Schlafli
schlafli Tetrahedron = Schlafli three three
schlafli Cube = Schlafli four three
schlafli Octahedron = Schlafli three four
schlafli Icosahedron = Schlafli three five
schlafli Dodecahedron = Schlafli five three

dual :: Solid -> Solid
dual Tetrahedron = Tetrahedron
dual Cube = Octahedron
dual Octahedron = Cube
dual Icosahedron = Dodecahedron
dual Dodecahedron = Icosahedron

-- | d = 4 - (p-2)(q-2): E d = 2pq, V = 4p/d, and the threshold p/V = d/4.
degree :: Schlafli -> Word16
degree (Schlafli p q) = subW16 four (mulW16 (subW16 p two) (subW16 q two))

-- | V >= n, as 4p >= d n.
holds :: Word16 -> Solid -> Answer
holds n s = case schlafli s of
  sch@(Schlafli p _) -> leW16 (mulW16 (degree sch) n) (mulW16 four p)

solidFor :: Word16 -> Solid
solidFor n = go [Tetrahedron, Cube, Icosahedron]
  where
    go [] = Dodecahedron
    go (s : ss) = case holds n s of
      Yes -> s
      No -> go ss

-- | The decision of a group of n with yes votes under a given solid.
decideWith :: Solid -> Word16 -> Word16 -> Proof
decideWith s n yes =
  let sch = schlafli s
      d = degree sch
      fourYes = mulW16 four yes
      dn = mulW16 d n
      verdict = case eqW16 n zero of
        Yes -> No
        No -> leW16 dn fourYes
   in Proof s sch d fourYes dn verdict

-- | The engine's decision: the solid it reads the group by, and the verdict, with the proof.
decide :: Word16 -> Word16 -> Proof
decide n yes = decideWith (solidFor n) n yes
