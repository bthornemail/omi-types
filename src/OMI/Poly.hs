{-# LANGUAGE NoImplicitPrelude #-}

-- | A polynomial over F2 in x1 to x6, as written, and its PIXELATE cast.
--   Tracks unbound/PLAN9: wsn/poly.wsn (the written form: "0", or terms
--   joined by "+", a term "1" or its variables joined by "*" in increasing
--   order) and wsn/poly.rules (the terms in increasing order of their masks,
--   none twice), the written form of Metaverse Kit's tools/wave28-poly-lib.js.
--
--   A 'Term' is a 6-bit mask, x_i its bit i - 1; a 'Poly' the set of its
--   terms, in their order, each once (constructors hidden, so a Poly is only
--   ever canonical). 'readPoly' reads the written bytes by the grammar and the
--   rules, saying which it refused by; 'writePoly' writes them back, byte for
--   byte as the library does.
--
--   The cast: the 64 points of x1 to x6, in the terms' own order, each a
--   pixel. A term lights the points at or above it (every variable it holds
--   set): its light cone, upward in the 6-cube; a polynomial lights the XOR of
--   its terms' cones, so a pixel is the parity of the terms below it.
--   'unpixelate' reads the terms back by the same rule downward (over F2 the
--   cast is its own inverse).
module OMI.Poly
  ( Term
  , Poly
  , Reading(..)
  , allTerms
  , termBits
  , terms
  , single
  , readPoly
  , writePoly
  , plus
  , pixelate
  , unpixelate
  ) where

import OMI.Kernel

-- | x1 to x6
data Term = Term Bit Bit Bit Bit Bit Bit

newtype Poly = Poly [Term]

data Reading = Written Poly | NotWritten | OutOfOrder | Repeated

data Order = Below | Same | Above

-- ---------------------------------------------------------------- terms

termBits :: Term -> [Bit]
termBits (Term a b c d e f) = [a, b, c, d, e, f]

terms :: Poly -> [Term]
terms (Poly ts) = ts

single :: Term -> Poly
single t = Poly [t]

mapL :: (a -> b) -> [a] -> [b]
mapL _ [] = []
mapL f (x : xs) = f x : mapL f xs

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : append xs ys

for :: [a] -> (a -> [b]) -> [b]
for [] _ = []
for (x : xs) f = append (f x) (for xs f)

-- | The 64 terms in their order, x1 the low bit and x6 the high: also the 64
--   points, the pixels.
allTerms :: [Term]
allTerms =
  for [O, I] (\f -> for [O, I] (\e -> for [O, I] (\d -> for [O, I] (\c -> for [O, I] (\b -> for [O, I] (\a -> [Term a b c d e f]))))))

bitOrder :: Bit -> Bit -> Order
bitOrder O I = Below
bitOrder I O = Above
bitOrder _ _ = Same

-- | By the high bit first, x6 to x1.
order :: Term -> Term -> Order
order (Term a b c d e f) (Term a' b' c' d' e' f') = go [(f, f'), (e, e'), (d, d'), (c, c'), (b, b'), (a, a')]
  where
    go [] = Same
    go ((x, y) : rest) = case bitOrder x y of
      Same -> go rest
      o -> o

-- | every variable of the first set in the second
below :: Term -> Term -> Bit
below s t = go (termBits s) (termBits t)
  where
    go (I : _) (O : _) = O
    go (_ : xs) (_ : ys) = go xs ys
    go _ _ = I

xor :: Bit -> Bit -> Bit
xor O b = b
xor I O = I
xor I I = O

parity :: [Bit] -> Bit
parity [] = O
parity (b : bs) = xor b (parity bs)

-- ---------------------------------------------------------------- the written form

zero', one', plusB, timesB, xB :: Byte
zero' = B (N O O I I) (N O O O O)
one' = B (N O O I I) (N O O O I)
plusB = B (N O O I O) (N I O I I)
timesB = B (N O O I O) (N I O I O)
xB = B (N O I I I) (N I O O O)

-- | the digits 1 to 6, as written, each with its variable's term
digits :: [(Byte, Term)]
digits =
  [ (B (N O O I I) (N O O O I), Term I O O O O O)
  , (B (N O O I I) (N O O I O), Term O I O O O O)
  , (B (N O O I I) (N O O I I), Term O O I O O O)
  , (B (N O O I I) (N O I O O), Term O O O I O O)
  , (B (N O O I I) (N O I O I), Term O O O O I O)
  , (B (N O O I I) (N O I I O), Term O O O O O I)
  ]

eqBit :: Bit -> Bit -> Bit
eqBit O O = I
eqBit I I = I
eqBit _ _ = O

andB :: Bit -> Bit -> Bit
andB I I = I
andB _ _ = O

eqByte :: Byte -> Byte -> Bit
eqByte (B (N a b c d) (N e f g h)) (B (N a' b' c' d') (N e' f' g' h')) =
  andB (andB (andB (eqBit a a') (eqBit b b')) (andB (eqBit c c') (eqBit d d'))) (andB (andB (eqBit e e') (eqBit f f')) (andB (eqBit g g') (eqBit h h')))

orT :: Term -> Term -> Term
orT (Term a b c d e f) (Term a' b' c' d' e' f') = Term (o a a') (o b b') (o c c') (o d d') (o e e') (o f f')
  where
    o O O = O
    o _ _ = I

none :: Term
none = Term O O O O O O

-- | split at a byte
splitAt' :: Byte -> [Byte] -> [[Byte]]
splitAt' s = go []
  where
    go acc [] = [rev acc]
    go acc (b : bs) = case eqByte b s of
      I -> rev acc : go [] bs
      O -> go (b : acc) bs
    rev = rev' []
    rev' a [] = a
    rev' a (x : xs) = rev' (x : a) xs

data Got a = Got a | Not

-- | A term by XONE to XSIX: "1", or x_i joined by "*", each later than the last.
readTerm :: [Byte] -> Got Term
readTerm [b] = case eqByte b one' of
  I -> Got none
  O -> factors [[b]]
readTerm bs = factors (splitAt' timesB bs)

factors :: [[Byte]] -> Got Term
factors = go digits none
  where
    go _ acc [] = Got acc
    go left acc ([x, d] : rest) = case eqByte x xB of
      I -> case from d left of
        Got (t, left') -> go left' (orT acc t) rest
        Not -> Not
      O -> Not
    go _ _ _ = Not
    -- the variable named, among those not yet passed
    from _ [] = Not
    from d ((c, t) : more) = case eqByte d c of
      I -> Got (t, more)
      O -> from d more

-- | The written bytes, by the grammar and then the rules.
readPoly :: [Byte] -> Reading
readPoly [b] = case eqByte b zero' of
  I -> Written (Poly [])
  O -> terms' [[b]]
readPoly bs = terms' (splitAt' plusB bs)

terms' :: [[Byte]] -> Reading
terms' ws = case all' (mapL readTerm ws) of
  Not -> NotWritten
  Got ts -> case repeated ts of
    I -> Repeated
    O -> case ascending ts of
      I -> Written (Poly ts)
      O -> OutOfOrder
  where
    all' [] = Got []
    all' (Not : _) = Not
    all' (Got t : rest) = case all' rest of
      Got r -> Got (t : r)
      Not -> Not
    repeated [] = O
    repeated (t : rest) = case member t rest of
      I -> I
      O -> repeated rest
    member _ [] = O
    member t (u : us) = case order t u of
      Same -> I
      _ -> member t us
    ascending (t : u : rest) = case order t u of
      Below -> ascending (u : rest)
      _ -> O
    ascending _ = I

-- | Written back, as the library writes it.
writePoly :: Poly -> [Byte]
writePoly (Poly []) = [zero']
writePoly (Poly ts) = joinWith plusB (mapL term ts)
  where
    term t = case vars t of
      [] -> [one']
      vs -> joinWith timesB (mapL (\d -> [xB, d]) vs)
    vars t = go (termBits t) digits
    go (I : bs) ((d, _) : ds) = d : go bs ds
    go (O : bs) (_ : ds) = go bs ds
    go _ _ = []
    joinWith _ [] = []
    joinWith _ [w] = w
    joinWith s (w : rest) = append w (s : joinWith s rest)

-- | The sum: the terms in one and not both.
plus :: Poly -> Poly -> Poly
plus (Poly xs) (Poly ys) = Poly (go xs ys)
  where
    go [] bs = bs
    go as [] = as
    go (a : as) (b : bs) = case order a b of
      Below -> a : go as (b : bs)
      Above -> b : go (a : as) bs
      Same -> go as bs

-- ---------------------------------------------------------------- PIXELATE

-- | The 64 pixels, in the points' order: each the parity of the terms at or
--   below it, the terms' light cones XORed.
pixelate :: Poly -> [Bit]
pixelate (Poly ts) = mapL (\p -> parity (mapL (\t -> below t p) ts)) allTerms

-- | The terms back from the 64 pixels: a term is present when the pixels at
--   or below it hold an odd number lit.
unpixelate :: [Bit] -> Poly
unpixelate px = Poly (keep allTerms)
  where
    lit = zip' allTerms px
    keep [] = []
    keep (t : rest) = case parity (mapL (\(p, v) -> andB (below p t) v) lit) of
      I -> t : keep rest
      O -> keep rest
    zip' (a : as) (b : bs) = (a, b) : zip' as bs
    zip' _ _ = []
