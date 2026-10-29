{-# LANGUAGE NoImplicitPrelude #-}

-- | A Patricia trie of word literals: each word a list of 'Byte', each value
--   reached by the word's own bits. A word is coded I, then its byte's eight
--   bits, for each byte, and O at the end, so no word's code begins another's
--   (A, AN and AND are three keys); the trie branches only where two codes
--   first differ, O to the left. Its shape is fixed by the set of words alone,
--   whatever order they came in, and its in-order keys are the words in byte
--   order, a shorter word before the longer it begins. A word's path, the
--   branch bits it takes, is static for a vocabulary: it changes only where a
--   word added differs from it first. Tracks unbound/PLAN9's word indexes
--   (the Dewey Decimal captions, WordNet), which give the keys their cues.
module OMI.Patricia
  ( Patricia
  , empty
  , insert
  , fromList
  , lookup
  , keys
  , path
  , sameShape
  ) where

import OMI.Kernel
import OMI.WSN (Answer(..), Read(..))

data Tree v
  = Tip [Bit] [Byte] v
  | Fork [Bit] (Tree v) (Tree v)

data Patricia v = Empty | Root (Tree v)

data Split = Split [Bit] [Bit] [Bit]

empty :: Patricia v
empty = Empty

code :: [Byte] -> [Bit]
code [] = [O]
code (B (N a b c d) (N e f g h) : rest) = I : a : b : c : d : e : f : g : h : code rest

eqBit :: Bit -> Bit -> Answer
eqBit O O = Yes
eqBit I I = Yes
eqBit _ _ = No

-- | The common prefix of two bit lists, and what is left of each.
split :: [Bit] -> [Bit] -> Split
split (x : xs) (y : ys) = case eqBit x y of
  Yes -> case split xs ys of Split p a b -> Split (x : p) a b
  No -> Split [] (x : xs) (y : ys)
split xs ys = Split [] xs ys

fork :: [Bit] -> Bit -> Tree v -> Tree v -> Tree v
fork p O new old = Fork p new old
fork p I new old = Fork p old new

-- | The bits are what is left of the word's code below the parent's branch.
put :: [Bit] -> [Byte] -> v -> Tree v -> Tree v
put bs k v (Tip ts tk tv) = case split bs ts of
  Split _ [] [] -> Tip ts k v
  Split p (b : br) (_ : tr) -> fork p b (Tip br k v) (Tip tr tk tv)
  Split _ _ _ -> Tip ts tk tv -- no code begins another
put bs k v (Fork ps l r) = case split bs ps of
  Split _ (O : br) [] -> Fork ps (put br k v l) r
  Split _ (I : br) [] -> Fork ps l (put br k v r)
  Split p (b : br) (_ : pr) -> fork p b (Tip br k v) (Fork pr l r)
  Split _ _ _ -> Fork ps l r -- no code begins another

insert :: [Byte] -> v -> Patricia v -> Patricia v
insert k v Empty = Root (Tip (code k) k v)
insert k v (Root t) = Root (put (code k) k v t)

fromList :: [Pair [Byte] v] -> Patricia v
fromList = go Empty
  where
    go t [] = t
    go t (p : ps) = go (insert (car p) (cdr p) t) ps

lookup :: [Byte] -> Patricia v -> Read v
lookup _ Empty = NoRead
lookup k (Root t) = find (code k) t
  where
    find bs (Tip ts _ tv) = case split bs ts of
      Split _ [] [] -> Read tv
      Split _ _ _ -> NoRead
    find bs (Fork ps l r) = case split bs ps of
      Split _ (O : br) [] -> find br l
      Split _ (I : br) [] -> find br r
      Split _ _ _ -> NoRead

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : append xs ys

-- | The words, in byte order.
keys :: Patricia v -> [[Byte]]
keys Empty = []
keys (Root t) = go t
  where
    go (Tip _ k _) = [k]
    go (Fork _ l r) = append (go l) (go r)

-- | The branch bits a word takes from the root to its tip, if it is a key.
path :: [Byte] -> Patricia v -> Read [Bit]
path _ Empty = NoRead
path k (Root t) = go (code k) t
  where
    go bs (Tip ts _ _) = case split bs ts of
      Split _ [] [] -> Read []
      Split _ _ _ -> NoRead
    go bs (Fork ps l r) = case split bs ps of
      Split _ (O : br) [] -> prepend O (go br l)
      Split _ (I : br) [] -> prepend I (go br r)
      Split _ _ _ -> NoRead
    prepend b (Read bs) = Read (b : bs)
    prepend _ NoRead = NoRead

eqBits :: [Bit] -> [Bit] -> Answer
eqBits [] [] = Yes
eqBits (x : xs) (y : ys) = case eqBit x y of
  Yes -> eqBits xs ys
  No -> No
eqBits _ _ = No

both :: Answer -> Answer -> Answer
both Yes Yes = Yes
both _ _ = No

-- | Two tries of the same shape: the same forks at the same bits, the same words at the tips.
sameShape :: Patricia v -> Patricia w -> Answer
sameShape Empty Empty = Yes
sameShape (Root a) (Root b) = go a b
  where
    go (Tip ts tk _) (Tip us uk _) = both (eqBits ts us) (eqBits (code tk) (code uk))
    go (Fork ps l r) (Fork qs m s) = both (eqBits ps qs) (both (go l m) (go r s))
    go _ _ = No
sameShape _ _ = No
