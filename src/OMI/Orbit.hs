{-# LANGUAGE NoImplicitPrelude #-}

-- | The orbit of 60 and its four blocks.
--   From "The OMI Protocol in Pure Haskell - Corrected", Part V.
--
--   The Corrected document used host integers (@Int@, @xorInt@, @Maybe@).
--   Here everything is a 'Byte' index: the orbit is built with 'xorByte',
--   and a block is read from bit patterns, not from numeric ranges.
module OMI.Orbit
  ( Block(..)
  , BlockReading(..)
  , blockOf
  , byte60
  , orbit60
  , diagonal
  , fourBlockFamily
  , orbitalBase
  ) where

import OMI.Bits (xorByte)
import OMI.Kernel

-- | The four descending runs of four in the orbit of 60:
--   Block0 = @< = > ?@ (60–63), Block1 = @8 9 : ;@ (56–59),
--   Block2 = @4 5 6 7@ (52–55), Block3 = @0 1 2 3@ (48–51).
data Block = Block0 | Block1 | Block2 | Block3

data BlockReading = InBlock Block | OutsideBlocks

-- | The block of a byte: high nibble 0011, then bits 3 and 2 pick the block.
blockOf :: Byte -> BlockReading
blockOf (B (N O O I I) (N I I _ _)) = InBlock Block0
blockOf (B (N O O I I) (N I O _ _)) = InBlock Block1
blockOf (B (N O O I I) (N O I _ _)) = InBlock Block2
blockOf (B (N O O I I) (N O O _ _)) = InBlock Block3
blockOf _ = OutsideBlocks

-- | 60 = 0011 1100.
byte60 :: Byte
byte60 = B (N O O I I) (N I I O O)

-- | 60 ⊕ n for n = 0 .. 63, in order.
orbit60 :: [Byte]
orbit60 = walk sixBitIndices
  where
    walk [] = []
    walk (n : ns) = xorByte byte60 n : walk ns

-- | The 64 six-bit indices 0 .. 63 as bytes, in ascending order.
sixBitIndices :: [Byte]
sixBitIndices =
  [ B (N O O a b) (N c d e f)
  | a <- bits, b <- bits, c <- bits, d <- bits, e <- bits, f <- bits ]
  where
    bits = [O, I]

-- | 12 = 0000 1100: the diagonal; 60's wordform at four bits.
diagonal :: Byte
diagonal = B (N O O O O) (N I I O O)

-- | {3, 7, 11, 15} = 3 ⊕ {0, 4, 8, 12}.
fourBlockFamily :: [Byte]
fourBlockFamily =
  [ B (N O O O O) (N O O I I)
  , B (N O O O O) (N O I I I)
  , B (N O O O O) (N I O I I)
  , B (N O O O O) (N I I I I)
  ]

-- | 19 = 0001 0011, fully orthogonal to the diagonal.
orbitalBase :: Byte
orbitalBase = B (N O O O I) (N O O I I)
