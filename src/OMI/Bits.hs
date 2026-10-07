{-# LANGUAGE NoImplicitPrelude #-}

-- | Bit-level operations on the kernel words: XOR and the three rotations
--   used by the delta law. Merged from the former @omi@ reference
--   (@OMI.Kernel@), rewritten for the nibble-pair 'Byte'.
--
--   Bit order: in @W16 (B (N a1 a2 a3 a4) (N a5 a6 a7 a8)) (B (N b1 ..) ..)@
--   @a1@ is the most significant bit and @b8@ the least.
module OMI.Bits
  ( xorBit
  , xorNibble
  , xorByte
  , xorWord16
  , xorWord32
  , rotl1
  , rotl3
  , rotr2
  ) where

import OMI.Kernel

xorBit :: Bit -> Bit -> Bit
xorBit O b = b
xorBit I O = I
xorBit I I = O

xorNibble :: Nibble -> Nibble -> Nibble
xorNibble (N a b c d) (N e f g h) =
  N (xorBit a e) (xorBit b f) (xorBit c g) (xorBit d h)

xorByte :: Byte -> Byte -> Byte
xorByte (B a b) (B c d) = B (xorNibble a c) (xorNibble b d)

xorWord16 :: Word16 -> Word16 -> Word16
xorWord16 (W16 a b) (W16 c d) = W16 (xorByte a c) (xorByte b d)

xorWord32 :: Word32 -> Word32 -> Word32
xorWord32 (W32 a b) (W32 c d) = W32 (xorWord16 a c) (xorWord16 b d)

-- | Rotate a 16-bit word left by one.
rotl1 :: Word16 -> Word16
rotl1 (W16 (B (N a1 a2 a3 a4) (N a5 a6 a7 a8)) (B (N b1 b2 b3 b4) (N b5 b6 b7 b8))) =
  W16 (B (N a2 a3 a4 a5) (N a6 a7 a8 b1)) (B (N b2 b3 b4 b5) (N b6 b7 b8 a1))

-- | Rotate a 16-bit word left by three.
rotl3 :: Word16 -> Word16
rotl3 (W16 (B (N a1 a2 a3 a4) (N a5 a6 a7 a8)) (B (N b1 b2 b3 b4) (N b5 b6 b7 b8))) =
  W16 (B (N a4 a5 a6 a7) (N a8 b1 b2 b3)) (B (N b4 b5 b6 b7) (N b8 a1 a2 a3))

-- | Rotate a 16-bit word right by two. (The former @omi@ pattern rotated
--   left by six; corrected and checked against @rotateR x 2@.)
rotr2 :: Word16 -> Word16
rotr2 (W16 (B (N a1 a2 a3 a4) (N a5 a6 a7 a8)) (B (N b1 b2 b3 b4) (N b5 b6 b7 b8))) =
  W16 (B (N b7 b8 a1 a2) (N a3 a4 a5 a6)) (B (N a7 a8 b1 b2) (N b3 b4 b5 b6))
