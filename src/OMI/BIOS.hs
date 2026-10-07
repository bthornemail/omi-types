{-# LANGUAGE NoImplicitPrelude #-}

-- | The BIOS: paths through the 5T / 6T / 8T / 10T gate chain.
--   From "The OMI Protocol in Pure Haskell - Corrected", Part VIII.
--
--   Renamed to avoid clashes with existing modules:
--   the document's @Path@ is 'GatePath' (Core already has @Path@), and its
--   @Kernel@ is 'BootKernel' (OMI.Kernel is the kernel-types module).
--
--   As in the document, each gate advances every 16-bit word by the delta
--   law with the word as its own correction, the 5T endpoint only reads,
--   and the 10T endpoint reads and drives. The document ignores the start
--   endpoint, and so does this port.
--
--   Author's decision (2026-10-07): a path starts from the relation it is
--   given, not from zero. Before any gate runs, the frame is tried
--   ("OMI.Try"): if the relation is not structured right, the path is
--   caught with the proper structure computed from 0, and the caller can
--   repair and try again.
module OMI.BIOS
  ( Endpoint(..)
  , Interior(..)
  , GatePath
  , mkGatePath
  , BIOS
  , mkBIOS
  , BootKernel
  , mkBootKernel
  , reset
  , executePath
  , boot
  , runKernel
  , bootClosed
  ) where

import OMI.Core (null16)
import OMI.Delta (delta)
import OMI.Handler (relationClosed)
import OMI.Kernel
import OMI.Relation
import OMI.Try

data Endpoint = Endpoint5T | Endpoint10T

data Interior = Interior6T | Interior8T

data GatePath = GatePath Endpoint Endpoint [Interior]

mkGatePath :: Endpoint -> Endpoint -> [Interior] -> GatePath
mkGatePath = GatePath

data BIOS = BIOS [GatePath]

mkBIOS :: [GatePath] -> BIOS
mkBIOS = BIOS

data BootKernel = BootKernel BIOS [Relation]

mkBootKernel :: BIOS -> [Relation] -> BootKernel
mkBootKernel = BootKernel

-- | The reset vector.
reset :: Word16
reset = null16

-- | Try one path from a given relation: frame first, then the gates.
executePath :: GatePath -> Relation -> Attempt Relation
executePath (GatePath _ end interiors) r =
  andThen (tryFrame r) (\framed -> Tried (applyEndpoint end (applyAll framed interiors)))
  where
    applyAll x [] = x
    applyAll x (_ : gs) = applyAll (advanceSelf x) gs
    applyEndpoint Endpoint5T x = x
    applyEndpoint Endpoint10T x = advanceSelf x

-- | Every path of the BIOS, tried from the same relation.
boot :: BIOS -> Relation -> [Attempt Relation]
boot (BIOS paths) r = run paths
  where
    run [] = []
    run (p : ps) = executePath p r : run ps

-- | Every path of the kernel's BIOS, tried from each of the kernel's relations.
runKernel :: BootKernel -> [Attempt Relation]
runKernel (BootKernel bios states) = each states
  where
    each [] = []
    each (s : ss) = append (boot bios s) (each ss)
    append [] ys = ys
    append (x : xs) ys = x : append xs ys

-- | Was every attempt tried (none caught), and did each close to zero?
bootClosed :: BootKernel -> Bool
bootClosed kernel = allClosed (runKernel kernel)
  where
    allClosed [] = Tru
    allClosed (Caught _ : _) = Fls
    allClosed (Tried r : rest) = case relationClosed r of
      Tru -> allClosed rest
      Fls -> Fls

-- Each 16-bit word advanced by the delta law with itself as correction.
advanceSelf :: Relation -> Relation
advanceSelf r =
  setW16h (setW16g (setW16f (setW16e (setW16d (setW16c (setW16b (setW16a r
    (self (relW16a r))) (self (relW16b r))) (self (relW16c r))) (self (relW16d r)))
    (self (relW16e r))) (self (relW16f r))) (self (relW16g r))) (self (relW16h r))
  where
    self w = delta w w
