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
  , bootClosed
  ) where

import OMI.Core (nullRel, null16)
import OMI.Delta (delta)
import OMI.Handler (relationClosed)
import OMI.Kernel
import OMI.Relation

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

-- | Run one path from the zero relation.
executePath :: GatePath -> Relation
executePath (GatePath _ end interiors) = applyEndpoint end (applyAll nullRel interiors)
  where
    applyAll r [] = r
    applyAll r (_ : gs) = applyAll (advanceSelf r) gs
    applyEndpoint Endpoint5T r = r
    applyEndpoint Endpoint10T r = advanceSelf r

-- | Every path of the BIOS, executed.
boot :: BIOS -> [Relation]
boot (BIOS paths) = run paths
  where
    run [] = []
    run (p : ps) = executePath p : run ps

-- | Does every path of the kernel's BIOS close to the zero relation?
bootClosed :: BootKernel -> Bool
bootClosed (BootKernel bios _) = allClosed (boot bios)
  where
    allClosed [] = Tru
    allClosed (r : rs) = case relationClosed r of
      Tru -> allClosed rs
      Fls -> Fls

-- Each 16-bit word advanced by the delta law with itself as correction.
advanceSelf :: Relation -> Relation
advanceSelf r =
  setW16h (setW16g (setW16f (setW16e (setW16d (setW16c (setW16b (setW16a r
    (self (relW16a r))) (self (relW16b r))) (self (relW16c r))) (self (relW16d r)))
    (self (relW16e r))) (self (relW16f r))) (self (relW16g r))) (self (relW16h r))
  where
    self w = delta w w
