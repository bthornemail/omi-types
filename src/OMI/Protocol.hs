{-# LANGUAGE NoImplicitPrelude #-}

-- | The protocol as one record of its parts.
--   From "The OMI Protocol in Pure Haskell - Corrected", Part X.
--
--   The document's @runProtocol@ took a host @String@ and returned
--   @Either String Attestation@. Here a declaration is an 'SExpr' and the
--   canvas pipeline decides; 'protocolCloses' reports whether the
--   attestation closes to the zero relation.
module OMI.Protocol
  ( Protocol
  , mkProtocol
  , defaultProtocol
  , protocolRuler
  , protocolBIOS
  , protocolKernel
  , runProtocol
  , protocolCloses
  ) where

import OMI.BIOS
import OMI.Core (nullRel)
import OMI.Handler (relationClosed)
import OMI.Lisp (SExpr)
import OMI.Pipeline (Attestation, attestationRelation, resolveDeclaration)
import OMI.Relation (Bool)
import OMI.Ruler

data Protocol = Protocol Ruler BIOS BootKernel

mkProtocol :: Ruler -> BIOS -> BootKernel -> Protocol
mkProtocol = Protocol

defaultProtocol :: Protocol
defaultProtocol = Protocol (mkRuler nullRel nullRel) (mkBIOS []) (mkBootKernel (mkBIOS []) [])

protocolRuler :: Protocol -> Ruler
protocolRuler (Protocol r _ _) = r

protocolBIOS :: Protocol -> BIOS
protocolBIOS (Protocol _ b _) = b

protocolKernel :: Protocol -> BootKernel
protocolKernel (Protocol _ _ k) = k

-- | Resolve a declaration through the canonical pipeline.
runProtocol :: Protocol -> SExpr -> Attestation
runProtocol _ expr = resolveDeclaration expr

-- | Does the attestation of a declaration close?
protocolCloses :: Protocol -> SExpr -> Bool
protocolCloses p expr = relationClosed (attestationRelation (runProtocol p expr))
