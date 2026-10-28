{-# LANGUAGE NoImplicitPrelude #-}

-- | Every OMI type in one import.
--   From "The OMI Protocol in Pure Haskell - Corrected", Part XI, mapped
--   onto this package's modules, and every type the other modules in
--   src/OMI export. Constructors stay hidden wherever the defining module
--   hides them; a type re-exported here without (..) keeps its constructors
--   in its own module. Three modules each define a Bool (Fls | Tru) and two a
--   DecisionTable: each is distinct, so the others come here as synonyms
--   named for their module (FanoBool, LispBool, RelationBool,
--   LispDecisionTable). OMI.Bits and OMI.Delta export functions only.
module OMI.Types
  ( -- * Kernel
    Null(..)
  , Pair(..)
  , Bit(..)
  , Nibble(..)
  , Byte(..)
  , Word16(..)
  , Word32(..)
  , Relation(..)
    -- * Core wrappers
  , Atom
  , Path
  , Receipt
  , Envelope
    -- * Ruler
  , Ruler
    -- * Quadratic forms
  , QuadraticForm
    -- * Orbit
  , Block(..)
  , BlockReading(..)
    -- * Handler
  , Position(..)
  , Radix(..)
  , Kind(..)
  , Separator(..)
  , Deviation
  , Operation(..)
    -- * Pipeline
  , Gauge
  , WittgensteinOperator
  , TruthGate
  , DecisionTable
  , KarnaughMap
  , Citation
  , Combinator
  , Delta
  , Blackboard
  , ProjectionFace
  , Attestation
    -- * BIOS
  , Endpoint(..)
  , Interior(..)
  , GatePath
  , BIOS
  , BootKernel
    -- * Torus
  , Scope(..)
  , Shape(..)
  , Cell(..)
  , Distance(..)
  , Weight(..)
    -- * Protocol
  , Protocol
    -- * try / catch / finally
  , Attempt(..)
    -- * Fano
  , Triad
  , Mask
  , FanoBool
    -- * Lisp and relations
  , SExpr
  , Rule
  , RuleSet
  , Token
  , LispBool
  , LispDecisionTable
  , RelationBool
    -- * Gauge, truth gates, Karnaugh
  , GaugePreHeader
  , TruthVector
  , TruthGateFamily
  , TruthGateRow
  , KarnaughRegion
    -- * Canvas
  , CanvasNode
  , CanvasEdge
  , Canvas
  , JsonCanvas
  , JsonCanvasNode
  , JsonCanvasEdge
    -- * Carrier
  , CarrierPrefix
  , AddressedFrame
  , UnaryRegister
  , CarrierFragment
  , CausalIndex
  , CarrierSymbol
    -- * Markdown evidence
  , MarkdownBlockKind
  , EvidenceChecksum
  , EvidenceSpan
  , MarkdownBlock
  , MarkdownExtraction
    -- * Memory, reconciliation, VCS
  , Bitboard
  , BitBlip
  , MemoryFace
  , CarrierFace
  , ReconcileState
  , VersionWitness
  , ReconcileResult
  , AcceptedReconciliation
  , VCSRecord
    -- * Scope
  , OmiScope
    -- * Adapter shells
  , StreamRecognition
  , NetFrame
  , GossipMessage
  , GossipReject
  , RuntimeCapability
  , RuntimeConfig
  , RuntimeNode
    -- * Sabbath
  , Mode
  , EventKind
  , Event
  , Refusal
  , Step
    -- * Wirth syntax notation
  , Answer(..)
  , Read(..)
  , Syntax
  ) where

import OMI.BIOS
import OMI.BQF
import OMI.Canvas (ProjectionFace, CanvasNode, CanvasEdge, Canvas)
import OMI.Core
import OMI.DecisionTable (DecisionTable)
import OMI.Gauge (Gauge, GaugePreHeader)
import OMI.Handler
import OMI.Karnaugh (KarnaughMap, KarnaughRegion)
import OMI.Kernel
import OMI.Orbit
import OMI.Pipeline (Attestation, Blackboard, Citation, Combinator, Delta)
import OMI.Protocol
import OMI.Ruler
import OMI.Torus
import OMI.TruthGate (TruthGate, TruthGateFamily, TruthGateRow)
import OMI.Try (Attempt(..))
import OMI.Wittgenstein (WittgensteinOperator, TruthVector)
import OMI.Canvas.JSON (JsonCanvas, JsonCanvasNode, JsonCanvasEdge)
import OMI.Carrier (CarrierPrefix, AddressedFrame, UnaryRegister, CarrierFragment, CausalIndex, CarrierSymbol)
import qualified OMI.Fano as Fano
import OMI.Fano (Triad, Mask)
import OMI.Gossip.Types (GossipMessage, GossipReject)
import qualified OMI.Lisp as Lisp
import OMI.Lisp (SExpr, Rule, RuleSet, Token)
import OMI.Markdown (MarkdownBlockKind, EvidenceChecksum, EvidenceSpan, MarkdownBlock, MarkdownExtraction)
import OMI.Memory (Bitboard, BitBlip, MemoryFace, CarrierFace, ReconcileState, VersionWitness, ReconcileResult)
import OMI.Net.Frame (NetFrame)
import OMI.Reconcile (AcceptedReconciliation)
import qualified OMI.Relation as Relation
import OMI.Runtime (RuntimeCapability, RuntimeConfig, RuntimeNode)
import OMI.Sabbath (Mode, EventKind, Event, Refusal, Step)
import OMI.Scope (OmiScope)
import OMI.Stream (StreamRecognition)
import OMI.VCS (VCSRecord)
import OMI.WSN (Answer(..), Read(..), Syntax)

-- | The three Bools and the second DecisionTable, named for their modules.
type FanoBool = Fano.Bool
type LispBool = Lisp.Bool
type RelationBool = Relation.Bool
type LispDecisionTable = Lisp.DecisionTable
