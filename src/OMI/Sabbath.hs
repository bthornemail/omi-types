{-# LANGUAGE NoImplicitPrelude #-}

-- | The Sabbath contract: no attestation while attestation is suspended.
--
--   Imported 2026-10-13 from the Sabbath Protocol documents
--   (_archive/_inbox/Sabbath Protocol, the same as
--   ~/devops/ulp-core-invariant ULP.Core.Contract.Sabbath). The author:
--   "these are real old documents ... but they have sound logic we can use".
--
--   Ported to this package's terms. The ULP event carried a version, a time,
--   an actor, a payload and two hashes; the contract reads only the event's
--   kind and its sequence number, so only those come across (and no hash is
--   identity here: types/AGENTS.md). The sequence number is an OMI 'Word32'.
--
--   Each step is total, the shape of a Z-notation operation made robust,
--   Op = (OpOk and Success) or Error: it either steps to the next mode or is
--   refused, with the reason and the sequence number of the refused event.
--
--   The rules are the Haskell document's, and they are the contract: the
--   author, 2026-10-13, asked which of the two is right, "the haskell
--   contract". The Coq proof (proofs/coq/04-execution/
--   SabbathProtocolRejectsRestAttestation.v) is weaker in two places: it
--   allows resuming when nothing was suspended, and suspending again (the
--   mode unchanged), where the contract refuses both. Both agree that an
--   attestation during the Sabbath is refused. The proofs' Coq is locked, so
--   the proof stays as it is; the test suite lists the two places it is weaker.
module OMI.Sabbath
  ( Mode(..)
  , EventKind(..)
  , Event(..)
  , Refusal(..)
  , Step(..)
  , nextMode
  , validate
  , step
  , run
  ) where

import OMI.Kernel (Word32)

data Mode = Normal | Sabbath

-- | The ULP event kinds. The Coq proof has the first five; Checkpoint is ULP's own.
data EventKind
  = Utterance
  | Attestation
  | AttestationSuspended
  | AttestationResumed
  | Ceremony
  | Checkpoint

-- | An event: its sequence number and its kind.
data Event = Event Word32 EventKind

data Refusal = SabbathViolation | InvalidModeTransition

-- | A step goes through to a mode, or is refused (with the refused event's sequence number).
data Step = Stepped Mode | Refused Refusal Word32

-- | The next mode, by event kind alone.
nextMode :: Mode -> EventKind -> Mode
nextMode _ AttestationSuspended = Sabbath
nextMode _ AttestationResumed = Normal
nextMode mode _ = mode

-- | The rules, for one event in the current mode: a refusal, or none (Stepped).
validate :: Mode -> Event -> Step
validate Sabbath (Event n Attestation) = Refused SabbathViolation n
validate Normal (Event n AttestationResumed) = Refused InvalidModeTransition n
validate Sabbath (Event n AttestationSuspended) = Refused InvalidModeTransition n
validate mode (Event _ kind) = Stepped (nextMode mode kind)

-- | One step: the same as 'validate'.
step :: Mode -> Event -> Step
step = validate

-- | A trace from a starting mode: the mode at the end, or the first refusal.
run :: Mode -> [Event] -> Step
run mode [] = Stepped mode
run mode (e : rest) = case step mode e of
  Stepped next -> run next rest
  refused -> refused
