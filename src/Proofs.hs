-- | Proofs module that re-exports all Proofs submodules
module Proofs 
  ( -- * Security Verification
    module Proofs.SecurityVerifier
    
    -- * Timeline Proofs
  , TimelineProof(..)
  , verifyTimelineProof
  , verifyTimeMapProofTL
    
    -- * ZK Proofs
  , ZKProof(..)
  , verifyZKProof
  , verifyTimeMapProofZK
  ) where

-- Import all Proofs submodules
import Proofs.SecurityVerifier
import Proofs.TimelineProof (TimelineProof(..), verifyTimelineProof)
import qualified Proofs.TimelineProof as TL (verifyTimeMapProof)
import Proofs.ZKProof (ZKProof(..), verifyZKProof)
import qualified Proofs.ZKProof as ZK (verifyTimeMapProof)
import Core.Timeline (TimelineId)
import qualified Core.TimeMap as CoreTimeMap
import Core.TimeMap (TimeMap)
import Programs.ProgramState (ProgramState)
import qualified Programs.Types as ProgramsTypes
import Programs.Types (TimeMap)
import Polysemy (Sem, Member)
import Polysemy.Error (Error)
import Polysemy.Embed (Embed)
import Core.Types (AppError)
import Proofs.ZKProof (ProofError)

-- Re-export with renamed functions to avoid conflicts
verifyTimeMapProofTL :: (Member (Error AppError) r, Member (Embed IO) r) => TimelineProof -> TimelineId -> CoreTimeMap.TimeMap -> ProgramState -> Sem r Bool
verifyTimeMapProofTL = TL.verifyTimeMapProof

verifyTimeMapProofZK :: (Member (Error ProofError) r, Member (Embed IO) r) => ZKProof -> ProgramsTypes.TimeMap -> Sem r Bool
verifyTimeMapProofZK = ZK.verifyTimeMapProof
