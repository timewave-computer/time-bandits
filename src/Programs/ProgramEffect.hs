{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
This module provides the Effect abstraction and related functionality.
It encapsulates explicit operations that programs can perform.

Effects are primitive state transitions that:
- Operate on resources in program memory, not directly on assets
- May result in timeline instructions for asset operations
- Always emit a trace record
-}
module Programs.ProgramEffect 
  ( -- * Core Types
    ProgramEffect(..)
  , EffectPayload(..)
  , ProgramGuardedEffect(..)
  
  -- * Compatibility with previous effect system
  , Effect(..)  -- Re-export from Types.Effect
  , Guard(..)   -- Re-export from Types.Guard
  , GuardedEffect(..) -- Re-export from Types.Guard
  , effectToByteString -- For compatibility with TransitionMessage module
  
  -- * Type Aliases
  , ResourceKey
  , Capability
  , FunctionName
  , Value
  
  -- * Effect Operations
  , createProgramEffect
  , replayProgramEffect
  , replayProgramEffects
  , topoSortProgramEffects
  , executeGuardedEffect
  
  -- * Escrow Effects
  , createEscrowEffect
  , createClaimEffect
  , createReleaseEffect
  
  -- * Ownership Effects
  , createOwnershipTransferEffect
  , createOwnershipVerificationEffect
  
  -- * Re-exports from Types modules
  , EffectId
  , FactSnapshot
  , emptyFactSnapshot
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Text (Text)
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import Core.Types (FactSnapshot, EffectId, ObservedFact, FactId, FactValue, ObservationProof, emptyFactSnapshot)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Core (Hash(..), EntityHash(..))
import Core.Types
  ( AppError(..)
  , LamportTime(..)
  )
import Core.Resource 
  ( Resource
  , Address
  , Amount
  , TokenId
  , ContractId
  , EscrowId
  , ClaimCondition(..)
  )
import Programs.ProgramTypes
  ( ProgramId
  , MemorySlot
  )
import Programs.ProgramState (ProgramState)
import Programs.Types (TimeMap)
import Core.Timeline (TimelineHash, TimelineClock(..))
import Data.Time.Clock (UTCTime)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortBy)
import Data.Graph (graphFromEdges, topSort)
import qualified Data.List as List

-- Import from TimeBandits modules
import Types.EffectTypes
  ( EffectId(..)
  , FactId(..)
  , FactValue(..)
  , ObservationProof(..)
  , ObservedFact(..)
  , FactSnapshot
  , emptyFactSnapshot
  , ResourceKey
  , Capability
  , FunctionName
  , Value
  )
import Types.Effect
  ( Effect
  , createEffect
  , replayEffect
  , replayEffects
  , topoSortEffects
  )
import Types.EffectPayload (EffectPayload(..))
import qualified Types.Guard as Guard

-- Import effect types from Types modules for compatibility
import Types.Effect (Effect(..))
import Types.Guard (Guard(..), GuardedEffect(..))

-- | An effect is a primitive state transition
data ProgramEffect
  = EscrowToProgram Resource ProgramId MemorySlot
  | InvokeProgram ProgramId FunctionName [Resource]
  | ClaimFromProgram ProgramId MemorySlot Resource
  | DelegateCapability Capability ProgramId Guard.Expiry
  | WatchResource ResourceKey Guard.Condition Guard.Trigger
  | TransferBetweenSlots MemorySlot MemorySlot Resource
  | CreateToken TokenId Amount  -- Results in timeline instruction
  | DestroyToken TokenId Amount  -- Results in timeline instruction
  | TransferToken TokenId Amount Address Address  -- Results in timeline instruction
  | CrossTimelineCall TimelineHash ProgramEffect
  | LogDiagnostic Text
  | AtomicBatch [ProgramEffect]
  -- New effects for Phase 2
  | CreateEscrow Resource Address Address ClaimCondition
  | ClaimEscrow EscrowId Address ByteString
  | ReleaseEscrow EscrowId Address
  | TransferOwnership ProgramId Address  -- Transfer program ownership
  | VerifyOwnership Resource Address    -- Verify resource ownership
  | AuthorizeInvoker ProgramId Address  -- Authorize address to invoke program
  | RevokeInvoker ProgramId Address     -- Revoke authorization
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- We're using the Guard definition imported from Types.Guard
-- rather than having a duplicate definition here

-- | Calculate a unique ID for an effect based on its content
-- This implements content-addressing for effects
calculateProgramEffectId :: ProgramEffect -> EffectId
calculateProgramEffectId effect =
  -- Create a temporary effect with a placeholder effectId to avoid circular references
  let serialized = S.encode effect
      -- Hash the serialized effect
      digest = SHA256.hash serialized
      hexDigest = Base16.encode digest
      -- Create a hash using the hexDigest encoded as ByteString not Text
      hash = Hash $ TE.encodeUtf8 $ TE.decodeUtf8 hexDigest
  in EffectId hash

-- | Create a new effect with proper ID and metadata
createProgramEffect :: [EffectId] -> FactSnapshot -> UTCTime -> EffectPayload -> ProgramEffect
createProgramEffect parents facts timestamp payloadData =
  case payloadData of
    CreateResourcePayload resourceKey data' ->
      -- Map to appropriate ProgramEffect constructor
      LogDiagnostic $ "Creating resource: " <> resourceKey
      
    TransferResourcePayload resourceKey address ->
      -- Map to appropriate ProgramEffect constructor for transfer
      -- Use a placeholder text that would be converted to TokenId elsewhere
      LogDiagnostic $ "Transfer resource: " <> resourceKey <> " to " <> address
      
    UpdateResourcePayload key value ->
      -- Map to memory update
      LogDiagnostic $ "Updating resource: " <> key
      
    DeleteResourcePayload key ->
      -- Map to resource deletion
      LogDiagnostic $ "Deleting resource: " <> key
      
    -- Handle other payload types with placeholder mappings
    _ -> LogDiagnostic $ "Unsupported payload type"

-- | Interface for replaying effects in causal order
replayProgramEffect :: ProgramState -> ProgramEffect -> (ProgramState, [ProgramEffect])
replayProgramEffect state effect = 
  case effect of
    CreateEscrow resource owner beneficiary claimCondition -> 
      -- Implementation for creating an escrow
      error "Escrow creation replay not implemented"
      
    TransferOwnership programId newOwner ->
      -- Implementation for transferring ownership
      error "Ownership transfer replay not implemented"
      
    LogDiagnostic message ->
      -- Simple diagnostic logging, no state change
      (state, [])
      
    -- Handle other effect types...
    _ -> (state, [])  -- Default case

-- | Replay a sequence of effects in causal order
-- This is the main entry point for effect replay
replayProgramEffects :: ProgramState -> [ProgramEffect] -> ProgramState
replayProgramEffects initialState effects =
  let sortedEffects = topoSortProgramEffects effects
  in List.foldl applyReplayedEffect initialState sortedEffects

-- | Apply a single effect during replay
applyReplayedEffect :: ProgramState -> ProgramEffect -> ProgramState
applyReplayedEffect state effect =
  let (newState, _) = replayProgramEffect state effect
  in newState

-- | Get parent effects for a ProgramEffect
-- Since ProgramEffect doesn't store parentage information directly,
-- we'll use a simple stub implementation for now
parentEffects :: ProgramEffect -> [EffectId]
parentEffects _ = [] -- Placeholder - in a real implementation, this would track dependencies

-- | Topologically sort effects based on their parent-child relationships
-- This ensures that effects are executed in the correct causal order
topoSortProgramEffects :: [ProgramEffect] -> [ProgramEffect]
topoSortProgramEffects effects =
  -- For now, just return the effects in their original order
  -- since we don't have parent-child relationships defined yet
  effects
  
  -- In a real implementation, we would do something like:
  {-
  -- Create a map of effectId to effect for quick lookups
  let effectMap = Map.fromList [(calculateProgramEffectId e, e) | e <- effects]
      
      -- Create edges for the graph: (effect, effectId, [parent effectIds])
      edges = [(e, calculateProgramEffectId e, parentEffects e) | e <- effects]
      
      -- Build a graph from the edges
      (graph, nodeFromVertex, vertexFromKey) = graphFromEdges edges
      
      -- Get the topologically sorted vertices
      sortedVertices = topSort graph
      
      -- Convert vertices back to effects
      sortedEffects = map ((\(e,_,_) -> e) . nodeFromVertex) sortedVertices
  in sortedEffects
  -}

-- | Create an observed fact
observeFact :: Text -> FactValue -> UTCTime -> ObservedFact
observeFact factIdText value observedAt = ObservedFact 
  { ofId = FactId factIdText "observation" (Hash "hash-placeholder")
  , ofValue = value
  , ofObservedAt = observedAt
  }

-- | A guarded effect pairs an effect with its guard
data ProgramGuardedEffect = ProgramGuardedEffect
  { pgGuard :: Guard.Guard           -- ^ The guard condition
  , pgEffect :: ProgramEffect         -- ^ The effect to apply when the guard is met
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Check if a guard condition is met
checkGuard :: ProgramState -> Guard.Guard -> Bool
checkGuard state guard = error "checkGuard not implemented for new Effect type"

-- | Execute a guarded effect
executeGuardedEffect :: ProgramState -> ProgramGuardedEffect -> ProgramState
executeGuardedEffect state guardedEffect =
  if checkGuard state (pgGuard guardedEffect)
  then let (newState, _) = replayProgramEffect state (pgEffect guardedEffect)
       in newState
  else state

-- Create our local version of createGuardedEffect for ProgramGuardedEffect
createProgramGuardedEffect :: Guard.Condition -> Guard.Trigger -> Maybe Guard.Expiry -> ProgramEffect -> ProgramGuardedEffect
createProgramGuardedEffect condition trigger expiry effect = 
  let guard = Guard.Guard 
        { Guard.guardCondition = condition
        , Guard.guardTrigger = trigger
        , Guard.guardExpiry = expiry
        }
  in ProgramGuardedEffect
        { pgGuard = guard
        , pgEffect = effect
        }

-- | Create an escrow effect with appropriate guards
createEscrowEffect ::
  (Member (Error AppError) r) =>
  Resource ->            -- Resource to escrow
  Address ->             -- Original owner
  Address ->             -- Beneficiary who can claim
  ClaimCondition ->      -- Condition for claiming
  Sem r ProgramGuardedEffect
createEscrowEffect resource owner beneficiary claimCondition = do
  -- Create the escrow effect
  let effect = CreateEscrow resource owner beneficiary claimCondition
      -- Ownership of the resource must be verified
      -- Use a simple ResourceCondition as a placeholder
      condition = Guard.ResourceCondition "ownership" owner
      trigger = Guard.ApplyEffect
      expiry = Nothing
  
  -- Return our program-specific guarded effect
  return $ createProgramGuardedEffect condition trigger expiry effect

-- | Create a claim effect with appropriate guards
createClaimEffect ::
  (Member (Error AppError) r) =>
  EscrowId ->           -- Escrow to claim
  Address ->            -- Claimant address
  ByteString ->         -- Proof data
  Sem r ProgramGuardedEffect
createClaimEffect escrowId claimant proofData = do
  -- Create the claim effect
  let effect = ClaimEscrow escrowId claimant proofData
      -- Use a simple ResourceCondition as a placeholder
      condition = Guard.ResourceCondition "escrow-claim" claimant
      trigger = Guard.ApplyEffect
      expiry = Nothing
  
  -- Return our program-specific guarded effect
  return $ createProgramGuardedEffect condition trigger expiry effect

-- | Create a release effect with appropriate guards
createReleaseEffect ::
  (Member (Error AppError) r) =>
  EscrowId ->           -- Escrow to release
  Address ->            -- Releaser address (must be owner)
  Sem r ProgramGuardedEffect
createReleaseEffect escrowId releaser = do
  -- Create the release effect
  let effect = ReleaseEscrow escrowId releaser
      -- The escrow must exist
      condition = Guard.ResourceCondition "escrow-exists" releaser
      trigger = Guard.ApplyEffect
      expiry = Nothing
  
  -- Return our program-specific guarded effect
  return $ createProgramGuardedEffect condition trigger expiry effect

-- | Create an ownership transfer effect with appropriate guards
createOwnershipTransferEffect ::
  (Member (Error AppError) r) =>
  ProgramId ->          -- Program to transfer
  Address ->            -- New owner
  Address ->            -- Current owner
  Sem r ProgramGuardedEffect
createOwnershipTransferEffect programId newOwner currentOwner = do
  -- Create the ownership transfer effect
  let effect = TransferOwnership programId newOwner
      -- The current owner must be verified
      condition = Guard.ResourceCondition "program-ownership" currentOwner
      trigger = Guard.ApplyEffect
      expiry = Nothing
  
  -- Return our program-specific guarded effect
  return $ createProgramGuardedEffect condition trigger expiry effect

-- | Create an ownership verification effect
createOwnershipVerificationEffect ::
  (Member (Error AppError) r) =>
  Resource ->           -- Resource to verify
  Address ->            -- Expected owner
  Sem r ProgramGuardedEffect
createOwnershipVerificationEffect resource expectedOwner = do
  -- Create the ownership verification effect
  let effect = VerifyOwnership resource expectedOwner
      -- Always allow this effect (it's just a verification)
      condition = Guard.OrCondition [] -- Empty OrCondition always evaluates to true
      trigger = Guard.ApplyEffect
      expiry = Nothing
  
  -- Return our program-specific guarded effect
  return $ createProgramGuardedEffect condition trigger expiry effect

-- | Convert an effect to a ByteString (for serialization/hashing)
-- This is for compatibility with the TransitionMessage module
effectToByteString :: Effect -> ByteString
effectToByteString effect = 
  -- Use the Serialize instance to encode the effect
  S.encode effect 