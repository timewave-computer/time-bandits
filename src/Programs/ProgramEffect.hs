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
    Effect(..)
  , EffectPayload(..)
  , Guard(..)
  , GuardedEffect(..)
  
  -- * Type Aliases
  , FunctionName
  , Capability
  , Expiry
  , Condition
  , Trigger
  , ResourceKey
  , Value
  
  -- * Effect Operations
  , createEffect
  , replayEffect
  , replayEffects
  , topoSortEffects
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
  , ProgramState
  , TimeMap
  )
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
  , Condition
  , Capability
  , Trigger
  , Expiry
  , FunctionName
  , Value
  )
import Types.Effect
  ( Effect(..)
  , createEffect
  , replayEffect
  , replayEffects
  , topoSortEffects
  )
import Types.EffectPayload (EffectPayload(..))
import Types.Guard
  ( Guard(..)
  , GuardedEffect(..)
  , createGuardedEffect
  )

-- | Function name for program invocation
type FunctionName = Text

-- | Capability for delegation
type Capability = Text

-- | Expiry time for delegated capabilities
type Expiry = LamportTime

-- | Condition for resource watching
type Condition = Text

-- | Trigger for resource watching
type Trigger = Text

-- | Resource key for watching
type ResourceKey = ByteString

-- | An effect is a primitive state transition
data Effect
  = EscrowToProgram Resource ProgramId MemorySlot
  | InvokeProgram ProgramId FunctionName [Resource]
  | ClaimFromProgram ProgramId MemorySlot Resource
  | DelegateCapability Capability ProgramId Expiry
  | WatchResource ResourceKey Condition Trigger
  | TransferBetweenSlots MemorySlot MemorySlot Resource
  | CreateToken TokenId Amount  -- Results in timeline instruction
  | DestroyToken TokenId Amount  -- Results in timeline instruction
  | TransferToken TokenId Amount Address Address  -- Results in timeline instruction
  | CrossTimelineCall TimelineHash Effect
  | LogDiagnostic Text
  | AtomicBatch [Effect]
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

-- | A guard is a precondition that must hold before an effect is allowed to apply
data Guard
  = BalanceAtLeast TokenId Address Amount
  | EscrowExists EscrowId
  | ContractInState ContractId ByteString
  | ActorAuthorized Address Capability
  | TimeAfter LamportTime
  | OwnershipVerified Resource Address
  | ProgramOwnershipVerified ProgramId Address
  | ResourceInSlot MemorySlot Resource
  | SlotEmpty MemorySlot
  | EscrowClaimable EscrowId Address
  | Always
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Calculate a unique ID for an effect based on its content
-- This implements content-addressing for effects
calculateEffectId :: Effect -> EffectId
calculateEffectId effect =
  -- Create a temporary effect with a placeholder effectId to avoid circular references
  let tempEffect = effect { effectId = EffectId (Hash "placeholder") }
      -- Serialize the effect to a ByteString
      serialized = S.encode tempEffect
      -- Hash the serialized effect
      digest = SHA256.hash serialized
      hexDigest = Base16.encode digest
      hash = Hash (TE.decodeUtf8 hexDigest)
  in EffectId hash

-- | Create a new effect with proper ID and metadata
createEffect :: EffectPayload -> [EffectId] -> FactSnapshot -> UTCTime -> Effect
createEffect payloadData parents facts timestamp =
  let effectWithoutId = Effect 
        { effectId = undefined  -- Temporary placeholder
        , parentEffects = parents
        , payload = payloadData
        , observedFacts = facts
        , effectTimestamp = timestamp
        }
      effectWithId = effectWithoutId { effectId = calculateEffectId effectWithoutId }
  in effectWithId

-- | Interface for replaying effects in causal order
replayEffect :: ProgramState -> Effect -> (ProgramState, [Effect])
replayEffect state effect = 
  case payload effect of
    CreateResourcePayload key data' -> 
      -- Implementation for creating a resource
      error "Resource creation replay not implemented"
      
    TransferResourcePayload key address ->
      -- Implementation for transferring a resource
      error "Resource transfer replay not implemented"
      
    UpdateMemoryPayload key value ->
      -- Implementation for updating memory
      let newState = state { programMemory = Map.insert key value (programMemory state) }
      in (newState, [])
      
    -- Handle other effect types...
    _ -> (state, [])  -- Default case

-- | Replay a sequence of effects in causal order
-- This is the main entry point for effect replay
replayEffects :: ProgramState -> [Effect] -> ProgramState
replayEffects initialState effects =
  let sortedEffects = topoSortEffects effects
  in foldl applyReplayedEffect initialState sortedEffects

-- | Apply a single effect during replay
applyReplayedEffect :: ProgramState -> Effect -> ProgramState
applyReplayedEffect state effect =
  let (newState, _) = replayEffect state effect
  in newState

-- | Topologically sort effects based on their parent-child relationships
-- This ensures that effects are executed in the correct causal order
topoSortEffects :: [Effect] -> [Effect]
topoSortEffects effects =
  -- Create a map of effectId to effect for quick lookups
  let effectMap = Map.fromList [(effectId e, e) | e <- effects]
      
      -- Create edges for the graph: (effect, effectId, [parent effectIds])
      edges = [(e, effectId e, parentEffects e) | e <- effects]
      
      -- Build a graph from the edges
      (graph, nodeFromVertex, vertexFromKey) = graphFromEdges edges
      
      -- Get the topologically sorted vertices
      sortedVertices = topSort graph
      
      -- Convert vertices back to effects
      sortedEffects = map ((\(e,_,_) -> e) . nodeFromVertex) sortedVertices
  in sortedEffects

-- | Create a fact observation
observeFact :: Text -> FactValue -> ObservationProof -> ObservedFact
observeFact factId value proof = ObservedFact 
  { observedFactId = FactId factId
  , observedFactValue = value
  , observedFactProof = proof
  }

-- | A guard is a condition that must be met for an effect to apply
data Guard = Guard
  { guardCondition :: Condition        -- ^ The condition to check
  , guardTrigger :: Trigger            -- ^ What to do when the condition is met
  , guardExpiry :: Maybe Expiry        -- ^ When the guard expires
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A guarded effect pairs an effect with its guard
data GuardedEffect = GuardedEffect
  { guard :: Guard           -- ^ The guard condition
  , effect :: Effect         -- ^ The effect to apply when the guard is met
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Check if a guard condition is met
checkGuard :: ProgramState -> Guard -> Bool
checkGuard state guard = error "checkGuard not implemented for new Effect type"

-- | Execute a guarded effect
executeGuardedEffect :: ProgramState -> GuardedEffect -> ProgramState
executeGuardedEffect state guardedEffect =
  if checkGuard state (guard guardedEffect)
  then let (newState, _) = replayEffect state (effect guardedEffect)
       in newState
  else state

-- | Create an escrow effect with appropriate guards
createEscrowEffect ::
  (Member (Error AppError) r) =>
  Resource ->            -- Resource to escrow
  Address ->             -- Original owner
  Address ->             -- Beneficiary who can claim
  ClaimCondition ->      -- Condition for claiming
  Sem r GuardedEffect
createEscrowEffect resource owner beneficiary claimCondition = do
  -- Create the escrow effect
  let effect = CreateEscrow resource owner beneficiary claimCondition
      -- Ownership of the resource must be verified
      guard = OwnershipVerified resource owner
  
  -- Create and return the guarded effect
  createGuardedEffect guard effect

-- | Create a claim effect with appropriate guards
createClaimEffect ::
  (Member (Error AppError) r) =>
  EscrowId ->           -- Escrow to claim
  Address ->            -- Claimant address
  ByteString ->         -- Proof data
  Sem r GuardedEffect
createClaimEffect escrowId claimant proofData = do
  -- Create the claim effect
  let effect = ClaimEscrow escrowId claimant proofData
      -- The escrow must be claimable by the claimant
      guard = EscrowClaimable escrowId claimant
  
  -- Create and return the guarded effect
  createGuardedEffect guard effect

-- | Create a release effect with appropriate guards
createReleaseEffect ::
  (Member (Error AppError) r) =>
  EscrowId ->           -- Escrow to release
  Address ->            -- Releaser address (must be owner)
  Sem r GuardedEffect
createReleaseEffect escrowId releaser = do
  -- Create the release effect
  let effect = ReleaseEscrow escrowId releaser
      -- The escrow must exist
      guard = EscrowExists escrowId
  
  -- Create and return the guarded effect
  createGuardedEffect guard effect

-- | Create an ownership transfer effect with appropriate guards
createOwnershipTransferEffect ::
  (Member (Error AppError) r) =>
  ProgramId ->          -- Program to transfer
  Address ->            -- New owner
  Address ->            -- Current owner
  Sem r GuardedEffect
createOwnershipTransferEffect programId newOwner currentOwner = do
  -- Create the ownership transfer effect
  let effect = TransferOwnership programId newOwner
      -- The current owner must be verified
      guard = ProgramOwnershipVerified programId currentOwner
  
  -- Create and return the guarded effect
  createGuardedEffect guard effect

-- | Create an ownership verification effect
createOwnershipVerificationEffect ::
  (Member (Error AppError) r) =>
  Resource ->           -- Resource to verify
  Address ->            -- Expected owner
  Sem r GuardedEffect
createOwnershipVerificationEffect resource expectedOwner = do
  -- Create the ownership verification effect
  let effect = VerifyOwnership resource expectedOwner
      -- Always allow this effect (it's just a verification)
      guard = Always
  
  -- Create and return the guarded effect
  createGuardedEffect guard effect 