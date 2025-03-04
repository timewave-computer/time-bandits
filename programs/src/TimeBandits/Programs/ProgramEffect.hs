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
module TimeBandits.ProgramEffect 
  ( -- * Core Types
    Effect(..)
  , Guard(..)
  , GuardedEffect(..)
  , FunctionName
  , Capability
  , Expiry
  , Condition
  , Trigger
  , ResourceKey
  
  -- * Effect Operations
  , createEffect
  , applyEffect
  , checkGuard
  , createGuardedEffect
  
  -- * Effect Execution
  , executeEffect
  , executeGuardedEffect
  
  -- * Escrow Effects
  , createEscrowEffect
  , createClaimEffect
  , createReleaseEffect
  
  -- * Ownership Effects
  , createOwnershipTransferEffect
  , createOwnershipVerificationEffect
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import TimeBandits.Types
  ( AppError(..)
  , LamportTime(..)
  )
import TimeBandits.Resource 
  ( Resource
  , Address
  , Amount
  , TokenId
  , ContractId
  , EscrowId
  , ClaimCondition(..)
  )
import TimeBandits.Program 
  ( ProgramId
  , MemorySlot
  , ProgramState
  , TimeMap
  )
import TimeBandits.Timeline (TimelineHash, TimelineClock(..))

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

-- | A guarded effect is an effect paired with a guard
data GuardedEffect = GuardedEffect Guard Effect
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create an effect
createEffect :: 
  (Member (Error AppError) r) => 
  Effect -> 
  Sem r Effect
createEffect effect = do
  -- In a real implementation, this might perform validation or normalization
  pure effect

-- | Apply an effect to a program state
applyEffect ::
  (Member (Error AppError) r) =>
  ProgramState ->
  Effect ->
  Sem r ProgramState
applyEffect state effect = do
  -- In a real implementation, this would apply the effect to the state
  -- For now, just return the state unchanged
  pure state

-- | Check if a guard condition holds
checkGuard ::
  (Member (Error AppError) r) =>
  TimeMap ->
  Guard ->
  Sem r Bool
checkGuard _ Always = pure True
checkGuard timeMap (TimeAfter requiredTime) = do
  -- Check if all timeline clocks are after the required time
  -- For now, just return True
  pure True
checkGuard _ (OwnershipVerified _ _) = pure True  -- Would verify ownership
checkGuard _ (ProgramOwnershipVerified _ _) = pure True  -- Would verify program ownership
checkGuard _ (ResourceInSlot _ _) = pure True  -- Would check if resource is in slot
checkGuard _ (SlotEmpty _) = pure True  -- Would check if slot is empty
checkGuard _ (EscrowClaimable _ _) = pure True  -- Would check if escrow is claimable
checkGuard _ _ = pure True  -- Other guards would be implemented in a real system

-- | Create a guarded effect
createGuardedEffect ::
  (Member (Error AppError) r) =>
  Guard ->
  Effect ->
  Sem r GuardedEffect
createGuardedEffect guard effect = do
  pure $ GuardedEffect guard effect

-- | Execute an effect
executeEffect ::
  (Member (Error AppError) r) =>
  ProgramState ->
  Effect ->
  Sem r ProgramState
executeEffect state effect = do
  -- In a real implementation, this would execute the effect
  -- For now, just return the state unchanged
  pure state

-- | Execute a guarded effect
executeGuardedEffect ::
  (Member (Error AppError) r) =>
  ProgramState ->
  GuardedEffect ->
  Sem r ProgramState
executeGuardedEffect state (GuardedEffect guard effect) = do
  -- Check the guard
  guardHolds <- checkGuard (programTimeMap state) guard
  if guardHolds
    then executeEffect state effect
    else throw $ "Guard condition failed for effect" :: AppError

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