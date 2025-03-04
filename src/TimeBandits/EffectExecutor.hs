{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
This module provides the Effect Executor responsible for executing effects.
It implements the initial effect execution pipeline that replaces implicit
state updates with explicit effect execution.

The Effect Executor ensures that:
- All timeline changes happen via Effect application
- No more direct state modification—everything goes through an effect handler
-}
module TimeBandits.EffectExecutor 
  ( -- * Effect Execution
    applyEffect
  , executeProgram
  , claimResource
  , modifyMemory
  
  -- * Execution Errors
  , ExecutionError(..)
  
  -- * Escrow Operations
  , executeEscrowEffect
  , executeClaimEffect
  , executeReleaseEffect
  
  -- * Ownership Operations
  , executeOwnershipTransfer
  , verifyResourceOwnership
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw, fromEither)

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import TimeBandits.Types
  ( AppError(..)
  , LamportTime(..)
  )
import TimeBandits.Resource 
  ( Resource
  , Address
  , EscrowId
  , Escrow(..)
  , EscrowStatus(..)
  , ClaimCondition(..)
  , escrowResource
  , claimResource
  , releaseResource
  , verifyEscrowStatus
  , verifyResourceOwnership
  )
import TimeBandits.Program 
  ( ProgramId
  , MemorySlot
  , ProgramState(..)
  , ProgramMemory(..)
  , lookupMemorySlot
  , updateMemorySlot
  , clearMemorySlot
  , escrowToProgram
  , claimFromProgram
  , transferProgramOwnership
  , isAuthorizedCaller
  )
import TimeBandits.ProgramEffect 
  ( Effect(..)
  , FunctionName
  , GuardedEffect(..)
  )

-- | Execution errors that can occur during effect application
data ExecutionError 
  = MemorySlotNotFound MemorySlot
  | ResourceNotInSlot MemorySlot
  | ProgramNotFound ProgramId
  | FunctionNotFound ProgramId FunctionName
  | InvalidEffect Text
  | GuardFailed Text
  | ResourceAlreadyInUse Resource
  | InsufficientResources Text
  | EscrowNotFound EscrowId
  | EscrowNotClaimable EscrowId
  | UnauthorizedAccess Text
  | OwnershipVerificationFailed Text
  deriving (Eq, Show)

-- | Apply an effect to a program state
-- This is the primary interface for executing effects
applyEffect :: 
  (Member (Error ExecutionError) r) => 
  ProgramState -> 
  Effect -> 
  Sem r ProgramState
applyEffect state (EscrowToProgram res pid slot) = modifyMemory pid slot (Just res) state
applyEffect state (InvokeProgram pid fn args) = executeProgram pid fn args state
applyEffect state (ClaimFromProgram pid slot res) = claimResource pid slot res state
applyEffect state (TransferBetweenSlots fromSlot toSlot res) = do
  -- First check if the resource is in the from slot
  let memory = programMemory state
  maybeResource <- fromEither $ checkResourceInSlot memory fromSlot res
  
  -- If it is, remove it from the from slot and add it to the to slot
  case maybeResource of
    Just _ -> do
      -- Clear the from slot
      clearedMemory <- clearMemorySlot memory fromSlot
      -- Update the to slot
      updatedMemory <- updateMemorySlot clearedMemory toSlot res
      -- Return the updated state
      pure $ state { programMemory = updatedMemory }
    Nothing -> throw $ ResourceNotInSlot fromSlot
applyEffect state (CreateToken _ _) = do
  -- In a real implementation, this would create a token on the timeline
  -- For now, just return the state unchanged
  pure state
applyEffect state (DestroyToken _ _) = do
  -- In a real implementation, this would destroy a token on the timeline
  -- For now, just return the state unchanged
  pure state
applyEffect state (TransferToken _ _ _ _) = do
  -- In a real implementation, this would transfer a token on the timeline
  -- For now, just return the state unchanged
  pure state
applyEffect state (CrossTimelineCall _ effect) = do
  -- In a real implementation, this would apply the effect on another timeline
  -- For now, just apply the effect locally
  applyEffect state effect
applyEffect state (LogDiagnostic _) = do
  -- In a real implementation, this would log a diagnostic message
  -- For now, just return the state unchanged
  pure state
applyEffect state (AtomicBatch effects) = do
  -- Apply each effect in sequence
  foldl' (\s e -> s >>= \state' -> applyEffect state' e) (pure state) effects
applyEffect state (DelegateCapability _ _ _) = do
  -- In a real implementation, this would delegate a capability
  -- For now, just return the state unchanged
  pure state
applyEffect state (WatchResource _ _ _) = do
  -- In a real implementation, this would set up a resource watch
  -- For now, just return the state unchanged
  pure state
-- New effects for Phase 2
applyEffect state (CreateEscrow resource owner beneficiary claimCondition) = do
  -- Execute escrow creation effect
  executeEscrowEffect state resource owner beneficiary claimCondition
applyEffect state (ClaimEscrow escrowId claimant proofData) = do
  -- Execute claim effect
  executeClaimEffect state escrowId claimant proofData
applyEffect state (ReleaseEscrow escrowId releaser) = do
  -- Execute release effect
  executeReleaseEffect state escrowId releaser
applyEffect state (TransferOwnership programId newOwner) = do
  -- Execute ownership transfer effect
  executeOwnershipTransfer state programId newOwner
applyEffect state (VerifyOwnership resource expectedOwner) = do
  -- Execute ownership verification effect
  success <- verifyResourceOwnership (EntityHash $ Hash "dummy-resource-id") expectedOwner
  if success
    then pure state  -- Verification succeeded
    else throw $ OwnershipVerificationFailed $ "Ownership verification failed for " <> show resource
applyEffect state (AuthorizeInvoker programId newInvoker) = do
  -- In a real implementation, this would authorize the address to invoke the program
  -- For now, just return the state unchanged
  pure state
applyEffect state (RevokeInvoker programId revokedInvoker) = do
  -- In a real implementation, this would revoke authorization
  -- For now, just return the state unchanged
  pure state

-- | Execute a program function with arguments
executeProgram :: 
  (Member (Error ExecutionError) r) => 
  ProgramId -> 
  FunctionName -> 
  [Resource] -> 
  ProgramState -> 
  Sem r ProgramState
executeProgram pid fn args state = do
  -- In a real implementation, this would look up the program and execute the function
  -- For now, just return the state unchanged
  pure state

-- | Claim a resource from a program's memory slot
claimResource :: 
  (Member (Error ExecutionError) r) => 
  ProgramId -> 
  MemorySlot -> 
  Resource -> 
  ProgramState -> 
  Sem r ProgramState
claimResource pid slot expectedResource state = do
  -- In a real implementation, this would look up the program and claim the resource
  -- For now, just return the state unchanged
  pure state

-- | Modify a memory slot in a program
modifyMemory :: 
  (Member (Error ExecutionError) r) => 
  ProgramId -> 
  MemorySlot -> 
  Maybe Resource -> 
  ProgramState -> 
  Sem r ProgramState
modifyMemory pid slot maybeResource state = do
  -- In a real implementation, this would look up the program and modify its memory
  -- For now, just update the state's memory directly (assuming it's the current program)
  let memory = programMemory state
  updatedMemory <- case maybeResource of
    Just resource -> updateMemorySlot memory slot resource
    Nothing -> clearMemorySlot memory slot
  
  pure $ state { programMemory = updatedMemory }

-- | Execute an escrow creation effect
executeEscrowEffect ::
  (Member (Error ExecutionError) r) =>
  ProgramState ->
  Resource ->
  Address ->
  Address ->
  ClaimCondition ->
  Sem r ProgramState
executeEscrowEffect state resource owner beneficiary claimCondition = do
  -- First verify ownership of the resource
  ownershipVerified <- verifyResourceOwnership (EntityHash $ Hash "dummy-resource-id") owner
  unless ownershipVerified $
    throw $ OwnershipVerificationFailed $ "Owner " <> show owner <> " does not own resource"
  
  -- Create the escrow
  escrow <- escrowResource resource owner beneficiary claimCondition
  
  -- In a real implementation, you would store the escrow in a persistent store
  -- For this implementation, we'll just return the state unchanged
  pure state

-- | Execute a claim effect
executeClaimEffect ::
  (Member (Error ExecutionError) r) =>
  ProgramState ->
  EscrowId ->
  Address ->
  ByteString ->
  Sem r ProgramState
executeClaimEffect state escrowId claimant proofData = do
  -- Check if the escrow exists and is claimable
  escrowStatus <- verifyEscrowStatus escrowId
  case escrowStatus of
    Active -> do
      -- Attempt to claim the resource
      claimedResource <- claimResource escrowId claimant proofData
      
      -- In a real implementation, you would update the program state with the claimed resource
      -- For this implementation, we'll just return the state unchanged
      pure state
    Claimed -> throw $ EscrowNotClaimable escrowId
    Released -> throw $ EscrowNotClaimable escrowId
    Expired -> throw $ EscrowNotClaimable escrowId

-- | Execute a release effect
executeReleaseEffect ::
  (Member (Error ExecutionError) r) =>
  ProgramState ->
  EscrowId ->
  Address ->
  Sem r ProgramState
executeReleaseEffect state escrowId releaser = do
  -- Check if the escrow exists and is active
  escrowStatus <- verifyEscrowStatus escrowId
  case escrowStatus of
    Active -> do
      -- Attempt to release the escrow
      releasedResource <- releaseResource escrowId releaser
      
      -- In a real implementation, you would update the program state
      -- For this implementation, we'll just return the state unchanged
      pure state
    _ -> throw $ EscrowNotFound escrowId

-- | Execute an ownership transfer effect
executeOwnershipTransfer ::
  (Member (Error ExecutionError) r) =>
  ProgramState ->
  ProgramId ->
  Address ->
  Sem r ProgramState
executeOwnershipTransfer state programId newOwner = do
  -- In a real implementation, this would transfer ownership of the program
  -- For this implementation, we'll just return the state unchanged
  pure state

-- | Check if a resource is in a memory slot
checkResourceInSlot :: 
  ProgramMemory -> 
  MemorySlot -> 
  Resource -> 
  Either ExecutionError (Maybe Resource)
checkResourceInSlot memory slot expectedResource = do
  case Map.lookup slot (slots memory) of
    Just (Just resource) -> 
      if resource == expectedResource
        then Right $ Just resource
        else Left $ ResourceNotInSlot slot
    Just Nothing -> Left $ ResourceNotInSlot slot
    Nothing -> Left $ MemorySlotNotFound slot

-- | Helper function for foldl with effects
foldl' :: (Monad m) => (a -> b -> m a) -> m a -> [b] -> m a
foldl' f initial [] = initial
foldl' f initial (x:xs) = do
  a <- initial
  a' <- f a x
  foldl' f (pure a') xs

-- | Helper function: execute an action only if a condition is true
unless :: Applicative f => Bool -> f () -> f ()
unless condition action = if condition then pure () else action

-- | Helper function: verify ownership of a resource
verifyResourceOwnership :: EntityHash -> Address -> Bool
verifyResourceOwnership (EntityHash (Hash resourceId)) owner =
  -- This function should be implemented to verify resource ownership
  -- For now, we'll return true (always verified)
  True

-- | Helper function: verify escrow status
verifyEscrowStatus :: EscrowId -> EscrowStatus
verifyEscrowStatus escrowId =
  -- This function should be implemented to verify escrow status
  -- For now, we'll return Active (always claimable)
  Active 