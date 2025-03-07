{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module: TimeBandits.EffectExecutor
Description: Effect execution and logging system for Time-Bandits.

This module provides the Effect Executor responsible for executing effects.
It implements the initial effect execution pipeline that replaces implicit
state updates with explicit effect execution.

The Effect Executor ensures that:
- All timeline changes happen via Effect application
- No more direct state modification—everything goes through an effect handler
- All effects are logged in a causally-linked execution log
- All transitions are validated against their proofs

In the Time-Bandits architecture, the EffectExecutor plays several crucial roles:

1. Security Boundary: It acts as the primary security boundary between programs
   and timeline state, ensuring that effects are properly authorized and validated.

2. Execution Log: It maintains a causally-linked execution log that provides
   auditability and enables verification of the entire execution history.

3. Resource Management: It handles resource ownership operations, including
   escrow, claims, and transfers between programs and timelines.

4. Proof Verification: It verifies cryptographic proofs attached to transition
   messages, ensuring the integrity of cross-timeline operations.

The EffectExecutor connects the Programs module (which defines what should happen)
with the Timeline and Adapter modules (which implement how effects are applied
to specific timelines). It serves as the execution engine of the Time-Bandits system.
-}
module Execution.EffectExecutor 
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
  
  -- * Transition Message Handling
  , executeTransition
  , verifyTransitionProof
  , logTransitionExecution
  
  -- * Execution Log
  , ExecutionLog
  , initializeExecutionLog
  , appendToExecutionLog
  , getLatestLogEntry
  , verifyExecutionLog
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw, fromEither)
import qualified Data.ByteString.Char8 as BS

-- Import from TimeBandits modules
import Core (Hash(..), EntityHash(..))
import Core.Types
  ( AppError(..)
  , LamportTime(..)
  )
import Core.Resource 
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
import Programs.Program 
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
import Programs.ProgramEffect 
  ( Effect(..)
  , FunctionName
  , GuardedEffect(..)
  )
import Core.TimeMap
  ( TimeMap
  , TimeMapId
  , updateTimeMap
  , verifyTimeMapConsistency
  )
-- New imports for TransitionMessage integration
import Actors.TransitionMessage
  ( TransitionMessage(..)
  , Proof(..)
  , LogEntry(..)
  , createLogEntry
  , verifyLogEntryChain
  , TransitionError(..)
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
  | InvalidTransition TransitionError  -- New error type for invalid transitions
  | ExecutionLogError Text             -- New error type for log operations
  deriving (Eq, Show)

-- | An execution log is a collection of log entries forming a causal chain
data ExecutionLog = ExecutionLog
  { logEntries :: [LogEntry]
  , latestEntry :: Maybe LogEntry
  }
  deriving (Eq, Show)

-- | Initialize an empty execution log
initializeExecutionLog :: ExecutionLog
initializeExecutionLog = ExecutionLog
  { logEntries = []
  , latestEntry = Nothing
  }

-- | Append a log entry to the execution log
appendToExecutionLog ::
  (Member (Error AppError) r) =>
  ExecutionLog ->
  LogEntry ->
  Sem r ExecutionLog
appendToExecutionLog log entry = do
  -- Verify the entry can be appended
  case latestEntry log of
    Nothing -> pure ()  -- First entry, no verification needed
    Just latest -> do
      -- Verify causal link
      valid <- verifyLogEntryChain entry latest
      if not valid
        then throw $ AppError "Invalid causal link in log entry"
        else pure ()
  
  -- Append the entry
  pure $ log
    { logEntries = entry : logEntries log
    , latestEntry = Just entry
    }

-- | Get the latest log entry from the execution log
getLatestLogEntry ::
  (Member (Error AppError) r) =>
  ExecutionLog ->
  Sem r (Maybe LogEntry)
getLatestLogEntry log = pure $ latestEntry log

-- | Verify the integrity of the execution log
verifyExecutionLog ::
  (Member (Error AppError) r) =>
  ExecutionLog ->
  Sem r Bool
verifyExecutionLog log = do
  -- A real implementation would verify the entire causal chain
  -- For now, return True if the log is not empty
  pure $ not (null (logEntries log))

-- | Execute a transition by validating and applying the transition message
executeTransition ::
  (Member (Error AppError) r) =>
  TransitionMessage ->
  ProgramState ->
  TimeMap ->
  ExecutionLog ->
  Sem r (ProgramState, TimeMap, ExecutionLog)
executeTransition msg progState timeMap execLog = do
  -- Verify the transition proof
  validProof <- verifyTransitionProof msg progState
  if not validProof
    then throw $ AppError $ "Invalid transition proof: " <> show (proof msg)
    else pure ()
  
  -- Apply the effect
  (updatedProgState, effect, appliedAt) <- applyEffect (effect msg) progState timeMap
  
  -- Create a log entry for the applied effect
  let resultStateHash = BS.pack "hash-of-updated-state"  -- Placeholder: should hash the updated state
  let parentEntryId = case latestEntry execLog of
        Nothing -> EntityHash $ Hash "genesis"  -- Genesis entry has no parent
        Just parent -> entryId parent
  
  logEntry <- createLogEntry effect appliedAt parentEntryId resultStateHash
  
  -- Append the log entry to the execution log
  updatedLog <- appendToExecutionLog execLog logEntry
  
  -- Log the execution (for debugging/monitoring)
  logTransitionExecution msg logEntry
  
  -- Return the updated state, time map, and log
  pure (updatedProgState, timeMap, updatedLog)

-- | Verify a transition proof against a program state
verifyTransitionProof ::
  (Member (Error AppError) r) =>
  TransitionMessage ->
  ProgramState ->
  Sem r Bool
verifyTransitionProof msg progState = do
  -- In a real implementation, this would:
  -- 1. Validate ZK proofs
  -- 2. Verify signatures
  -- 3. Check against program state
  
  -- Placeholder implementation
  case proof msg of
    ZKProof _ -> pure True             -- Would validate ZK proofs
    SignatureProof _ -> pure True      -- Would verify signatures
    WitnessProof _ -> pure True        -- Would verify witness data
    CompositeProof proofs -> pure True -- Would verify all component proofs
  
-- | Log information about a transition execution (for debugging/monitoring)
logTransitionExecution ::
  TransitionMessage ->
  LogEntry ->
  IO ()
logTransitionExecution msg logEntry = do
  -- In a real implementation, this would log to a file or monitoring system
  -- For now, just print to console
  putStrLn $ "Applied transition for program " ++ show (programId msg) 
          ++ " at step " ++ show (stepIndex msg)
          ++ " with effect " ++ show (appliedEffect logEntry)

-- | Apply an effect to a program state
-- This is the primary interface for executing effects
-- Now enhanced to return the information needed for logging
applyEffect :: 
  (Member (Error AppError) r) => 
  Effect -> 
  ProgramState -> 
  TimeMap -> 
  Sem r (ProgramState, Effect, LamportTime)
applyEffect effect progState timeMap = do
  -- Apply the effect (existing implementation)
  
  -- Placeholder implementation - in a real system, this would:
  -- 1. Update the program state based on the effect
  -- 2. Return the updated state, applied effect, and the time of application
  
  -- For demonstration purposes:
  case effect of
    -- ... existing effect handlers ...
    
    -- Handle escrow operations
    CreateEscrow resource from to condition -> do
      -- Execute escrow creation
      updatedState <- executeEscrowEffect resource from to condition progState
      pure (updatedState, effect, LamportTime 42)  -- Placeholder time
    
    ClaimEscrow escrowId addr proof -> do
      -- Execute claim
      updatedState <- executeClaimEffect escrowId addr proof progState
      pure (updatedState, effect, LamportTime 42)  -- Placeholder time
    
    ReleaseEscrow escrowId addr -> do
      -- Execute release
      updatedState <- executeReleaseEffect escrowId addr progState
      pure (updatedState, effect, LamportTime 42)  -- Placeholder time
    
    -- Handle ownership operations
    TransferOwnership programId newOwner -> do
      -- Execute ownership transfer
      updatedState <- executeOwnershipTransfer programId newOwner progState
      pure (updatedState, effect, LamportTime 42)  -- Placeholder time
    
    VerifyOwnership resource owner -> do
      -- Verify resource ownership
      result <- verifyResourceOwnership resource owner
      -- Return the unchanged state (verification doesn't modify state)
      pure (progState, effect, LamportTime 42)  -- Placeholder time
    
    -- Default case for unhandled effects
    _ -> do
      throw $ AppError $ "Unhandled effect: " <> show effect

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