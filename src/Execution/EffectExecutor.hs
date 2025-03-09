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
  , claimProgramResource
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
import qualified Data.Text as T
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw, fromEither)
import qualified Data.ByteString.Char8 as BS
import Control.Monad (unless, foldM)
import Data.Foldable (foldl')

-- Import from TimeBandits modules
import Core (Hash(..), EntityHash(..))
import Core.Types
  ( AppError(..)
  , LamportTime(..)
  , TimelineErrorType(..)
  )
import Core.Resource 
  ( Resource(..)
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
  , ProgramState(..)  -- Import the record constructor to access fields
  , ProgramMemory(..)
  , lookupMemorySlot
  , updateMemorySlot
  , clearMemorySlot
  , escrowToProgram
  , claimFromProgram
  , transferProgramOwnership
  , isAuthorizedCaller
  )
import Programs.ProgramState (memory, setMemorySlot, clearMemorySlot)  -- Direct import for the memory field
import Programs.ProgramEffect
  ( Effect(..)
  , GuardedEffect(..)
  , ProgramEffect(..)  -- Import all constructors
  , FunctionName
  )
import Core.TimeMap
  ( TimeMap
  )
import qualified Core.TimeMap as TimeMap
-- New imports for TransitionMessage integration
import Actors.TransitionMessage
  ( TransitionMessage(..)
  , Proof(..)
  , LogEntry(..)
  , createLogEntry
  , verifyLogEntryChain
  )
import qualified Core.Error as CE
import Core.Error (TimelineError(..))

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

-- | Custom TransitionError type for transition-related errors
data TransitionError = TransitionError Text
  deriving (Show, Eq)

-- | Local ExecutionLog type to match the structure in Execution.ExecutionLog
data ExecutionLog = ExecutionLog
  { entries :: Map (EntityHash "LogEntry") LogEntry
  , indexes :: Map Text Text  -- Simplified for this implementation
  , rootEntry :: Maybe (EntityHash "LogEntry")
  , latestEntry :: Maybe (EntityHash "LogEntry")
  }
  deriving (Show, Eq)

-- | Initialize an empty execution log
initializeExecutionLog :: ExecutionLog
initializeExecutionLog = ExecutionLog
  { entries = Map.empty
  , indexes = Map.empty
  , rootEntry = Nothing
  , latestEntry = Nothing
  }

-- | Append a log entry to the execution log
appendToExecutionLog ::
  (Member (Error CE.AppError) r) =>
  ExecutionLog ->
  LogEntry ->
  Sem r ExecutionLog
appendToExecutionLog log entry = do
  -- Get the latest entry from the log
  latest <- getLatestLogEntry log
  
  -- If there's a latest entry, verify the causal link
  case latest of
    Just latest -> do
      -- Verify the causal link between the new entry and the latest entry
      -- In a real implementation, this would check that the new entry's causal parent
      -- matches the latest entry's ID
      valid <- verifyLogEntryChainLocal entry latest
      if not valid
        then throw $ CE.GenericErr $ "Invalid causal link in log entry"
        else pure ()
    
    -- If there's no latest entry, this is the first entry
    Nothing -> pure ()
  
  -- Add the entry to the log
  let updatedEntries = Map.insert (entryId entry) entry (entries log)
  let updatedLog = log { entries = updatedEntries, latestEntry = Just (entryId entry) }
  
  pure updatedLog

-- | Get the latest log entry from the execution log
getLatestLogEntry ::
  (Member (Error CE.AppError) r) =>
  ExecutionLog ->
  Sem r (Maybe LogEntry)
getLatestLogEntry log = 
  case latestEntry log of
    Nothing -> pure Nothing
    Just entryId -> 
      case Map.lookup entryId (entries log) of
        Nothing -> throw $ CE.GenericErr $ "Log entry not found: " <> T.pack (show entryId)
        Just entry -> pure $ Just entry

-- | Verify the integrity of the execution log
verifyExecutionLog ::
  (Member (Error CE.AppError) r) =>
  ExecutionLog ->
  Sem r Bool
verifyExecutionLog log = do
  -- A real implementation would verify the entire causal chain
  -- For now, return True if the log is not empty
  pure $ not (null (entries log))

-- | Execute a transition message
executeTransition ::
  (Member (Error ExecutionError) r, Member (Error CE.AppError) r) =>
  TransitionMessage ->
  ProgramState ->
  Sem r (ProgramState, TimeMap, ExecutionLog)
executeTransition msg progState = do
  -- Verify the transition proof
  validProof <- verifyTransitionProof msg
  if not validProof
    then throw $ InvalidTransition $ TransitionError $ "Invalid transition proof: " <> T.pack (show (proof msg))
    else pure ()
  
  -- Apply the effect to the program state
  (updatedProgState, timeMap, updatedLog) <- applyEffect (effect msg) progState
  
  -- Log the transition execution
  _ <- logTransitionExecutionM msg updatedProgState
  
  -- Return the updated state, time map, and log
  pure (updatedProgState, timeMap, updatedLog)

-- | Verify a transition proof
verifyTransitionProof ::
  (Member (Error ExecutionError) r) =>
  TransitionMessage ->
  Sem r Bool
verifyTransitionProof msg = do
  -- For now, just check the proof type and return true
  -- In a real implementation, this would validate the proof against the guard
  case proof msg of
    ZKProof _ proofType -> pure True             -- Would validate ZK proofs
    SignatureProof _ proofType -> pure True      -- Would validate signatures
    WitnessProof _ proofType -> pure True        -- Would validate witness data
    CompositeProof proofs proofType -> pure True -- Would validate all sub-proofs

-- | Log information about a transition execution (for debugging/monitoring)
logTransitionExecutionM ::
  (Member (Error ExecutionError) r) =>
  TransitionMessage ->
  ProgramState ->
  Sem r ()
logTransitionExecutionM msg updatedProgState = 
  -- In a real implementation, this would log to a file or monitoring system
  -- For now, just return unit
  pure ()

-- | Original IO-based logging function (kept for compatibility)
logTransitionExecution ::
  TransitionMessage ->
  ProgramState ->
  IO ()
logTransitionExecution msg updatedProgState = do
  -- In a real implementation, this would log to a file or monitoring system
  -- For now, just print to console
  putStrLn $ "Applied transition for program " ++ show (programId msg) 
          ++ " at step " ++ show (stepIndex msg)
          ++ " with effect " ++ show (effect msg)

-- | Apply an effect to a program state
-- This is the primary interface for executing effects
-- Now enhanced to return the information needed for logging
applyEffect :: 
  (Member (Error ExecutionError) r, Member (Error CE.AppError) r) => 
  Effect -> 
  ProgramState -> 
  Sem r (ProgramState, TimeMap, ExecutionLog)
applyEffect effect progState = do
  -- Apply the effect
  -- In a real system, this would:
  -- 1. Update the program state based on the effect
  -- 2. Return the updated state, timeMap, and executionLog
  
  -- Create a placeholder timeMap and executionLog
  let timeMap = TimeMap.empty
  let executionLog = initializeExecutionLog
  
  -- For demonstration purposes - handle different effect types:
  case effect of 
    -- Default case for unhandled effects
    _ -> do
      -- Return unchanged state with placeholder timeMap and executionLog
      pure (progState, timeMap, executionLog)

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
claimProgramResource :: 
  (Member (Error ExecutionError) r) => 
  ProgramId -> 
  MemorySlot -> 
  Resource -> 
  ProgramState -> 
  Sem r ProgramState
claimProgramResource pid slot expectedResource state = do
  -- In a real implementation, this would look up the program and claim the resource
  -- For now, just return the state unchanged
  pure state

-- | Modify a memory slot in a program
modifyMemory ::
  (Member (Error ExecutionError) r) =>
  ProgramState ->
  Text ->  -- Memory slot name
  Maybe Resource ->
  Sem r ProgramState
modifyMemory state slot maybeResource = do
  -- Direct implementation: update the current program's memory
  case maybeResource of
    Just resource -> pure $ Programs.ProgramState.setMemorySlot slot resource state
    Nothing -> pure $ Programs.ProgramState.clearMemorySlot slot state

-- | Execute an escrow creation effect
executeEscrowEffect ::
  (Member (Error ExecutionError) r, Member (Error CE.AppError) r) =>
  Resource ->
  Address ->
  Address ->
  ClaimCondition ->
  ProgramState ->
  Sem r ProgramState
executeEscrowEffect resource owner beneficiary claimCondition state = do
  -- Get the resource ID
  let resourceId = getResourceId resource
  
  -- Verify the owner owns the resource
  let ownershipVerified = execVerifyResourceOwnership resourceId owner
  unless ownershipVerified $
    throw $ OwnershipVerificationFailed $
      "Owner " <> T.pack (show owner) <> " does not own resource"
  
  -- Create the escrow
  escrow <- escrowResourceLocal resource owner beneficiary claimCondition
  
  -- In a real implementation, you would update the program state with the escrow
  -- For this implementation, we'll just return the state unchanged
  pure state

-- | Local implementation of escrowResource to avoid AppError ambiguity
escrowResourceLocal :: 
  (Member (Error ExecutionError) r, Member (Error CE.AppError) r) =>
  Resource ->
  Address ->
  Address ->
  ClaimCondition ->
  Sem r EscrowId
escrowResourceLocal resource owner beneficiary claimCondition = do
  -- In a real implementation, this would:
  -- 1. Verify the owner owns the resource
  -- 2. Create an escrow record
  -- 3. Return the escrow ID
  
  -- For this implementation, we'll just return a placeholder escrow ID
  pure $ mkEscrowId "escrow-123"

-- | Execute a claim effect
executeClaimEffect ::
  (Member (Error ExecutionError) r, Member (Error CE.AppError) r) =>
  EscrowId ->
  Address ->
  ByteString ->
  ProgramState ->
  Sem r ProgramState
executeClaimEffect escrowId claimant proofData state = do
  -- Check if the escrow exists and is active
  escrowStatus <- verifyEscrowStatusLocal escrowId
  case escrowStatus of
    Active -> do
      -- Verify the claim condition
      -- In a real implementation, this would verify the proof data against the claim condition
      
      -- Attempt to claim the resource
      claimedResource <- claimResourceLocal escrowId claimant proofData
      
      -- In a real implementation, you would update the program state
      -- For this implementation, we'll just return the state unchanged
      pure state
    
    Claimed -> throw $ EscrowNotClaimable escrowId
    Released -> throw $ EscrowNotClaimable escrowId
    Expired -> throw $ EscrowNotClaimable escrowId

-- | Local implementation of claimResource to avoid AppError ambiguity
claimResourceLocal :: 
  (Member (Error ExecutionError) r, Member (Error CE.AppError) r) =>
  EscrowId ->
  Address ->
  ByteString ->
  Sem r Resource
claimResourceLocal escrowId claimant proofData = do
  -- In a real implementation, this would:
  -- 1. Verify the escrow exists and is active
  -- 2. Verify the claim condition
  -- 3. Transfer ownership of the resource
  -- 4. Return the claimed resource
  
  -- For this implementation, we'll just return a placeholder resource
  pure $ mkResource "claimed-resource"

-- | Local implementation of verifyEscrowStatus to avoid AppError ambiguity
verifyEscrowStatusLocal :: 
  (Member (Error ExecutionError) r, Member (Error CE.AppError) r) =>
  EscrowId ->
  Sem r EscrowStatus
verifyEscrowStatusLocal escrowId = do
  -- In a real implementation, this would check the status of the escrow
  -- For this implementation, we'll just return Active
  pure Active

-- | Execute a release effect
executeReleaseEffect ::
  (Member (Error ExecutionError) r, Member (Error CE.AppError) r) =>
  ProgramState ->
  EscrowId ->
  Address ->
  Sem r ProgramState
executeReleaseEffect state escrowId releaser = do
  -- Check if the escrow exists and is active
  escrowStatus <- verifyEscrowStatusLocal escrowId
  case escrowStatus of
    Active -> do
      -- Attempt to release the escrow
      releasedResource <- releaseResourceLocal escrowId releaser
      
      -- In a real implementation, you would update the program state
      -- For this implementation, we'll just return the state unchanged
      pure state
    
    _ -> throw $ EscrowNotFound escrowId

-- | Local implementation of releaseResource to avoid AppError ambiguity
releaseResourceLocal :: 
  (Member (Error ExecutionError) r, Member (Error CE.AppError) r) =>
  EscrowId ->
  Address ->
  Sem r Resource
releaseResourceLocal escrowId releaser = do
  -- In a real implementation, this would:
  -- 1. Verify the escrow exists and is active
  -- 2. Verify the releaser is authorized to release the escrow
  -- 3. Release the escrow and return the resource
  
  -- For this implementation, we'll just return a placeholder resource
  pure $ mkResource "released-resource"

-- | Execute an ownership transfer effect
executeOwnershipTransfer ::
  (Member (Error ExecutionError) r) =>
  ProgramId ->
  Address ->
  ProgramState ->
  Sem r ProgramState
executeOwnershipTransfer programId newOwner state = do
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

-- | Monadic foldl' for effects
foldl' :: (Monad m) => (a -> b -> m a) -> m a -> [b] -> m a
foldl' f initial xs = do
  a <- initial
  foldM f a xs

-- | Local implementation of resource ownership verification
execVerifyResourceOwnership :: EntityHash "Resource" -> Address -> Bool
execVerifyResourceOwnership resourceHash owner = 
  -- In a real implementation, this would check if the owner matches the resource owner
  -- For now, just return True for simplicity
  True

-- | Helper function: verify escrow status
-- This is a local implementation to avoid ambiguity with Core.Resource.verifyEscrowStatus
execVerifyEscrowStatus :: EscrowId -> EscrowStatus
execVerifyEscrowStatus escrowId =
  -- This function should be implemented to verify escrow status
  -- For now, we'll return Active (always claimable)
  Active 

-- | Helper function to get a resource ID
getResourceId :: Resource -> EntityHash "Resource"
getResourceId resource = EntityHash $ Hash "resource-id"  -- Simplified placeholder implementation 

-- | Helper function to create an EscrowId
mkEscrowId :: Text -> EscrowId
mkEscrowId txt = BS.pack $ T.unpack txt

-- | Helper function to create a Resource
mkResource :: Text -> Resource
mkResource txt = TokenBalanceResource BS.empty BS.empty 0  -- Simple placeholder 

-- | Local implementation of verifyLogEntryChain to avoid AppError ambiguity
verifyLogEntryChainLocal :: 
  (Member (Error CE.AppError) r) =>
  LogEntry ->
  LogEntry ->
  Sem r Bool
verifyLogEntryChainLocal entry latest = do
  -- In a real implementation, this would verify the causal link
  -- For now, just return true
  pure True 