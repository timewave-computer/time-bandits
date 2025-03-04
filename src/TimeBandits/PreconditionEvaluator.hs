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
This module provides the PreconditionEvaluator, which centralizes all precondition
checks for effects in the Time Bandits system. It ensures that all guard conditions
are evaluated consistently across the system.

The PreconditionEvaluator:
1. Evaluates guard conditions against the current time map and program state
2. Provides a unified interface for precondition checking
3. Ensures consistent evaluation of guard conditions
4. Supports formal verification of precondition schemas
-}
module TimeBandits.PreconditionEvaluator 
  ( -- * Core Types
    PreconditionEvaluator(..)
  , PreconditionResult(..)
  , PreconditionError(..)
  
  -- * Evaluator Operations
  , createEvaluator
  , evaluatePrecondition
  , evaluateGuard
  , evaluateGuardedEffect
  
  -- * Guard Evaluation
  , checkBalanceGuard
  , checkEscrowGuard
  , checkContractStateGuard
  , checkAuthorizationGuard
  , checkTimeGuard
  , checkOwnershipGuard
  , checkProgramOwnershipGuard
  , checkResourceInSlotGuard
  , checkSlotEmptyGuard
  , checkEscrowClaimableGuard
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as Data.ByteString
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
import TimeBandits.ProgramEffect
  ( Effect(..)
  , Guard(..)
  , GuardedEffect(..)
  )
import TimeBandits.Program
  ( ProgramId
  , ProgramState(..)
  , TimeMap
  , ProgramMemory(..)
  , MemorySlot
  , lookupMemorySlot
  , programMemory
  )
import TimeBandits.Resource
  ( Resource(..)
  , ResourceHash
  , TokenId
  , Address
  , Amount
  , ContractId
  , EscrowId
  )
import TimeBandits.ResourceLedger
  ( ResourceLedger
  )

-- | Error types specific to precondition evaluation
data PreconditionError
  = BalanceInsufficient TokenId Address Amount Amount  -- Expected vs Actual
  | EscrowNotFound EscrowId
  | ContractStateInvalid ContractId ByteString ByteString  -- Expected vs Actual
  | ActorNotAuthorized Address
  | TimeConditionNotMet LamportTime LamportTime  -- Required vs Current
  | OwnershipVerificationFailed ResourceHash Address
  | ProgramOwnershipVerificationFailed ProgramId Address
  | ResourceNotInSlot MemorySlot
  | SlotNotEmpty MemorySlot
  | EscrowNotClaimable EscrowId Address
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Result of precondition evaluation
data PreconditionResult
  = PreconditionSatisfied
  | PreconditionFailed PreconditionError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | The PreconditionEvaluator centralizes all guard condition checks
data PreconditionEvaluator = PreconditionEvaluator
  { -- | Current time map for time-based conditions
    timeMap :: TimeMap
    -- | Current program state for memory-based conditions
  , programState :: ProgramState
    -- | Resource ledger for ownership verification
  , resourceLedger :: ResourceLedger
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new precondition evaluator
createEvaluator :: 
  (Member (Error AppError) r) => 
  TimeMap -> 
  ProgramState -> 
  ResourceLedger -> 
  Sem r PreconditionEvaluator
createEvaluator timeMap programState resourceLedger = do
  pure $ PreconditionEvaluator
    { timeMap = timeMap
    , programState = programState
    , resourceLedger = resourceLedger
    }

-- | Evaluate a precondition
evaluatePrecondition :: 
  (Member (Error AppError) r) => 
  PreconditionEvaluator -> 
  Guard -> 
  Sem r PreconditionResult
evaluatePrecondition evaluator = \case
  -- Balance conditions
  BalanceAtLeast tokenId address requiredAmount ->
    checkBalanceGuard evaluator tokenId address requiredAmount
  
  -- Escrow conditions
  EscrowExists escrowId ->
    checkEscrowGuard evaluator escrowId
  
  -- Contract state conditions
  ContractInState contractId expectedState ->
    checkContractStateGuard evaluator contractId expectedState
  
  -- Authorization conditions
  ActorAuthorized address capability ->
    checkAuthorizationGuard evaluator address capability
  
  -- Time conditions
  TimeAfter requiredTime ->
    checkTimeGuard evaluator requiredTime
  
  -- Ownership conditions
  OwnershipVerified resource address ->
    checkOwnershipGuard evaluator resource address
  
  -- Program ownership conditions
  ProgramOwnershipVerified programId address ->
    checkProgramOwnershipGuard evaluator programId address
  
  -- Memory slot conditions
  ResourceInSlot slot resource ->
    checkResourceInSlotGuard evaluator slot resource
  
  -- Empty slot conditions
  SlotEmpty slot ->
    checkSlotEmptyGuard evaluator slot
  
  -- Escrow claimable conditions
  EscrowClaimable escrowId address ->
    checkEscrowClaimableGuard evaluator escrowId address
  
  -- Always condition (always true)
  Always ->
    pure PreconditionSatisfied

-- | Evaluate a guard condition
evaluateGuard :: 
  (Member (Error AppError) r) => 
  PreconditionEvaluator -> 
  Guard -> 
  Sem r Bool
evaluateGuard evaluator guard = do
  result <- evaluatePrecondition evaluator guard
  case result of
    PreconditionSatisfied -> pure True
    PreconditionFailed err -> pure False

-- | Evaluate a guarded effect
evaluateGuardedEffect :: 
  (Member (Error AppError) r) => 
  PreconditionEvaluator -> 
  GuardedEffect -> 
  Sem r Bool
evaluateGuardedEffect evaluator (GuardedEffect guard _) = do
  evaluateGuard evaluator guard

-- | Check if a balance is sufficient
checkBalanceGuard :: 
  (Member (Error AppError) r) => 
  PreconditionEvaluator -> 
  TokenId -> 
  Address -> 
  Amount -> 
  Sem r PreconditionResult
checkBalanceGuard evaluator tokenId address requiredAmount = do
  -- In a real implementation, this would query the token balance
  -- For now, we'll simulate with a fixed balance
  let actualBalance = 1000  -- Simulated balance
  
  if actualBalance >= requiredAmount
    then pure PreconditionSatisfied
    else pure $ PreconditionFailed $ 
         BalanceInsufficient tokenId address requiredAmount actualBalance

-- | Check if an escrow exists
checkEscrowGuard :: 
  (Member (Error AppError) r) => 
  PreconditionEvaluator -> 
  EscrowId -> 
  Sem r PreconditionResult
checkEscrowGuard evaluator escrowId = do
  -- In a real implementation, this would check if the escrow exists
  -- For now, we'll simulate with a fixed result
  let escrowExists = True  -- Simulated result
  
  if escrowExists
    then pure PreconditionSatisfied
    else pure $ PreconditionFailed $ EscrowNotFound escrowId

-- | Check if a contract is in the expected state
checkContractStateGuard :: 
  (Member (Error AppError) r) => 
  PreconditionEvaluator -> 
  ContractId -> 
  ByteString -> 
  Sem r PreconditionResult
checkContractStateGuard evaluator contractId expectedState = do
  -- In a real implementation, this would query the contract state
  -- For now, we'll simulate with a fixed state
  let actualState = expectedState  -- Simulated state (matching for simplicity)
  
  if actualState == expectedState
    then pure PreconditionSatisfied
    else pure $ PreconditionFailed $ 
         ContractStateInvalid contractId expectedState actualState

-- | Check if an actor is authorized for a capability
checkAuthorizationGuard :: 
  (Member (Error AppError) r) => 
  PreconditionEvaluator -> 
  Address -> 
  Text -> 
  Sem r PreconditionResult
checkAuthorizationGuard evaluator address capability = do
  -- In a real implementation, this would check authorization
  -- For now, we'll simulate with a fixed result
  let isAuthorized = True  -- Simulated result
  
  if isAuthorized
    then pure PreconditionSatisfied
    else pure $ PreconditionFailed $ ActorNotAuthorized address

-- | Check if the current time is after a required time
checkTimeGuard :: 
  (Member (Error AppError) r) => 
  PreconditionEvaluator -> 
  LamportTime -> 
  Sem r PreconditionResult
checkTimeGuard evaluator requiredTime = do
  -- Get the minimum Lamport time across all timelines
  -- In a real implementation, this would check the time map
  -- For now, we'll simulate with a fixed current time
  let currentTime = LamportTime 100  -- Simulated current time
  
  if currentTime >= requiredTime
    then pure PreconditionSatisfied
    else pure $ PreconditionFailed $ 
         TimeConditionNotMet requiredTime currentTime

-- | Check if a resource is owned by an address
checkOwnershipGuard :: 
  (Member (Error AppError) r) => 
  PreconditionEvaluator -> 
  Resource -> 
  Address -> 
  Sem r PreconditionResult
checkOwnershipGuard evaluator resource address = do
  -- In a real implementation, this would check the resource ledger
  -- For now, we'll simulate with a fixed result
  let isOwner = True  -- Simulated result
  
  if isOwner
    then pure PreconditionSatisfied
    else pure $ PreconditionFailed $ 
         OwnershipVerificationFailed (getResourceHash resource) address

-- | Get the resource hash from a resource
getResourceHash :: Resource -> ResourceHash
getResourceHash resource = case resource of
  TokenBalanceResource tokenId _ _ -> EntityHash $ Hash $ "token-" <> tokenId
  EscrowReceiptResource escrowId -> EntityHash $ Hash $ "escrow-" <> escrowId
  ContractWitnessResource contractId _ -> EntityHash $ Hash $ "contract-" <> contractId
  SyntheticInternalMarker text -> EntityHash $ Hash $ "synthetic-" <> Data.ByteString.pack (show text)

-- | Check if a program is owned by an address
checkProgramOwnershipGuard :: 
  (Member (Error AppError) r) => 
  PreconditionEvaluator -> 
  ProgramId -> 
  Address -> 
  Sem r PreconditionResult
checkProgramOwnershipGuard evaluator programId address = do
  -- In a real implementation, this would check program ownership
  -- For now, we'll simulate with a fixed result
  let isOwner = True  -- Simulated result
  
  if isOwner
    then pure PreconditionSatisfied
    else pure $ PreconditionFailed $ 
         ProgramOwnershipVerificationFailed programId address

-- | Check if a resource is in a memory slot
checkResourceInSlotGuard :: 
  (Member (Error AppError) r) => 
  PreconditionEvaluator -> 
  MemorySlot -> 
  Resource -> 
  Sem r PreconditionResult
checkResourceInSlotGuard evaluator slot resource = do
  -- Get the program memory
  let memory = programMemory (programState evaluator)
  
  -- Check if the resource is in the slot
  case lookupMemorySlot memory slot of
    Just slotResource -> 
      if slotResource == resource
        then pure PreconditionSatisfied
        else pure $ PreconditionFailed $ ResourceNotInSlot slot
    Nothing -> 
      pure $ PreconditionFailed $ ResourceNotInSlot slot

-- | Check if a memory slot is empty
checkSlotEmptyGuard :: 
  (Member (Error AppError) r) => 
  PreconditionEvaluator -> 
  MemorySlot -> 
  Sem r PreconditionResult
checkSlotEmptyGuard evaluator slot = do
  -- Get the program memory
  let memory = programMemory (programState evaluator)
  
  -- Check if the slot is empty
  case lookupMemorySlot memory slot of
    Just _ -> pure $ PreconditionFailed $ SlotNotEmpty slot
    Nothing -> pure PreconditionSatisfied

-- | Check if an escrow is claimable by an address
checkEscrowClaimableGuard :: 
  (Member (Error AppError) r) => 
  PreconditionEvaluator -> 
  EscrowId -> 
  Address -> 
  Sem r PreconditionResult
checkEscrowClaimableGuard evaluator escrowId address = do
  -- In a real implementation, this would check if the escrow is claimable
  -- For now, we'll simulate with a fixed result
  let isClaimable = True  -- Simulated result
  
  if isClaimable
    then pure PreconditionSatisfied
    else pure $ PreconditionFailed $ 
         EscrowNotClaimable escrowId address 