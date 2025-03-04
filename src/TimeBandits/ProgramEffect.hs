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
import TimeBandits.Resource (Resource, Address, Amount, TokenId, ContractId, EscrowId)
import TimeBandits.Program (ProgramId, MemorySlot, ProgramState, TimeMap)
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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A guard is a precondition that must hold before an effect is allowed to apply
data Guard
  = BalanceAtLeast TokenId Address Amount
  | EscrowExists EscrowId
  | ContractInState ContractId ByteString
  | ActorAuthorized Address Capability
  | TimeAfter LamportTime
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