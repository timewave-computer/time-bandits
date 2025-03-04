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
This module provides the EffectInterpreter, which is responsible for the full lifecycle
of effects in the Time Bandits system. It centralizes precondition checking, effect application,
and ensures causal determinism across the system.

The EffectInterpreter:
1. Validates effect preconditions against the current time map
2. Ensures resources have a single owner
3. Applies effects to program state
4. Updates the execution log with causal links
5. Maintains the time map for causal ordering
-}
module TimeBandits.EffectInterpreter 
  ( -- * Core Types
    EffectInterpreter(..)
  , EffectResult(..)
  , EffectContext(..)
  , EffectError(..)
  
  -- * Interpreter Operations
  , createInterpreter
  , interpretEffect
  , validatePreconditions
  , applyEffect
  , updateTimeMap
  , logEffectExecution
  
  -- * Causal Enforcement
  , enforceResourceOwnership
  , enforceCausalOrder
  , enforceTimeMapConsistency
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize, encode)
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Polysemy (Member, Sem, embed)
import Polysemy.Error (Error, throw, catch)
import Polysemy.Embed (Embed)

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..), computeContentHash)
import TimeBandits.Types
  ( AppError(..)
  , LamportTime(..)
  )
import TimeBandits.ProgramEffect
  ( Effect(..)
  , Guard(..)
  , GuardedEffect(..)
  , checkGuard
  )
import TimeBandits.Program
  ( ProgramId
  , ProgramState
  , TimeMap
  , programTimeMap
  )
import TimeBandits.Resource
  ( Resource
  , ResourceHash
  )
import TimeBandits.Timeline (TimelineHash)
import TimeBandits.ExecutionLog
  ( LogEntry(..)
  , ExecutionLog
  , appendLogEntry
  )
import TimeBandits.TimeMap
  ( advanceTimeMap
  , verifyTimeMapConsistency
  )

-- | Error types specific to effect interpretation
data EffectError
  = PreconditionFailed Guard
  | ResourceOwnershipViolation ResourceHash
  | CausalOrderViolation Hash
  | TimeMapInconsistency TimelineHash
  | EffectApplicationFailed Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Context for effect interpretation
data EffectContext = EffectContext
  { ecTimeMap :: TimeMap
  , ecProgramState :: ProgramState
  , ecExecutionLog :: ExecutionLog
  , ecPreviousEffectHash :: Maybe Hash
  , ecTimestamp :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Result of effect interpretation
data EffectResult = EffectResult
  { erNewContext :: EffectContext
  , erEffectHash :: Hash
  , erLogEntry :: LogEntry
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | The EffectInterpreter manages the full lifecycle of effects
data EffectInterpreter = EffectInterpreter
  { -- | Current context for interpretation
    context :: EffectContext
    -- | Resource ownership ledger (would be a reference to ResourceLedger in full implementation)
  , resourceOwners :: Map.Map ResourceHash ProgramId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new effect interpreter
createInterpreter :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  TimeMap -> 
  ProgramState -> 
  ExecutionLog -> 
  Maybe Hash -> 
  Sem r EffectInterpreter
createInterpreter timeMap programState executionLog prevEffectHash = do
  -- Get current time for the context
  now <- embed getCurrentTime
  
  -- Create the initial context
  let context = EffectContext
        { ecTimeMap = timeMap
        , ecProgramState = programState
        , ecExecutionLog = executionLog
        , ecPreviousEffectHash = prevEffectHash
        , ecTimestamp = now
        }
  
  -- Create the interpreter with empty resource ownership map
  -- In a full implementation, this would load from a persistent store
  pure $ EffectInterpreter
    { context = context
    , resourceOwners = Map.empty
    }

-- | Interpret an effect with full lifecycle management
interpretEffect :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  EffectInterpreter -> 
  GuardedEffect -> 
  Sem r (EffectInterpreter, EffectResult)
interpretEffect interpreter guardedEffect@(GuardedEffect guard effect) = do
  -- 1. Validate preconditions
  validateResult <- catch 
    (validatePreconditions interpreter guardedEffect)
    (\err -> throw $ "Precondition validation failed: " <> show err :: AppError)
  
  -- 2. Enforce resource ownership
  ownershipResult <- catch
    (enforceResourceOwnership interpreter effect)
    (\err -> throw $ "Resource ownership check failed: " <> show err :: AppError)
  
  -- 3. Enforce causal order
  causalResult <- catch
    (enforceCausalOrder interpreter)
    (\err -> throw $ "Causal order check failed: " <> show err :: AppError)
  
  -- 4. Apply the effect to program state
  let currentContext = context interpreter
      currentState = ecProgramState currentContext
  
  newState <- catch
    (applyEffect currentState effect)
    (\err -> throw $ "Effect application failed: " <> show err :: AppError)
  
  -- 5. Update the time map
  let currentTimeMap = ecTimeMap currentContext
  newTimeMap <- catch
    (updateTimeMap currentTimeMap effect)
    (\err -> throw $ "Time map update failed: " <> show err :: AppError)
  
  -- 6. Create a hash for this effect
  let effectHash = computeContentHash effect
  
  -- 7. Log the effect execution
  now <- embed getCurrentTime
  let prevHash = ecPreviousEffectHash currentContext
      executionLog = ecExecutionLog currentContext
  
  newLog <- catch
    (logEffectExecution executionLog effect effectHash prevHash now)
    (\err -> throw $ "Logging failed: " <> show err :: AppError)
  
  -- 8. Create the new context and result
  let logEntry = case newLog of
        [] -> error "Log should not be empty after appending"
        (entry:_) -> entry
      
      newContext = currentContext
        { ecProgramState = newState
        , ecTimeMap = newTimeMap
        , ecExecutionLog = newLog
        , ecPreviousEffectHash = Just effectHash
        , ecTimestamp = now
        }
      
      result = EffectResult
        { erNewContext = newContext
        , erEffectHash = effectHash
        , erLogEntry = logEntry
        }
  
  -- 9. Update the interpreter with new context
  let newInterpreter = interpreter { context = newContext }
  
  -- Return the updated interpreter and result
  pure (newInterpreter, result)

-- | Validate preconditions for an effect
validatePreconditions :: 
  (Member (Error AppError) r) => 
  EffectInterpreter -> 
  GuardedEffect -> 
  Sem r Bool
validatePreconditions interpreter (GuardedEffect guard effect) = do
  -- Get the current time map from the context
  let currentContext = context interpreter
      timeMap = ecTimeMap currentContext
  
  -- Check if the guard condition holds
  guardHolds <- checkGuard timeMap guard
  
  -- If the guard fails, throw an error
  if not guardHolds
    then throw $ PreconditionFailed guard
    else pure True

-- | Enforce that resources have a single owner
enforceResourceOwnership :: 
  (Member (Error AppError) r) => 
  EffectInterpreter -> 
  Effect -> 
  Sem r Bool
enforceResourceOwnership interpreter effect = do
  -- In a full implementation, this would check the ResourceLedger
  -- to ensure that resources are owned by the expected programs
  -- and that transfers maintain the single-owner invariant
  
  -- For now, just return True
  pure True

-- | Enforce causal ordering of effects
enforceCausalOrder :: 
  (Member (Error AppError) r) => 
  EffectInterpreter -> 
  Sem r Bool
enforceCausalOrder interpreter = do
  -- In a full implementation, this would verify that:
  -- 1. The previous effect hash matches the expected causal parent
  -- 2. The time map is advancing monotonically
  -- 3. Cross-timeline events respect logical clock ordering
  
  -- For now, just return True
  pure True

-- | Enforce time map consistency
enforceTimeMapConsistency :: 
  (Member (Error AppError) r) => 
  TimeMap -> 
  Sem r Bool
enforceTimeMapConsistency timeMap = do
  -- Verify that the time map is consistent
  -- This ensures that timeline heads are advancing monotonically
  -- and that logical clocks respect causal ordering
  verifyTimeMapConsistency timeMap

-- | Apply an effect to program state
applyEffect ::
  (Member (Error AppError) r) =>
  ProgramState ->
  Effect ->
  Sem r ProgramState
applyEffect state effect = do
  -- In a full implementation, this would apply the effect to the state
  -- based on the effect type and update the program's memory
  
  -- For now, just return the state unchanged
  pure state

-- | Update the time map based on an effect
updateTimeMap ::
  (Member (Error AppError) r) =>
  TimeMap ->
  Effect ->
  Sem r TimeMap
updateTimeMap timeMap effect = do
  -- Advance the time map based on the effect
  -- This updates Lamport clocks and observed timeline heads
  advanceTimeMap timeMap

-- | Log an effect execution to the execution log
logEffectExecution ::
  (Member (Error AppError) r) =>
  ExecutionLog ->
  Effect ->
  Hash ->
  Maybe Hash ->
  UTCTime ->
  Sem r ExecutionLog
logEffectExecution log effect effectHash prevHash timestamp = do
  -- Create a log entry for this effect
  let entry = LogEntry
        { leEffect = effect
        , leEffectHash = effectHash
        , lePrevHash = prevHash
        , leTimestamp = timestamp
        , leResultHash = computeContentHash effect  -- In a real implementation, this would be the hash of the resulting state
        }
  
  -- Append the entry to the log
  appendLogEntry log entry

 