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
This module implements a centralized effect interpreter that:
1. Validates effect preconditions using the current TimeMap
2. Applies the effect
3. Updates program memory
4. Updates the TimeMap
5. Appends an entry to the ExecutionLog

All state transitions must pass through the interpreter - no effect-specific code
should modify program state directly.
-}
module TimeBandits.EffectInterpreter 
  ( -- * Core Functions
    interpretEffect
  , validateEffect
  , applyEffect
  , updateTimeMap
  , logEffect
  
  -- * Types
  , EffectResult(..)
  , EffectInterpretationError(..)
  , ExecutionContext(..)
  ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Polysemy (Member, Sem, Embed, embed)
import Polysemy.Error (Error, throw, runError, fromEither)

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import TimeBandits.Types
  ( AppError(..)
  , LamportTime(..)
  , ProgramErrorType(..)
  )
import TimeBandits.Effect
  ( Effect(..)
  )
import TimeBandits.Program
  ( Program
  , ProgramId
  , ProgramState
  , ProgramMemory
  , MemorySlot
  )
import TimeBandits.Resource
  ( Resource
  , ResourceId
  , ResourceType(..)
  )
import TimeBandits.Timeline
  ( Timeline
  , TimelineId
  , Event(..)
  )
import TimeBandits.TimeMap
  ( TimeMap
  , updateTimeMapWithTimeline
  , verifyTimeMapConsistency
  )
import TimeBandits.TransitionMessage
  ( TransitionMessage(..)
  , TransitionProof(..)
  , verifyTransitionMessage
  )
import TimeBandits.ExecutionLog
  ( ExecutionLog
  , LogEntry(..)
  , appendToExecutionLog
  )

-- | Result of effect interpretation
data EffectResult = EffectResult
  { resultProgram :: Program
  , resultTimeMap :: TimeMap
  , resultLog :: LogEntry
  , resultResources :: [Resource]
  , resultProof :: Maybe TransitionProof
  }
  deriving (Eq, Show, Generic)

-- | Errors that can occur during effect interpretation
data EffectInterpretationError
  = InvalidPrecondition Text
  | ResourceOwnershipError ResourceId
  | TimeMapInconsistency Text
  | ExecutionError Text
  | ProofGenerationError Text
  | LoggingError Text
  deriving (Eq, Show, Generic)

-- | Execution context for effect interpretation
data ExecutionContext = ExecutionContext
  { contextProgram :: Program
  , contextTimeMap :: TimeMap
  , contextLog :: ExecutionLog
  , contextTimestamp :: UTCTime
  , contextParentEffect :: Maybe Hash
  }
  deriving (Eq, Show, Generic)

-- | Interpret an effect within an execution context
interpretEffect :: 
  (Member (Error AppError) r, Member (Error EffectInterpretationError) r, Member (Embed IO) r) => 
  TransitionMessage ->
  ExecutionContext ->
  Sem r EffectResult
interpretEffect message context = do
  -- 1. Validate the transition message (signature, proof, resources)
  isValid <- verifyTransitionMessage message (contextTimeMap context)
  unless isValid $
    throw $ InvalidPrecondition "Invalid transition message"
  
  -- 2. Validate effect preconditions
  validateEffect (transitionEffect message) context
  
  -- 3. Apply the effect
  (newProgram, affectedResources) <- applyEffect (transitionEffect message) context
  
  -- 4. Update the TimeMap with any affected timelines
  newTimeMap <- updateTimeMap (transitionEffect message) (contextTimeMap context) affectedResources
  
  -- 5. Log the effect application
  logEntry <- logEffect (transitionEffect message) newProgram newTimeMap context
  
  -- Return the updated state and log entry
  return EffectResult
    { resultProgram = newProgram
    , resultTimeMap = newTimeMap
    , resultLog = logEntry
    , resultResources = affectedResources
    , resultProof = transitionProof message
    }

-- | Validate that an effect can be applied within the current context
validateEffect :: 
  (Member (Error EffectInterpretationError) r) => 
  Effect ->
  ExecutionContext ->
  Sem r ()
validateEffect effect context = do
  -- Validate based on effect type
  case effect of
    -- EscrowToProgram: Validate resource ownership
    EscrowToProgram resource targetProgram slot ->
      validateResourceOwnership resource (contextProgram context)
    
    -- ClaimFromProgram: Validate resource is in the slot
    ClaimFromProgram sourceProgram slot recipient ->
      validateSlotOccupied sourceProgram slot
    
    -- InvokeProgram: Validate program exists and calling program has permission
    InvokeProgram targetProgram function args ->
      validateProgramAccess targetProgram (contextProgram context)
    
    -- DelegateCapability: Validate the delegator has the capability
    DelegateCapability capability targetProgram expiry ->
      validateCapabilityOwnership capability (contextProgram context)
    
    -- WatchResource: Validate resource exists
    WatchResource resourceKey condition trigger ->
      validateResourceExists resourceKey
    
    -- AtomicBatch: Validate all contained effects
    AtomicBatch effects ->
      mapM_ (\e -> validateEffect e context) effects

-- | Apply an effect to the current program state
applyEffect :: 
  (Member (Error AppError) r, Member (Error EffectInterpretationError) r, Member (Embed IO) r) => 
  Effect ->
  ExecutionContext ->
  Sem r (Program, [Resource])
applyEffect effect context = do
  -- Apply based on effect type
  case effect of
    -- EscrowToProgram: Move resource to target program's memory
    EscrowToProgram resource targetProgram slot -> do
      -- Implementation would update program memory
      return (contextProgram context, [resource])
    
    -- ClaimFromProgram: Retrieve resource from source program's memory
    ClaimFromProgram sourceProgram slot recipient -> do
      -- Implementation would update program memory
      return (contextProgram context, [])
    
    -- InvokeProgram: Execute a function in another program
    InvokeProgram targetProgram function args -> do
      -- Implementation would invoke the program
      return (contextProgram context, [])
    
    -- DelegateCapability: Grant a capability to another program
    DelegateCapability capability targetProgram expiry -> do
      -- Implementation would update capability registry
      return (contextProgram context, [])
    
    -- WatchResource: Set up a watch condition
    WatchResource resourceKey condition trigger -> do
      -- Implementation would set up a watch
      return (contextProgram context, [])
    
    -- AtomicBatch: Apply all contained effects atomically
    AtomicBatch effects -> do
      -- Apply each effect in sequence
      foldM (\(prog, res) e -> do
        (newProg, newRes) <- applyEffect e (context { contextProgram = prog })
        return (newProg, res ++ newRes)
      ) (contextProgram context, []) effects

-- | Update the TimeMap based on the effect and affected resources
updateTimeMap :: 
  (Member (Error EffectInterpretationError) r) => 
  Effect ->
  TimeMap ->
  [Resource] ->
  Sem r TimeMap
updateTimeMap effect currentTimeMap affectedResources = do
  -- Get all affected timelines
  let affectedTimelines = getAffectedTimelines effect affectedResources
  
  -- Update the TimeMap for each affected timeline
  foldM (\timeMap timeline -> do
    -- In a real implementation, this would update the TimeMap with the latest
    -- state of the timeline, increment logical clocks, etc.
    -- For now, we just return the unchanged TimeMap
    return timeMap
  ) currentTimeMap affectedTimelines

-- | Log the application of an effect
logEffect :: 
  (Member (Error EffectInterpretationError) r, Member (Embed IO) r) => 
  Effect ->
  Program ->
  TimeMap ->
  ExecutionContext ->
  Sem r LogEntry
logEffect effect program timeMap context = do
  -- Get the current time
  timestamp <- liftIO getCurrentTime
  
  -- Get the parent effect hash if available
  let parentHash = fromMaybe (Hash "genesis") (contextParentEffect context)
  
  -- Create the log entry
  let entry = LogEntry
        { logEffect = effect
        , logTimestamp = timestamp
        , logParentHash = parentHash
        , logProgramState = computeStateHash program
        , logTimeMap = timeMap
        , logProof = Nothing -- Would include proofs in a real implementation
        }
  
  return entry

-- Helper functions

-- | Validate that a program owns a resource
validateResourceOwnership :: 
  (Member (Error EffectInterpretationError) r) => 
  Resource ->
  Program ->
  Sem r ()
validateResourceOwnership resource program = do
  -- In a real implementation, this would check resource ownership
  -- For now, it's just a placeholder
  return ()

-- | Validate that a memory slot is occupied
validateSlotOccupied :: 
  (Member (Error EffectInterpretationError) r) => 
  ProgramId ->
  MemorySlot ->
  Sem r ()
validateSlotOccupied programId slot = do
  -- In a real implementation, this would check if the slot contains a resource
  -- For now, it's just a placeholder
  return ()

-- | Validate that a program has access to another program
validateProgramAccess :: 
  (Member (Error EffectInterpretationError) r) => 
  ProgramId ->
  Program ->
  Sem r ()
validateProgramAccess targetProgram callingProgram = do
  -- In a real implementation, this would check program access permissions
  -- For now, it's just a placeholder
  return ()

-- | Validate that a program owns a capability
validateCapabilityOwnership :: 
  (Member (Error EffectInterpretationError) r) => 
  Text ->
  Program ->
  Sem r ()
validateCapabilityOwnership capability program = do
  -- In a real implementation, this would check capability ownership
  -- For now, it's just a placeholder
  return ()

-- | Validate that a resource exists
validateResourceExists :: 
  (Member (Error EffectInterpretationError) r) => 
  Text ->
  Sem r ()
validateResourceExists resourceKey = do
  -- In a real implementation, this would check if the resource exists
  -- For now, it's just a placeholder
  return ()

-- | Get all timelines affected by an effect
getAffectedTimelines :: Effect -> [Resource] -> [TimelineId]
getAffectedTimelines _ _ = 
  -- In a real implementation, this would determine which timelines are affected
  -- For now, it's just a placeholder returning an empty list
  []

-- | Compute a hash of the program state
computeStateHash :: Program -> Hash
computeStateHash _ =
  -- In a real implementation, this would compute a hash of the program state
  -- For now, it's just a placeholder
  Hash "program-state-hash"

-- | Helper function to lift IO actions into Sem
liftIO :: Member (Embed IO) r => IO a -> Sem r a
liftIO = embed

-- | Unwrap a Maybe or throw an error
unwrapMaybe :: Member (Error EffectInterpretationError) r => Maybe a -> Text -> Sem r a
unwrapMaybe Nothing err = throw $ ExecutionError err
unwrapMaybe (Just x) _ = return x

-- | Apply a verification and throw on failure
unless :: Member (Error EffectInterpretationError) r => Bool -> Sem r () -> Sem r ()
unless condition action =
  when (not condition) action

 