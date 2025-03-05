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
Module: Programs.ProgramState
Description: Mutable runtime state for program execution

This module defines the mutable state of programs in the Time-Bandits architecture,
separating runtime state from the immutable program definition.

The program state includes:
1. Current memory contents (resources in slots)
2. Execution history (applied effects)
3. Time map (observed timeline states)
4. Resource claims (owned resources)

Separating state from definition enables:
- Clear boundaries for persistence
- Simpler state transitions
- Focused security validation
-}
module Programs.ProgramState
  ( -- * Core types
    ProgramState(..)
  , ExecutionLog(..)
  , LogEntry(..)
  
  -- * State management
  , createProgramState
  , updateProgramState
  
  -- * Memory operations
  , getMemorySlot
  , setMemorySlot
  , clearMemorySlot
  
  -- * Resource management
  , claimResource
  , releaseResource
  , getResourceClaims
  
  -- * Time map operations
  , updateTimeMap
  , getTimeMap
  
  -- * Execution log
  , logExecution
  , getExecutionLog
  ) where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Serialize (Serialize)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- Import from Core modules 
import Core.Common (EntityHash)
import Core.Resource (Resource, EscrowId)
import Core.Timeline (TimelineHash, BlockHeader)
import Core.Types (LamportTime)

-- Import from Programs modules
import Programs.Types
  ( MemorySlot(..)
  , ProgramMemory(..)
  , ResourceClaim(..)
  , TimeMap(..)
  , ProgramId
  )

-- | Log entry for program execution
data LogEntry = LogEntry
  { logTimestamp :: UTCTime
  , logAction :: Text
  , logData :: ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Execution log tracks all program actions
newtype ExecutionLog = ExecutionLog
  { logEntries :: [LogEntry]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Program state contains the mutable runtime state
data ProgramState = ProgramState
  { programId :: ProgramId
  , memory :: ProgramMemory
  , timeMap :: TimeMap
  , resourceClaims :: Map.Map Resource ResourceClaim
  , executionLog :: ExecutionLog
  , lastUpdated :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new, empty program state
createProgramState :: ProgramId -> UTCTime -> ProgramState
createProgramState pid now = ProgramState
  { programId = pid
  , memory = ProgramMemory Map.empty
  , timeMap = TimeMap Map.empty Map.empty
  , resourceClaims = Map.empty
  , executionLog = ExecutionLog []
  , lastUpdated = now
  }

-- | Update the program state with a new timestamp
updateProgramState :: ProgramState -> UTCTime -> ProgramState
updateProgramState state now = state { lastUpdated = now }

-- | Get the value of a memory slot
getMemorySlot :: MemorySlot -> ProgramState -> Maybe Resource
getMemorySlot slot state = 
  case Map.lookup slot (slots $ memory state) of
    Just resource -> resource
    Nothing -> Nothing

-- | Set a memory slot to a specific resource
setMemorySlot :: MemorySlot -> Resource -> ProgramState -> ProgramState
setMemorySlot slot resource state =
  let mem = memory state
      newSlots = Map.insert slot (Just resource) (slots mem)
      newMem = mem { slots = newSlots }
  in state { memory = newMem }

-- | Clear a memory slot
clearMemorySlot :: MemorySlot -> ProgramState -> ProgramState
clearMemorySlot slot state =
  let mem = memory state
      newSlots = Map.insert slot Nothing (slots mem)
      newMem = mem { slots = newSlots }
  in state { memory = newMem }

-- | Claim a resource for the program
claimResource :: Resource -> EscrowId -> LamportTime -> Maybe LamportTime -> ProgramState -> ProgramState
claimResource resource escrowId timestamp expiry state =
  let claim = ResourceClaim
        { claimedResource = resource
        , claimEscrowId = escrowId
        , claimTimestamp = timestamp
        , claimExpiryTime = expiry
        }
      newClaims = Map.insert resource claim (resourceClaims state)
  in state { resourceClaims = newClaims }

-- | Release a claimed resource
releaseResource :: Resource -> ProgramState -> ProgramState
releaseResource resource state =
  let newClaims = Map.delete resource (resourceClaims state)
  in state { resourceClaims = newClaims }

-- | Get all resource claims
getResourceClaims :: ProgramState -> [ResourceClaim]
getResourceClaims = Map.elems . resourceClaims

-- | Update the time map with new timeline information
updateTimeMap :: TimelineHash -> LamportTime -> BlockHeader -> ProgramState -> ProgramState
updateTimeMap timeline time header state =
  let tm = timeMap state
      newTimelines = Map.insert timeline time (timelines tm)
      newHeads = Map.insert timeline header (observedHeads tm)
      newTimeMap = tm { timelines = newTimelines, observedHeads = newHeads }
  in state { timeMap = newTimeMap }

-- | Get the current time map
getTimeMap :: ProgramState -> TimeMap
getTimeMap = timeMap

-- | Log an execution entry
logExecution :: Text -> ByteString -> UTCTime -> ProgramState -> ProgramState
logExecution action payload timestamp state =
  let entry = LogEntry
        { logTimestamp = timestamp
        , logAction = action
        , logData = payload
        }
      log = executionLog state
      newEntries = entry : logEntries log
      newLog = log { logEntries = newEntries }
  in state { executionLog = newLog }

-- | Get the execution log
getExecutionLog :: ProgramState -> ExecutionLog
getExecutionLog = executionLog 