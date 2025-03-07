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
import Core.Common (EntityHash, LamportTime)
import Core.Resource (Resource, ResourceId, EscrowId)
import Core.Timeline (TimelineHash, BlockHeader)
import Core.TimeMap (TimeMap(..))

-- Import from Types modules
import Types.EffectTypes
  ( EffectId
  , FactSnapshot
  , emptyFactSnapshot
  )
import Types.Effect (Effect)

-- Import from Programs modules
import Programs.Types
  ( MemorySlot(..)
  , ProgramMemory(..)
  , ResourceClaim(..)
  , TimeMap(..)
  , ProgramId
  )
import Core.ExecutionLog (ExecutionLog(..))

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

-- | Program memory model
data ProgramMemory = ProgramMemory
  { slots :: Map Text (Maybe Resource)  -- Memory slots that can hold resources
  , values :: Map Text Text             -- Key-value store for program state
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Resource claim by a program
data ResourceClaim = ResourceClaim
  { claimId :: Text
  , claimTimeMap :: TimeMap
  , claimExpiryTime :: UTCTime
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Program capability
data ProgramCapability
  = CanTransferResource
  | CanObserveTimeline
  | CanCallProgram
  | CanReceiveMessage
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Address type for authorized callers
type Address = Text

-- | Value type for program memory
type Value = Text

-- | The current state of a program
data ProgramState = ProgramState
  { -- Basic program information
    programId :: ProgramId
  , programName :: Text
  , programVersion :: Integer
  , programProtocolVersion :: Integer
  
  -- Effect DAG structure (new as per refactor)
  , rootEffect :: Maybe EffectId
  , effects :: Map EffectId Effect
  , currentFacts :: FactSnapshot
  
  -- Program memory and resources (compatible with old fields)
  , memory :: ProgramMemory                      -- For backward compatibility
  , resourceClaims :: Map ResourceId ResourceClaim -- For backward compatibility
  , programMemory :: Map Text Value               -- New key-value store
  , programResources :: Map ResourceId Resource   -- New resource tracking
  , programCapabilities :: [ProgramCapability]
  
  -- Execution and security
  , timeMap :: TimeMap                          -- For backward compatibility
  , programTimeMap :: TimeMap                   -- New field
  , programAuthorizedCallers :: [Address]
  , programCheckpoints :: [Checkpoint]
  , executionLog :: ExecutionLog                -- For backward compatibility
  , programLogs :: [ExecutionLog]               -- New field for multiple logs
  , lastUpdated :: UTCTime                      -- For backward compatibility
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A checkpoint in program execution (for upgrades and replay)
data Checkpoint = Checkpoint
  { checkpointId :: Text
  , checkpointTimestamp :: UTCTime
  , checkpointEffectId :: EffectId
  , checkpointMemory :: Map Text Value
  , checkpointResources :: Map ResourceId Resource
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new program state
createProgramState :: ProgramId -> UTCTime -> ProgramState
createProgramState pid now = ProgramState
  { programId = pid
  , programName = ""
  , programVersion = 0
  , programProtocolVersion = 0
  , rootEffect = Nothing
  , effects = Map.empty
  , currentFacts = emptyFactSnapshot
  , memory = ProgramMemory Map.empty Map.empty  -- Backward compatibility
  , resourceClaims = Map.empty                  -- Backward compatibility
  , programMemory = Map.empty
  , programResources = Map.empty
  , programCapabilities = []
  , timeMap = TimeMap Map.empty Map.empty       -- Backward compatibility
  , programTimeMap = TimeMap Map.empty Map.empty
  , programAuthorizedCallers = []
  , programCheckpoints = []
  , executionLog = ExecutionLog []              -- Backward compatibility
  , programLogs = []
  , lastUpdated = now
  }

-- | Get a resource from a memory slot
getMemorySlot :: Text -> ProgramState -> Maybe Resource
getMemorySlot slot state = 
  case Map.lookup slot (slots $ memory state) of
    Just resource -> resource
    Nothing -> Nothing

-- | Set a memory slot to hold a resource
setMemorySlot :: Text -> Resource -> ProgramState -> ProgramState
setMemorySlot slot resource state =
  let mem = memory state
      newSlots = Map.insert slot (Just resource) (slots mem)
      newMem = mem { slots = newSlots }
  in state { memory = newMem 
           , programMemory = Map.insert slot (resourceToValue resource) (programMemory state)
           }

-- | Helper to convert a resource to text for new memory model
resourceToValue :: Resource -> Text
resourceToValue = error "resourceToValue not implemented yet"

-- | Clear a memory slot
clearMemorySlot :: Text -> ProgramState -> ProgramState
clearMemorySlot slot state =
  let mem = memory state
      newSlots = Map.insert slot Nothing (slots mem)
      newMem = mem { slots = newSlots }
  in state { memory = newMem
           , programMemory = Map.delete slot (programMemory state)
           }

-- | Claim a resource for the program
claimResource :: Resource -> TimeMap -> UTCTime -> ProgramState -> ProgramState
claimResource resource tm expiry state =
  let resourceId = error "resourceId not implemented yet" -- Extract ID from resource
      claim = ResourceClaim
        { claimId = error "claimId not implemented yet" -- Generate claim ID
        , claimTimeMap = tm
        , claimExpiryTime = expiry
        }
      newClaims = Map.insert resourceId claim (resourceClaims state)
      newResources = Map.insert resourceId resource (programResources state)
  in state { resourceClaims = newClaims
           , programResources = newResources
           }

-- | Release a claimed resource
releaseResource :: Resource -> ProgramState -> ProgramState
releaseResource resource state =
  let resourceId = error "resourceId not implemented yet" -- Extract ID from resource
      newClaims = Map.delete resourceId (resourceClaims state)
      newResources = Map.delete resourceId (programResources state)
  in state { resourceClaims = newClaims
           , programResources = newResources
           }

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
      programTm = programTimeMap state
      newProgramTimelines = Map.insert timeline time (timelines programTm)
      newProgramHeads = Map.insert timeline header (observedHeads programTm)
      newProgramTimeMap = programTm { timelines = newProgramTimelines, observedHeads = newProgramHeads }
  in state { timeMap = newTimeMap
           , programTimeMap = newProgramTimeMap
           , lastUpdated = error "getCurrentTime not available" -- Update lastUpdated
           }

-- | Get the current time map
getTimeMap :: ProgramState -> TimeMap
getTimeMap = timeMap

-- | Log an execution entry
logExecution :: Text -> UTCTime -> ProgramState -> ProgramState
logExecution payload timestamp state =
  let entry = ExecutionLogEntry
        { logTimestamp = timestamp
        , logData = payload
        }
      log = executionLog state
      newEntries = logEntries log ++ [entry]
      newLog = log { logEntries = newEntries }
      
      -- Also update the new logs field
      programLog = case programLogs state of
        [] -> ExecutionLog []
        (x:xs) -> x
      newProgramEntries = logEntries programLog ++ [entry]
      newProgramLog = programLog { logEntries = newProgramEntries }
      newProgramLogs = newProgramLog : tail (programLogs state ++ [ExecutionLog []])
  in state { executionLog = newLog
           , programLogs = newProgramLogs
           , lastUpdated = timestamp
           }

-- | Get the execution log
getExecutionLog :: ProgramState -> ExecutionLog
getExecutionLog = executionLog 