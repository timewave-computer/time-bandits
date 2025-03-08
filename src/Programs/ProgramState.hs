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
import qualified Core.TimeMap as CoreTimeMap

-- Import from Types modules
import Types.EffectTypes
  ( EffectId
  , FactSnapshot
  , emptyFactSnapshot
  )
import Types.Effect (Effect)

-- Import from Programs modules
import qualified Programs.Types as ProgramsTypes
import Programs.Types
  ( MemorySlot(..)
  , ProgramId
  )
import qualified Core.ExecutionLog as CoreExecutionLog

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
  { slots :: Map.Map Text (Maybe Resource)  -- Memory slots that can hold resources
  , values :: Map.Map Text Text             -- Key-value store for program state
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Resource claim by a program
data ResourceClaim = ResourceClaim
  { claimId :: Text
  , claimTimeMap :: ProgramsTypes.TimeMap
  , claimExpiryTime :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
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

-- | Checkpoint for program state
data Checkpoint = Checkpoint
  { checkpointTimestamp :: UTCTime
  , checkpointHash :: EntityHash "Checkpoint"  -- Using a type-level string instead of unit
  , checkpointDescription :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | The current state of a program
data ProgramState = ProgramState
  { -- Basic program information
    programId :: ProgramId
  , programName :: Text
  , programVersion :: Integer
  , programProtocolVersion :: Integer
  
  -- Effect DAG structure (new as per refactor)
  , rootEffect :: Maybe EffectId
  , effects :: Map.Map EffectId Effect
  , currentFacts :: FactSnapshot
  
  -- Program memory and resources (compatible with old fields)
  , memory :: ProgramMemory                      -- For backward compatibility
  , resourceClaims :: Map.Map ResourceId ResourceClaim -- For backward compatibility
  , programMemory :: Map.Map Text Value               -- New key-value store
  , programResources :: Map.Map ResourceId Resource   -- New resource tracking
  , programCapabilities :: [ProgramCapability]
  
  -- Execution and security
  , timeMap :: ProgramsTypes.TimeMap                          -- For backward compatibility
  , programTimeMap :: ProgramsTypes.TimeMap                   -- New field
  , programAuthorizedCallers :: [Address]
  , programCheckpoints :: [Checkpoint]
  , executionLog :: ExecutionLog                -- For backward compatibility
  , programLogs :: [ExecutionLog]               -- New field for multiple logs
  , lastUpdated :: UTCTime                      -- For backward compatibility
  }
  deriving stock (Eq, Show, Generic)
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
  , timeMap = ProgramsTypes.TimeMap Map.empty Map.empty       -- Backward compatibility
  , programTimeMap = ProgramsTypes.TimeMap Map.empty Map.empty
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
claimResource :: Resource -> ProgramsTypes.TimeMap -> UTCTime -> ProgramState -> ProgramState
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
      newTimelines = Map.insert timeline time (ProgramsTypes.timelines tm)
      newHeads = Map.insert timeline header (ProgramsTypes.observedHeads tm)
      newTimeMap = tm { ProgramsTypes.timelines = newTimelines, ProgramsTypes.observedHeads = newHeads }
      programTm = programTimeMap state
      newProgramTimelines = Map.insert timeline time (ProgramsTypes.timelines programTm)
      newProgramHeads = Map.insert timeline header (ProgramsTypes.observedHeads programTm)
      newProgramTimeMap = programTm { ProgramsTypes.timelines = newProgramTimelines, ProgramsTypes.observedHeads = newProgramHeads }
  in state { timeMap = newTimeMap
           , programTimeMap = newProgramTimeMap
           , lastUpdated = error "getCurrentTime not available" -- Update lastUpdated
           }

-- | Get the current time map
getTimeMap :: ProgramState -> ProgramsTypes.TimeMap
getTimeMap = timeMap

-- | Log an execution action
logExecution :: ProgramState -> Text -> ByteString -> UTCTime -> ProgramState
logExecution state action data_ timestamp =
  let 
      -- Create a new log entry
      entry = LogEntry
        { logTimestamp = timestamp
        , logAction = action
        , logData = data_
        }
      
      -- Get the current log and append the new entry
      currentLog = executionLog state
      newEntries = entry : logEntries currentLog
      newLog = currentLog { logEntries = newEntries }
      
  in
  state { executionLog = newLog, lastUpdated = timestamp }

-- | Get the execution log
getExecutionLog :: ProgramState -> ExecutionLog
getExecutionLog = executionLog

-- | Update a program state with new values
updateProgramState :: ProgramState -> Map.Map Text Value -> UTCTime -> ProgramState
updateProgramState state newValues timestamp =
  let 
      -- Update the program memory with new values
      currentMemory = programMemory state
      updatedMemory = Map.union newValues currentMemory
  in
  state { programMemory = updatedMemory
        , lastUpdated = timestamp
        } 