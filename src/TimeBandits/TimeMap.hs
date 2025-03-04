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
This module provides the TimeMap abstraction and related functionality.
It encapsulates a snapshot of multiple timelines and their logical clocks,
allowing programs to execute against a consistent view of time across timelines.

Time Maps are:
- Snapshots of timeline states at specific logical times
- Used to enforce causal ordering across timelines
- The basis for cross-timeline consistency guarantees
- Used to prevent time-based attacks and race conditions
-}
module TimeBandits.TimeMap 
  ( -- * Core Types
    TimeMap(..)
  , TimeMapId
  , TimeMapEntry(..)
  , TimeMapError(..)
  
  -- * TimeMap Operations
  , createTimeMap
  , updateTimeMap
  , getTimelineState
  , verifyTimeMapConsistency
  , ensureCausalOrder
  
  -- * Re-exports from Types and Timeline
  , TimelineHash
  , LamportTime(..)
  , BlockHeader(..)
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import TimeBandits.Types
  ( TimelineHash
  , LamportTime(..)
  , AppError(..)
  , TimelineErrorType(..)
  )
import TimeBandits.Timeline (BlockHeader(..))

-- | Unique identifier for a TimeMap
type TimeMapId = EntityHash TimeMap

-- | Entry in a TimeMap for a specific timeline
data TimeMapEntry = TimeMapEntry
  { timelineHash :: TimelineHash
  , observedHead :: BlockHeader
  , lamportTime :: LamportTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | TimeMap is a snapshot of multiple timelines at specific logical times
data TimeMap = TimeMap
  { timeMapId :: TimeMapId
  , entries :: Map TimelineHash TimeMapEntry
  , createdAt :: LamportTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Errors specific to TimeMap operations
data TimeMapError
  = TimelineNotInMap TimelineHash
  | CausalOrderViolation TimelineHash LamportTime LamportTime
  | InconsistentTimeMap TimeMapId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new TimeMap with initial timeline entries
createTimeMap :: 
  (Member (Error AppError) r) => 
  [TimeMapEntry] -> 
  LamportTime ->
  Sem r TimeMap
createTimeMap initialEntries creationTime = do
  -- Create a map from the initial entries
  let entriesMap = Map.fromList [(timelineHash entry, entry) | entry <- initialEntries]
  
  -- Create a TimeMap ID from a hash of the entries
  let timeMapId = EntityHash $ Hash "time-map-id" -- In a real implementation, this would be a hash of the entries
  
  pure $ TimeMap
    { timeMapId = timeMapId
    , entries = entriesMap
    , createdAt = creationTime
    }

-- | Update a TimeMap with a new timeline entry
updateTimeMap ::
  (Member (Error AppError) r) =>
  TimeMap ->
  TimeMapEntry ->
  Sem r TimeMap
updateTimeMap timeMap entry = do
  -- Check if the timeline already exists in the map
  let tlHash = timelineHash entry
  let existingEntry = Map.lookup tlHash (entries timeMap)
  
  -- Verify causal ordering if the timeline already exists
  case existingEntry of
    Just existing -> do
      if lamportTime entry > lamportTime existing
        then pure ()
        else throw $ TimelineError $ TimelineGenericError $ 
              "Causal order violation: new time " ++ show (lamportTime entry) ++ 
              " is not greater than existing time " ++ show (lamportTime existing)
    Nothing -> pure ()
  
  -- Update the TimeMap with the new entry
  let updatedEntries = Map.insert tlHash entry (entries timeMap)
  
  pure $ timeMap { entries = updatedEntries }

-- | Get the state of a specific timeline from the TimeMap
getTimelineState ::
  (Member (Error AppError) r) =>
  TimeMap ->
  TimelineHash ->
  Sem r TimeMapEntry
getTimelineState timeMap tlHash = do
  -- Look up the timeline in the TimeMap
  case Map.lookup tlHash (entries timeMap) of
    Just entry -> pure entry
    Nothing -> throw $ TimelineError $ TimelineGenericError $ 
                "Timeline " ++ show tlHash ++ " not found in TimeMap"

-- | Verify the consistency of a TimeMap
verifyTimeMapConsistency ::
  (Member (Error AppError) r) =>
  TimeMap ->
  Sem r Bool
verifyTimeMapConsistency timeMap = do
  -- In a real implementation, this would verify various consistency properties
  -- For now, just return True
  pure True

-- | Ensure causal ordering between two timeline states
ensureCausalOrder ::
  (Member (Error AppError) r) =>
  TimeMap ->
  TimelineHash ->  -- Source timeline
  TimelineHash ->  -- Target timeline
  Sem r Bool
ensureCausalOrder timeMap sourceTl targetTl = do
  -- Get the entries for both timelines
  sourceEntry <- getTimelineState timeMap sourceTl
  targetEntry <- getTimelineState timeMap targetTl
  
  -- Check if the source happens before the target
  let sourceTime = lamportTime sourceEntry
  let targetTime = lamportTime targetEntry
  
  -- In a real implementation, this would be more sophisticated
  -- For now, just check if source time is less than target time
  pure (sourceTime < targetTime) 