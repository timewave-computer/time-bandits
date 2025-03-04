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
This module provides the Timeline abstraction and related functionality.
It encapsulates all timeline interactions and provides a consistent interface
for working with different types of timelines (blockchains, rollups, off-chain logs).

Timelines are causally ordered event logs that:
- May be a blockchain, rollup, off-chain log, or data availability layer
- Define their own consistency guarantees
- Own their own clock (block height, slot number, timestamp)
- Define how resources are created, transferred, destroyed
-}
module TimeBandits.Timeline 
  ( -- * Core Types
    Timeline(..)
  , TimelineClock(..)
  , TimelineId
  , Event(..)
  , BlockHeader(..)
  
  -- * Timeline Operations
  , createTimeline
  , observeTimeline
  , appendToTimeline
  , getTimelineHead
  , verifyTimelineConsistency
  
  -- * Re-exports from Types
  , TimelineHash
  , TimelineEvent(..)
  , TimelineEventType(..)
  , TimelineLog(..)
  , TimelineBlock(..)
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import TimeBandits.Types
  ( TimelineHash
  , TimelineEvent(..)
  , TimelineEventType(..)
  , TimelineLog(..)
  , TimelineBlock(..)
  , LamportTime(..)
  , AppError(..)
  , TimelineErrorType(..)
  )

-- | Unique identifier for a Timeline
type TimelineId = EntityHash Timeline

-- | Timeline clock represents the time concept within a timeline
-- This could be block height, slot number, timestamp, etc.
data TimelineClock 
  = BlockHeightClock Int
  | SlotNumberClock Int
  | TimestampClock UTCTime
  | LamportClock LamportTime
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Event interface for timeline entries
class Event e where
  -- | Get the event payload
  eventPayload :: e -> ByteString
  -- | Get the event timestamp
  eventTimestamp :: e -> LamportTime
  -- | Verify event validity
  verifyEvent :: e -> Bool

-- | Block header contains metadata about a block in a timeline
data BlockHeader = BlockHeader
  { bhTimeline :: TimelineId
  , bhHeight :: Int
  , bhPrevBlockHash :: Maybe Hash
  , bhMerkleRoot :: Hash
  , bhTimestamp :: LamportTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Timeline is a causally ordered event log
data Timeline = Timeline
  { timelineId :: TimelineId
  , eventLog :: [Event]
  , clock :: TimelineClock
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new timeline with a specified clock type
createTimeline :: 
  (Member (Error AppError) r) => 
  ByteString -> 
  TimelineClock -> 
  Sem r Timeline
createTimeline name initialClock = do
  -- Create a timeline hash from the name
  let timelineId = EntityHash $ Hash name
  
  -- Check if timeline already exists (would be implemented with state effect in practice)
  -- For now, just create a new timeline
  pure $ Timeline
    { timelineId = timelineId
    , eventLog = []
    , clock = initialClock
    }

-- | Observe a timeline and subscribe to its events
observeTimeline ::
  (Member (Error AppError) r) =>
  TimelineHash ->
  Sem r Timeline
observeTimeline timelineHash = do
  -- In a real implementation, this would connect to the timeline and start observing it
  -- For now, throw an error since we don't have the actual timeline
  throw $ TimelineError $ TimelineNotFound timelineHash

-- | Append an event to a timeline
appendToTimeline ::
  (Member (Error AppError) r, Event e) =>
  Timeline ->
  e ->
  Sem r Timeline
appendToTimeline timeline event = do
  -- In a real implementation, this would validate and append the event to the timeline
  -- For now, just return the timeline unchanged
  pure timeline

-- | Get the current head of a timeline
getTimelineHead ::
  (Member (Error AppError) r) =>
  Timeline ->
  Sem r BlockHeader
getTimelineHead timeline = do
  -- In a real implementation, this would return the latest block header
  -- For now, create a dummy block header
  pure $ BlockHeader
    { bhTimeline = timelineId timeline
    , bhHeight = 0
    , bhPrevBlockHash = Nothing
    , bhMerkleRoot = Hash "dummy-merkle-root"
    , bhTimestamp = case clock timeline of
        LamportClock t -> t
        BlockHeightClock h -> LamportTime h
        SlotNumberClock s -> LamportTime s
        TimestampClock _ -> LamportTime 0
    }

-- | Verify the consistency of a timeline
verifyTimelineConsistency ::
  (Member (Error AppError) r) =>
  Timeline ->
  Sem r Bool
verifyTimelineConsistency _ = do
  -- In a real implementation, this would verify the timeline's consistency guarantees
  -- For now, just return True
  pure True 