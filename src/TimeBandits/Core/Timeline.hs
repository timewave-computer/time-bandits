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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: TimeBandits.Core.Timeline
Description: Timeline abstraction and operations for the Time-Bandits system.

This module provides the Timeline abstraction and related functionality.
It encapsulates all timeline interactions and provides a consistent interface
for working with different types of timelines (blockchains, rollups, off-chain logs).

Timelines are causally ordered event logs that:
- May be a blockchain, rollup, off-chain log, or data availability layer
- Define their own consistency guarantees
- Own their own clock (block height, slot number, timestamp)
- Define how resources are created, transferred, destroyed

In the Time-Bandits architecture, Timelines serve as the foundational layer for:
1. Recording and verifying causal history of events
2. Establishing a consistent ordering of operations
3. Enabling cross-timeline resource transfers through proofs
4. Providing the basis for time-related security properties

The Timeline module connects with Adapters for interacting with specific timeline
implementations, and provides core primitives used by Programs and Actors to 
interact with timeline state and events.

@since 0.1.0
-}
module TimeBandits.Core.Timeline 
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
  
  -- * Adapter Functions
  , adaptGenerateProof
  , adaptVerifyProof
  , adaptSendMessage
  , adaptBroadcastMessage
  ) where

-- Import documentation of standard extensions
import TimeBandits.Core.Common.Extensions


import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Serialize (Serialize, encode)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import qualified Data.Sequence as Seq

-- Import from TimeBandits modules
import TimeBandits.Core.Common.Types (Hash(..), computeHash)
import TimeBandits.Core.TimelineId (TimelineId(..), fromHash)
import TimeBandits.Core.Types
  ( TimelineHash
  , TimelineEvent(..)
  , TimelineEventType(..)
  , TimelineLog(..)
  , TimelineBlock(..)
  , LamportTime(..)
  , TimelineErrorType(..)
  )
import TimeBandits.Core.Error (AppError(..), TimelineError(..))

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
  , eventLog :: [ByteString]  -- Store serialized events
  , clock :: TimelineClock
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new timeline with a name and initial clock
createTimeline ::
  (Member (Error AppError) r) => 
  ByteString -> 
  TimelineClock -> 
  Sem r Timeline
createTimeline name initialClock = do
  -- Create a timeline hash from the name
  let hash = computeHash name
      timelineId = fromHash (unHash hash)
  
  -- Create a new timeline with empty event log
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
  throw $ TimelineErr $ InvalidChain $ "Timeline not found: " <> show timelineHash

-- | Append an event to a timeline
appendToTimeline ::
  (Member (Error AppError) r, Event e, Serialize e) =>
  Timeline ->
  e ->
  Sem r Timeline
appendToTimeline timeline event = do
  -- Serialize the event
  let serializedEvent = encode event
  
  -- Append to the event log
  pure $ timeline { eventLog = eventLog timeline ++ [serializedEvent] }

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
        BlockHeightClock h -> LamportTime (fromIntegral h)
        SlotNumberClock s -> LamportTime (fromIntegral s)
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

-- | Adapter function to generate a proof for a timeline event
adaptGenerateProof ::
  (Member (Error AppError) r) =>
  Timeline ->
  ByteString ->
  Sem r ByteString
adaptGenerateProof _ eventData = do
  -- In a real implementation, this would call the appropriate adapter to generate a proof
  -- For now, just return the event data as the "proof"
  pure eventData

-- | Adapter function to verify a proof from a timeline
adaptVerifyProof ::
  (Member (Error AppError) r) =>
  Timeline ->
  ByteString ->
  ByteString ->
  Sem r Bool
adaptVerifyProof _ _ _ = do
  -- In a real implementation, this would call the appropriate adapter to verify a proof
  -- For now, just return True
  pure True

-- | Adapter function to send a message to a timeline
adaptSendMessage ::
  (Member (Error AppError) r) =>
  Timeline ->
  ByteString ->
  Sem r Bool
adaptSendMessage _ _ = do
  -- In a real implementation, this would call the appropriate adapter to send a message
  -- For now, just return True
  pure True

-- | Adapter function to broadcast a message to a timeline
adaptBroadcastMessage ::
  (Member (Error AppError) r) =>
  Timeline ->
  ByteString ->
  Sem r Bool
adaptBroadcastMessage _ _ = do
  -- In a real implementation, this would call the appropriate adapter to broadcast a message
  -- For now, just return True
  pure True

-- | Get a timeline by its hash
getTimelineByHash :: Member (Error AppError) r => TimelineHash -> Sem r Timeline
getTimelineByHash timelineHash = do
  -- This is a placeholder implementation
  -- In a real implementation, we would look up the timeline in storage
  throw $ TimelineErr $ InvalidChain $ "Timeline not found: " <> show timelineHash
