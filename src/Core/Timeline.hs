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

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize, encode)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)

-- Import from TimeBandits modules
import TimeBandits.Core.Core (Hash(..), EntityHash(..), Message(..))
import TimeBandits.Core.Types
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
  , eventLog :: [ByteString]  -- Store serialized events
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
  throw $ TimelineError $ TimelineNotFound timelineHash

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

-- | Adapter functions for backward compatibility with old Effects interface

-- | Adapter for generating a proof (compatibility with old TimelineProof interface)
adaptGenerateProof :: 
  (Member (Error AppError) r) => 
  Hash -> 
  Sem r Hash
adaptGenerateProof sourceHash = do
  -- In a real implementation, this would generate a cryptographic proof
  -- For now, just return a dummy hash
  pure $ Hash "proof-hash"

-- | Adapter for verifying a proof (compatibility with old TimelineProof interface)
adaptVerifyProof :: 
  (Member (Error AppError) r) => 
  Hash -> 
  Hash -> 
  Sem r Bool
adaptVerifyProof proofHash sourceHash = do
  -- In a real implementation, this would verify the proof
  -- For now, just return True
  pure True

-- | Adapter for sending a message (compatibility with old TimelineMessage interface)
adaptSendMessage :: 
  (Member (Error AppError) r, Message msg) => 
  msg -> 
  Sem r ()
adaptSendMessage msg = do
  -- In a real implementation, this would send the message
  -- For now, do nothing
  pure ()

-- | Adapter for broadcasting a message (compatibility with old TimelineMessage interface)
adaptBroadcastMessage :: 
  (Member (Error AppError) r, Message msg) => 
  msg -> 
  Sem r ()
adaptBroadcastMessage msg = do
  -- In a real implementation, this would broadcast the message
  -- For now, do nothing
  pure () 