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
Module: Core.Timeline
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
module Core.Timeline 
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
import Data.Serialize (Serialize, encode)
import qualified Data.Serialize as S
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)

-- Hide encodeUtf8 from Prelude to resolve the ambiguity
import Prelude hiding (encodeUtf8)

-- Internal imports
import Core.Common ()
import Core.Types
  ( TimelineEvent(..)
  , TimelineEventType(..)
  , TimelineLog(..)
  , TimelineBlock(..)
  , AppError(..)
  , TimelineErrorType(..)
  , TimelineHash
  , EntityHash(..)
  , Hash(..)
  )
import Core.Message (Message)

-- | Unique identifier for a Timeline
type TimelineId = Text  -- Changed from EntityHash Timeline to Text for simplicity

-- | Timeline clock represents the time concept within a timeline
-- This could be block height, slot number, timestamp, etc.
data TimelineClock 
  = BlockHeightClock Int
  | SlotNumberClock Int
  | TimestampClock UTCTime
  | LamportClock Word64
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Event interface for timeline entries
class Event e where
  -- | Get the event payload
  eventPayload :: e -> ByteString
  -- | Get the event timestamp
  eventTimestamp :: e -> Word64
  -- | Verify event validity
  verifyEvent :: e -> Bool

-- | A timeline represents a blockchain or ledger
data Timeline = Timeline
  { timelineHash :: Text  -- Changed from TimelineHash to Text for simplicity
  , timelineName :: Text
  , timelineType :: Text
  , timelineId :: Text    -- Changed from TimelineId to Text for simplicity
  , eventLog :: [ByteString]  -- Store serialized events
  , timelineClock :: TimelineClock
  }
  deriving stock (Show, Eq, Generic)

-- Manual Serialize instance for Timeline
instance Serialize Timeline where
  put timeline = do
    S.put (TE.encodeUtf8 $ timelineId timeline)
    S.put (TE.encodeUtf8 $ timelineHash timeline)
    S.put (TE.encodeUtf8 $ timelineType timeline)
    S.put (TE.encodeUtf8 $ timelineName timeline)
    S.put (timelineClock timeline)
    S.put (eventLog timeline)
  get = do
    idBytes <- S.get
    hashBytes <- S.get
    typeBytes <- S.get
    nameBytes <- S.get
    clock <- S.get
    events <- S.get
    return $ Timeline
      { timelineId = TE.decodeUtf8 idBytes
      , timelineHash = TE.decodeUtf8 hashBytes
      , timelineType = TE.decodeUtf8 typeBytes
      , timelineName = TE.decodeUtf8 nameBytes
      , timelineClock = clock
      , eventLog = events
      }

-- | Block header contains information about a block in a timeline
data BlockHeader = BlockHeader
  { blockHeight :: Word64
  , blockHash :: ByteString
  , blockTimestamp :: Word64
  , prevBlockHash :: ByteString
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | Create a new timeline with a specified clock type
createTimeline :: 
  (Member (Error AppError) r) => 
  Text -> 
  TimelineClock -> 
  Sem r Timeline
createTimeline name initialClock = do
  -- Create a timeline hash from the name
  let timelineId = Text.pack $ "timeline-" ++ Text.unpack name
      -- Use a placeholder hash and type
      hash = "generated-timeline-hash"
      timelineType = "default"
  
  -- Create a new timeline with empty event log
  pure $ Timeline
    { timelineHash = hash
    , timelineName = name
    , timelineType = timelineType
    , timelineId = timelineId
    , eventLog = []
    , timelineClock = initialClock
    }

-- | Observe a timeline and subscribe to its events
observeTimeline ::
  (Member (Error AppError) r) =>
  Text ->
  Sem r Timeline
observeTimeline timelineHashText = do
  -- In a real implementation, this would connect to the timeline and start observing it
  -- For now, throw an error since we don't have the actual timeline
  -- Create a dummy TimelineHash for the error
  let dummyHash = EntityHash $ Hash $ TE.encodeUtf8 timelineHashText
  throw $ TimelineError $ TimelineNotFound dummyHash

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
getTimelineHead _ = do
  -- In a real implementation, this would return the latest block header
  -- For now, create a dummy block header
  pure $ BlockHeader
    { blockHeight = 0
    , blockHash = "dummy-block-hash"
    , blockTimestamp = 0
    , prevBlockHash = "dummy-prev-block-hash"
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
adaptGenerateProof _ = do
  -- In a real implementation, this would generate a cryptographic proof
  -- For now, just return a dummy hash
  pure $ Hash "proof-hash"

-- | Adapter for verifying a proof (compatibility with old TimelineProof interface)
adaptVerifyProof :: 
  (Member (Error AppError) r) => 
  Hash -> 
  Hash -> 
  Sem r Bool
adaptVerifyProof _ _ = do
  -- In a real implementation, this would verify the proof
  -- For now, just return True
  pure True

-- | Adapter for sending a message (compatibility with old TimelineMessage interface)
adaptSendMessage :: 
  (Member (Error AppError) r, Message msg) => 
  msg -> 
  Sem r ()
adaptSendMessage _ = do
  -- In a real implementation, this would send the message
  -- For now, do nothing
  pure ()

-- | Adapter for broadcasting a message (compatibility with old TimelineMessage interface)
adaptBroadcastMessage :: 
  (Member (Error AppError) r, Message msg) => 
  msg -> 
  Sem r ()
adaptBroadcastMessage _ = do
  -- In a real implementation, this would broadcast the message
  -- For now, do nothing
  pure () 