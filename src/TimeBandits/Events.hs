{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
This module provides a unified event system using type classes to handle
different event types consistently and efficiently.
It defines:
1. Core type classes for events
2. Event validation and verification
3. Event processing and effects
4. Common event utilities
-}
module TimeBandits.Events (
  -- * Type Classes
  Event (..),
  EventLogger (..),
  Message (..),

  -- * Event Types
  EventMetadata (..),
  EventResult (..),
  ValidationResult (..),
  EventError (..),
  LogEntry (..),

  -- * Message Types
  MessageResult (..),
  MessageError (..),

  -- * Event Utilities
  createEventMetadata,
  verifyEventSignature,
  createLogEntry,

  -- * Message Utilities
  createAuthenticatedMessage,
  verifyMessageSignature,
) where

import Data.ByteString ()
import Data.Map.Strict ()
import Data.Serialize (Serialize, encode)
import Data.Text ()
import Data.Time.Clock (UTCTime)
import Polysemy
import Polysemy.Error (Error, throw)
import TimeBandits.Core (
  ActorEvent (..),
  ActorHash,
  EntityHash (..),
  EventContent (..),
  EventMetadata (..),
  Hash (..),
  LamportTime,
  LogEntry (..),
  PrivKey (..),
  PubKey (..),
  ResourceEvent (..),
  ResourceHash,
  TimelineEvent (..),
  TimelineHash,
  computeMessageHash,
  signMessage,
 )
import TimeBandits.Types qualified as Core (
  Actor (..),
  AuthenticatedMessage (..),
  ContentAddressedMessage (..),
  Signature (..),
  verifySignature,
 )

-- | Core type class for events in the system
class (Serialize e) => Event e where
  -- | Get the content hash of the event
  contentHash :: e -> Hash
  contentHash = computeMessageHash

  -- | Convert event to event content
  toEventContent :: e -> EventContent

  -- | Get the timeline this event belongs to
  eventTimeline :: e -> TimelineHash

  -- | Get the actor who created this event
  eventActor :: e -> ActorHash

  -- | Get the previous event hash (if any)
  previousEventHash :: e -> Maybe Hash

  -- | Get the event's metadata
  metadata :: e -> EventMetadata

  -- | Verify the event's signature
  verifySignature :: e -> Bool

-- | Root timeline hash for actor events
rootTimelineHash :: TimelineHash
rootTimelineHash = EntityHash $ Hash "root-timeline"

-- | Type class for event logging
class EventLogger e where
  -- | Create a log entry from an event
  toLogEntry :: e -> LogEntry EventContent

  -- | Get all log entries for a timeline
  getTimelineLog :: (Member (Error EventError) r) => e -> TimelineHash -> Sem r [LogEntry EventContent]

  -- | Get all log entries for an actor
  getActorLog :: (Member (Error EventError) r) => e -> ActorHash -> Sem r [LogEntry EventContent]

  -- | Get all log entries for a resource
  getResourceLog :: (Member (Error EventError) r) => e -> ResourceHash -> Sem r [LogEntry EventContent]

  -- | Append a log entry to a timeline
  appendToTimeline :: (Member (Error EventError) r) => e -> TimelineHash -> LogEntry EventContent -> Sem r ()

-- | Result of event processing
data EventResult e
  = -- | Event processed successfully
    Success e
  | -- | Event processing deferred (e.g., missing deps)
    Deferred e
  | -- | Event processing failed
    Failed EventError
  deriving stock (Show, Eq)

-- | Result of event validation
data ValidationResult
  = -- | Event is valid
    Valid
  | -- | Event is invalid with reason
    Invalid Text
  | -- | Event depends on missing data
    MissingDependency Text
  deriving stock (Show, Eq)

-- | Errors that can occur during event handling
data EventError
  = -- | Event failed validation
    ValidationError Text
  | -- | Error during processing
    ProcessingError Text
  | -- | Error storing/retrieving event
    StorageError Text
  | -- | Invalid signature
    SignatureError Text
  | -- | Missing or invalid dependency
    DependencyError Text
  deriving stock (Show, Eq)

-- | Create event metadata with current time and signature
createEventMetadata ::
  ( Member (Error EventError) r
  ) =>
  LamportTime ->
  UTCTime ->
  PrivKey ->
  ActorHash ->
  TimelineHash ->
  ByteString ->
  Sem r EventMetadata
createEventMetadata timestamp now privKey actor timeline content =
  case signMessage privKey content of
    Left err -> throw $ SignatureError "Failed to sign event"
    Right signature ->
      pure $
        EventMetadata
          { emTimestamp = timestamp
          , emCreatedAt = now
          , emSignature = signature
          , emSigner = derivePubKey privKey
          , emActor = actor
          , emTimeline = timeline
          }

-- | Verify an event's signature
verifyEventSignature :: (Event e) => e -> Bool
verifyEventSignature event =
  let meta = metadata event
      pubKey = emSigner meta
      sig = emSignature meta
      content = toEventContent event
   in Core.verifySignature pubKey (encode content) sig

-- Helper function to derive public key from private key
derivePubKey :: PrivKey -> PubKey
derivePubKey (PrivKey priv) = PubKey priv -- TODO: Implement proper public key derivation

-- | Create a log entry from an event and metadata
createLogEntry :: (Event e) => e -> LogEntry EventContent
createLogEntry event =
  LogEntry
    { leContent = toEventContent event
    , leMetadata = metadata event
    , lePrevHash = previousEventHash event
    , leHash = contentHash event
    }

-- | Instance for ActorEvent
instance Event ActorEvent where
  toEventContent event = ActorEventContent $ aeContent event
  eventTimeline = const rootTimelineHash
  eventActor event = emActor $ aeMetadata event
  previousEventHash = fmap unEntityHash . aePreviousEvent
  metadata = aeMetadata
  verifySignature event =
    let meta = aeMetadata event
        sig = emSignature meta
        content = encode $ aeContent event
     in Core.verifySignature (emSigner meta) content sig

instance EventLogger ActorEvent where
  toLogEntry = createLogEntry
  getTimelineLog _ _ = undefined -- TODO: Implement
  getActorLog _ _ = undefined -- TODO: Implement
  getResourceLog _ _ = pure [] -- Actors don't have resource logs
  appendToTimeline _ _ _ = undefined -- TODO: Implement

-- | Instance for ResourceEvent
instance Event ResourceEvent where
  toEventContent event = ResourceEventContent $ reContent event
  eventTimeline event = emTimeline $ reMetadata event
  eventActor event = emActor $ reMetadata event
  previousEventHash = fmap unEntityHash . rePreviousEvent
  metadata = reMetadata
  verifySignature event =
    let meta = reMetadata event
        sig = emSignature meta
        content = encode $ reContent event
     in Core.verifySignature (emSigner meta) content sig

instance EventLogger ResourceEvent where
  toLogEntry = createLogEntry
  getTimelineLog _ _ = undefined -- TODO: Implement
  getActorLog _ _ = pure [] -- Resources don't have actor logs
  getResourceLog _ _ = undefined -- TODO: Implement
  appendToTimeline _ _ _ = undefined -- TODO: Implement

-- | Instance for TimelineEvent
instance Event TimelineEvent where
  toEventContent event = TimelineEventContent $ teContent event
  eventTimeline event = emTimeline $ teMetadata event
  eventActor = teActor
  previousEventHash event = unEntityHash <$> tePreviousEvent event
  metadata = teMetadata
  verifySignature event =
    let meta = teMetadata event
        sig = emSignature meta
        content = encode $ teContent event
     in Core.verifySignature (emSigner meta) content sig

instance EventLogger TimelineEvent where
  toLogEntry = createLogEntry
  getTimelineLog _ _ = pure [] -- Timelines don't have timeline logs
  getActorLog _ _ = pure [] -- Timelines don't have actor logs
  getResourceLog _ _ = pure [] -- Timelines don't have resource logs
  appendToTimeline _ _ _ = pure () -- TODO: Implement

-- | Core type class for messages in the system
class (Serialize m) => Message m where
  -- | Get the content hash of the message
  messageHash :: m -> Hash
  messageHash = computeMessageHash

  -- | Get the sender of the message
  messageSender :: m -> Core.Actor

  -- | Get the destination of the message (if any)
  messageDestination :: m -> Maybe ActorHash

  -- | Get the message signature
  messageSignature :: m -> Core.Signature

  -- | Get the message content
  messageContent :: m -> ByteString

  -- | Convert message to event content (if applicable)
  toEvent :: m -> Maybe EventContent

  -- | Verify the message's signature
  verifyMessageSignature :: m -> Bool

-- | Result of message processing
data MessageResult m
  = -- | Message processed successfully
    MessageSuccess m
  | -- | Message processing deferred
    MessageDeferred m
  | -- | Message processing failed
    MessageFailed MessageError
  deriving stock (Show, Eq)

-- | Errors that can occur during message handling
data MessageError
  = -- | Message failed validation
    MessageValidationError Text
  | -- | Error during processing
    MessageProcessingError Text
  | -- | Invalid signature
    MessageSignatureError Text
  | -- | Delivery error
    DeliveryError Text
  | -- | Conversion error
    ConversionError Text
  deriving stock (Show, Eq)

-- | Create an authenticated message
createAuthenticatedMessage ::
  ( Member (Error MessageError) r
  ) =>
  ByteString ->
  PrivKey ->
  Core.Actor ->
  Maybe ActorHash ->
  Sem r (Core.AuthenticatedMessage ByteString)
createAuthenticatedMessage content privKey sender destination =
  case signMessage privKey content of
    Left err -> throw $ MessageSignatureError "Failed to sign message"
    Right signature -> do
      let msgHash = computeMessageHash content
          payload = Core.ContentAddressedMessage msgHash content
      pure $ Core.AuthenticatedMessage msgHash sender destination payload signature

-- | Instance for AuthenticatedMessage
instance Message (Core.AuthenticatedMessage ByteString) where
  messageSender = Core.amSender
  messageDestination = Core.amDestination
  messageSignature = Core.amSignature
  messageContent = Core.camContent . Core.amPayload
  messageHash = Core.amHash

  toEvent msg =
    -- Try to decode the message content as an EventContent
    -- This is a simplified implementation
    Nothing -- TODO: Implement proper conversion

  verifyMessageSignature msg =
    let content = Core.camContent $ Core.amPayload msg
        sig = Core.amSignature msg
        -- TODO: Get proper public key from sender
        pubKey = PubKey "TODO"
     in Core.verifySignature pubKey content sig
