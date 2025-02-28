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

Note: Common utilities have been moved to TimeBandits.Utils module for better organization.
-}
module TimeBandits.Events (
  -- * Type Classes
  Event (..),
  Message (..),

  -- * Event Types
  EventMetadata (..),
  LogEntry (..),

  -- * Re-exports from Utils

  -- ** Crypto Utilities
  derivePubKeyFromPrivKey,

  -- ** Event Utilities
  createEventMetadata,
  verifyEventSignature,
  createLogEntry,
  rootTimelineHash,

  -- ** Message Utilities
  createAuthenticatedMessage,
  verifyMessageSignatureWithKey,
) where

import Data.ByteString (ByteString)
import Data.Serialize (Serialize, encode)
import TimeBandits.Core (
  ActorEvent (..),
  ActorHash,
  EntityHash (..),
  EventContent (..),
  EventMetadata (..),
  Hash,
  LogEntry (..),
  PrivKey (..),
  PubKey (..),
  ResourceEvent (..),
  ResourceHash,
  TimelineEvent (..),
  TimelineHash,
  computeMessageHash,
 )
import TimeBandits.Types qualified as Core (
  Actor (..),
  AuthenticatedMessage (..),
  ContentAddressedMessage (..),
  Signature (..),
  verifySignature,
 )
import TimeBandits.Utils (
  createAuthenticatedMessage,
  createEventMetadata,
  createLogEntry,
  derivePubKeyFromPrivKey,
  rootTimelineHash,
  verifyEventSignature,
  verifyMessageSignatureWithKey,
 )

-- -----------------------------------------------------------------------------
-- Type Classes
-- -----------------------------------------------------------------------------

{- | Core type class for events in the system
This typeclass provides a unified interface for all event types, enabling:
- Consistent hashing and content addressing
- Conversion between specific event types and generic event content
- Timeline and actor association
- Event chaining through previous event references
- Access to event metadata
- Signature verification for authenticity
This abstraction allows the system to handle different event types uniformly.
-}
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

{- | Core type class for messages in the system
This typeclass provides a unified interface for all message types, enabling:
- Content addressing through message hashing
- Sender and destination identification
- Signature verification for authenticity
- Content access
- Conversion between messages and events when applicable
This abstraction allows the system to handle different message types uniformly
and integrate with the event system.
-}
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

-- -----------------------------------------------------------------------------
-- Type Class Instances
-- -----------------------------------------------------------------------------

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
