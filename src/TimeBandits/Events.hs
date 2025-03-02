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

module TimeBandits.Events (
  -- * Type Classes
  Event (..),
  Message (..),

  -- * Event Types
  EventMetadata (..),
  LogEntry (..),

  -- * Re-exports from Utils
  derivePubKeyFromPrivKey,
  createEventMetadata,
  verifyEventSignature,
  createLogEntry,
  rootTimelineHash,
  createAuthenticatedMessage,
  verifyMessageSignatureWithKey,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Serialize (Serialize, encode, decode)
import TimeBandits.Core (computeMessageHash)
import TimeBandits.Types
import TimeBandits.Core qualified as Core
import TimeBandits.Utils

-- | Core type class for events in the system
class (Serialize e) => Event e where
  contentHash :: e -> Hash
  contentHash = computeMessageHash
  toEventContent :: e -> EventContent
  eventTimeline :: e -> TimelineHash
  eventActor :: e -> ActorHash
  previousEventHash :: e -> Maybe Hash
  metadata :: e -> EventMetadata
  verifySignature :: e -> Bool

-- | Core type class for messages in the system
class (Serialize m) => Message m where
  messageHash :: m -> Hash
  messageHash = computeMessageHash
  messageSender :: m -> Core.Actor
  messageDestination :: m -> Maybe ActorHash
  messageSignature :: m -> Core.Signature
  messageContent :: m -> ByteString
  toEvent :: m -> Maybe EventContent
  verifyMessageSignature :: m -> Bool

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
    case decode (Core.camContent $ Core.amPayload msg) of
      Right eventContent -> Just eventContent
      Left _ -> Nothing

  verifyMessageSignature msg =
    let content = Core.camContent $ Core.amPayload msg
        sig = Core.amSignature msg
        -- In a real implementation, we would need to retrieve the public key associated with 
        -- the sender actor from a registry or lookup service.
        -- Use a deterministic key derivation based on actor ID for demonstration
        sender = Core.amSender msg
        senderActorId = actorId sender
        pubKey = PubKey $ "key-for-" <> BS.pack (show senderActorId)
     in Core.verifySignature pubKey content sig

-- | Helper to get a public key for an actor (would connect to a key management system)
getPublicKeyForActor :: ActorHash -> PubKey
getPublicKeyForActor actorId = 
    -- In a real implementation, this would query a key management system
    -- For demonstration purposes, we're generating a deterministic key based on actor ID
    PubKey $ "key-for-" <> BS.pack (show actorId)
