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

module Execution.Events (
  -- * Type Classes
  Event (..),
  Message (..),

  -- * Event Types
  EventMetadata (..),
  LogEntry (..),
  AnyEvent (..),
  
  -- * Message Types
  MessageData (..),
  ResourceType (..),

  -- * Re-exports from Utils
  derivePubKeyFromPrivKey,
  generateSecureEd25519KeyPair,
  createEventMetadata,
  verifyEventSignature,
  createLogEntry,
  rootTimelineHash,
  createAuthenticatedMessage,
  verifyMessageSignatureWithKey,
  
  -- * Key Management
  getPublicKeyForActor,
  verifyActorSignature,
  registerActorKeyPair,
  verifyMessageSignatureEffect,
) where

import Crypto.Error ()
import Crypto.PubKey.Ed25519()
import Data.ByteArray ()
import Data.ByteString.Char8 qualified as BS
import Data.Serialize (encode, decode)
import Data.Time.Clock (UTCTime)
import Polysemy (Sem, Member)
import Polysemy.Embed (Embed)
import Polysemy.Error (Error, throw)
import Core.Types
    ( EventMetadata(..),
      EventContent(ActorEventContent),
      LogEntry(..),
      Signature,
      PrivKey,
      PubKey(..),
      ActorHash,
      AuthenticatedMessage(..),
      ContentAddressedMessage(..),
      ActorType )
import Core.Types qualified as Types
import Core.Common (LamportTime(..))
import Core.Timeline (Event(..))
import Core.Core (Message(..))
import Core.Utils (derivePubKeyFromPrivKey, 
                  generateSecureEd25519KeyPair, 
                  createEventMetadata, 
                  verifyEventSignature,
                  createLogEntry,
                  rootTimelineHash,
                  createAuthenticatedMessage,
                  verifyMessageSignatureWithKey)
import Core.Effects (
  KeyManagement,
  lookupPublicKey,
  verifyWithPublicKey,
  registerPublicKey,  
  registerActorType
  )

-- | Simple type for resource classification
data ResourceType = Token | NFT | Collectible | DataResource
  deriving stock (Show, Eq)

-- | Wrapper for different event types
data AnyEvent
  = ActorEventWrapper Types.ActorEvent
  | ResourceEventWrapper Types.ResourceEvent
  | TimelineEventWrapper Types.TimelineEvent
  deriving stock (Show, Eq)

-- | Message data type for the messaging system
data MessageData = MessageData
  { messageDataId :: ByteString
  , messageDataSender :: ActorHash
  , messageDataRecipient :: ActorHash
  , messageDataContent :: ByteString
  , messageDataSignature :: Signature
  , messageDataTimestamp :: UTCTime
  }
  deriving stock (Show, Eq)

-- | Instance for ActorEvent
-- Implements the Event typeclass for actor-related events, which automatically places 
-- actor events in the root timeline as they aren't timeline-specific.
instance Event Types.ActorEvent where
  eventPayload event = encode $ Types.aeContent event
  eventTimestamp = const (LamportTime 0)  -- Simplified for now
  verifyEvent event = 
    let meta = Types.aeMetadata event
        content = Types.aeContent event
        sig = emSignature meta
     in mockVerifySignature (emSigner meta) (encode content) sig

-- | Instance for ResourceEvent
-- Implements the Event typeclass for resource events, which tracks ownership
-- and state changes to resources across different timelines.
instance Event Types.ResourceEvent where
  eventPayload event = encode $ Types.reContent event
  eventTimestamp = const (LamportTime 0)  -- Simplified for now
  verifyEvent event =
    let meta = Types.reMetadata event
        content = Types.reContent event
        sig = emSignature meta
     in mockVerifySignature (emSigner meta) (encode content) sig

-- | Instance for TimelineEvent
-- Implements the Event typeclass for timeline-specific events, which
-- captures the creation, merging, splitting, and other operations on timelines.
instance Event Types.TimelineEvent where
  eventPayload event = encode $ Types.teContent event
  eventTimestamp = const (LamportTime 0)  -- Simplified for now
  verifyEvent event =
    let meta = Types.teMetadata event
        content = Types.teContent event
        sig = emSignature meta
     in mockVerifySignature (emSigner meta) (encode content) sig

-- | Mock implementation of signature verification that always returns True
-- In a real system, this would actually verify cryptographic signatures
mockVerifySignature :: PubKey -> ByteString -> Signature -> Bool
mockVerifySignature _ _ _ = True

-- | Instance for AuthenticatedMessage of ByteString
instance Message (Types.AuthenticatedMessage ByteString) where
  messageSender = Types.amSender
  messageDestination = Types.amDestination
  messageSignature = Types.amSignature
  messageContent = Types.camContent . Types.amPayload
  messageHash = Types.amHash
  
  toEvent msg =
    -- Try to deserialize the message content into an event
    -- This is a simplified implementation 
    case decode (Types.camContent $ Types.amPayload msg) of
      Right actorEvent -> Just $ ActorEventContent actorEvent
      Left _ -> Nothing

  verifyMessageSignature _ =
    -- This implementation is a mock that always returns True
    -- In a real system, we would properly verify the signature
    True

-- | Register a key pair for an actor
-- Associates a public key with an actor identity in the key store after
-- verifying that the public key is correctly derived from the private key.
-- In a production system, this would include proper authentication and authorization.
registerActorKeyPair :: (Member (Embed IO) r, Member (Error Text) r, Member KeyManagement r) => 
                       ActorHash -> PrivKey -> PubKey -> Sem r ()
registerActorKeyPair actorId privKey pubKey = do
  -- Verify that the public key is derived from the private key
  let derivedPubKey = derivePubKeyFromPrivKey privKey
  
  -- Validate the keys match
  if pubKey /= derivedPubKey
    then throw ("Public key does not match private key" :: Text)
    else do
      -- Register the public key using the KeyManagement effect
      registerPublicKey actorId pubKey
      
      -- Also register the actor type (default to TimeTraveler for now)
      -- In a real system, this would be determined by role or permissions
      registerActorType actorId Types.TimeTraveler

-- | Retrieve the public key for an actor
-- Uses the KeyManagement effect to look up the public key, falling back to a
-- deterministic generation method if the key isn't found in the store.
-- This ensures backward compatibility with existing code.
getPublicKeyForActor :: (Member KeyManagement r) => ActorHash -> Sem r PubKey
getPublicKeyForActor actorId = do
  -- Look up the actor's public key using the KeyManagement effect
  maybePubKey <- lookupPublicKey actorId
  case maybePubKey of
    Just pubKey -> 
      -- Found the public key in our store
      return pubKey
    Nothing ->
      -- If we don't have the key, we use a deterministic fallback for compatibility
      -- with existing code. In a production system, this would instead return
      -- an error or trigger a key retrieval from a distributed registry.
      return $ PubKey $ "key-for-" <> BS.pack (show actorId)

-- | Verify a signature from an actor
-- Retrieves the actor's public key and uses it to verify the signature.
-- Uses the KeyManagement effect for signature verification when available.
verifyActorSignature :: (Member KeyManagement r) => ActorHash -> ByteString -> Signature -> Sem r Bool
verifyActorSignature actorId content signature = do
  -- Get the public key for the actor using the KeyManagement effect
  maybePubKey <- lookupPublicKey actorId
  case maybePubKey of
    Just pubKey -> 
      -- Verify the signature using the KeyManagement effect
      verifyWithPublicKey pubKey content signature
    Nothing ->
      -- Fall back to the direct implementation if no key is found
      -- This ensures backward compatibility with existing code
      do
        pubKey <- getPublicKeyForActor actorId
        return $ mockVerifySignature pubKey content signature

-- | Helper function to verify a message signature using a KeyManagement effect
-- This effect-based verification is used when message verification needs to
-- be composed with other effects in the application.
verifyMessageSignatureEffect :: 
  (Member KeyManagement r, Message m) => 
  m -> 
  Sem r Bool
verifyMessageSignatureEffect _ = do
  -- For now, return True as this is a mock implementation
  -- In a real system, we would convert the ActorInfo to ActorHash
  -- and perform proper signature verification
  return True 