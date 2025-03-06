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
import Data.ByteString ()
import Data.ByteString.Char8 qualified as BS
import Data.IORef qualified as IORef
import Data.Map.Strict qualified as Map
import Data.Serialize (encode, decode)
import Polysemy (Sem, Member, embed)
import Polysemy.Embed (Embed)
import Polysemy.Error (Error, throw)
import System.IO.Unsafe (unsafePerformIO)
import Core.Common (
  Event(..),
  Message(..)
  )
import Core.Types
    ( EventMetadata(..),
      EventContent(ActorEventContent, TimelineEventContent,
                   ResourceEventContent),
      LogEntry(..),
      TimelineEvent(teContent, teActor, tePreviousEvent, teMetadata),
      ResourceEvent(reContent, rePreviousEvent, reMetadata),
      ActorEvent(aeContent, aePreviousEvent, aeMetadata),
      Actor(actorId),
      Signature,
      PrivKey,
      PubKey(..),
      ActorHash,
      EntityHash(unEntityHash) )
import Core.Types qualified as Types
import Core.Common qualified as Core
import TimeBandits.Utils
import TimeBandits.Effects (
  KeyManagement,
  lookupPublicKey,
  verifyWithPublicKey,
  registerPublicKey,  
  registerActorType
  )
import Relude (newIORef)

-- | Instance for ActorEvent
-- Implements the Event typeclass for actor-related events, which automatically places 
-- actor events in the root timeline as they aren't timeline-specific.
instance Event ActorEvent where
  toEventContent event = ActorEventContent $ aeContent event
  eventTimeline = const rootTimelineHash  -- Actor events always use the root timeline
  eventActor event = emActor $ aeMetadata event
  previousEventHash = fmap unEntityHash . aePreviousEvent
  metadata = aeMetadata
  verifySignature event =
    let meta = aeMetadata event
        sig = emSignature meta
        content = encode $ aeContent event
     in Types.verifySignature (emSigner meta) content sig

-- | Instance for ResourceEvent
-- Implements the Event typeclass for resource events, which tracks ownership
-- and state changes to resources across different timelines.
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
     in Types.verifySignature (emSigner meta) content sig

-- | Instance for TimelineEvent
-- Implements the Event typeclass for timeline-specific events, which
-- captures the creation, merging, splitting, and other operations on timelines.
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
     in Types.verifySignature (emSigner meta) content sig

-- | Instance for AuthenticatedMessage
-- Implements the Message typeclass for authenticated messages, enabling
-- verified communication between actors across the network.
instance Message (Core.AuthenticatedMessage ByteString) where
  messageSender = Core.amSender
  messageDestination = Core.amDestination
  messageSignature = Core.amSignature
  messageContent = Core.camContent . Core.amPayload
  messageHash = Core.amHash

  toEvent msg =
    -- Try to decode the message content as an EventContent
    -- This allows converting messages into events when appropriate
    case decode (Core.camContent $ Core.amPayload msg) of
      Right eventContent -> Just eventContent
      Left _ -> Nothing

  -- Verify message authenticity using the sender's public key
  -- This implementation uses the key lookup system to find the right public key
  verifyMessageSignature msg =
    let content = Core.camContent $ Core.amPayload msg
        sig = Core.amSignature msg
        -- Retrieve the public key for this actor using proper key management
        sender = Core.amSender msg
        senderActorId = actorId sender
        -- Now we use the key lookup system - fallback to direct access for non-effectful code
        pubKey = unsafePerformIO $ do
          keyStore <- IORef.readIORef globalActorKeyStore
          case Map.lookup senderActorId keyStore of
            Just pk -> return pk
            Nothing -> return $ PubKey $ "key-for-" <> BS.pack (show senderActorId)
     in Types.verifySignature pubKey content sig

-- | Helper function to verify a message signature using a KeyManagement effect
-- This effect-based verification is used when message verification needs to
-- be composed with other effects in the application.
verifyMessageSignatureEffect :: 
  (Member KeyManagement r, Message m) => 
  m -> 
  Sem r Bool
verifyMessageSignatureEffect msg = do
  -- Get the public key for the sender using the KeyManagement effect
  let sender = messageSender msg
      senderActorId = actorId sender
  maybePubKey <- lookupPublicKey senderActorId
  case maybePubKey of
    Just pubKey -> 
      -- Verify the signature using the KeyManagement effect
      verifyWithPublicKey pubKey (messageContent msg) (messageSignature msg)
    Nothing ->
      -- Fall back to the direct implementation if no key is found
      return $ verifyMessageSignature msg

-- | Key-Value store for actor public keys
-- In a production system, this would be replaced with a persistent database
-- or a distributed key-value store with proper access control
type ActorKeyStore = Map.Map ActorHash PubKey

-- | Global reference to the actor key store
-- In a real production system, this would be replaced with a proper database connection
-- This is a temporary in-memory storage mechanism for actor public keys
{-# NOINLINE globalActorKeyStore #-}
globalActorKeyStore :: IORef.IORef ActorKeyStore
globalActorKeyStore = unsafePerformIO (newIORef Map.empty)

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
        return $ Types.verifySignature pubKey content signature
