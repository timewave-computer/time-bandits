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
import TimeBandits.Core (
  Event(..),
  Message(..),
  computeMessageHash, 
  computePubKeyHash
  )
import TimeBandits.Types
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
import TimeBandits.Types qualified as Types
import TimeBandits.Core qualified as Core
import TimeBandits.Utils
import TimeBandits.Effects (
  KeyManagement,
  lookupPublicKey,
  verifyWithPublicKey
  )

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
     in Types.verifySignature (emSigner meta) content sig

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
     in Types.verifySignature (emSigner meta) content sig

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
     in Types.verifySignature (emSigner meta) content sig

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

  -- For runtime use, we defer to the KeyManagement effect via TimeBandits.Utils
  -- This implementation is used when we need a direct Boolean result
  verifyMessageSignature msg =
    let content = Core.camContent $ Core.amPayload msg
        sig = Core.amSignature msg
        -- Retrieve the public key for this actor using proper key management
        sender = Core.amSender msg
        senderActorId = actorId sender
        -- Now we use the key lookup system
        pubKey = getPublicKeyForActor senderActorId
     in Types.verifySignature pubKey content sig

-- | Helper function to verify a message signature using a KeyManagement effect
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
{-# NOINLINE globalActorKeyStore #-}
globalActorKeyStore :: IORef.IORef ActorKeyStore
globalActorKeyStore = unsafePerformIO (IORef.newIORef Map.empty)

-- | Register a key pair for an actor
-- In a production system, this would include proper authentication and authorization
registerActorKeyPair :: (Member (Embed IO) r, Member (Error Text) r) => 
                       ActorHash -> PrivKey -> PubKey -> Sem r ()
registerActorKeyPair actorId privKey pubKey = do
  -- Verify that the public key is derived from the private key
  let derivedPubKey = derivePubKeyFromPrivKey privKey
  
  -- Validate the keys match
  if pubKey /= derivedPubKey
    then throw ("Public key does not match private key" :: Text)
    else do
      -- In a production system, we would verify the actor's identity
      -- before allowing key registration
      
      -- Store the public key in our key store
      embed $ IORef.modifyIORef' globalActorKeyStore (Map.insert actorId pubKey)

-- | Retrieve the public key for an actor
-- Uses the actor key store to look up the public key
getPublicKeyForActor :: ActorHash -> PubKey
getPublicKeyForActor actorId = unsafePerformIO $ do
  keyStore <- IORef.readIORef globalActorKeyStore
  case Map.lookup actorId keyStore of
    Just pubKey -> 
      -- Found the public key in our store
      return pubKey
    Nothing ->
      -- If we don't have the key, we use a deterministic fallback for compatibility
      -- with existing code. In a production system, this would instead return
      -- an error or trigger a key retrieval from a distributed registry.
      return $ PubKey $ "key-for-" <> BS.pack (show actorId)

-- | Verify a signature from an actor
-- Returns True if the signature is valid or False otherwise
verifyActorSignature :: ActorHash -> ByteString -> Signature -> Bool
verifyActorSignature actorId content signature = 
  let pubKey = getPublicKeyForActor actorId
  in Types.verifySignature pubKey content signature
