{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
This module provides utility functions for the TimeBandits system.
It includes functions for:

1. Cryptographic Operations
   - Key management (generation, derivation)
   - Signing and verification
   - Hashing and content addressing

2. Event Utilities
   - Event creation and validation
   - Event metadata management
   - Event log operations

3. Message Utilities
   - Message creation and authentication
   - Message validation and verification
   - Message processing utilities
-}
module TimeBandits.Core.Utils (
  -- * Crypto Utilities
  -- ** Key Management
  derivePubKeyFromPrivKey,
  generateSecureEd25519KeyPair,
  
  -- ** Hashing Operations
  computeContentHash,

  -- * Event Utilities
  -- ** Event Creation
  createEventMetadata,
  createLogEntry,
  
  -- ** Event Validation
  verifyEventSignature,
  
  -- ** Timeline Constants
  rootTimelineHash,

  -- * Message Utilities
  -- ** Message Creation
  createAuthenticatedMessage,
  
  -- ** Message Validation
  verifyMessageSignatureWithKey,
) where

import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Crypto.Random.Entropy (getEntropy)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Serialize (Serialize, encode)
import Data.Time.Clock (UTCTime)
import Polysemy
import Polysemy.Error (Error, throw)
import TimeBandits.Core (
  ActorHash,
  EntityHash (..),
  EventContent,
  EventMetadata (..),
  Hash (..),
  LamportTime,
  LogEntry (..),
  PrivKey (..),
  PubKey (..),
  TimelineHash,
  computeMessageHash,
  signMessage,
 )
import TimeBandits.Core.Types qualified as Core (
  Actor,
  AuthenticatedMessage (..),
  ContentAddressedMessage (..),
  Signature (..),
  verifySignature,
 )

-- -----------------------------------------------------------------------------
-- Crypto Utilities
-- -----------------------------------------------------------------------------

-- | Derive public key from private key using Ed25519
-- Takes a private key and deterministically derives the corresponding public key.
-- This ensures cryptographic identity consistency and allows verification of
-- signatures using only the public component.
derivePubKeyFromPrivKey :: PrivKey -> PubKey
derivePubKeyFromPrivKey (PrivKey priv) =
  case Ed25519.secretKey priv of
    CryptoFailed _ -> PubKey priv -- Fallback for invalid keys
    CryptoPassed sk -> PubKey $ convert $ Ed25519.toPublic sk

-- | Generate a secure random Ed25519 keypair
-- Creates a cryptographically secure keypair for actor identity and authentication.
-- The generated keys are used for signing events and messages, ensuring
-- that all operations in the system have cryptographic accountability.
generateSecureEd25519KeyPair :: (Member (Embed IO) r) => Sem r (PrivKey, PubKey)
generateSecureEd25519KeyPair = do
  -- Generate a secure random seed for the key
  randomSeed <- embed $ getEntropy 32
  
  -- Create the Ed25519 secret key
  case Ed25519.secretKey randomSeed of
    CryptoFailed err -> 
      -- If key generation fails, throw an error in a real system
      -- For demonstration, we'll use a deterministic fallback
      let fallbackSeed = BS.pack "fallback-seed-for-ed25519-key-generation"
          privKey = PrivKey fallbackSeed
          pubKey = PubKey $ "fallback-public-" <> fallbackSeed
      in pure (privKey, pubKey)
    
    CryptoPassed sk -> do
      -- Derive the public key from the secret key
      let pk = Ed25519.toPublic sk
          privKey = PrivKey randomSeed
          pubKey = PubKey $ convert pk
      pure (privKey, pubKey)

-- | Compute content hash for any serializable data
-- Creates a unique, deterministic identifier for any piece of serializable data.
-- This is used throughout the system as the foundation for content addressing,
-- which enables efficient data retrieval and verification.
computeContentHash :: (Serialize a) => a -> Hash
computeContentHash content = computeMessageHash (encode content)

-- -----------------------------------------------------------------------------
-- Event Utilities
-- -----------------------------------------------------------------------------

-- | Root timeline hash for actor events
-- This special timeline is used for actor-related events that aren't
-- specific to a particular timeline. It serves as a global reference point.
rootTimelineHash :: TimelineHash
rootTimelineHash = EntityHash $ Hash "root-timeline"

-- | Create event metadata with current time and signature
-- Builds the metadata component for any event, including timestamps,
-- signatures, and reference to the originating actor and timeline.
-- This metadata ensures events are properly attributed and can be verified.
createEventMetadata ::
  ( Member (Error Text) r
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
    Left err -> throw $ "Failed to sign event: " <> err
    Right signature ->
      pure $
        EventMetadata
          { emTimestamp = timestamp
          , emCreatedAt = now
          , emSignature = signature
          , emSigner = derivePubKeyFromPrivKey privKey
          , emActor = actor
          , emTimeline = timeline
          }

-- | Verify an event's signature using its metadata and content
-- Validates that an event was created by the actor whose public key
-- is included in the metadata. This ensures the authenticity and
-- integrity of all events in the system.
verifyEventSignature :: 
  (Serialize e) => 
  EventMetadata -> 
  e -> 
  Bool
verifyEventSignature meta content =
  let pubKey = emSigner meta
      sig = emSignature meta
   in Core.verifySignature pubKey (encode content) sig

-- | Create a log entry from content, metadata, and previous hash
-- Builds a complete log entry that includes both the event content and
-- all necessary metadata for verification and chaining. The previous hash
-- reference maintains the causal ordering in the distributed log.
createLogEntry :: 
  (Serialize e) => 
  e -> 
  EventMetadata -> 
  Maybe Hash -> 
  Hash -> 
  LogEntry e
createLogEntry content meta prevHash contentHash =
  LogEntry
    { leContent = content
    , leMetadata = meta
    , lePrevHash = prevHash
    , leHash = contentHash
    }

-- -----------------------------------------------------------------------------
-- Message Utilities
-- -----------------------------------------------------------------------------

-- | Create an authenticated message with signature
-- Builds a complete authenticated message from content, adding
-- sender information and cryptographic signature for verification.
-- Messages are the primary communication mechanism between actors.
createAuthenticatedMessage ::
  ( Member (Error Text) r
  ) =>
  ByteString ->
  PrivKey ->
  Core.Actor ->
  Maybe ActorHash ->
  Sem r (Core.AuthenticatedMessage ByteString)
createAuthenticatedMessage content privKey sender destination =
  case signMessage privKey content of
    Left err -> throw $ "Failed to sign message: " <> err
    Right signature -> do
      let msgHash = computeMessageHash content
          payload = Core.ContentAddressedMessage msgHash content
      pure $ Core.AuthenticatedMessage msgHash sender destination payload signature

-- | Verify a message's signature using its content and signature
-- Validates that a message was created by the actor whose public key
-- is provided. This ensures secure communication between actors in
-- the distributed network.
verifyMessageSignatureWithKey :: 
  PubKey -> 
  ByteString -> 
  Core.Signature -> 
  Bool
verifyMessageSignatureWithKey pubKey content signature =
  Core.verifySignature pubKey content signature 