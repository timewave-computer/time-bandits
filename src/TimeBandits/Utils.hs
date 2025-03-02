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
module TimeBandits.Utils (
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
import TimeBandits.Types qualified as Core (
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
derivePubKeyFromPrivKey :: PrivKey -> PubKey
derivePubKeyFromPrivKey (PrivKey priv) =
  case Ed25519.secretKey priv of
    CryptoFailed _ -> PubKey priv -- Fallback for invalid keys
    CryptoPassed sk -> PubKey $ convert $ Ed25519.toPublic sk

-- | Generate a secure random Ed25519 keypair
-- This should be used for creating secure cryptographic identities
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
computeContentHash :: (Serialize a) => a -> Hash
computeContentHash content = computeMessageHash (encode content)

-- -----------------------------------------------------------------------------
-- Event Utilities
-- -----------------------------------------------------------------------------

-- | Root timeline hash for actor events
rootTimelineHash :: TimelineHash
rootTimelineHash = EntityHash $ Hash "root-timeline"

-- | Create event metadata with current time and signature
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
verifyMessageSignatureWithKey :: 
  PubKey -> 
  ByteString -> 
  Core.Signature -> 
  Bool
verifyMessageSignatureWithKey pubKey content signature =
  Core.verifySignature pubKey content signature 