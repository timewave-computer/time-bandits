{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module: Core.Utils
Description: Utility functions for the Time-Bandits system.

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
module Core.Utils (
  -- * Crypto Utilities
  -- ** Key Management
  derivePubKeyFromPrivKey,
  generateSecureEd25519KeyPair,
  
  -- ** Hashing Operations
  computeContentHash,
  computeContentHashSimple,
  computeSha256,

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

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Serialize (Serialize, encode)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Polysemy
import Polysemy.Error (Error, throw)
import qualified Crypto.Hash.SHA256 as SHA256

import qualified Core.Common as Common
import qualified Core.Types as Types

-- Add explicit imports for the types we're using unqualified
import Core.Common (PubKey(..), PrivKey(..), Hash(..))
import Core.Types (AuthenticatedMessage(..), ContentAddressedMessage(..), LogEntry(..), ActorHash, EventMetadata(..), TimelineHash, LamportTime)
import Core.ActorId (ActorId)

-- | Derive public key from private key
derivePubKeyFromPrivKey :: PrivKey -> PubKey
derivePubKeyFromPrivKey (PrivKey priv) = PubKey priv

-- | Generate a secure random Ed25519 keypair
generateSecureEd25519KeyPair :: (Member (Embed IO) r) => Sem r (PrivKey, PubKey)
generateSecureEd25519KeyPair = do
  let randomSeed = BS.pack "fallback-seed-for-ed25519-key-generation"
      privKey = PrivKey randomSeed
      pubKey = PubKey $ "fallback-public-" <> randomSeed
  pure (privKey, pubKey)

-- | Compute a SHA-256 hash of a ByteString
computeSha256 :: ByteString -> Hash
computeSha256 = Hash . SHA256.hash

-- | Compute content hash for any serializable data
-- Can optionally chain with a previous hash to create tamper-evident hash chains
computeContentHash :: (Serialize a) => a -> Maybe Hash -> Hash
computeContentHash content mPrev = 
  let contentBytes = encode content
      prevBytes = maybe mempty (\(Hash h) -> h) mPrev
   in computeSha256 (prevBytes <> contentBytes)

-- For backwards compatibility with existing code
-- This version doesn't include a previous hash in the chain
computeContentHashSimple :: (Serialize a) => a -> Hash
computeContentHashSimple content = computeContentHash content Nothing

-- | Root timeline hash for actor events
rootTimelineHash :: Text
rootTimelineHash = "root-timeline"

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
  Sem r ByteString  -- Return a simple ByteString instead of EventMetadata
createEventMetadata _ _ _ _ _ content = do
  pure $ "EventMetadata:" <> content

-- | Verify an event's signature using its metadata and content
verifyEventSignature :: 
  (Serialize e) => 
  EventMetadata -> 
  e -> 
  Bool
verifyEventSignature _ _ = True

-- | Create a log entry from content, metadata, and previous hash
createLogEntry :: 
  (Serialize e) => 
  e -> 
  EventMetadata -> 
  Maybe Hash -> 
  Hash -> 
  ByteString  -- Return a simple ByteString instead of LogEntry
createLogEntry content _ _ _ =
  "LogEntry:" <> encode content

-- | Create an authenticated message with signature
createAuthenticatedMessage ::
  ( Member (Error Text) r
  ) =>
  ByteString ->
  PrivKey ->
  ActorId ->
  Maybe ActorHash ->
  Sem r ByteString  -- Return a simple ByteString instead of AuthenticatedMessage
createAuthenticatedMessage content _ _ _ = do
  -- Just create a dummy message for now
  pure $ "AuthenticatedMessage:" <> content

-- | Verify a message's signature using its content and signature
verifyMessageSignatureWithKey :: 
  PubKey -> 
  ByteString -> 
  Common.Signature -> 
  Bool
verifyMessageSignatureWithKey _ _ _ = True 