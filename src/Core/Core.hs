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
Module: Core.Core
Description: Core functionality and primitives for the Time-Bandits system.

This module provides the fundamental primitives and abstractions for the Time-Bandits system,
including type classes, cryptographic functions, and error handling utilities.

Key components:
  * Basic type classes: 'Event' and 'Message' for the event-based architecture
  * Cryptographic functions for hashing, message verification, and signing
  * P2P networking helpers for node selection and rendezvous hashing
  * Core error handling utilities

The Core module serves as the foundation for the entire Time-Bandits architecture,
providing the essential building blocks used by all other components of the system.
All timeline operations, actor communications, resource management, and program execution
depend on the primitives defined here.
-}
module Core.Core (
  -- * Re-exports from Types
  ActorEvent(..),
  ActorEventType(..),
  ResourceEvent(..),
  ResourceEventType(..),
  TimelineEvent(..),
  TimelineEventType(..),
  EventContent(..),
  EntityHash(..),
  Hash(..),
  ActorHash,
  ResourceHash,
  TimelineHash,
  LamportTime(..),
  PubKey(..),
  PrivKey(..),
  Signature(..),
  ActorType(..),
  Actor(..),
  SystemConfig(..),
  Resource(..),
  ResourceCapability(..),
  LogEntry(..),
  Log(..),
  MapOfTime(..),
  SyncPoint(..),
  TransientDatastore(..),
  TransientStoredItem(..),
  EventMetadata(..),
  AppError(..),
  TimelineErrorType(..),
  ResourceErrorType(..),
  ActorErrorType(..),
  CryptoErrorType(..),
  StorageErrorType(..),
  ContentAddressedMessage(..),
  AuthenticatedMessage(..),
  UnifiedResourceTransaction(..),
  TransactionValidationResult(..),
  signMessage,
  
  -- * Type Classes
  Event (..),
  Message (..),

  -- * Hash Functions
  computeSha256,
  computeHash,
  computeAnchorProof,
  computePubKeyHash,
  computeMessageHash,
  computeAuthMessageHash,
  computeLogEntryHash,
  computeStoredItemHash,
  
  -- * P2P Helpers
  computeNodeScore,
  selectNodesForKey,
  
  -- * Error Handling
  eitherToError,
) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString ()
import Data.Serialize (Serialize, encode)
import Polysemy
import Polysemy.Error (Error, throw)
import Core.Types

-- Add necessary imports for P2P functions
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Relude ()
import Data.Word ()

-- | Core type class for events in the system
-- Events represent state changes in the system, tracked in append-only logs.
-- All events are signed by actors and form a causal chain through their previous hash references.
-- This enables verifiability of the entire state history while maintaining causal consistency.
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
-- Messages are used for communication between actors and can contain events.
-- All messages are authenticated (signed) and can be verified against the sender's public key.
-- This enables secure peer-to-peer communication in the decentralized network.
class (Serialize m) => Message m where
  messageHash :: m -> Hash
  messageHash = computeMessageHash
  messageSender :: m -> Actor
  messageDestination :: m -> Maybe ActorHash
  messageSignature :: m -> Signature
  messageContent :: m -> ByteString
  toEvent :: m -> Maybe EventContent
  verifyMessageSignature :: m -> Bool

-- | Compute a score for a node based on its hash and a key.
-- This is a core building block for rendezvous hashing, allowing
-- deterministic selection of nodes based on content.
-- Used to consistently route messages and data to appropriate nodes
-- in the peer-to-peer network.
computeNodeScore :: ActorHash -> ByteString -> Word64
computeNodeScore nodeHash key = 
  let nodeBytes = case nodeHash of 
        EntityHash (Hash bytes) -> bytes
      
      -- Combine key and node id for hashing
      combinedBytes = key <> nodeBytes
      
      -- Compute hash
      hash = SHA256.hash combinedBytes
      
      -- Use first 8 bytes as a score (convert to Word64)
      scoreBytes = BS.take 8 hash
      scoreWord = byteStringToWord64 scoreBytes
  in
  scoreWord

-- | Convert ByteString to Word64 (big-endian)
-- Safely converts a hash fragment to a numeric score.
-- This conversion is needed for the rendezvous hashing algorithm
-- to turn cryptographic hashes into sortable numeric values.
byteStringToWord64 :: ByteString -> Word64
byteStringToWord64 bs =
  let bs' = BS.append (BS.replicate (8 - BS.length bs) 0) bs
      w64 = LBS.fromStrict bs'
  in fromIntegral $ LBS.foldr (\b a -> a * 256 + fromIntegral b) 0 w64

-- | Select nodes for a key using rendezvous hashing.
-- Given a key and a list of nodes, returns the best nodes to handle
-- that key based on their computed scores.
-- This function implements consistent hashing to ensure data locality
-- and minimize repartitioning when nodes join or leave the network.
selectNodesForKey :: ByteString -> [ActorHash] -> Int -> [ActorHash]
selectNodesForKey key nodes count =
  let scoredNodes = map (\node -> (node, computeNodeScore node key)) nodes
      -- Sort by score (highest first)
      sortedNodes = map fst $ sortOn (Down . snd) scoredNodes
  in
  take count sortedNodes

-- | Computes the SHA-256 hash of a given ByteString.
-- This is the fundamental hashing function used throughout the system.
-- All content addressing and cryptographic verification is built on top
-- of this basic hash function.
computeSha256 :: ByteString -> Hash
computeSha256 = Hash . SHA256.hash

-- | Computes a hash for any serialized content, with an optional previous hash.
-- Used for creating hash chains where each hash depends on the previous one.
-- This is essential for maintaining the integrity of event sequences.
-- The inclusion of the previous hash creates tamper-evident chains that
-- ensure causal consistency in distributed logs.
computeHash :: (Serialize a) => a -> Maybe Hash -> Hash
computeHash content mPrev =
  let prevBytes = maybe mempty (\(Hash h) -> h) mPrev
      contentBytes = encode content
   in computeSha256 (prevBytes <> contentBytes)

-- | Get the content-addressable hash of a public key.
-- This creates a unique identifier for actors in the system based on their public key.
-- Actor identities are derived from their public keys, ensuring that actors can
-- only sign messages and events that will be verifiable by their public identity.
computePubKeyHash :: PubKey -> ActorHash
computePubKeyHash (PubKey bytes) = EntityHash $ computeSha256 bytes

-- | Compute a cryptographic receipt anchor proof for provenance tracking.
-- This creates a verifiable proof that links a resource to its timeline history,
-- essential for establishing the provenance chain across timelines.
-- When resources move between timelines, this proof maintains their history.
computeAnchorProof :: [TimelineHash] -> TimelineHash -> Hash
computeAnchorProof prevChain newTimeline =
  computeSha256 $ encode (map unEntityHash prevChain, unEntityHash newTimeline)

-- | Helper function to compute a content-addressed message hash.
-- Creates a unique identifier for any serializable content,
-- forming the basis of the content-addressable storage system.
-- This enables efficient lookup and verification of messages by their content.
computeMessageHash :: (Serialize a) => a -> Hash
computeMessageHash content =
  computeSha256 $ encode content

-- | Helper function to compute an authenticated message hash.
-- Includes sender, destination, payload, and signature in the hash
-- to ensure the entire message context is captured in the identifier.
-- This comprehensive hash enables verification of the full message context.
computeAuthMessageHash :: (Serialize a) => AuthenticatedMessage a -> Hash
computeAuthMessageHash msg =
  computeSha256 $
    encode
      ( amSender msg
      , amDestination msg
      , amPayload msg
      , amSignature msg
      )

-- | Helper function to compute a log entry's hash.
-- Combines content, metadata, and previous hash to create a unique identifier
-- for each log entry, maintaining the integrity of the append-only log.
-- Each log entry's hash depends on its previous entry, creating a chain.
computeLogEntryHash :: (Serialize a) => LogEntry a -> Hash
computeLogEntryHash entry =
  computeSha256 $
    encode
      ( leContent entry
      , leMetadata entry
      , lePrevHash entry
      )

-- | Helper function to compute a stored item's hash.
-- Used for content-addressable storage in the transient datastore,
-- ensuring data integrity and enabling efficient retrieval.
-- By hashing all components, we create a unique identifier for verification.
computeStoredItemHash :: TransientStoredItem -> Hash
computeStoredItemHash item =
  computeSha256 $
    encode
      ( siKey item
      , siContent item
      , siTimestamp item
      , siSignature item
      )

-- | Convert Either to Error effect.
-- Simplifies error handling by converting Either values to Polysemy Error effects,
-- allowing for more composable error handling throughout the codebase.
-- This helps standardize error management across the application.
eitherToError :: (Member (Error AppError) r) => Either AppError a -> Sem r a
eitherToError = either throw return
