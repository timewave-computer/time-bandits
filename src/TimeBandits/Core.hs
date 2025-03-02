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

module TimeBandits.Core (
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
import Data.ByteString (ByteString)
import Data.Serialize (Serialize, encode)
import Polysemy
import Polysemy.Error (Error, throw)
import TimeBandits.Types

-- Add necessary imports for P2P functions
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Ord (Down(..))
import Data.List (sortOn)
import Data.Word (Word64)

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
  messageSender :: m -> Actor
  messageDestination :: m -> Maybe ActorHash
  messageSignature :: m -> Signature
  messageContent :: m -> ByteString
  toEvent :: m -> Maybe EventContent
  verifyMessageSignature :: m -> Bool

-- Add this helper for consistent node selection
{- | Compute a score for a node based on its hash and a key.
This is a core building block for rendezvous hashing, allowing
deterministic selection of nodes based on content.
-}
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

{- | Convert ByteString to Word64 (big-endian)
Safely converts a hash fragment to a numeric score.
-}
byteStringToWord64 :: ByteString -> Word64
byteStringToWord64 bs =
  let bs' = BS.append (BS.replicate (8 - BS.length bs) 0) bs
      w64 = LBS.fromStrict bs'
  in fromIntegral $ LBS.foldr (\b a -> a * 256 + fromIntegral b) 0 w64

{- | Select nodes for a key using rendezvous hashing.
Given a key and a list of nodes, returns the best nodes to handle
that key based on their computed scores.
-}
selectNodesForKey :: ByteString -> [ActorHash] -> Int -> [ActorHash]
selectNodesForKey key nodes count =
  let scoredNodes = map (\node -> (node, computeNodeScore node key)) nodes
      -- Sort by score (highest first)
      sortedNodes = map fst $ sortOn (Down . snd) scoredNodes
  in
  take count sortedNodes

{- | Computes the SHA-256 hash of a given ByteString.
This is the fundamental hashing function used throughout the system.
-}
computeSha256 :: ByteString -> Hash
computeSha256 = Hash . SHA256.hash

{- | Computes a hash for any serialized content, with an optional previous hash.
Used for creating hash chains where each hash depends on the previous one.
This is essential for maintaining the integrity of event sequences.
-}
computeHash :: (Serialize a) => a -> Maybe Hash -> Hash
computeHash content mPrev =
  let prevBytes = maybe mempty (\(Hash h) -> h) mPrev
      contentBytes = encode content
   in computeSha256 (prevBytes <> contentBytes)

{- | Get the content-addressable hash of a public key.
This creates a unique identifier for actors in the system based on their public key.
-}
computePubKeyHash :: PubKey -> ActorHash
computePubKeyHash (PubKey bytes) = EntityHash $ computeSha256 bytes

{- | Compute a cryptographic receipt anchor proof for provenance tracking.
This creates a verifiable proof that links a resource to its timeline history,
essential for establishing the provenance chain across timelines.
-}
computeAnchorProof :: [TimelineHash] -> TimelineHash -> Hash
computeAnchorProof prevChain newTimeline =
  computeSha256 $ encode (map unEntityHash prevChain, unEntityHash newTimeline)

{- | Helper function to compute a content-addressed message hash.
Creates a unique identifier for any serializable content,
forming the basis of the content-addressable storage system.
-}
computeMessageHash :: (Serialize a) => a -> Hash
computeMessageHash content =
  computeSha256 $ encode content

{- | Helper function to compute an authenticated message hash.
Includes sender, destination, payload, and signature in the hash
to ensure the entire message context is captured in the identifier.
-}
computeAuthMessageHash :: (Serialize a) => AuthenticatedMessage a -> Hash
computeAuthMessageHash msg =
  computeSha256 $
    encode
      ( amSender msg
      , amDestination msg
      , amPayload msg
      , amSignature msg
      )

{- | Helper function to compute a log entry's hash.
Combines content, metadata, and previous hash to create a unique identifier
for each log entry, maintaining the integrity of the append-only log.
-}
computeLogEntryHash :: (Serialize a) => LogEntry a -> Hash
computeLogEntryHash entry =
  computeSha256 $
    encode
      ( leContent entry
      , leMetadata entry
      , lePrevHash entry
      )

{- | Helper function to compute a stored item's hash.
Used for content-addressable storage in the transient datastore,
ensuring data integrity and enabling efficient retrieval.
-}
computeStoredItemHash :: TransientStoredItem -> Hash
computeStoredItemHash item =
  computeSha256 $
    encode
      ( siKey item
      , siContent item
      , siTimestamp item
      , siSignature item
      )

{- | Convert Either to Error effect.
Simplifies error handling by converting Either values to Polysemy Error effects,
allowing for more composable error handling throughout the codebase.
-}
eitherToError :: (Member (Error AppError) r) => Either AppError a -> Sem r a
eitherToError = either throw return
