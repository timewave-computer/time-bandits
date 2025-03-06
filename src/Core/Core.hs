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
module Core.Core 
  ( -- Re-export Core modules
    -- * Types from Core.Types
    module Core.Types
    
    -- * Core functionality
  , Event
  , computeNodeScore
  , byteStringToWord64
  , selectNodesForKey
  , computePubKeyHash
  , computeAnchorProof
  , computeMessageHash
  , computeAuthMessageHash
  , computeLogEntryHash
  , computeStoredItemHash
  , eitherToError
  ) where

import Core.Types
import Core.Effects hiding (UnifiedResourceTransaction(..))
import Core.Resource hiding (Resource(..))
import Core.ResourceLedger hiding (ResourceNotFound(..))
import Core.Serialize
import Core.Timeline hiding (Event(..))
import Core.TimelineDescriptor
import Core.TimeMap
import Core.Utils
import Core.Message

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString ()
import Data.Serialize (Serialize, encode)
import Polysemy
import Polysemy.Error (Error, throw)
import Core.Types
import Core.Utils (computeContentHash)
import qualified Core.Common

-- Add necessary imports for P2P functions
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Relude ()
import Data.Word ()

import Core.Types

-- | Helper function to convert Core.Common.Hash to Core.Types.Hash
convertHash :: Core.Common.Hash -> Hash
convertHash (Core.Common.Hash bs) = Hash bs

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

-- | Get the content-addressable hash of a public key.
-- This is used in various parts of the system to identify public keys uniquely.
-- Uses SHA-256 to compute a consistent hash of the serialized public key bytes.
computePubKeyHash :: PubKey -> EntityHash Actor
computePubKeyHash (PubKey bytes) = EntityHash $ convertHash $ Core.Utils.computeSha256 bytes

-- | Compute a proof for anchoring a timeline to its parent chain.
-- Used when creating timeline branches to establish causal links between parent and child.
computeAnchorProof :: [EntityHash Timeline] -> EntityHash Timeline -> EntityHash Timeline
computeAnchorProof prevChain newTimeline =
  EntityHash $ convertHash $ Core.Utils.computeSha256 $ encode (map unEntityHash prevChain, unEntityHash newTimeline)

-- | Get the hash of a message.
-- Used for content addressing and verifying message integrity.
computeMessageHash :: (Serialize a) => a -> Hash
computeMessageHash content =
  convertHash $ Core.Utils.computeContentHash content Nothing

-- | Helper function to compute an authenticated message hash.
-- Creates a unique identifier for an authenticated message by hashing
-- its sender, recipient, signature, and payload hash.
computeAuthMessageHash :: (Serialize a) => AuthenticatedMessage a -> Hash
computeAuthMessageHash msg =
  convertHash $ Core.Utils.computeSha256 $
    encode
      ( amSender msg
      , amDestination msg
      , amSignature msg
      , amHash msg
      )

-- | Helper function to compute a log entry hash.
-- Creates a unique identifier for a log entry by hashing
-- its content, timestamp, previous entry hash, and metadata.
computeLogEntryHash :: (Serialize a) => LogEntry a -> Hash
computeLogEntryHash entry =
  convertHash $ Core.Utils.computeSha256 $
    encode
      ( leContent entry
      , leMetadata entry
      , lePrevHash entry
      )

-- | Helper function to compute a stored item hash.
-- Used in the distributed data store to compute content-addressed keys.
computeStoredItemHash :: TransientStoredItem -> Hash
computeStoredItemHash item =
  convertHash $ Core.Utils.computeSha256 $
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
