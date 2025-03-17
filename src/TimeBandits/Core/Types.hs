{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: TimeBandits.Core.Types
Description: Core type definitions for the Time-Bandits system

This module provides the fundamental types used throughout the Time Bandits application.
It contains primitive types, data structures, and type classes that form the foundation
of the system's domain model.

Due to the original size of the Core.Types module, the types have been split into
multiple modules for better organization and maintenance. This module contains
the most fundamental types, while more specialized types are in their respective
modules.

@since 0.1.0
-}
module TimeBandits.Core.Types (
  -- * Re-export primitive types from Common
  Hash(..),
  EntityHash(..),
  ActorHash,
  ResourceHash,
  TimelineHash,
  PubKey(..),
  PrivKey(..),
  Signature(..),
  LamportTime(..),
  
  -- * Effect Types
  EffectId,
  
  -- * Event Types
  ActorEvent (..),
  ActorEventType (..),
  ResourceEvent (..),
  ResourceEventType (..),
  TimelineEvent (..),
  TimelineEventType (..),
  EventContent (..),
  EventMetadata (..),

  -- * Log Types
  LogEntry (..),
  Log (..),
  MapOfTime (MapOfTime, mtTimelines, mtSyncPoints, mtGlobalTime),
  ResourceLog,
  TimelineLog (..),
  TimelineBlock (..),
  Trie (..),
  insertTrie,
  elemsTrie,
  fromListTrie,
  emptyTrie,
  lookupTrie,
  SyncPoint (..),

  -- * Message Types
  AuthenticatedMessage (..),
  ContentAddressedMessage (..),

  -- * Error Types
  TimelineErrorType (..),
  ResourceErrorType (..),
  ActorErrorType (..),
  StorageErrorType (..),
  AppError (..),

  -- * Actor Types
  ActorType (..),
  ActorInfo (..),

  -- * Resource Types
  ResourceInfo (..),
  ResourceCapability (..),
  
  -- * Transaction Types
  UnifiedResourceTransaction (..),
  TransactionValidationResult (..)
) where

-- Import documentation of standard extensions
import TimeBandits.Core.Common.Extensions


-- External libraries
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime, diffUTCTime, addUTCTime)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Serialize (Serialize)
-- No direct imports from Data.Serialize for serialization instances
-- import qualified Data.Serialize as S
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as C8
import Data.Time.Format (formatTime, parseTimeM, defaultTimeLocale)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (fromGregorian)

-- TimeBandits common modules
import TimeBandits.Core.Common.Types
    ( Hash(..)
    , EntityHash(..)
    , ActorHash
    , ResourceHash
    , TimelineHash
    , PubKey(..)
    , PrivKey(..)
    , Signature(..)
    , LamportTime(..)
    )
import TimeBandits.Core.Error (AppError(..))

-- Import standardized serialization
import TimeBandits.Core.Common.Serialize ()

-- Import the necessary modules, adding this near other import statements
import TimeBandits.Core.Concurrency.Types (EffectId)

-- Import error types
import TimeBandits.Core.Error.Types (CryptoErrorType(..))

-- | Represents an actor in the system.
data ActorType = TimeTraveler | Validator | Observer
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | An actor in the system with their public key hash and role
data ActorInfo = ActorInfo
  { actorId :: ActorHash -- Hash of the public key
  , actorType :: ActorType
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | Capabilities that can be granted by resources
data ResourceCapability
  = TransferCapability -- Can transfer the resource
  | UpdateCapability -- Can update resource metadata
  | DelegateCapability -- Can delegate capabilities to others
  | CreateChildCapability -- Can create child resources
  | ValidateCapability -- Can validate timeline operations
  | ObserveCapability -- Can observe timeline data
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

{- | A digital object tracked across timelines with UTXO-like structure.
Resources are the fundamental units of value in the system and follow
an unspent transaction output (UTXO) model where:
- Resources are created as outputs of transactions
- Resources can be consumed (spent) only once as inputs to new transactions
- Resources maintain their provenance chain across timelines
- Resources have capabilities that determine what actions can be performed with them
-}
data ResourceInfo = ResourceInfo
  { resourceId :: ResourceHash -- Unique identifier (SHA-256)
  , resourceOrigin :: TimelineHash -- Origin timeline
  , resourceOwner :: ActorHash -- Current owner (hash of their public key)
  , resourceCapabilities :: [ResourceCapability] -- Capabilities granted by this resource
  , resourceMeta :: ByteString -- Arbitrary metadata
  , resourceSpentBy :: Maybe Hash -- Hash of the transaction that spent this resource
  , resourceParents :: [ResourceHash] -- Parent resources that were consumed to create this one
  , resourceTimestamp :: LamportTime -- When the resource was created
  , resourceProvenanceChain :: [TimelineHash] -- Tracks resource movement across timelines
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | Event metadata with all necessary information
data EventMetadata = EventMetadata
  { emTimestamp :: LamportTime
  , emCreatedAt :: UTCTime
  , emActor :: ActorHash
  , emTimeline :: TimelineHash
  , emSignature :: Signature
  , emSigner :: PubKey
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | Timeline-specific error types
data TimelineErrorType
  = TimelineNotFound TimelineHash
  | TimelineAlreadyExists TimelineHash
  | TimelineMergeConflict TimelineHash TimelineHash
  | InvalidTimelineState Text
  | UnauthorizedTimelineAccess ActorHash
  | TimelineGenericError Text
  deriving stock (Show, Eq)

-- | Resource-specific error types
data ResourceErrorType
  = ResourceNotFound ResourceHash
  | ResourceAlreadyExists ResourceHash
  | UnauthorizedResourceAccess ActorHash
  | InvalidResourceState Text
  | ResourceAlreadySpent ResourceHash
  deriving stock (Show, Eq)

-- | Actor-specific error types
data ActorErrorType
  = ActorNotFound ActorHash
  | ActorAlreadyExists ActorHash
  | InvalidActorRole ActorType
  | UnauthorizedActorOperation ActorHash
  deriving stock (Show, Eq)

-- | Storage-specific error types
data StorageErrorType
  = ItemNotFound Hash
  | StorageFailure Text
  | ReplicationFailure Text
  deriving stock (Show, Eq)

-- | Events that can happen to an actor
data ActorEvent = ActorEvent
  { aeContent :: ActorEventType
  -- ^ The type of actor event
  , aeMetadata :: EventMetadata
  -- ^ Common event metadata
  , aePreviousEvent :: Maybe (EntityHash "ActorEvent")
  -- ^ Previous event in chain
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | Events that can happen to a resource
data ResourceEvent = ResourceEvent
  { reContent :: ResourceEventType
  -- ^ The type of resource event
  , reMetadata :: EventMetadata
  -- ^ Common event metadata
  , rePreviousEvent :: Maybe (EntityHash "ResourceEvent")
  -- ^ Previous event in chain
  , reActor :: ActorHash
  -- ^ Actor who created this event
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | Events that can happen in a timeline
data TimelineEvent = TimelineEvent
  { teContent :: TimelineEventType
  -- ^ The type of timeline event
  , teMetadata :: EventMetadata
  -- ^ Common event metadata
  , tePreviousEvent :: Maybe (EntityHash "TimelineEvent")
  -- ^ Previous event in chain
  , teActor :: ActorHash
  -- ^ Actor who created this event
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | Types of actor events
data ActorEventType
  = ActorRegistered ActorInfo -- Actor was registered
  | ActorRoleChanged ActorType -- Role was changed
  | ActorDeactivated -- Actor was deactivated
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | Resource event types
data ResourceEventType
  = ResourceCreated ResourceInfo -- Resource was created
  | ResourceTransferred UnifiedResourceTransaction -- Resource was transferred to a new owner
  | ResourceCapabilityChecked -- Check if a resource is unspent and has capability
      { rcCheckedResource :: ResourceHash
      , rcCheckedFor :: ActorHash
      , rcCapability :: ResourceCapability
      , rcResult :: Bool
      }
  | ResourceVerified -- Resource was verified as valid
      { rvResource :: ResourceHash
      , rvVerifier :: ActorHash
      , rvResult :: Bool
      }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | Types of timeline events
data TimelineEventType
  = TimelineCreated -- Timeline was created
  | TimelineMerged TimelineHash -- Merged with another timeline
  | TimelineSplit TimelineHash -- Split into new timeline
  | TimelineFinalized Hash -- New Merkle root computed
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | Content of an event
data EventContent
  = ActorEventContent ActorEventType
  | ResourceEventContent ResourceEventType
  | TimelineEventContent TimelineEventType
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | Represents a log entry in an append-only log.
data LogEntry a = LogEntry
  { leHash :: Hash -- Hash of the entry content and metadata
  , leContent :: a -- The event content
  , leMetadata :: EventMetadata
  , lePrevHash :: Maybe Hash
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

newtype Log a = Log {unLog :: [LogEntry a]}
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | A content-addressed message
data ContentAddressedMessage a = ContentAddressedMessage
  { camHash :: Hash -- Hash of the message content
  , camContent :: a -- The actual message content
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | A content-addressed authenticated message
data AuthenticatedMessage a = AuthenticatedMessage
  { amHash :: Hash -- Hash of the message content and metadata
  , amSender :: ActorInfo -- Sender actor
  , amDestination :: Maybe ActorHash -- Optional recipient hash (if direct messaging)
  , amPayload :: ContentAddressedMessage a -- Message payload
  , amSignature :: Signature -- Cryptographic proof of authenticity
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | A finalized block of events in a timeline
data TimelineBlock = TimelineBlock
  { tbEvents :: [LogEntry TimelineEventType]
  , tbMerkleRoot :: Hash
  , tbPrevBlock :: Maybe Hash
  , tbTimestamp :: LamportTime
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | Log of events for a specific resource
type ResourceLog = [LogEntry ResourceEventType]

-- | A timeline-specific log containing all events
data TimelineLog = TimelineLog
  { tlEvents :: Trie (LogEntry TimelineEventType)
  , tlLatestMerkleRoot :: Maybe Hash
  , tlTimelineId :: TimelineHash
  , tlLastProcessedTime :: LamportTime
  , tlSyncPoints :: [Hash] -- References to sync points this timeline participates in
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | The global registration log for all actors in the system
data MapOfTime = MapOfTime
  { mtTimelines :: Map.Map TimelineHash TimelineLog -- Individual timeline logs
  , mtSyncPoints :: Map.Map Hash SyncPoint -- Cross-timeline synchronization points
  , mtGlobalTime :: LamportTime -- Global Lamport clock
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | A synchronization point between timelines
data SyncPoint = SyncPoint
  { spTimelines :: [TimelineHash] -- Timelines involved in sync
  , spEvent :: LogEntry TimelineEventType -- The event that created the sync point
  , spTimestamp :: LamportTime -- When the sync occurred
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | A simple Trie implementation using Map
-- This is a lightweight wrapper around Map to provide a trie-like interface
-- for efficient key-value storage within the timeline logs and other data structures.
-- The Trie allows for efficient lookups and iterations over timeline data.
newtype Trie a = Trie {unTrie :: Map.Map ByteString a}
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | Create an empty Trie structure
-- Used as an initial state for timeline logs and other trie-based collections.
emptyTrie :: Trie a
emptyTrie = Trie Map.empty

-- | Insert a value into a Trie at the specified key
-- Used to add new entries to timeline logs and other trie-based data structures.
insertTrie :: ByteString -> a -> Trie a -> Trie a
insertTrie key val (Trie m) = Trie (Map.insert key val m)

-- | Look up a value in a Trie by its key
-- Retrieves entries from timeline logs and other trie-based collections.
lookupTrie :: ByteString -> Trie a -> Maybe a
lookupTrie key (Trie m) = Map.lookup key m

-- | Create a Trie from a list of key-value pairs
-- Efficiently converts a list of entries into a trie structure for
-- quick initialization of timeline logs and other collections.
fromListTrie :: [(ByteString, a)] -> Trie a
fromListTrie = Trie . Map.fromList

-- | Get all values stored in a Trie
-- Retrieves all entries from a trie without their keys, useful for
-- processing all items in a timeline log or other collection.
elemsTrie :: Trie a -> [a]
elemsTrie (Trie m) = Map.elems m

{- | A unified resource transaction that integrates with the message system
This represents a bundle of messages that form a transaction.
The transaction follows the UTXO model where:
- Input resources are consumed (marked as spent)
- Output resources are created
- The transaction is signed by an actor with appropriate capabilities
- The transaction maintains the provenance chain across timelines
This design ensures atomic operations and maintains a complete audit trail.
-}
data UnifiedResourceTransaction = UnifiedResourceTransaction
  { urtInputs :: [AuthenticatedMessage ResourceInfo] -- Input resources (must be unspent)
  , urtOutputs :: [ContentAddressedMessage ResourceInfo] -- Output resources to be created
  , urtMetadata :: ByteString -- Additional transaction metadata
  , urtTimestamp :: LamportTime -- When the transaction occurred
  , urtSigner :: ActorInfo -- Who authorized the transaction
  , urtSignature :: Signature -- Proof of authorization
  , urtProvenanceChain :: [TimelineHash] -- Tracks resource movement across timelines
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

{- | Result of a resource transaction validation
This represents the three possible states of transaction validation:
- Valid: The transaction can be executed
- Invalid: The transaction cannot be executed (with reason)
- Deferred: The transaction validation is postponed (e.g., waiting for inputs)
This allows for flexible transaction processing in distributed environments.
-}
data TransactionValidationResult
  = TransactionValid -- Transaction is valid
  | TransactionInvalid ByteString -- Transaction is invalid with reason
  | TransactionDeferred -- Transaction validation deferred (e.g., waiting for inputs)
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- NOTE: Serialize UTCTime instance is now imported from TimeBandits.Core.Common.Serialize
-- This ensures consistent serialization across the codebase
