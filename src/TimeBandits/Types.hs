{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
This module provides the core types used throughout the Time Bandits application.
-}
module TimeBandits.Types (
  -- * Core Types
  Hash (..),
  EntityHash (..),
  PubKey (..),
  PrivKey (..),
  Signature (..),
  ActorHash,
  ResourceHash,
  TimelineHash,

  -- * Event Types
  ActorEvent (..),
  ActorEventType (..),
  ResourceEvent (..),
  ResourceEventType (..),
  TimelineEvent (..),
  TimelineEventType (..),
  EventContent (..),

  -- * Log Types
  LogContent (..),
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
  SyncPoint (..),

  -- * Resource Types
  Resource (..),

  -- * Message Types
  AuthenticatedMessage (..),

  -- * Core Data Types
  AppError (..),
  TimelineErrorType (..),
  ResourceErrorType (..),
  ActorErrorType (..),
  CryptoErrorType (..),
  StorageErrorType (..),
  SystemConfig (..),
  defaultSystemConfig,

  -- * Actor Types
  ActorType (..),
  Actor (..),

  -- * Lamport Clock Types
  LamportTime (..),

  -- * Transient Datastore Types
  TransientDatastore (..),
  TransientStoredItem (..),

  -- * Event Metadata Types
  EventMetadata (..),

  -- * New Types
  ContentAddressedMessage (..),

  -- * Resource Capabilities
  ResourceCapability (..),

  -- * Resource Transaction Types
  ResourceTransaction (..),

  -- * Cryptographic Functions
  signMessage,
  verifySignature,
) where

import Data.ByteString ()

-- For instances
import Data.Map.Strict ()

-- For instances
import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteArray (convert)
import Data.Map.Strict qualified as Map
import Data.Serialize qualified as S
import Data.Text ()
import Data.Time (Day (..), DiffTime, UTCTime (..))
import GHC.Generics ()

-- | Instance for serializing UTCTime
instance S.Serialize UTCTime where
  put (UTCTime day time) = do
    S.put (toModifiedJulianDay day)
    S.put (realToFrac time :: Double)
  get = do
    day <- fmap ModifiedJulianDay (S.get :: S.Get Integer)
    time <- fmap (realToFrac :: Double -> DiffTime) (S.get :: S.Get Double)
    pure $ UTCTime day time

-- | Represents a SHA-256 content addressable hash.
newtype Hash = Hash ByteString
  deriving stock (Eq, Ord, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | A type-safe wrapper for hashes of different entities
newtype EntityHash a = EntityHash {unEntityHash :: Hash}
  deriving stock (Eq, Ord, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | Type aliases for common entity hashes
type ActorHash = EntityHash Actor

type ResourceHash = EntityHash Resource
type TimelineHash = EntityHash Timeline

-- | Phantom type for Timeline entity
data Timeline

-- | Represents a logical timestamp in a distributed system
newtype LamportTime = LamportTime Int
  deriving stock (Eq, Ord, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | Represents a public key, which uniquely identifies actors.
newtype PubKey = PubKey ByteString
  deriving stock (Eq, Ord, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | Represents a private key, used for signing messages.
newtype PrivKey = PrivKey ByteString
  deriving stock (Eq, Ord, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | Represents a cryptographic signature.
newtype Signature = Signature ByteString
  deriving stock (Eq, Ord, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | Represents an actor in the system.
data ActorType = TimeTraveler | Validator | Observer
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | An actor in the system with their public key hash and role
data Actor = Actor
  { actorId :: ActorHash -- Hash of the public key
  , actorType :: ActorType
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | System configuration parameters
data SystemConfig = SystemConfig
  { scReplicationFactor :: Int
  -- ^ Number of replicas required for consensus
  , scTimeoutDuration :: Int
  -- ^ Timeout duration in milliseconds
  , scMaxRetries :: Int
  -- ^ Maximum number of retries for operations
  }
  deriving stock (Show, Eq)

-- | Default system configuration
defaultSystemConfig :: SystemConfig
defaultSystemConfig =
  SystemConfig
    { scReplicationFactor = 3
    , scTimeoutDuration = 5000
    , scMaxRetries = 3
    }

-- | A digital object tracked across timelines.
data Resource = Resource
  { resourceId :: ResourceHash -- Unique identifier (SHA-256)
  , resourceOrigin :: TimelineHash -- Origin timeline
  , resourceOwner :: ActorHash -- Current owner (hash of their public key)
  , resourceCapabilities :: [ResourceCapability] -- Capabilities granted by this resource
  , resourceMeta :: ByteString -- Arbitrary metadata
  , resourceSpentBy :: Maybe Hash -- Hash of the transaction that spent this resource
  , resourceParents :: [ResourceHash] -- Parent resources that were consumed to create this one
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

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
  deriving anyclass (S.Serialize)

-- | Events that can happen to an actor
data ActorEvent = ActorEvent
  { aeContent :: ActorEventType
  -- ^ The type of actor event
  , aeMetadata :: EventMetadata
  -- ^ Common event metadata
  , aePreviousEvent :: Maybe (EntityHash ActorEvent)
  -- ^ Previous event in chain
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | Events that can happen to a resource
data ResourceEvent = ResourceEvent
  { reContent :: ResourceEventType
  -- ^ The type of resource event
  , reMetadata :: EventMetadata
  -- ^ Common event metadata
  , rePreviousEvent :: Maybe (EntityHash ResourceEvent)
  -- ^ Previous event in chain
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | Events that can happen in a timeline
data TimelineEvent = TimelineEvent
  { teContent :: TimelineEventType
  -- ^ The type of timeline event
  , teMetadata :: EventMetadata
  -- ^ Common event metadata
  , tePreviousEvent :: Maybe (EntityHash TimelineEvent)
  -- ^ Previous event in chain
  , teActor :: ActorHash
  -- ^ Actor who created this event
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | Represents an entry in an append-only log.
data LogContent a
  = Regular
      { lcContent :: a -- Standard log entry
      , lcTimestamp :: LamportTime -- Logical timestamp
      }
  | Upgrade
      { newLogPointer :: Hash -- Points to a new log (for version upgrades)
      , upgradeCondition :: Int -- Condition triggering the upgrade (e.g., height threshold)
      , lcTimestamp :: LamportTime -- Logical timestamp
      }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | A content-addressed message
data ContentAddressedMessage a = ContentAddressedMessage
  { camHash :: Hash -- Hash of the message content
  , camContent :: a -- The actual message content
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | A content-addressed authenticated message
data AuthenticatedMessage a = AuthenticatedMessage
  { amHash :: Hash -- Hash of the message content and metadata
  , amSender :: Actor -- Sender actor
  , amDestination :: Maybe ActorHash -- Optional recipient hash (if direct messaging)
  , amPayload :: ContentAddressedMessage a -- Message payload
  , amSignature :: Signature -- Cryptographic proof of authenticity
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | A content-addressed log entry
data LogEntry a = LogEntry
  { leHash :: Hash -- Hash of the entry content and metadata
  , leContent :: a -- The event content
  , leMetadata :: EventMetadata
  , lePrevHash :: Maybe Hash
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

newtype Log a = Log {unLog :: [LogEntry a]}
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | The global registration log for all actors in the system
data MapOfTime = MapOfTime
  { mtTimelines :: Map TimelineHash TimelineLog -- Individual timeline logs
  , mtSyncPoints :: Map Hash SyncPoint -- Cross-timeline synchronization points
  , mtGlobalTime :: LamportTime -- Global Lamport clock
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | A synchronization point between timelines
data SyncPoint = SyncPoint
  { spTimelines :: [TimelineHash] -- Timelines involved in sync
  , spEvent :: LogEntry TimelineEventType -- The event that created the sync point
  , spTimestamp :: LamportTime -- When the sync occurred
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | Represents an authenticated message exchanged between actors.
data AuthorizationPolicy = AuthorizationPolicy
  { policyRequiredSignatures :: Int
  , policyAllowedKeys :: [ActorHash] -- List of allowed actor hashes
  , policyOperation :: Text
  , policyResource :: Maybe Resource
  }
  deriving stock (Show, Eq)

-- | Represents the decentralized transient storage network
data TransientDatastore = TransientDatastore
  { tdReplicationFactor :: Int
  -- ^ Number of nodes that replicate each stored item
  , tdTimeBandits :: [Actor]
  -- ^ Available Time Bandit nodes
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | A content-addressed stored item
data TransientStoredItem = TransientStoredItem
  { siHash :: Hash -- Hash of the item content
  , siKey :: ByteString
  -- ^ Key used for rendezvous hashing
  , siContent :: ByteString
  -- ^ Content to be stored
  , siTimestamp :: LamportTime
  -- ^ When the item was stored
  , siSignature :: Signature
  -- ^ Signature of the content
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | Types of actor events
data ActorEventType
  = ActorRegistered Actor -- Actor was registered
  | ActorRoleChanged ActorType -- Role was changed
  | ActorDeactivated -- Actor was deactivated
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | Types of resource events
data ResourceEventType
  = ResourceCreated Resource -- Resource was created
  | ResourceConsumed ResourceTransaction -- Resource was consumed and new ones created
  | ResourceCapabilityChecked -- Check if a resource is unspent and has capability
      { rcCheckedResource :: ResourceHash
      , rcCheckedFor :: ActorHash
      , rcCapability :: ResourceCapability
      , rcResult :: Bool
      }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | Types of timeline events
data TimelineEventType
  = TimelineCreated -- Timeline was created
  | TimelineMerged TimelineHash -- Merged with another timeline
  | TimelineSplit TimelineHash -- Split into new timeline
  | TimelineFinalized Hash -- New Merkle root computed
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | A finalized block of events in a timeline
data TimelineBlock = TimelineBlock
  { tbEvents :: [LogEntry TimelineEventType]
  , tbMerkleRoot :: Hash
  , tbPrevBlock :: Maybe Hash
  , tbTimestamp :: LamportTime
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | Log of events for a specific resource
type ResourceLog = [LogEntry ResourceEventType]

-- | Log of events for a specific actor
type ActorLog = [LogEntry ActorEventType]

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
  deriving anyclass (S.Serialize)

-- | Content of an event
data EventContent
  = ActorEventContent ActorEventType
  | ResourceEventContent ResourceEventType
  | TimelineEventContent TimelineEventType
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

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
  deriving anyclass (S.Serialize)

-- | Application errors
data AppError
  = TimelineError TimelineErrorType
  | ResourceError ResourceErrorType
  | ActorError ActorErrorType
  | CryptoError CryptoErrorType
  | StorageError StorageErrorType
  | NetworkError Text
  | AuthorizationError Text
  | TimeoutError Text
  deriving stock (Show, Eq)

-- | Timeline-specific error types
data TimelineErrorType
  = TimelineNotFound TimelineHash
  | TimelineAlreadyExists TimelineHash
  | TimelineMergeConflict TimelineHash TimelineHash
  | InvalidTimelineState Text
  | UnauthorizedTimelineAccess ActorHash
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

-- | Crypto-specific error types
data CryptoErrorType
  = InvalidSignatureError
  | InvalidKeyPairError
  | SigningError Text
  deriving stock (Show, Eq)

-- | Storage-specific error types
data StorageErrorType
  = ItemNotFound Hash
  | StorageFailure Text
  | ReplicationFailure Text
  deriving stock (Show, Eq)

-- | A resource transaction that consumes and creates resources
data ResourceTransaction = ResourceTransaction
  { rtInputs :: [ResourceHash] -- Resources to be consumed
  , rtOutputs :: [Resource] -- New resources to be created
  , rtTimestamp :: LamportTime -- When the transaction occurred
  , rtSigner :: PubKey -- Who authorized the transaction
  , rtSignature :: Signature -- Proof of authorization
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | A simple Trie implementation using Map
newtype Trie a = Trie {unTrie :: Map ByteString a}
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

-- | Helper functions for Trie
emptyTrie :: Trie a
emptyTrie = Trie Map.empty

insertTrie :: ByteString -> a -> Trie a -> Trie a
insertTrie key val (Trie m) = Trie (Map.insert key val m)

lookupTrie :: ByteString -> Trie a -> Maybe a
lookupTrie key (Trie m) = Map.lookup key m

fromListTrie :: [(ByteString, a)] -> Trie a
fromListTrie = Trie . Map.fromList

elemsTrie :: Trie a -> [a]
elemsTrie (Trie m) = Map.elems m

-- | Sign a message with a private key
signMessage :: PrivKey -> ByteString -> Either Text Signature
signMessage (PrivKey privKey) msg =
  case Ed25519.secretKey privKey of
    CryptoFailed err -> Left $ "Invalid private key: " <> show err
    CryptoPassed sk -> Right $ Signature $ convert (Ed25519.sign sk (Ed25519.toPublic sk) msg)

-- | Verify a signature with a public key
verifySignature :: PubKey -> ByteString -> Signature -> Bool
verifySignature (PubKey pubKey) msg (Signature sig) =
  case Ed25519.publicKey pubKey of
    CryptoFailed _ -> False
    CryptoPassed pk ->
      case Ed25519.signature sig of
        CryptoFailed _ -> False
        CryptoPassed sig' -> Ed25519.verify pk msg sig'
