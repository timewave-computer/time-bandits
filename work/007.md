# Refactor 008: End-to-End Implementation Checklist

## Objective

This document provides a **detailed, step-by-step implementation plan** for four critical components of the Time Bandits system that will enable end-to-end functionality:

1. **P2P Network Communication Layer**: Implementation of the robust peer-to-peer communication system described in `network_adapter_plan.md`.
2. **Advanced Concurrency Model**: Implementation of the resource-scoped concurrency model with temporal combinators described in `adr_005_concurrency.md`.
3. **Fact Observation Rules in TOML**: Implementation of the configurable fact extraction system described in `adr_012_timekeeper_indexing_and_log_reconstruction.md`.
4. **Unified Log Reconstruction**: Implementation of the on-demand log reconstruction capabilities described in `adr_012_timekeeper_indexing_and_log_reconstruction.md`.

Each implementation step includes:
- **Context** - Why the step matters and how it fits into the overall system
- **Implementation strategy** - How to implement the component
- **Affected code** - Specific modules and files to modify or create
- **Success criteria** - What defines successful implementation
- **Testing plan** - How to verify the implementation works correctly

## Component 1: P2P Network Communication Layer

### Context
The P2P Network Communication Layer enables Bandit nodes to communicate with each other, facilitating distributed consensus and data sharing. This component is essential for transitioning from a local simulation environment to a fully distributed network.

### Implementation Steps

#### Step 1.1: Create Core NetworkAdapter Interface

**Implementation Strategy**:
- Create a `NetworkAdapter` typeclass that defines the core P2P communication interface
- Implement basic peer identity and message types
- Define the adapter configuration structure

**Affected Code**:
- Modify `src/Adapters/NetworkAdapter.hs` 
- Create `src/Types/Network.hs` for network-specific types

**Code Structure**:
```haskell
-- NetworkAdapter.hs
module Adapters.NetworkAdapter
  ( NetworkAdapter(..)
  , createNetworkAdapter
  , PeerId(..)
  , MessageType(..)
  ) where

import Types.Network

class NetworkAdapter a where
  -- Core communication functions
  broadcastMessage :: a -> MessageType -> ByteString -> IO (Either NetworkError MessageId)
  sendDirectMessage :: a -> PeerId -> MessageType -> ByteString -> IO (Either NetworkError MessageId)
  
  -- Subscription management
  subscribeToMessages :: a -> MessageType -> (PeerId -> ByteString -> IO ()) -> IO SubscriptionId
  unsubscribeFromMessages :: a -> SubscriptionId -> IO ()
  
  -- Peer management
  discoverPeers :: a -> IO [PeerId]
  getPeerStatus :: a -> PeerId -> IO (Maybe PeerStatus)
  
  -- Lifecycle management
  startAdapter :: a -> IO ()
  stopAdapter :: a -> IO ()
```

**Success Criteria**:
- Complete NetworkAdapter typeclass definition
- Type definitions for all required network operations
- Clean interface that can be implemented for different network backends

**Testing**:
- Create unit tests for type validation
- Mock NetworkAdapter implementation for testing

#### Step 1.2: Implement In-Memory Network Adapter

**Implementation Strategy**:
- Create an in-memory implementation of the NetworkAdapter for testing and single-process simulations
- Use in-memory channels for message passing
- Implement virtual peer IDs and statuses

**Affected Code**:
- Create `src/Adapters/Network/InMemoryAdapter.hs`

**Code Structure**:
```haskell
-- InMemoryAdapter.hs
module Adapters.Network.InMemoryAdapter
  ( createInMemoryAdapter
  ) where

import Adapters.NetworkAdapter
import qualified Data.Map as Map
import Control.Concurrent.Chan

data InMemoryAdapter = InMemoryAdapter
  { peers :: Map PeerId PeerChannel
  , subscriptions :: Map SubscriptionId (MessageType, PeerId -> ByteString -> IO ())
  , config :: NetworkConfig
  }

createInMemoryAdapter :: NetworkConfig -> IO InMemoryAdapter
-- Implementation of all NetworkAdapter functions
```

**Success Criteria**:
- Fully working in-memory network adapter
- Ability to send messages between virtual peers
- Support for subscriptions to message types

**Testing**:
- Test basic message passing between virtual peers
- Test subscription and notification
- Test peer discovery and status

#### Step 1.3: Implement QUIC-based Network Adapter

**Implementation Strategy**:
- Implement a real network adapter using the QUIC protocol for efficient, secure communication
- Support TLS for encrypted communication
- Implement robust error handling and retry logic

**Affected Code**:
- Create `src/Adapters/Network/QUICAdapter.hs`
- Create `src/Adapters/Network/TLS.hs` for TLS support

**Code Structure**:
```haskell
-- QUICAdapter.hs
module Adapters.Network.QUICAdapter
  ( createQUICAdapter
  ) where

import Adapters.NetworkAdapter
import Adapters.Network.TLS
import Network.QUIC

data QUICAdapter = QUICAdapter
  { quicConfig :: QUICConfig
  , connections :: Map PeerId QUICConnection
  , subscriptions :: Map SubscriptionId (MessageType, PeerId -> ByteString -> IO ())
  , serverState :: ServerState
  }

createQUICAdapter :: NetworkConfig -> IO QUICAdapter
-- Implementation of all NetworkAdapter functions
```

**Success Criteria**:
- Fully functional QUIC-based network adapter
- Support for secure, encrypted communications
- Efficient handling of peer connections and message passing

**Testing**:
- Test local network communication between two QUIC adapters
- Test connection recovery after network interruption
- Test message delivery guarantees
- Test TLS encryption and certificate validation

#### Step 1.4: Implement Message Serialization and Protocol Version

**Implementation Strategy**:
- Define serialization format for messages
- Implement protocol versioning to support future upgrades
- Create utilities for encoding/decoding messages

**Affected Code**:
- Create `src/Adapters/Network/Protocol.hs`
- Create `src/Adapters/Network/Serialization.hs`

**Code Structure**:
```haskell
-- Protocol.hs
module Adapters.Network.Protocol
  ( ProtocolVersion(..)
  , MessageEnvelope(..)
  , serializeMessage
  , deserializeMessage
  ) where

data ProtocolVersion = ProtocolV1 | ProtocolV2

data MessageEnvelope = MessageEnvelope
  { version :: ProtocolVersion
  , sender :: PeerId
  , messageType :: MessageType
  , timestamp :: UTCTime
  , payload :: ByteString
  , signature :: Maybe Signature
  }
```

**Success Criteria**:
- Complete message serialization/deserialization
- Protocol versioning support
- Efficient binary encoding

**Testing**:
- Test serialization/deserialization roundtrip
- Test protocol version compatibility
- Test handling of malformed messages

#### Step 1.5: Implement Peer Discovery and Bootstrap

**Implementation Strategy**:
- Implement mechanism for discovering peers using rendezvous hashing instead of DHT
- Support static bootstrap peers
- Leverage existing rendezvous hashing for deterministic node selection and discovery
- Implement peer list exchange protocol

**Affected Code**:
- Create `src/Adapters/Network/Discovery.hs`
- Modify `src/Adapters/Network/QUICAdapter.hs`
- Reuse existing code from `Adapters.Network` for rendezvous hashing

**Code Structure**:
```haskell
-- Discovery.hs
module Adapters.Network.Discovery
  ( discoverPeers
  , maintainPeerList
  , DiscoveryConfig(..)
  ) where

data DiscoveryConfig = DiscoveryConfig
  { bootstrapPeers :: [PeerAddress]
  , discoveryInterval :: Int
  , maxPeersPerQuery :: Int  -- Maximum peers to query at once
  }

-- Use rendezvous hashing to find peers to query about other peers
discoverPeers :: DiscoveryConfig -> [PeerId] -> IO [PeerId]
maintainPeerList :: DiscoveryConfig -> (PeerId -> IO ()) -> IO ThreadId

-- Internal helper for using rendezvous hashing
selectPeersToQuery :: [PeerId] -> ByteString -> Int -> [PeerId]
selectPeersToQuery knownPeers queryKey maxPeers = 
  -- Uses existing rendezvous hashing to select subset of peers
  take maxPeers $ sortBy (comparing (computeNodeScore queryKey . unPeerId)) knownPeers
```

**Success Criteria**:
- Functional peer discovery mechanism using rendezvous hashing
- Support for bootstrap peers
- Background peer list maintenance
- Efficient distribution of peer knowledge throughout the network

**Testing**:
- Test discovery with static bootstrap peers
- Test peer list exchange
- Test rendezvous hashing for deterministic peer selection
- Test peer list maintenance and updates

**Architectural Integration**:
- The peer discovery mechanism will reuse the existing rendezvous hashing implementation to maintain consistency with the overall system architecture.
- This approach reduces complexity by avoiding the need for a separate DHT, while still providing consistent and deterministic peer discovery.
- Nodes will use the same hashing algorithm for both peer discovery and resource location, creating a unified approach to network topology management.

### Component 1 Success Criteria
- Complete implementation of P2P network communication layer
- Support for both in-memory and real network adapters
- Message passing, subscriptions, and peer discovery
- Protocol versioning and secure communication
- Efficient peer discovery using rendezvous hashing instead of DHT

## Component 2: Advanced Concurrency Model

### Context
The resource-scoped concurrency model described in ADR-005 enables safe concurrent execution across multiple timelines while maintaining causal consistency. This implementation will provide the temporal combinators needed for expressing concurrent workflows in Time Bandits programs.

### Implementation Steps

#### Step 2.1: Implement Resource-Scoped Locks

**Implementation Strategy**:
- Implement resource locking mechanism for atomic effect application
- Create resource lock manager for concurrency control
- Support distributed locks across Bandit nodes

**Affected Code**:
- Create `src/Core/Concurrency/ResourceLock.hs`
- Create `src/Core/Concurrency/LockManager.hs`

**Code Structure**:
```haskell
-- ResourceLock.hs
module Core.Concurrency.ResourceLock
  ( ResourceLock(..)
  , acquireLock
  , releaseLock
  , withResourceLock
  ) where

data ResourceLock = ResourceLock
  { resourceId :: ResourceId
  , owner :: Maybe EffectId
  , acquiredAt :: UTCTime
  }

acquireLock :: ResourceId -> EffectId -> IO (Either LockError ResourceLock)
releaseLock :: ResourceLock -> IO (Either LockError ())
withResourceLock :: ResourceId -> EffectId -> IO a -> IO (Either LockError a)
```

**Success Criteria**:
- Working resource lock mechanism
- Atomic lock acquisition and release
- Support for resource-level concurrency

**Testing**:
- Test concurrent lock acquisition
- Test lock release
- Test timeout handling
- Test deadlock detection

#### Step 2.2: Implement Per-Resource Effect Logs

**Implementation Strategy**:
- Create per-resource effect logs that track applied effects
- Implement append-only log structure
- Support content-addressed effect storage

**Affected Code**:
- Create `src/Core/Concurrency/EffectLog.hs`
- Modify `src/Core/Resource.hs`

**Code Structure**:
```haskell
-- EffectLog.hs
module Core.Concurrency.EffectLog
  ( EffectLog(..)
  , appendEffect
  , getEffectHistory
  , getLatestEffect
  ) where

data EffectLog = EffectLog
  { resourceId :: ResourceId
  , effects :: [Effect]
  , latestHash :: Hash
  }

appendEffect :: EffectLog -> Effect -> IO EffectLog
getEffectHistory :: EffectLog -> TimeRange -> IO [Effect]
getLatestEffect :: EffectLog -> IO (Maybe Effect)
```

**Success Criteria**:
- Functioning per-resource effect logs
- Append-only log structure
- Content addressing for effects

**Testing**:
- Test effect appending
- Test concurrent reads/writes
- Test history retrieval
- Test content addressing

#### Step 2.3: Implement Temporal Combinators

**Implementation Strategy**:
- Implement the temporal combinators described in ADR-005
- Support watch, barrier, race, fork, invoke, and callback
- Create runtime support for these combinators

**Affected Code**:
- Create `src/Core/Concurrency/Combinators.hs`
- Create `src/Core/Concurrency/Runtime.hs`

**Code Structure**:
```haskell
-- Combinators.hs
module Core.Concurrency.Combinators
  ( watch
  , barrier
  , race
  , fork
  , invoke
  , callback
  ) where

import Core.Types
import Core.Effect

-- Watch for a condition on a resource
watch :: ResourceId -> (ResourceState -> Bool) -> IO WatchHandle

-- Wait for multiple conditions to be satisfied
barrier :: [WatchHandle] -> IO BarrierHandle

-- Execute branches concurrently, return when any completes
race :: [IO a] -> IO a

-- Start a concurrent branch
fork :: IO a -> IO ForkHandle

-- Call another program asynchronously
invoke :: ProgramId -> Invocation -> IO InvocationHandle

-- Register a response handler
callback :: InvocationHandle -> (Response -> IO a) -> IO CallbackHandle
```

**Success Criteria**:
- Complete implementation of all temporal combinators
- Runtime support for concurrent execution
- Safe resource access across concurrent branches

**Testing**:
- Test each combinator individually
- Test combinations of combinators
- Test error handling and cancellation
- Test resource safety under concurrency

#### Step 2.4: Implement TECL Integration for Concurrency

**Implementation Strategy**:
- Extend TECL language to support concurrency primitives
- Implement TECL parser and interpreter for concurrent operations
- Create transpiler for converting TECL to underlying concurrency primitives

**Affected Code**:
- Modify `src/Core/TECL.hs`
- Create `src/Core/TECL/Concurrency.hs`

**Code Structure**:
```haskell
-- TECL/Concurrency.hs
module Core.TECL.Concurrency
  ( parseConcurrencyStatement
  , evaluateConcurrencyStatement
  , translateToConcurrencyEffect
  ) where

import Core.TECL
import Core.Concurrency.Combinators

data ConcurrencyStatement
  = WatchStatement Condition Block
  | BarrierStatement [Condition] Block
  | RaceStatement [Block] Block
  | ForkStatement Block
  | InvokeStatement ProgramId Invocation
  | CallbackStatement InvocationRef Block

parseConcurrencyStatement :: String -> Either ParseError ConcurrencyStatement
evaluateConcurrencyStatement :: ConcurrencyStatement -> IO Result
translateToConcurrencyEffect :: ConcurrencyStatement -> [Effect]
```

**Success Criteria**:
- TECL syntax for concurrency operations
- Parser support for concurrency statements
- Execution semantics for concurrent operations

**Testing**:
- Test parsing of concurrent TECL programs
- Test execution of concurrent operations
- Test conversion to effects

#### Step 2.5: Implement Distributed Concurrency Support

**Implementation Strategy**:
- Extend concurrency model to work across distributed Bandit nodes
- Implement distributed lock acquisition and release
- Support consistent view of concurrent operations

**Affected Code**:
- Create `src/Core/Concurrency/Distributed.hs`
- Modify `src/Core/Concurrency/LockManager.hs`

**Code Structure**:
```haskell
-- Distributed.hs
module Core.Concurrency.Distributed
  ( DistributedLockManager(..)
  , createDistributedLockManager
  , acquireDistributedLock
  , releaseDistributedLock
  ) where

import Adapters.NetworkAdapter
import Core.Concurrency.ResourceLock

data DistributedLockManager = DistributedLockManager
  { networkAdapter :: NetworkAdapter
  , localLocks :: Map ResourceId ResourceLock
  , remoteLocks :: Map ResourceId (PeerId, ResourceLock)
  }

createDistributedLockManager :: NetworkAdapter -> IO DistributedLockManager
acquireDistributedLock :: DistributedLockManager -> ResourceId -> EffectId -> IO (Either LockError ResourceLock)
releaseDistributedLock :: DistributedLockManager -> ResourceLock -> IO (Either LockError ())
```

**Success Criteria**:
- Distributed lock acquisition and release
- Consistent view of resource state across nodes
- Handling of network failures

**Testing**:
- Test distributed lock acquisition
- Test recovery from node failure
- Test concurrent operations across nodes

### Component 2 Success Criteria
- Complete implementation of resource-scoped concurrency
- Working temporal combinators
- TECL integration for concurrency
- Distributed concurrency support

## Component 3: Fact Observation Rules in TOML

### Context
The fact observation rules system described in ADR-012 enables Time Keepers to extract relevant facts from blockchain data using configurable rules. This implementation will enable the definition and application of these rules in a standardized format.

### Implementation Steps

#### Step 3.1: Define Fact Observation Rule Schema

**Implementation Strategy**:
- Define the schema for fact observation rules in TOML
- Create type definitions for rule components
- Implement rule validation

**Affected Code**:
- Create `src/Core/FactObservation/Rules.hs`
- Create `src/Core/FactObservation/Schema.hs`

**Code Structure**:
```haskell
-- Rules.hs
module Core.FactObservation.Rules
  ( FactObservationRule(..)
  , FactType(..)
  , ProofType(..)
  , PathExpression(..)
  , validateRule
  ) where

data FactObservationRule = FactObservationRule
  { ruleId :: Text
  , factType :: FactType
  , path :: PathExpression
  , proof :: ProofType
  , conditions :: Maybe [Condition]
  }

data FactType = PriceObservation | DepositObservation | WithdrawalObservation | CustomFact Text

data ProofType = InclusionProof | HeaderProof | StateProof | NoProof

data PathExpression = PathExpression
  { source :: Text
  , selector :: Text
  }

validateRule :: FactObservationRule -> Either ValidationError ()
```

**Success Criteria**:
- Complete rule schema definition
- Type-safe representation of rules
- Rule validation

**Testing**:
- Test rule creation
- Test rule validation
- Test rule serialization/deserialization

#### Step 3.2: Implement TOML Parser for Fact Rules

**Implementation Strategy**:
- Create TOML parser specifically for fact observation rules
- Support rule sets with multiple rules
- Implement validation and error reporting

**Affected Code**:
- Create `src/Core/FactObservation/TOMLParser.hs`

**Code Structure**:
```haskell
-- TOMLParser.hs
module Core.FactObservation.TOMLParser
  ( parseRuleSet
  , parseRule
  , serializeRuleSet
  , serializeRule
  ) where

import Core.FactObservation.Rules
import qualified Toml

data RuleSet = RuleSet
  { rules :: [FactObservationRule]
  , metadata :: Map Text Text
  }

parseRuleSet :: Text -> Either ParseError RuleSet
parseRule :: Toml.Table -> Either ParseError FactObservationRule
serializeRuleSet :: RuleSet -> Text
serializeRule :: FactObservationRule -> Toml.Table
```

**Success Criteria**:
- Working TOML parser for fact rules
- Support for rule sets
- Error reporting for invalid rules

**Testing**:
- Test parsing valid rule files
- Test error handling for invalid files
- Test roundtrip serialization

#### Step 3.3: Implement Rule Application Engine

**Implementation Strategy**:
- Create rule application engine that applies rules to blockchain data
- Support different blockchain data formats
- Implement fact extraction and transformation

**Affected Code**:
- Create `src/Core/FactObservation/RuleEngine.hs`
- Create `src/Core/FactObservation/Extractors.hs`

**Code Structure**:
```haskell
-- RuleEngine.hs
module Core.FactObservation.RuleEngine
  ( RuleEngine(..)
  , createRuleEngine
  , applyRules
  , applyRule
  ) where

import Core.FactObservation.Rules
import Core.Types

data RuleEngine = RuleEngine
  { rules :: [FactObservationRule]
  , extractors :: Map Text Extractor
  }

type Extractor = BlockData -> PathExpression -> IO (Either ExtractorError FactValue)

createRuleEngine :: [FactObservationRule] -> IO RuleEngine
applyRules :: RuleEngine -> BlockData -> IO [Either RuleError Fact]
applyRule :: RuleEngine -> FactObservationRule -> BlockData -> IO (Either RuleError Fact)
```

**Success Criteria**:
- Rule application engine
- Support for multiple fact types
- Error handling for rule application

**Testing**:
- Test rule application on mock blockchain data
- Test extraction of different fact types
- Test error handling for failed extractions

#### Step 3.4: Implement Timeline-Specific Extractors

**Implementation Strategy**:
- Create extractors for specific blockchain timelines (Ethereum, Celestia)
- Implement format-specific data extraction
- Support different proof types

**Affected Code**:
- Create `src/Core/FactObservation/Extractors/Ethereum.hs`
- Create `src/Core/FactObservation/Extractors/Celestia.hs`

**Code Structure**:
```haskell
-- Extractors/Ethereum.hs
module Core.FactObservation.Extractors.Ethereum
  ( createEthereumExtractor
  , ethereumBalanceExtractor
  , ethereumEventExtractor
  , ethereumStorageExtractor
  ) where

import Core.FactObservation.RuleEngine
import Adapters.EthereumAdapter

createEthereumExtractor :: EthereumAdapter -> Extractor
ethereumBalanceExtractor :: EthereumAdapter -> BlockData -> PathExpression -> IO (Either ExtractorError FactValue)
ethereumEventExtractor :: EthereumAdapter -> BlockData -> PathExpression -> IO (Either ExtractorError FactValue)
ethereumStorageExtractor :: EthereumAdapter -> BlockData -> PathExpression -> IO (Either ExtractorError FactValue)
```

**Success Criteria**:
- Working extractors for specific blockchains
- Support for common fact types
- Integration with blockchain adapters

**Testing**:
- Test extractors on sample blockchain data
- Test different path expressions
- Test proof generation

#### Step 3.5: Integrate with Time Keeper

**Implementation Strategy**:
- Integrate fact observation rules with Time Keeper
- Support loading rules from configuration
- Implement fact observation pipeline

**Affected Code**:
- Modify `src/Actors/TimeKeeper.hs`
- Create `src/Actors/TimeKeeper/FactObservation.hs`

**Code Structure**:
```haskell
-- TimeKeeper/FactObservation.hs
module Actors.TimeKeeper.FactObservation
  ( FactObservationConfig(..)
  , createFactObserver
  , startFactObservation
  , stopFactObservation
  ) where

import Core.FactObservation.Rules
import Core.FactObservation.RuleEngine

data FactObservationConfig = FactObservationConfig
  { ruleFiles :: [FilePath]
  , observationInterval :: Int
  , factBroadcastEnabled :: Bool
  }

createFactObserver :: FactObservationConfig -> TimelineAdapter -> IO FactObserver
startFactObservation :: FactObserver -> (Fact -> IO ()) -> IO ThreadId
stopFactObservation :: FactObserver -> IO ()
```

**Success Criteria**:
- Time Keeper integration with fact rules
- Configuration-based rule loading
- Fact observation and processing

**Testing**:
- Test rule loading in Time Keeper
- Test observation pipeline
- Test fact broadcasting

### Component 3 Success Criteria
- Complete implementation of fact observation rules
- TOML-based rule definition
- Rule application engine
- Integration with Time Keeper

## Component 4: Unified Log Reconstruction

### Context
The unified log reconstruction capabilities described in ADR-012 enable the rebuilding of fact logs and program execution history from blockchain data and gossip. This implementation will provide tools for on-demand log reconstruction and validation.

### Implementation Steps

#### Step 4.1: Implement Fact Log Storage

**Implementation Strategy**:
- Create storage mechanism for fact logs
- Support content-addressed storage
- Implement efficient querying and retrieval

**Affected Code**:
- Create `src/Core/FactLog/Storage.hs`
- Create `src/Core/FactLog/Query.hs`

**Code Structure**:
```haskell
-- Storage.hs
module Core.FactLog.Storage
  ( FactLogStorage(..)
  , createFactLogStorage
  , storeFact
  , retrieveFact
  , getFactHistory
  ) where

data FactLogStorage = FactLogStorage
  { backend :: StorageBackend
  , indexer :: FactIndexer
  }

data StorageBackend = InMemoryStorage | FileStorage FilePath | DatabaseStorage ConnectionString

createFactLogStorage :: StorageBackend -> IO FactLogStorage
storeFact :: FactLogStorage -> Fact -> IO (Either StorageError FactID)
retrieveFact :: FactLogStorage -> FactID -> IO (Either StorageError Fact)
getFactHistory :: FactLogStorage -> TimelineID -> TimeRange -> IO (Either StorageError [Fact])
```

**Success Criteria**:
- Working fact log storage
- Content-addressed fact storage
- Efficient fact retrieval

**Testing**:
- Test fact storage and retrieval
- Test query performance
- Test content addressing

#### Step 4.2: Implement Log Reconstruction Engine

**Implementation Strategy**:
- Create log reconstruction engine
- Support rebuilding logs from blockchain data
- Implement verification of reconstructed logs

**Affected Code**:
- Create `src/Core/FactLog/Reconstruction.hs`
- Create `src/Core/FactLog/Verification.hs`

**Code Structure**:
```haskell
-- Reconstruction.hs
module Core.FactLog.Reconstruction
  ( LogReconstructor(..)
  , createLogReconstructor
  , reconstructFactLog
  , verifyReconstructedLog
  ) where

import Core.FactObservation.Rules
import Core.FactObservation.RuleEngine

data LogReconstructor = LogReconstructor
  { ruleEngine :: RuleEngine
  , timelineAdapter :: TimelineAdapter
  , storage :: FactLogStorage
  }

createLogReconstructor :: RuleEngine -> TimelineAdapter -> FactLogStorage -> IO LogReconstructor
reconstructFactLog :: LogReconstructor -> TimelineID -> BlockRange -> IO (Either ReconstructionError FactLog)
verifyReconstructedLog :: LogReconstructor -> FactLog -> IO (Either VerificationError VerificationResult)
```

**Success Criteria**:
- Working log reconstruction
- Support for different blockchains
- Log verification

**Testing**:
- Test log reconstruction from blockchain data
- Test verification of reconstructed logs
- Test handling of missing data

#### Step 4.3: Implement Distributed Log Gossip

**Implementation Strategy**:
- Implement fact log gossip protocol
- Support peer-to-peer fact sharing
- Implement log reconciliation

**Affected Code**:
- Create `src/Core/FactLog/Gossip.hs`
- Create `src/Core/FactLog/Reconciliation.hs`

**Code Structure**:
```haskell
-- Gossip.hs
module Core.FactLog.Gossip
  ( FactGossipProtocol(..)
  , createFactGossipProtocol
  , startGossip
  , stopGossip
  , broadcastFact
  ) where

import Adapters.NetworkAdapter
import Core.FactLog.Storage

data FactGossipProtocol = FactGossipProtocol
  { networkAdapter :: NetworkAdapter
  , storage :: FactLogStorage
  , gossipConfig :: GossipConfig
  }

data GossipConfig = GossipConfig
  { broadcastInterval :: Int
  , maxFactsPerMessage :: Int
  , priorityFactTypes :: [FactType]
  }

createFactGossipProtocol :: NetworkAdapter -> FactLogStorage -> GossipConfig -> IO FactGossipProtocol
startGossip :: FactGossipProtocol -> IO ThreadId
stopGossip :: FactGossipProtocol -> IO ()
broadcastFact :: FactGossipProtocol -> Fact -> IO ()
```

**Success Criteria**:
- Working fact gossip protocol
- Peer-to-peer fact sharing
- Configurable gossip behavior

**Testing**:
- Test fact broadcasting
- Test gossip protocol
- Test reconciliation

#### Step 4.4: Implement Log Auditing and Verification

**Implementation Strategy**:
- Create tools for auditing fact logs
- Support verification against blockchain data
- Implement tamper detection

**Affected Code**:
- Create `src/Core/FactLog/Audit.hs`
- Create `src/Core/FactLog/TamperDetection.hs`

**Code Structure**:
```haskell
-- Audit.hs
module Core.FactLog.Audit
  ( FactLogAuditor(..)
  , createFactLogAuditor
  , auditFactLog
  , generateAuditReport
  ) where

import Core.FactLog.Storage
import Core.FactLog.Verification

data FactLogAuditor = FactLogAuditor
  { storage :: FactLogStorage
  , timelineAdapter :: TimelineAdapter
  , verificationConfig :: VerificationConfig
  }

data VerificationConfig = VerificationConfig
  { verifyProofs :: Bool
  , verifySignatures :: Bool
  , verifyTimestamps :: Bool
  }

createFactLogAuditor :: FactLogStorage -> TimelineAdapter -> VerificationConfig -> IO FactLogAuditor
auditFactLog :: FactLogAuditor -> TimelineID -> TimeRange -> IO (Either AuditError AuditResult)
generateAuditReport :: AuditResult -> IO AuditReport
```

**Success Criteria**:
- Fact log auditing tools
- Verification against blockchain data
- Tamper detection

**Testing**:
- Test log auditing
- Test tamper detection
- Test audit reporting

#### Step 4.5: Integrate with Bandit Nodes

**Implementation Strategy**:
- Integrate log reconstruction with Bandit nodes
- Support on-demand reconstruction
- Implement log sharing between Bandits

**Affected Code**:
- Modify `src/Actors/TimeBandit.hs`
- Create `src/Actors/TimeBandit/LogManagement.hs`

**Code Structure**:
```haskell
-- TimeBandit/LogManagement.hs
module Actors.TimeBandit.LogManagement
  ( LogManager(..)
  , createLogManager
  , startLogManager
  , requestLogReconstruction
  , shareLog
  ) where

import Core.FactLog.Reconstruction
import Core.FactLog.Gossip

data LogManager = LogManager
  { reconstructor :: LogReconstructor
  , gossipProtocol :: FactGossipProtocol
  , storage :: FactLogStorage
  }

createLogManager :: LogReconstructor -> FactGossipProtocol -> FactLogStorage -> IO LogManager
startLogManager :: LogManager -> IO ThreadId
requestLogReconstruction :: LogManager -> TimelineID -> TimeRange -> IO (Either Error FactLog)
shareLog :: LogManager -> PeerId -> TimelineID -> TimeRange -> IO ()
```

**Success Criteria**:
- Integration with Bandit nodes
- On-demand log reconstruction
- Log sharing between Bandits

**Testing**:
- Test integration with Bandit nodes
- Test on-demand reconstruction
- Test log sharing

### Component 4 Success Criteria
- Complete implementation of unified log reconstruction
- Fact log storage and retrieval
- Log reconstruction from blockchain data
- Log gossip and sharing
- Log auditing and verification

## Overall Success Criteria

The implementation will be considered successful when:

1. The P2P Network Communication Layer enables reliable message passing between distributed nodes
2. The Advanced Concurrency Model supports safe concurrent execution with temporal combinators
3. The Fact Observation Rules system allows configurable extraction of facts from blockchain data
4. The Unified Log Reconstruction capabilities enable on-demand rebuilding and verification of fact logs

All components should work together to enable end-to-end functionality of the Time Bandits system, allowing it to operate as a distributed network with proper concurrency control and fact observation capabilities.

## Testing Strategy

In addition to the component-specific tests, the following integration tests should be implemented:

1. **End-to-End Network Test**: Deploy multiple Bandit nodes on different machines, test message passing, peer discovery, and resilience to network failures.

2. **Concurrent Program Execution Test**: Create and run programs that use the temporal combinators, verify correct execution and resource safety.

3. **Fact Observation Pipeline Test**: Configure fact observation rules, apply them to real or simulated blockchain data, verify fact extraction and propagation.

4. **Log Reconstruction and Verification Test**: Deliberately remove fact logs, test on-demand reconstruction, verify the reconstructed logs match the original.

These tests will verify that all components work together correctly in a realistic environment. 