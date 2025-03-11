# ADR 014: Network Adapter

## Status

Proposed

## Context

Time Bandits requires a robust peer-to-peer communication system to enable distributed consensus and data sharing between nodes. Unlike blockchain adapters (Ethereum, Celestia) that interact with external blockchains, this adapter focuses on direct node-to-node communication within the Time Bandits network.

Currently, our system lacks a standardized approach for handling peer-to-peer communication, which makes it difficult to implement distributed features like consensus algorithms, shared state, and coordinated effect execution. As we move towards a more distributed architecture, we need a well-defined network layer that handles the complexities of network communication while providing a simple interface for the rest of the system.

The Time Bandits network consists of two main types of nodes:
1. **Bandit Nodes**: Full nodes that participate in consensus, execution, and data storage
2. **Party Traveler Clients**: Lightweight clients that interact with the network to access data and services

These different node types require different communication patterns and priorities, which our network adapter needs to support.

Key challenges that must be addressed include:
1. Reliable message delivery in potentially unreliable network conditions
2. Secure communication with authentication and encryption
3. Peer discovery and management with support for different peer types
4. Handling network partitions and reconnections
5. Supporting various network topologies
6. Providing a consistent interface for both local and wide-area networks
7. Allowing nodes to control their peer connections through whitelists/blacklists
8. Prioritizing certain peer connections based on node type and requirements

## Decision

We will implement a dedicated NetworkAdapter module that provides a standardized interface for all peer-to-peer communication in the Time Bandits system. The adapter will abstract away the complexities of network communication and provide a simple, consistent API for the rest of the system.

### Core Components

#### Configuration

The NetworkAdapter will be configurable to support different deployment scenarios:

```haskell
data NetworkConfig = NetworkConfig
  { listenAddress :: String         -- Address to listen on (e.g., "0.0.0.0")
  , listenPort :: Int               -- Port to listen on
  , bootstrapPeers :: [PeerAddress] -- Initial peers to connect to
  , maxConnections :: Int           -- Maximum number of concurrent connections 
  , connectionTimeout :: Int        -- Connection timeout in milliseconds
  , useEncryption :: Bool           -- Whether to use TLS/encryption
  , tlsConfig :: Maybe TLSConfig    -- TLS configuration if encryption is enabled
  , nodeName :: String              -- Human-readable node identifier
  , messageBufferSize :: Int        -- Size of message buffer for async processing
  , peerListMode :: PeerListMode    -- Whitelist or blacklist mode
  , peerList :: [PeerId]            -- List of peers to whitelist or blacklist
  , peerTypeWeights :: Map PeerType Int -- Prioritization weights for peer types
  }

data PeerListMode = Whitelist | Blacklist
  deriving (Show, Eq)

data PeerType = BanditNode | PartyTravelerClient | UnknownPeer
  deriving (Show, Eq, Ord)
```

#### State Management

The adapter will maintain state about connected peers and network conditions:

```haskell
data NetworkState = NetworkState
  { activePeers :: Map PeerId PeerInfo     -- Connected and active peers
  , pendingPeers :: Map PeerId PeerInfo    -- Peers attempting to connect
  , messageHandlers :: [MessageHandler]    -- Registered message handlers
  , networkStats :: NetworkStats           -- Statistics about network activity
  , asyncJobs :: [Async ()]               -- Background jobs for network operations
  , priorityQueue :: PriorityQueue Message -- Priority queue for outgoing messages
  }

data PeerInfo = PeerInfo
  { peerId :: PeerId                      -- Unique peer identifier
  , peerType :: PeerType                  -- Type of peer (Bandit, Client, etc.)
  , connectionStatus :: ConnectionStatus  -- Current connection status
  , address :: PeerAddress               -- Network address of peer
  , latency :: Int                       -- Last measured latency in ms
  , lastSeen :: UTCTime                  -- Last time peer was seen
  , messagesSent :: Int                  -- Count of messages sent to peer
  , messagesReceived :: Int              -- Count of messages received from peer
  , connection :: Connection             -- Active connection if connected
  , priority :: Int                      -- Connection priority (higher = more important)
  }
```

#### Message Format

A standardized message format will ensure consistency across the system:

```haskell
data Message = Message
  { messageId :: UUID
  , messageType :: MessageType
  , sender :: PeerId
  , recipient :: Maybe PeerId  -- Nothing means broadcast
  , timestamp :: UTCTime
  , payload :: ByteString
  , signature :: Maybe ByteString
  , priority :: MessagePriority -- Priority level for processing
  }

data MessageType
  = PeerDiscovery
  | PeerStatus
  | EffectPropagation
  | TimeMapUpdate
  | ResourceUpdate
  | ProgramSync
  | ControlMessage
  deriving (Show, Eq, Enum)

data MessagePriority = Low | Normal | High | Critical
  deriving (Show, Eq, Ord, Enum)
```

### Core API

The NetworkAdapter will expose the following core functions:

1. **Adapter Creation and Management**
   ```haskell
   createNetworkAdapter :: NetworkConfig -> IO NetworkAdapter
   ```

2. **Message Operations**
   ```haskell
   -- Send a message to all connected peers
   broadcastMessage :: NetworkState -> MessageType -> ByteString -> MessagePriority -> IO (Either NetworkError [PeerId])
   
   -- Send a message to a specific peer
   sendDirectMessage :: NetworkState -> PeerId -> MessageType -> ByteString -> MessagePriority -> IO (Either NetworkError ())
   
   -- Subscribe to a specific message type
   subscribeToMessages :: NetworkState -> MessageType -> (Message -> IO ()) -> IO NetworkState
   ```

3. **Peer Management**
   ```haskell
   -- Get the status of a peer
   getPeerStatus :: NetworkState -> PeerId -> IO (Either NetworkError PeerInfo)
   
   -- Discover new peers from the network
   discoverPeers :: NetworkState -> IO (Either NetworkError [PeerInfo])
   
   -- Add a new peer
   addPeer :: NetworkState -> PeerAddress -> PeerType -> Int -> IO NetworkState
   
   -- Remove a peer
   removePeer :: NetworkState -> PeerId -> IO NetworkState
   
   -- Update peer whitelist/blacklist
   updatePeerList :: NetworkState -> PeerListMode -> [PeerId] -> IO NetworkState
   
   -- Set peer priority
   setPeerPriority :: NetworkState -> PeerId -> Int -> IO NetworkState
   
   -- Set peer type weights
   setPeerTypeWeights :: NetworkState -> Map PeerType Int -> IO NetworkState
   ```

### Peer Types and Prioritization

The NetworkAdapter will support different peer types with configurable prioritization:

1. **Bandit Nodes**: Full nodes that participate in consensus and execution. These are typically prioritized for program execution and consensus.

2. **Party Traveler Clients**: Lightweight clients that access data and services. These are prioritized for serving data requests.

3. **Unknown Peers**: Newly discovered peers that haven't been classified yet.

Peer connections will be managed based on:

1. **Whitelist/Blacklist**: Nodes can be configured to only connect to specific peers (whitelist) or to connect to any peer except specific ones (blacklist).

2. **Priority Levels**: Each peer connection will have a priority level that determines resource allocation during high load.

3. **Peer Type Weights**: Global weights can be assigned to different peer types to influence connection prioritization.

### Security Measures

The NetworkAdapter will implement several security features:

1. **Authentication**: Verifying the identity of peers through public-key cryptography
2. **Encryption**: Using TLS for secure communication
3. **Message Integrity**: Including signatures to verify message authenticity
4. **Peer Validation**: Implementing measures to prevent Sybil attacks
5. **Rate Limiting**: Protecting against DoS attacks
6. **Access Control**: Using whitelist/blacklist to control which peers can connect

## Consequences

### Positive

1. **Simplified Communication**: The rest of the system can use a consistent, high-level API for network operations without worrying about the underlying details.
   
2. **Enhanced Security**: Built-in security features ensure that all communication is authenticated and encrypted.
   
3. **Better Reliability**: Automatic handling of connection issues, retries, and reconnections improves the overall reliability of the system.
   
4. **Modularity**: The adapter pattern allows for different network implementations (e.g., WebRTC, libp2p, custom TCP) without changing the rest of the codebase.
   
5. **Observability**: Centralized logging and metrics collection for all network operations.

6. **Flexibility**: Support for different peer types and prioritization allows nodes to optimize their connections for their specific needs.

### Negative

1. **Additional Complexity**: Introducing another layer adds some complexity to the system.
   
2. **Performance Overhead**: The abstraction may introduce some overhead compared to direct network calls.
   
3. **Deployment Complexity**: Proper configuration for different network environments (local, cloud, etc.) will require careful documentation and potentially additional tooling.

4. **Participation Limitations**: Nodes that significantly restrict their peer connections through strict whitelisting may limit their ability to participate in certain programs or access the full network state.

### Neutral

1. **Dependencies**: The implementation will require several external libraries for networking, cryptography, and concurrency management.

2. **Configuration Tradeoffs**: Different configurations will offer trade-offs between security, performance, and network participation.

## Implementation Plan

1. Set up the basic module structure
2. Implement core communication primitives (connect, send, receive)
3. Add peer discovery and management with support for peer types
4. Implement message serialization and handling with priority
5. Add security features (authentication, encryption)
6. Implement advanced features (reconnection, error handling, whitelist/blacklist)
7. Add message prioritization and resource allocation
8. Add comprehensive testing
9. Document usage patterns and examples 