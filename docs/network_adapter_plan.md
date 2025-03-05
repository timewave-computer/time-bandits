# Network Adapter Implementation Plan

This document outlines the plan for implementing the `NetworkAdapter` module for Time Bandits.

## Overview

The `NetworkAdapter` will handle peer-to-peer communication between Time Bandits nodes, enabling distributed consensus and data sharing. Unlike the blockchain adapters (Ethereum, Celestia), this adapter is focused on direct node-to-node communication rather than interacting with external blockchains.

## Implementation Requirements

1. The adapter should provide a simple, reliable interface for P2P communication
2. Support both local and wide-area network communication
3. Implement security features (authentication, encryption, message integrity)
4. Provide robust error handling and retry mechanisms
5. Include proper logging of all operations
6. Support various network topologies (direct, mesh, etc.)
7. Handle network partitions and reconnection gracefully

## Module Structure

```haskell
module Adapters.NetworkAdapter
  ( createNetworkAdapter
  -- Network operations  
  , NetworkConfig(..)
  , NetworkState(..)
  , PeerId(..)
  , MessageType(..)
  , ConnectionStatus(..)
  -- Core functions
  , broadcastMessage
  , sendDirectMessage
  , subscribeToMessages
  , getPeerStatus
  , discoverPeers
  ) where
```

## Core Components

### Configuration

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
  }

data TLSConfig = TLSConfig
  { certPath :: FilePath            -- Path to TLS certificate
  , keyPath :: FilePath             -- Path to TLS private key
  , caCertPath :: Maybe FilePath    -- Optional CA certificate for validation
  }

data PeerAddress = PeerAddress
  { host :: String
  , port :: Int
  , peerId :: Maybe PeerId
  }
```

### State Management

```haskell
data NetworkState = NetworkState
  { activePeers :: Map PeerId PeerInfo     -- Connected and active peers
  , pendingPeers :: Map PeerId PeerInfo    -- Peers attempting to connect
  , messageHandlers :: [MessageHandler]    -- Registered message handlers
  , networkStats :: NetworkStats           -- Statistics about network activity
  , asyncJobs :: [Async ()]               -- Background jobs for network operations
  }

data PeerInfo = PeerInfo
  { peerId :: PeerId                      -- Unique peer identifier
  , connectionStatus :: ConnectionStatus  -- Current connection status
  , address :: PeerAddress               -- Network address of peer
  , latency :: Int                       -- Last measured latency in ms
  , lastSeen :: UTCTime                  -- Last time peer was seen
  , messagesSent :: Int                  -- Count of messages sent to peer
  , messagesReceived :: Int              -- Count of messages received from peer
  , connection :: Connection             -- Active connection if connected
  }

data ConnectionStatus
  = Connected
  | Connecting
  | Disconnected
  | Banned
  deriving (Show, Eq)

data NetworkStats = NetworkStats
  { totalMessagesSent :: Int
  , totalMessagesReceived :: Int
  , totalBytesReceived :: Integer
  , totalBytesSent :: Integer
  , uptime :: Int  -- Seconds
  , connectedPeerCount :: Int
  }
```

### Message Format

```haskell
data Message = Message
  { messageId :: UUID
  , messageType :: MessageType
  , sender :: PeerId
  , recipient :: Maybe PeerId  -- Nothing means broadcast
  , timestamp :: UTCTime
  , payload :: ByteString
  , signature :: Maybe ByteString
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

type MessageHandler = Message -> NetworkState -> IO (NetworkState, Maybe Message)
```

## Key Functions

1. **Adapter Creation**
   ```haskell
   createNetworkAdapter :: NetworkConfig -> IO NetworkAdapter
   ```

2. **Core Network Operations**
   ```haskell
   -- Send a message to all connected peers
   broadcastMessage :: NetworkState -> MessageType -> ByteString -> IO (Either NetworkError [PeerId])
   
   -- Send a message to a specific peer
   sendDirectMessage :: NetworkState -> PeerId -> MessageType -> ByteString -> IO (Either NetworkError ())
   
   -- Subscribe to a specific message type
   subscribeToMessages :: NetworkState -> MessageType -> (Message -> IO ()) -> IO NetworkState
   
   -- Get the status of a peer
   getPeerStatus :: NetworkState -> PeerId -> IO (Either NetworkError PeerInfo)
   
   -- Discover new peers from the network
   discoverPeers :: NetworkState -> IO (Either NetworkError [PeerInfo])
   ```

3. **Peer Management Functions**
   ```haskell
   addPeer :: NetworkState -> PeerAddress -> IO NetworkState
   removePeer :: NetworkState -> PeerId -> IO NetworkState
   banPeer :: NetworkState -> PeerId -> String -> IO NetworkState
   reconnectToPeers :: NetworkState -> IO NetworkState
   ```

4. **Utility Functions**
   ```haskell
   signMessage :: PrivateKey -> Message -> IO Message
   verifyMessage :: PublicKey -> Message -> IO Bool
   serializeMessage :: Message -> ByteString
   deserializeMessage :: ByteString -> Either String Message
   ```

## Security

The NetworkAdapter will implement several security features:

1. **Authentication**: Verify the identity of peers through public-key cryptography
2. **Encryption**: Support TLS for secure communication
3. **Message Integrity**: Include signatures to verify message authenticity
4. **Peer Validation**: Implement measures to prevent Sybil attacks
5. **Rate Limiting**: Protect against DoS attacks

## Dependencies

- `network` - For basic networking support
- `tls` - For TLS/encryption support
- `cryptonite` - For cryptographic operations
- `stm` - For concurrent state management
- `async` - For asynchronous operations
- `uuid` - For unique message identifiers
- `text`, `bytestring` - For data handling

## Testing Strategy

1. **Unit Tests**
   - Test message serialization/deserialization
   - Test state management functions
   - Test peer management logic

2. **Integration Tests**
   - Test communication between multiple local nodes
   - Test handling of network errors and partitions
   - Test reconnection behavior

3. **Load Tests**
   - Test with high message volumes
   - Test with many connected peers
   - Test bandwidth and latency under load

## Implementation Steps

1. Set up the basic module structure
2. Implement core communication primitives (connect, send, receive)
3. Add peer discovery and management
4. Implement message serialization and handling
5. Add security features (authentication, encryption)
6. Implement advanced features (reconnection, error handling)
7. Add comprehensive testing
8. Document usage patterns and examples

## Timeline Estimate

- Basic structure and communication primitives: 3 days
- Peer management and discovery: 2 days
- Message handling and serialization: 2 days
- Security implementation: 3 days
- Advanced features and error handling: 3 days
- Testing and refinement: 4 days

Total: Approximately 2-3 weeks of development effort

## Challenges and Considerations

1. Network partitions and node failures must be handled gracefully
2. Security is critical for a distributed system
3. Performance optimization will be important for high-throughput scenarios
4. NAT traversal may be needed for nodes behind firewalls
5. The system should scale well with increasing numbers of nodes 