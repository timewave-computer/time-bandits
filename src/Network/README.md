# Time Bandits - P2P Network Communication Layer

This directory contains the implementation of Component 1 of the Time Bandits project: the P2P network communication layer.

## Overview

The P2P network communication layer provides:
- Peer discovery using rendezvous hashing
- Connection management with peers
- Topic-based publish/subscribe messaging
- Logging of network events
- Protocol versioning and feature negotiation

## Key Components

### Network Manager

The `Network.Manager` module is the main interface for the P2P network layer. It provides functions for:
- Creating and managing peer connections
- Sending and receiving messages
- Topic subscriptions
- Integration with the peer discovery mechanism

### Peer Discovery

The `Network.Discovery.PeerDiscovery` module implements the peer discovery mechanism using rendezvous hashing. This approach:
- Avoids the need for a separate DHT (Distributed Hash Table)
- Provides deterministic peer selection
- Ensures efficient distribution of peer knowledge

### Protocol Versioning

The `Network.Protocol.Version` module manages protocol versioning and feature negotiation:
- Semantic versioning (major.minor.patch)
- Feature flags for capability negotiation
- Version compatibility checking
- Binary serialization for network transport

### Hashing Utilities

The `Core.Hashing` module provides the cryptographic primitives used by the rendezvous hashing mechanism.

## Architecture

The network layer is designed to be:
1. **Decentralized**: No central server or coordination point
2. **Scalable**: Can handle a large number of nodes
3. **Resilient**: Continues to function when nodes join or leave the network
4. **Efficient**: Minimizes network traffic for discovery
5. **Versioned**: Supports protocol evolution with backward compatibility

## Usage

### Creating a Network Manager

```haskell
-- Create a log store
store <- createLogStore (Just "logs")

-- Define bootstrap peers
bootstrapPeers <- [...]

-- Create network configuration
let config = NetworkConfig
      { networkNodeId = "my-node-id"
      , networkListenAddr = myAddress
      , networkBootstrapPeers = bootstrapPeers
      , networkMaxPeers = 20
      , networkLogStore = store
      , networkDiscoveryInterval = 30000  -- 30 seconds
      , networkProtocolVersion = currentVersion
      }

-- Create and start the network manager
withNetworkManager config $ \manager -> do
  -- Use the network manager
  ...
```

### Subscribing to Topics

```haskell
-- Define a callback for messages
let callback message = do
      putStrLn $ "Received message on topic: " ++ messageTopic message

-- Subscribe to a topic
subscribe manager "important-updates" callback
```

### Sending Messages

```haskell
-- Send a message to a specific peer
sendMessage manager "peer-id" "topic" payload

-- Broadcast to all peers subscribed to a topic
broadcastMessage manager "topic" payload
```

### Protocol Versioning

The network layer uses semantic versioning and feature flags:

```haskell
-- Check if two versions are compatible
compatible <- isCompatible myVersion peerVersion

-- Get detailed validation result
case validateVersion myVersion peerVersion of
  Left errorMsg -> 
    putStrLn $ "Incompatible version: " ++ errorMsg
  Right successMsg -> 
    putStrLn $ "Version OK: " ++ successMsg

-- Get supported features
let features = supportedFeatures peerVersion
```

## Testing

The P2P network layer includes comprehensive tests:
- Unit tests for individual components
- Integration tests for component interactions
- Simulated network conditions to test resilience
- Protocol version compatibility tests

Run the tests with:
```
cabal test
``` 