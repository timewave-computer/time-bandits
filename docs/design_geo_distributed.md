# Geo-Distributed Mode Implementation

## Overview

The Geo-Distributed mode allows the Time Bandits system to run across multiple machines in different geographic locations. This document describes the implementation of this mode using QUIC (Quick UDP Internet Connections) as the transport protocol.

## Architecture

The Geo-Distributed mode builds on the existing P2P network infrastructure but adds QUIC-based networking for secure, reliable, and efficient communication between nodes. The implementation consists of the following components:

1. **QUIC Server**: Each node runs a QUIC server that listens for incoming connections from other nodes.
2. **QUIC Client**: Each node can also act as a client to connect to other nodes.
3. **Certificate Management**: Self-signed certificates are generated for secure communication.
4. **Peer Discovery**: Nodes can discover each other through bootstrap nodes or peer exchange.
5. **Message Routing**: Messages are routed through the P2P network to reach their destination.

## Implementation Details

### QUIC Configuration

The QUIC implementation uses the following configuration options:

```haskell
data QuicConfig = QuicConfig
  { qcBindAddress :: SockAddr        -- Address to bind the QUIC server to
  , qcBindPort :: Int                -- Port to bind the QUIC server to
  , qcCertPath :: FilePath           -- Path to the certificate file
  , qcKeyPath :: FilePath            -- Path to the private key file
  , qcNetworkMode :: QuicNetworkMode -- Network mode
  , qcBootstrapNodes :: [SockAddr]   -- Bootstrap nodes to connect to
  , qcConnectionTimeout :: Int       -- Connection timeout in milliseconds
  , qcMaxConnections :: Int          -- Maximum number of connections
  , qcMaxMessageSize :: Int          -- Maximum message size in bytes
  , qcPingInterval :: Int            -- Ping interval in milliseconds
  }
```

### Network Modes

The QUIC implementation supports three network modes:

```haskell
data QuicNetworkMode
  = ClientMode     -- Client-only mode
  | ServerMode     -- Server-only mode
  | HybridMode     -- Both client and server mode
```

### Message Types

The QUIC implementation supports the following message types:

```haskell
data QuicMessage
  = QuicPing UTCTime
  | QuicPong UTCTime
  | QuicDiscover
  | QuicAnnounce QuicPeer
  | QuicPeerList [QuicPeer]
  | QuicTransition ByteString  -- Serialized transition message
  | QuicResourceRequest ByteString  -- Serialized resource request
  | QuicResourceResponse ByteString  -- Serialized resource response
  | QuicError QuicError
```

### Security

The QUIC implementation uses the following security features:

1. **TLS 1.3**: All communications are encrypted using TLS 1.3.
2. **Certificate Verification**: Certificates are verified to ensure secure communication.
3. **Message Authentication**: All messages are authenticated to prevent tampering.
4. **Connection Security**: QUIC provides built-in security features such as connection migration and 0-RTT resumption.

### Integration with Existing Infrastructure

The QUIC implementation integrates with the existing P2P network infrastructure through the following components:

1. **P2P Configuration Conversion**: The `p2pConfigToQuicConfig` function converts a P2P configuration to a QUIC configuration.
2. **Actor Integration**: The QUIC server is associated with an actor, allowing it to participate in the Time Bandits system.
3. **Message Routing**: Messages are routed through the P2P network to reach their destination.

## Usage

### Starting a QUIC Server

```haskell
-- Create a QUIC configuration
let config = defaultQuicConfig {
      qcBindAddress = SockAddrInet 8443 (tupleToHostAddress (127, 0, 0, 1)),
      qcBindPort = 8443,
      qcNetworkMode = ServerMode
    }

-- Start the QUIC server
server <- startQuicServer config serverActor pubKey
```

### Connecting to a Peer

```haskell
-- Connect to a peer
conn <- connectToQuicPeer server (SockAddrInet 8443 (tupleToHostAddress (127, 0, 0, 1)))

-- Send a message
sendQuicMessage server conn (QuicPing now)
```

### Broadcasting a Message

```haskell
-- Broadcast a message to all peers
broadcastQuicMessage server (QuicDiscover)
```

## Testing

The QUIC implementation can be tested using the `scripts/test-quic.hs` script, which provides a simple server and client for testing the QUIC implementation.

### Server Mode

```bash
./scripts/test-quic.hs server
```

### Client Mode

```bash
./scripts/test-quic.hs client
```

## Future Improvements

1. **Connection Pooling**: Implement connection pooling to improve performance.
2. **Load Balancing**: Implement load balancing to distribute connections across multiple servers.
3. **Congestion Control**: Implement congestion control to prevent network congestion.
4. **Flow Control**: Implement flow control to prevent overwhelming receivers.
5. **Multipath**: Implement multipath to improve reliability and performance.
6. **0-RTT Resumption**: Implement 0-RTT resumption to improve connection establishment time.
7. **Connection Migration**: Implement connection migration to improve reliability.
8. **NAT Traversal**: Implement NAT traversal to improve connectivity in restricted networks.
9. **Peer Discovery**: Implement more sophisticated peer discovery mechanisms.
10. **Message Routing**: Implement more sophisticated message routing algorithms.

## Conclusion

The QUIC-based Geo-Distributed mode provides a secure, reliable, and efficient way for the Time Bandits system to run across multiple machines in different geographic locations. It builds on the existing P2P network infrastructure and adds QUIC-based networking for improved performance and security. 