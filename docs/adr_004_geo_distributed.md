# ADR-004: Geo-Distributed Mode Implementation

## Status

Accepted

## Context

The Time Bandits system needs to operate efficiently across multiple machines in different geographic locations while maintaining security and performance. The existing local-only implementation does not provide the features needed for geo-distributed deployment, such as secure network transport, certificate management, and peer discovery.

## Decision

We will implement a Geo-Distributed mode for Time Bandits using QUIC (Quick UDP Internet Connections) as the transport protocol. QUIC provides a modern, secure, and efficient foundation for our distributed system with built-in TLS 1.3 encryption and UDP-based transport.

### Architecture

The implementation will consist of the following components:

1. **QUIC Server**: Handles incoming connections and requests.
2. **QUIC Client**: Manages outgoing connections to other Time Bandits nodes.
3. **Certificate Management**: Generates and validates TLS certificates.
4. **Peer Discovery**: Automatic and manual peer discovery mechanisms.
5. **Message Routing**: Efficient routing of messages between nodes.

### QUIC Configuration Options

- **Server Port**: Configurable port for the QUIC server.
- **Client Connection Timeout**: Maximum time to wait for connection establishment.
- **TLS Certificate Path**: Location of TLS certificates for secure communication.
- **Maximum Message Size**: Configurable limit on message size.
- **Retry Token**: Enable/disable retry token for DoS protection.

### Network Modes

1. **ClientMode**: Node acts only as a client, connecting to existing server nodes.
2. **ServerMode**: Node acts as a server, accepting incoming connections from clients.
3. **HybridMode**: Node acts as both client and server, enabling peer-to-peer operation.

### Message Types

- **QuicPing/QuicPong**: Health check mechanisms.
- **QuicDiscover**: Peer discovery request.
- **QuicDiscoveryResponse**: Response containing known peers.
- **QuicMessage**: Generic message for application-level communication.
- **QuicBroadcast**: Message to be propagated to all connected peers.

## Consequences

### Security

The Geo-Distributed mode provides several security enhancements:

- **TLS 1.3 Encryption**: All communications are encrypted using the latest TLS protocol.
- **Certificate Verification**: All peers must present valid certificates.
- **Message Authentication**: All messages are authenticated to prevent spoofing.
- **Connection Security**: QUIC's connection ID ensures connection validity, even with changing IP addresses.

### Performance

- **Multiplexing**: Multiple streams over a single connection reduce latency.
- **Connection Migration**: Connections can migrate across network changes without disruption.
- **Low Latency**: Designed for minimal round-trips during connection establishment.
- **UDP-Based**: Avoids head-of-line blocking issues present in TCP.

### Scalability

- **Dynamic Peer Discovery**: Nodes can discover peers automatically.
- **Resource Efficiency**: QUIC's design minimizes resource usage for idle connections.
- **Geographic Distribution**: System can operate effectively across global regions.

## Implementation

### Integration with Existing Infrastructure

The QUIC implementation integrates with the existing P2P network infrastructure through:

- **Configuration Conversion**: Translation between `P2PConfig` and `QuicConfig`.
- **Actor Integration**: QUIC nodes register with actor system for message handling.
- **Logging**: Integration with the existing logging infrastructure.

### Usage

#### Starting a QUIC Server

```haskell
main = do
  config <- defaultQuicConfig
    { serverPort = 4433
    , certificatePath = "/path/to/cert"
    }
  server <- startQuicServer config handleMessage
  -- Application logic
  stopQuicServer server
```

#### Connecting to a Peer

```haskell
connectToPeer :: QuicClient -> PeerAddress -> IO Connection
connectToPeer client address = do
  conn <- quicConnect client address
  -- Send initial messages
  quicSend conn (QuicPing "Hello")
  return conn
```

#### Broadcasting a Message

```haskell
broadcastMessage :: QuicServer -> QuicMessage -> IO ()
broadcastMessage server msg = do
  quicBroadcast server (QuicBroadcast msg)
```

### Testing

A testing script (`scripts/test-quic.hs`) is provided that demonstrates both server and client modes, allowing for verification of connectivity and message passing.

### Future Improvements

- **Connection Pooling**: Implement connection caching for frequently contacted peers.
- **Load Balancing**: Distribute connections across multiple server instances.
- **Congestion Control**: Add custom congestion control algorithms for Time Bandits traffic patterns.
- **NAT Traversal**: Improve handling of NAT scenarios for better connectivity.

### Conclusion

The QUIC-based Geo-Distributed mode provides a secure, efficient, and modern foundation for operating Time Bandits across geographic locations. It simplifies deployments in distributed environments while maintaining the security properties required by the system. 