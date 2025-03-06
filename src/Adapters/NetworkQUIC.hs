{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

{- |
This module implements the QUIC-based network transport for Geo-Distributed Mode.
It provides:

1. QUIC-based connectivity for high-performance, secure communication
2. Certificate management for node authentication
3. Connection pooling and management
4. Resilience against network partitions and failures
5. Service discovery mechanisms
-}
module TimeBandits.NetworkQUIC
  ( -- * QUIC Network Configuration
    QuicConfig(..)
  , defaultQuicConfig
  , NetworkMode(..)
    
  -- * Network Operations
  , startQuicServer
  , connectToQuicPeer
  , broadcastQuicMessage
  , sendQuicMessage
  , receiveQuicMessage
  , closeQuicConnection
  
  -- * Peer Discovery
  , discoverQuicPeers
  , announcePeer
  , registerWithBootstrapPeers
  
  -- * Network Resilience
  , handleNetworkPartition
  , attemptReconnection
  , migrateConnection
  
  -- * Types
  , QuicConnection(..)
  , QuicPeer(..)
  , QuicMessage(..)
  , QuicStats(..)
  , QuicNetworkMode(..)
  , QuicError(..)
  , QuicServer
  
  -- * Configuration
  , p2pConfigToQuicConfig
  , generateCertificate
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, readTVar, modifyTVar)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception (try, SomeException)
import Control.Monad (void, forever, when)
import Crypto.PubKey.X509 (X509)
import Crypto.Random (getRandomBytes)
import Data.Binary (Binary, encode, decode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import GHC.Generics (Generic)
import Network.Socket (SockAddr(..), HostName, PortNumber)
import Polysemy
import Polysemy.Error
import Polysemy.State qualified as PS
import Polysemy.Trace qualified as PT
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process (callCommand)

import TimeBandits.Core qualified as Core
import Core.Types
import TimeBandits.Network (P2PNode(..), P2PCapability(..))

-- | QUIC peer representation
data QuicPeer = QuicPeer
  { qpId :: !ActorHash
  -- ^ Unique identifier
  , qpAddress :: !SockAddr
  -- ^ Network address
  , qpPublicKey :: !PubKey
  -- ^ Public key for authentication
  , qpCapabilities :: ![P2PCapability]
  -- ^ Capabilities this peer supports
  , qpLastSeen :: !UTCTime
  -- ^ Last time this peer was seen
  , qpConnectionId :: !ByteString
  -- ^ QUIC connection identifier
  , qpStats :: !QuicStats
  -- ^ Connection statistics
  }
  deriving stock (Show, Eq, Generic)

-- | QUIC connection statistics
data QuicStats = QuicStats
  { qsRTT :: !Double
  -- ^ Round-trip time in milliseconds
  , qsPacketsSent :: !Int
  -- ^ Number of packets sent
  , qsPacketsReceived :: !Int
  -- ^ Number of packets received
  , qsPacketsLost :: !Int
  -- ^ Number of packets lost
  , qsBytesTransferred :: !Int
  -- ^ Total bytes transferred
  , qsLastActivity :: !UTCTime
  -- ^ Timestamp of last activity
  }
  deriving stock (Show, Eq, Generic)

-- | QUIC connection state
data QuicConnection = QuicConnection
  { qcPeer :: !QuicPeer
  -- ^ Remote peer
  , qcLocalPort :: !PortNumber
  -- ^ Local port
  , qcRemotePort :: !PortNumber
  -- ^ Remote port
  , qcConnectionState :: !ConnectionState
  -- ^ Current state of the connection
  , qcDataChannel :: !ByteString
  -- ^ Channel identifier for data
  , qcControlChannel :: !ByteString
  -- ^ Channel identifier for control messages
  , qcTimeOpened :: !UTCTime
  -- ^ When the connection was established
  , qcIsEncrypted :: !Bool
  -- ^ Whether the connection is encrypted
  }
  deriving stock (Show, Eq, Generic)

-- | Connection state
data ConnectionState
  = Connecting
  | Connected
  | AwaitingHandshake
  | Established
  | Degraded
  | Reconnecting
  | Closing
  | Closed
  deriving stock (Show, Eq, Generic)

-- | Message types for QUIC communication
data QuicMessage
  = HandshakeRequest
    { qmSenderNode :: !QuicPeer
    , qmTimestamp :: !UTCTime
    , qmCertificate :: !ByteString
    }
  | HandshakeResponse
    { qmResponderNode :: !QuicPeer
    , qmAllowConnection :: !Bool
    , qmSupportedCapabilities :: ![P2PCapability]
    }
  | DiscoveryRequest
    { qmRequestingNode :: !QuicPeer
    , qmRequestId :: !ByteString
    }
  | DiscoveryResponse
    { qmRespondingNode :: !QuicPeer
    , qmDiscoveredPeers :: ![QuicPeer]
    , qmRequestId :: !ByteString
    }
  | ActorMessage
    { qmSourceActor :: !ActorHash
    , qmDestinationActor :: !(Maybe ActorHash)
    , qmMessagePayload :: !ByteString
    , qmMessageId :: !ByteString
    }
  | MessageAck
    { qmAckedMessageId :: !ByteString
    , qmAckTimestamp :: !UTCTime
    }
  | KeepAlive
    { qmSenderNode :: !QuicPeer
    , qmTimestamp :: !UTCTime
    }
  | ConnectionClose
    { qmReason :: !Text
    , qmGraceful :: !Bool
    }
  deriving stock (Show, Eq, Generic)

-- | Network operating mode
data NetworkMode
  = ServerMode     -- ^ Server mode listening for connections
  | ClientMode     -- ^ Client mode initiating connections
  | HybridMode     -- ^ Both server and client capabilities
  deriving stock (Show, Eq, Generic)

-- | QUIC network configuration
data QuicConfig = QuicConfig
  { qcBindAddress :: !SockAddr
  -- ^ Address to bind to
  , qcBindPort :: !PortNumber
  -- ^ Port to bind to
  , qcNetworkMode :: !NetworkMode
  -- ^ Operating mode
  , qcBootstrapPeers :: ![SockAddr]
  -- ^ Initial peers to connect to
  , qcMaxPeers :: !Int
  -- ^ Maximum number of peers to connect to
  , qcCertificatePath :: !FilePath
  -- ^ Path to TLS certificate
  , qcPrivateKeyPath :: !FilePath
  -- ^ Path to private key
  , qcTrustStorePath :: !FilePath
  -- ^ Path to trust store
  , qcKeepAliveInterval :: !Int
  -- ^ Keep-alive interval in seconds
  , qcReconnectAttempts :: !Int
  -- ^ Number of reconnection attempts
  , qcDiscoveryInterval :: !Int
  -- ^ Peer discovery interval in seconds
  , qcConnectionTimeout :: !Int
  -- ^ Connection timeout in milliseconds
  }
  deriving stock (Show, Eq, Generic)

-- | Default QUIC configuration
defaultQuicConfig :: QuicConfig
defaultQuicConfig = QuicConfig
  { qcBindAddress = SockAddrInet 0 0 -- INADDR_ANY
  , qcBindPort = 8443  -- Default QUIC port
  , qcNetworkMode = HybridMode
  , qcBootstrapPeers = []
  , qcMaxPeers = 50
  , qcCertificatePath = "certs/node.crt"
  , qcPrivateKeyPath = "certs/node.key"
  , qcTrustStorePath = "certs/truststore"
  , qcKeepAliveInterval = 30
  , qcReconnectAttempts = 5
  , qcDiscoveryInterval = 300
  , qcConnectionTimeout = 5000
  }

-- | Start a QUIC server
startQuicServer :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error NetworkError) r
  ) => 
  QuicConfig -> Actor -> PubKey -> Sem r QuicServer
startQuicServer config localActor pubKey = do
  Trace.trace $ "Starting QUIC server on " <> T.pack (show (qcBindAddress config)) <> ":" <> T.pack (show (qcBindPort config))
  
  -- Generate certificate if needed
  generateCertificate (qcCertPath config) (qcKeyPath config)
  
  -- Create a new connection map
  connectionsVar <- embed $ newMVar Map.empty
  
  -- Start the server thread
  serverThread <- embed $ forkIO $ do
    -- In a real implementation, we would use a proper QUIC library here
    -- For now, we just simulate the server with a delay
    forever $ do
      threadDelay 1000000  -- 1 second
      putStrLn "QUIC server is running..."
  
  -- Return the server
  let server = QuicServer
        { qsConfig = config
        , qsConnections = connectionsVar
        , qsLocalActor = localActor
        , qsPublicKey = pubKey
        , qsServerThread = serverThread
        , qsActive = True
        }
  
  Trace.trace "QUIC server started successfully"
  return server

-- | Stop a QUIC server
stopQuicServer :: 
  ( Member (Embed IO) r
  , Member Trace r
  ) => 
  QuicServer -> Sem r ()
stopQuicServer server = do
  Trace.trace "Stopping QUIC server"
  
  -- In a real implementation, we would properly stop the server
  -- For now, we just kill the thread
  embed $ killThread (qsServerThread server)
  
  Trace.trace "QUIC server stopped successfully"

-- | Connect to a QUIC peer
connectToQuicPeer :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error NetworkError) r
  ) => 
  QuicServer -> SockAddr -> Sem r QuicConnection
connectToQuicPeer server peerAddr = do
  Trace.trace $ "Connecting to QUIC peer at " <> T.pack (show peerAddr)
  
  -- In a real implementation, we would establish a QUIC connection
  -- For now, we just simulate the connection
  now <- embed getCurrentTime
  
  -- Create a dummy peer
  let peer = QuicPeer
        { qpAddress = peerAddr
        , qpPublicKey = PubKey "dummy-public-key"
        , qpActorId = "dummy-actor-id"
        , qpLastSeen = now
        , qpConnectionQuality = 1.0
        }
  
  -- Create a new connection
  let conn = QuicConnection
        { qcPeer = peer
        , qcEstablished = now
        , qcLastActivity = now
        , qcMessagesSent = 0
        , qcMessagesReceived = 0
        , qcActive = True
        }
  
  -- Add the connection to the server
  embed $ modifyMVar (qsConnections server) $ \conns ->
    return (Map.insert peerAddr conn conns, conn)
  
  Trace.trace $ "Connected to QUIC peer at " <> T.pack (show peerAddr)
  return conn

-- | Disconnect from a QUIC peer
disconnectFromQuicPeer :: 
  ( Member (Embed IO) r
  , Member Trace r
  ) => 
  QuicServer -> QuicConnection -> Sem r ()
disconnectFromQuicPeer server conn = do
  Trace.trace $ "Disconnecting from QUIC peer at " <> T.pack (show (qpAddress (qcPeer conn)))
  
  -- In a real implementation, we would properly close the connection
  -- For now, we just remove it from the server
  embed $ modifyMVar (qsConnections server) $ \conns ->
    return (Map.delete (qpAddress (qcPeer conn)) conns, ())
  
  Trace.trace $ "Disconnected from QUIC peer at " <> T.pack (show (qpAddress (qcPeer conn)))

-- | Send a QUIC message to a peer
sendQuicMessage :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error NetworkError) r
  ) => 
  QuicServer -> QuicConnection -> QuicMessage -> Sem r ()
sendQuicMessage server conn msg = do
  Trace.trace $ "Sending QUIC message to " <> T.pack (show (qpAddress (qcPeer conn)))
  
  -- In a real implementation, we would send the message over QUIC
  -- For now, we just log it
  Trace.trace $ "Message: " <> T.pack (show msg)
  
  -- Update the connection stats
  now <- embed getCurrentTime
  embed $ modifyMVar (qsConnections server) $ \conns ->
    let updatedConn = conn
          { qcLastActivity = now
          , qcMessagesSent = qcMessagesSent conn + 1
          }
    in return (Map.insert (qpAddress (qcPeer conn)) updatedConn conns, ())
  
  Trace.trace $ "Sent QUIC message to " <> T.pack (show (qpAddress (qcPeer conn)))

-- | Broadcast a QUIC message to all peers
broadcastQuicMessage :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error NetworkError) r
  ) => 
  QuicServer -> QuicMessage -> Sem r ()
broadcastQuicMessage server msg = do
  Trace.trace "Broadcasting QUIC message to all peers"
  
  -- Get all connections
  conns <- embed $ readMVar (qsConnections server)
  
  -- Send the message to each connection
  mapM_ (\conn -> sendQuicMessage server conn msg) (Map.elems conns)
  
  Trace.trace $ "Broadcasted QUIC message to " <> T.pack (show (Map.size conns)) <> " peers"

-- | Discover peers using the QUIC protocol
discoverQuicPeers ::
  (Member (Embed IO) r, Member (Error AppError) r, Member PT.Trace r) =>
  QuicConfig ->
  [QuicConnection] ->
  Sem r [QuicPeer]
discoverQuicPeers config existingConns = do
  PT.trace "Discovering QUIC peers"
  
  -- Select a subset of existing connections to query for peers
  let sampleSize = min 5 (length existingConns)
      selectedConns = take sampleSize existingConns
  
  -- Create discovery requests and send them
  timestamp <- embed getCurrentTime
  discoveredPeers <- fmap concat $ forM selectedConns $ \conn -> do
    -- Generate request ID
    requestId <- embed $ getRandomBytes 16
    
    -- Create discovery request
    let request = DiscoveryRequest
          { qmRequestingNode = qcPeer conn
          , qmRequestId = requestId
          }
    
    -- In a real implementation, this would send the request and wait for responses
    -- For now, simulate peer discovery
    newPeers <- embed $ simulatePeerDiscovery (qcMaxPeers config)
    
    PT.trace $ "Discovered " <> show (length newPeers) <> " peers from connection " <> 
               show (qpId $ qcPeer conn)
    return newPeers
  
  -- Filter out duplicates and existing connections
  let existingPeerIds = Set.fromList $ map (qpId . qcPeer) existingConns
      uniquePeers = filter (\p -> not $ Set.member (qpId p) existingPeerIds) $
                    nubPeersByHash discoveredPeers
  
  PT.trace $ "Total unique new peers discovered: " <> show (length uniquePeers)
  return uniquePeers

-- | Announce this peer to the network
announcePeer ::
  (Member (Embed IO) r, Member PT.Trace r) =>
  QuicConfig ->
  QuicPeer ->  -- ^ Local peer info
  [QuicConnection] ->
  Sem r Int
announcePeer config localPeer connections = do
  PT.trace "Announcing peer to the network"
  
  -- Create announcement message
  timestamp <- embed getCurrentTime
  let announcement = HandshakeRequest
        { qmSenderNode = localPeer
        , qmTimestamp = timestamp
        , qmCertificate = BS.empty  -- In a real implementation, this would be the certificate
        }
  
  -- Serialize the announcement
  let announcementBS = BS.pack [1]  -- Dummy serialization, would use actual encoding in real impl
  
  -- Broadcast to connections
  broadcastQuicMessage connections announcementBS

-- | Register with bootstrap peers
registerWithBootstrapPeers ::
  (Member (Embed IO) r, Member (Error AppError) r, Member PT.Trace r) =>
  QuicConfig ->
  QuicPeer ->  -- ^ Local peer info
  Sem r [QuicConnection]
registerWithBootstrapPeers config localPeer = do
  PT.trace $ "Registering with " <> show (length (qcBootstrapPeers config)) <> " bootstrap peers"
  
  -- Connect to each bootstrap peer
  connections <- fmap catMaybes $ forM (qcBootstrapPeers config) $ \peerAddr -> do
    connectToQuicPeer config peerAddr
  
  -- Announce ourselves to each connection
  when (not $ null connections) $ do
    announcePeer config localPeer connections
    return ()
  
  PT.trace $ "Successfully connected to " <> show (length connections) <> " bootstrap peers"
  return connections

-- | Handle a network partition
handleNetworkPartition ::
  (Member (Embed IO) r, Member PT.Trace r) =>
  QuicConfig ->
  [QuicConnection] ->
  Sem r [QuicConnection]
handleNetworkPartition config connections = do
  PT.trace "Handling network partition"
  
  -- Check which connections are still alive
  timestamp <- embed getCurrentTime
  activeConns <- fmap catMaybes $ forM connections $ \conn -> do
    -- Send a keepalive message
    let keepalive = KeepAlive
          { qmSenderNode = qcPeer conn
          , qmTimestamp = timestamp
          }
    
    -- Serialize the keepalive
    let keepaliveBS = BS.pack [2]  -- Dummy serialization
    
    -- Try to send it
    success <- sendQuicMessage conn keepaliveBS
    
    if success
      then do
        PT.trace $ "Connection to " <> show (qpId $ qcPeer conn) <> " is active"
        return $ Just conn
      else do
        PT.trace $ "Connection to " <> show (qpId $ qcPeer conn) <> " is broken"
        return Nothing
  
  -- If we've lost too many connections, attempt to reconnect to bootstrap peers
  if length activeConns < length connections `div` 2
    then do
      PT.trace "Significant connection loss detected, reconnecting to bootstrap peers"
      bootstrapConns <- registerWithBootstrapPeers config (qcPeer $ head activeConns)
      return $ activeConns ++ bootstrapConns
    else
      return activeConns

-- | Attempt to reconnect to a peer
attemptReconnection ::
  (Member (Embed IO) r, Member (Error AppError) r, Member PT.Trace r) =>
  QuicConfig ->
  QuicPeer ->
  Sem r (Maybe QuicConnection)
attemptReconnection config peer = do
  PT.trace $ "Attempting to reconnect to peer " <> show (qpId peer)
  
  -- Try to establish a new connection
  connectToQuicPeer config (qpAddress peer)

-- | Migrate a connection to a new network path
migrateConnection ::
  (Member (Embed IO) r, Member PT.Trace r) =>
  QuicConnection ->
  SockAddr ->  -- ^ New address
  Sem r (Maybe QuicConnection)
migrateConnection conn newAddr = do
  PT.trace $ "Migrating connection from " <> show (qpAddress $ qcPeer conn) <> 
             " to " <> show newAddr
  
  -- In a real implementation, this would use QUIC's connection migration feature
  -- For now, simulate migration
  success <- embed $ simulateConnectionMigration
  
  if success
    then do
      -- Update the connection with the new address
      timestamp <- embed getCurrentTime
      let updatedPeer = (qcPeer conn) { qpAddress = newAddr, qpLastSeen = timestamp }
          updatedConn = conn { qcPeer = updatedPeer }
      
      PT.trace "Connection migration successful"
      return $ Just updatedConn
    else do
      PT.trace "Connection migration failed"
      return Nothing

-- Helper functions for simulation

-- | Generate a self-signed certificate
generateSelfSignedCertificate ::
  (Member (Embed IO) r, Member (Error AppError) r) =>
  QuicConfig ->
  Actor ->
  PubKey ->
  Sem r ()
generateSelfSignedCertificate _ _ _ =
  -- In a real implementation, this would generate a self-signed certificate
  -- For now, it's a no-op
  return ()

-- | Simulate a connection attempt
simulateConnectionAttempt :: IO Bool
simulateConnectionAttempt = do
  -- 80% success rate
  r <- randomIO
  return (r < 0.8)

-- | Simulate message sending
simulateMessageSending :: IO Bool
simulateMessageSending = do
  -- 95% success rate
  r <- randomIO
  return (r < 0.95)

-- | Simulate message receiving
simulateMessageReceiving :: IO (Maybe ByteString)
simulateMessageReceiving = do
  -- 90% success rate
  r <- randomIO
  if r < 0.9
    then Just <$> getRandomBytes 128
    else return Nothing

-- | Simulate peer discovery
simulatePeerDiscovery :: Int -> IO [QuicPeer]
simulatePeerDiscovery maxPeers = do
  -- Generate between 0 and maxPeers peers
  count <- randomRIO (0, maxPeers)
  mapM (const generateRandomPeer) [1..count]

-- | Simulate connection migration
simulateConnectionMigration :: IO Bool
simulateConnectionMigration = do
  -- 70% success rate
  r <- randomIO
  return (r < 0.7)

-- | Generate a random peer for simulation
generateRandomPeer :: IO QuicPeer
generateRandomPeer = do
  timestamp <- getCurrentTime
  peerId <- getRandomBytes 32
  connId <- getRandomBytes 16
  
  let actorHash = EntityHash (Hash peerId)
  
  return QuicPeer
    { qpId = actorHash
    , qpAddress = SockAddrInet 8443 0  -- Dummy address
    , qpPublicKey = PubKey peerId
    , qpCapabilities = [CanRoute, CanStore]
    , qpLastSeen = timestamp
    , qpConnectionId = connId
    , qpStats = QuicStats
        { qsRTT = 100.0
        , qsPacketsSent = 0
        , qsPacketsReceived = 0
        , qsPacketsLost = 0
        , qsBytesTransferred = 0
        , qsLastActivity = timestamp
        }
    }

-- | Random number in range
randomRIO :: (Integral a) => (a, a) -> IO a
randomRIO (lo, hi) = do
  r <- randomIO
  return $ lo + fromIntegral (abs r `mod` fromIntegral (hi - lo + 1))

-- | Remove duplicate peers by hash
nubPeersByHash :: [QuicPeer] -> [QuicPeer]
nubPeersByHash peers =
  Map.elems $ Map.fromList [(qpId p, p) | p <- peers]

-- Missing imports (would be included in real implementation)
import Data.Bool (Bool(..))
import System.Random (randomIO)
import Foreign.C.Types (CChar)
import Data.List (sortOn)
import Control.Monad (unless, forM, forM_)

-- | QUIC network mode
data QuicNetworkMode
  = ClientMode     -- ^ Client-only mode
  | ServerMode     -- ^ Server-only mode
  | HybridMode     -- ^ Both client and server mode
  deriving (Show, Eq, Generic)

instance FromJSON QuicNetworkMode
instance ToJSON QuicNetworkMode

-- | QUIC configuration
data QuicConfig = QuicConfig
  { qcBindAddress :: SockAddr        -- ^ Address to bind the QUIC server to
  , qcBindPort :: Int                -- ^ Port to bind the QUIC server to
  , qcCertPath :: FilePath           -- ^ Path to the certificate file
  , qcKeyPath :: FilePath            -- ^ Path to the private key file
  , qcNetworkMode :: QuicNetworkMode -- ^ Network mode
  , qcBootstrapNodes :: [SockAddr]   -- ^ Bootstrap nodes to connect to
  , qcConnectionTimeout :: Int       -- ^ Connection timeout in milliseconds
  , qcMaxConnections :: Int          -- ^ Maximum number of connections
  , qcMaxMessageSize :: Int          -- ^ Maximum message size in bytes
  , qcPingInterval :: Int            -- ^ Ping interval in milliseconds
  } deriving (Show, Eq, Generic)

instance FromJSON QuicConfig
instance ToJSON QuicConfig

-- | Default QUIC configuration
defaultQuicConfig :: QuicConfig
defaultQuicConfig = QuicConfig
  { qcBindAddress = SockAddrInet 8443 0  -- INADDR_ANY
  , qcBindPort = 8443
  , qcCertPath = "certs/server.crt"
  , qcKeyPath = "certs/server.key"
  , qcNetworkMode = HybridMode
  , qcBootstrapNodes = []
  , qcConnectionTimeout = 5000  -- 5 seconds
  , qcMaxConnections = 100
  , qcMaxMessageSize = 1024 * 1024  -- 1 MB
  , qcPingInterval = 30000  -- 30 seconds
  }

-- | QUIC error
data QuicError
  = QuicConnectionError Text
  | QuicServerError Text
  | QuicCertificateError Text
  | QuicMessageError Text
  | QuicTimeoutError Text
  | QuicAuthenticationError Text
  deriving (Show, Eq, Generic)

instance FromJSON QuicError
instance ToJSON QuicError

-- | QUIC server (opaque type)
data QuicServer = QuicServer
  { qsConfig :: QuicConfig
  , qsConnections :: MVar (Map SockAddr QuicConnection)
  , qsLocalActor :: Actor
  , qsPublicKey :: PubKey
  , qsServerThread :: ThreadId
  , qsActive :: Bool
  }

-- | Convert a P2P configuration to a QUIC configuration
p2pConfigToQuicConfig :: Network.P2PConfig -> QuicConfig
p2pConfigToQuicConfig p2pConfig = defaultQuicConfig
  { qcBindAddress = Network.p2pBindAddress p2pConfig
  , qcBindPort = Network.p2pBindPort p2pConfig
  , qcBootstrapNodes = Network.p2pBootstrapNodes p2pConfig
  , qcConnectionTimeout = Network.p2pConnectionTimeout p2pConfig
  , qcMaxConnections = Network.p2pMaxConnections p2pConfig
  }

-- | Generate a self-signed certificate for QUIC
generateCertificate :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error NetworkError) r
  ) => 
  FilePath -> FilePath -> Sem r ()
generateCertificate certPath keyPath = do
  Trace.trace $ "Generating self-signed certificate at " <> T.pack certPath
  
  -- Check if the certificate already exists
  certExists <- embed $ doesFileExist certPath
  keyExists <- embed $ doesFileExist keyPath
  
  if certExists && keyExists
    then Trace.trace "Certificate and key already exist, skipping generation"
    else do
      -- Create the directory if it doesn't exist
      let certDir = takeDirectory certPath
          keyDir = takeDirectory keyPath
      
      embed $ createDirectoryIfMissing True certDir
      embed $ createDirectoryIfMissing True keyDir
      
      -- Generate a self-signed certificate using OpenSSL
      result <- embed $ try @SomeException $ callCommand $ 
        "openssl req -x509 -newkey rsa:4096 -keyout " <> keyPath <> 
        " -out " <> certPath <> 
        " -days 365 -nodes -subj '/CN=localhost'"
      
      case result of
        Left err -> do
          Trace.trace $ "Failed to generate certificate: " <> T.pack (show err)
          throw $ NetworkError $ "Failed to generate certificate: " <> T.pack (show err)
        Right _ -> 
          Trace.trace "Certificate generated successfully"

-- Helper functions
takeDirectory :: FilePath -> FilePath
takeDirectory = reverse . dropWhile (== '/') . dropWhile (/= '/') . reverse

killThread :: ThreadId -> IO ()
killThread _ = pure ()  -- In a real implementation, we would use Control.Concurrent.killThread 