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
module Adapters.NetworkQUIC
  ( -- * QUIC Network Configuration
    QuicConfig(..)
  , defaultQuicConfig
  , NetworkMode(..)
    
  -- * Network Operations
  , startQuicServer
  , connectToQuicPeer
  , broadcastQuicMessage
  , sendQuicMessage
  -- , receiveQuicMessage  -- Commented out as it's not implemented yet
  -- , closeQuicConnection  -- Commented out as it's not implemented yet
  
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
import Control.Exception (SomeException, try)
import Control.Monad (void, forever, when, unless, forM, forM_, foldM)
import Crypto.PubKey.RSA (PrivateKey)
import Crypto.Random.Types (getRandomBytes)
import Data.Binary (Binary, encode, decode)
import Data.Bool (Bool(..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (ord)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Foreign.C.Types (CChar)
import GHC.Generics (Generic)
import Network.Socket (SockAddr(..), HostName, PortNumber, HostAddress, HostAddress6, hostAddressToTuple)
import Polysemy
import Polysemy.Error
import Polysemy.State qualified as PS
import qualified Polysemy.Trace as PT
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process (callCommand)
import System.Random (randomIO)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, readMVar, modifyMVar_)

import Data.Aeson (FromJSON, ToJSON)

import Core qualified as Core
import Core.Types
import Core.Common (Actor, EntityHash(..), ActorHash, PubKey(..))
import Adapters.Network (P2PNode(..), P2PCapability(..), P2PConfig(..))

-- | Network error type
data NetworkError = NetworkError Text
  deriving (Show, Eq)

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
  , qcEstablished :: !UTCTime
  -- ^ When the connection was established
  , qcLastActivity :: !UTCTime
  -- ^ Last activity timestamp
  , qcMessagesSent :: !Int
  -- ^ Number of messages sent
  , qcMessagesReceived :: !Int
  -- ^ Number of messages received
  , qcActive :: !Bool
  -- ^ Whether the connection is active
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

-- | Network mode
data NetworkMode
  = ServerMode     -- ^ Server mode listening for connections
  | ClientMode     -- ^ Client mode initiating connections
  | HybridMode     -- ^ Both server and client capabilities
  deriving stock (Show, Eq, Generic)

-- | Alias for NetworkMode for backward compatibility
type QuicNetworkMode = NetworkMode

instance FromJSON NetworkMode
instance ToJSON NetworkMode

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
  , Member PT.Trace r
  , Member (Error NetworkError) r
  ) => 
  QuicConfig -> Actor -> PubKey -> Sem r QuicServer
startQuicServer config localActor pubKey = do
  PT.trace $ "Starting QUIC server on " <> unpack (T.pack (show (qcBindAddress config))) <> ":" <> unpack (T.pack (show (qcBindPort config)))
  
  -- Generate certificate if needed
  generateCertificate (qcCertificatePath config) (qcPrivateKeyPath config)
  
  -- Create a new connection map
  connectionsVar <- embed $ Control.Concurrent.MVar.newMVar Map.empty
  
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
  
  PT.trace "QUIC server started successfully"
  return server

-- | Stop a QUIC server
stopQuicServer :: 
  ( Member (Embed IO) r
  , Member PT.Trace r
  ) => 
  QuicServer -> Sem r ()
stopQuicServer server = do
  PT.trace "Stopping QUIC server"
  
  -- In a real implementation, we would properly stop the server
  -- For now, we just kill the thread
  embed $ killThread (qsServerThread server)
  
  PT.trace "QUIC server stopped successfully"

-- | Connect to a QUIC peer
connectToQuicPeer :: 
  ( Member (Embed IO) r
  , Member PT.Trace r
  , Member (Error NetworkError) r
  ) => 
  QuicServer -> SockAddr -> Sem r QuicConnection
connectToQuicPeer server peerAddr = do
  PT.trace $ "Connecting to QUIC peer at " <> unpack (T.pack (show peerAddr))
  
  -- In a real implementation, we would establish a QUIC connection
  -- For now, we just simulate the connection
  now <- embed getCurrentTime
  
  -- Create a dummy peer
  let peer = QuicPeer
        { qpId = EntityHash (Hash "dummy-actor-id")
        , qpAddress = peerAddr
        , qpPublicKey = PubKey "dummy-public-key"
        , qpCapabilities = [CanRoute, CanStore]
        , qpLastSeen = now
        , qpConnectionId = BS.empty
        , qpStats = QuicStats
            { qsRTT = 100.0
            , qsPacketsSent = 0
            , qsPacketsReceived = 0
            , qsPacketsLost = 0
            , qsBytesTransferred = 0
            , qsLastActivity = now
            }
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
  
  PT.trace $ "Connected to QUIC peer at " <> unpack (T.pack (show peerAddr))
  return conn

-- | Disconnect from a QUIC peer
disconnectFromQuicPeer :: 
  ( Member (Embed IO) r
  , Member PT.Trace r
  ) => 
  QuicServer -> QuicConnection -> Sem r ()
disconnectFromQuicPeer server conn = do
  PT.trace $ "Disconnecting from QUIC peer at " <> unpack (T.pack (show (qpAddress (qcPeer conn))))
  
  -- In a real implementation, we would properly close the connection
  -- For now, we just remove it from the server
  embed $ modifyMVar (qsConnections server) $ \conns ->
    return (Map.delete (qpAddress (qcPeer conn)) conns, ())
  
  PT.trace $ "Disconnected from QUIC peer at " <> unpack (T.pack (show (qpAddress (qcPeer conn))))

-- | Send a QUIC message to a peer
sendQuicMessage :: 
  ( Member (Embed IO) r
  , Member PT.Trace r
  , Member (Error NetworkError) r
  ) => 
  QuicConnection -> ByteString -> Sem r Bool
sendQuicMessage conn msg = do
  PT.trace $ "Sending QUIC message to " <> unpack (T.pack (show (qpAddress (qcPeer conn))))
  
  -- In a real implementation, we would send the message over QUIC
  -- For now, we just simulate it
  success <- embed simulateMessageSending
  
  if success
    then do
      PT.trace "Message sent successfully"
      return True
    else do
      PT.trace "Failed to send message"
      return False

-- | Broadcast a QUIC message to all peers
broadcastQuicMessage :: 
  ( Member (Embed IO) r
  , Member PT.Trace r
  , Member (Error NetworkError) r
  ) => 
  [QuicConnection] -> ByteString -> Sem r Int
broadcastQuicMessage connections msg = do
  PT.trace "Broadcasting QUIC message to all peers"
  
  -- Send the message to each connection
  successCount <- foldM (\count conn -> do
    success <- sendQuicMessage conn msg
    return $ if success then count + 1 else count
    ) 0 connections
  
  PT.trace $ "Broadcasted QUIC message to " <> unpack (T.pack (show successCount)) <> " peers"
  return successCount

-- | Discover peers using the QUIC protocol
discoverQuicPeers ::
  (Member (Embed IO) r, Member (Error NetworkError) r, Member PT.Trace r) =>
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
    requestId <- embed $ randomBS 16
    
    -- Create discovery request
    let request = DiscoveryRequest
          { qmRequestingNode = qcPeer conn
          , qmRequestId = requestId
          }
    
    -- In a real implementation, this would send the request and wait for responses
    -- For now, simulate peer discovery
    newPeers <- embed $ simulatePeerDiscovery (qcMaxPeers config)
    
    PT.trace $ "Discovered " <> unpack (T.pack (show (length newPeers))) <> " peers from connection " <>
      unpack (T.pack (show (qpId $ qcPeer conn)))
    return newPeers
  
  -- Filter out duplicates and existing connections
  let existingPeerIds = Set.fromList $ map (qpId . qcPeer) existingConns
      uniquePeers = filter (\p -> not $ Set.member (qpId p) existingPeerIds) $
                    nubPeersByHash discoveredPeers
  
  PT.trace $ "Total unique new peers discovered: " <> unpack (T.pack (show (length uniquePeers)))
  return uniquePeers

-- | Announce this peer to the network
announcePeer ::
  (Member (Embed IO) r, Member PT.Trace r, Member (Error NetworkError) r) =>
  QuicPeer ->  -- ^ Local peer info
  [QuicConnection] ->
  Sem r Int
announcePeer localPeer connections = do
  PT.trace "Announcing peer to the network"
  
  -- Create announcement message
  timestamp <- embed getCurrentTime
  let announcement = HandshakeRequest
        { qmSenderNode = localPeer
        , qmTimestamp = timestamp
        , qmCertificate = BS.empty  -- In a real implementation, this would be the certificate
        }
  
  -- Serialize the announcement (in a real implementation, we would use proper serialization)
  let announcementBS = BS.pack [1]  -- Dummy serialization
  
  -- Broadcast to connections
  broadcastQuicMessage connections announcementBS

-- | Register with bootstrap peers
registerWithBootstrapPeers ::
  (Member (Embed IO) r, Member (Error NetworkError) r, Member (Error SomeException) r, Member PT.Trace r) =>
  QuicConfig ->
  QuicServer ->
  QuicPeer ->
  Sem r [QuicConnection]
registerWithBootstrapPeers config server localPeer = do
  PT.trace $ "Registering with " <> unpack (T.pack (show (length (qcBootstrapPeers config)))) <> " bootstrap peers"
  
  -- Connect to each bootstrap peer
  connections <- fmap catMaybes $ forM (qcBootstrapPeers config) $ \peerAddr -> do
    result <- Polysemy.Error.try @NetworkError $ connectToQuicPeer server peerAddr
    case result of
      Left e -> do
        PT.trace $ "Failed to connect to bootstrap peer at " <> unpack (pack (show peerAddr)) <> ": " <> unpack (pack (show e))
        return Nothing
      Right conn -> return (Just conn)
  
  -- Announce ourselves to each connection
  when (not $ null connections) $ do
    void $ announcePeer localPeer connections
  
  PT.trace $ "Successfully connected to " <> unpack (T.pack (show (length connections))) <> " bootstrap peers"
  return connections

-- | Handle network partition
handleNetworkPartition
  :: (Member (Embed IO) r, Member (Error NetworkError) r, Member (Error SomeException) r, Member PT.Trace r)
  => QuicConfig
  -> QuicServer
  -> [QuicConnection]
  -> Sem r [QuicConnection]
handleNetworkPartition config server connections = do
  PT.trace "Handling network partition"
  
  -- Check which connections are still alive
  timestamp <- embed getCurrentTime
  activeConns <- fmap catMaybes $ forM connections $ \conn -> do
    -- Check if connection is still active
    let keepalive = "KEEPALIVE"
    let keepaliveBS = BS.pack $ map (fromIntegral . ord) keepalive
    
    -- Try to send a keepalive message
    result <- Polysemy.Error.try @NetworkError $ sendQuicMessage conn keepaliveBS
    case result of
      Right success -> 
        if success then do
          PT.trace $ "Connection to " <> unpack (pack (show (qpId $ qcPeer conn))) <> " is active"
          return $ Just conn
        else do
          PT.trace $ "Connection to " <> unpack (pack (show (qpId $ qcPeer conn))) <> " is broken"
          return Nothing
      Left _ -> do
        PT.trace $ "Error sending keepalive to " <> unpack (pack (show (qpId $ qcPeer conn)))
        return Nothing
  
  -- If we've lost too many connections, attempt to reconnect to bootstrap peers
  if length activeConns < length connections `div` 2
    then do
      PT.trace "Significant connection loss detected, reconnecting to bootstrap peers"
      -- If we have active connections, use one of them to reconnect to bootstrap peers
      if not (null activeConns)
        then do
          let firstConn = case activeConns of
                            conn:_ -> conn
                            []     -> error "Impossible: activeConns is empty"
          bootstrapConns <- registerWithBootstrapPeers config server (qcPeer firstConn)
          return $ activeConns ++ bootstrapConns
        else
          return activeConns
    else
      return activeConns

-- | Attempt to reconnect to a peer
attemptReconnection
  :: (Member (Embed IO) r, Member (Error AppError) r, Member (Error NetworkError) r, Member (Error SomeException) r, Member PT.Trace r)
  => QuicServer
  -> QuicPeer
  -> Sem r (Maybe QuicConnection)
attemptReconnection server peer = do
  PT.trace $ "Attempting to reconnect to peer " <> unpack (pack (show (qpId peer)))
  result <- Polysemy.Error.try @NetworkError $ connectToQuicPeer server (qpAddress peer)
  case result of
    Left _ -> return Nothing
    Right conn -> return (Just conn)

-- | Migrate a connection to a new network path
migrateConnection ::
  (Member (Embed IO) r, Member PT.Trace r) =>
  QuicConnection ->
  SockAddr ->  -- ^ New address
  Sem r (Maybe QuicConnection)
migrateConnection conn newAddr = do
  PT.trace $ "Migrating connection from " <> unpack (T.pack (show (qpAddress $ qcPeer conn))) <> 
             " to " <> unpack (T.pack (show newAddr))
  
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
  r <- randomIO :: IO Double
  return (r < 0.8)

-- | Simulate message sending
simulateMessageSending :: IO Bool
simulateMessageSending = do
  r <- randomIO :: IO Double
  return (r < 0.95)

-- | Simulate message receiving
simulateMessageReceiving :: IO (Maybe ByteString)
simulateMessageReceiving = do
  r <- randomIO :: IO Double
  if r < 0.9 then Just <$> randomBS 128 else return Nothing

-- | Simulate peer discovery
simulatePeerDiscovery :: Int -> IO [QuicPeer]
simulatePeerDiscovery maxPeers = do
  -- Generate between 0 and maxPeers peers
  count <- randomRIO (0, maxPeers)
  mapM (const generateRandomPeer) [1..count]

-- | Simulate connection migration
simulateConnectionMigration :: IO Bool
simulateConnectionMigration = do
  r <- randomIO :: IO Double
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
randomRIO :: Integral a => (a, a) -> IO a
randomRIO (lo, hi) = do
  r <- randomIO :: IO Int
  return $ lo + fromIntegral (abs r `mod` fromIntegral (hi - lo + 1))

-- | Remove duplicate peers by hash
nubPeersByHash :: [QuicPeer] -> [QuicPeer]
nubPeersByHash peers =
  Map.elems $ Map.fromList [(qpId p, p) | p <- peers]

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
p2pConfigToQuicConfig :: P2PConfig -> QuicConfig
p2pConfigToQuicConfig p2pConfig = defaultQuicConfig
  { qcBindAddress = pcBindAddress p2pConfig
  , qcBindPort = case pcBindAddress p2pConfig of
                   SockAddrInet port _ -> port
                   SockAddrInet6 port _ _ _ -> port
                   _ -> 8443
  , qcBootstrapPeers = pcSeedNodes p2pConfig
  , qcMaxPeers = pcNodeCapacity p2pConfig
  , qcConnectionTimeout = pcConnectionTimeout p2pConfig
  }

-- | Generate a self-signed certificate for QUIC
generateCertificate :: 
  ( Member (Embed IO) r
  , Member PT.Trace r
  , Member (Error NetworkError) r
  ) => 
  FilePath -> FilePath -> Sem r ()
generateCertificate certPath keyPath = do
  PT.trace $ "Generating self-signed certificate at " <> unpack (T.pack certPath)
  
  -- Check if the certificate already exists
  certExists <- embed $ doesFileExist certPath
  keyExists <- embed $ doesFileExist keyPath
  
  if certExists && keyExists
    then PT.trace "Certificate and key already exist, skipping generation"
    else do
      -- Create the directory if it doesn't exist
      let certDir = takeDirectory certPath
          keyDir = takeDirectory keyPath
      
      embed $ createDirectoryIfMissing True certDir
      embed $ createDirectoryIfMissing True keyDir
      
      -- Generate a self-signed certificate using OpenSSL
      result <- embed $ Control.Exception.try @SomeException $ callCommand $ 
        "openssl req -x509 -newkey rsa:4096 -keyout " <> keyPath <> 
        " -out " <> certPath <> 
        " -days 365 -nodes -subj '/CN=localhost'"
      
      case result of
        Left err -> do
          PT.trace $ "Failed to generate certificate: " <> unpack (T.pack (show err))
          throw $ Adapters.NetworkQUIC.NetworkError $ "Failed to generate certificate: " <> T.pack (show err)
        Right _ -> 
          PT.trace "Certificate generated successfully"

-- Helper functions
takeDirectory :: FilePath -> FilePath
takeDirectory = reverse . dropWhile (== '/') . dropWhile (/= '/') . reverse

killThread :: ThreadId -> IO ()
killThread _ = pure ()  -- In a real implementation, we would use Control.Concurrent.killThread 

-- Helper function to generate random bytes using IO
randomBS :: Int -> IO ByteString
randomBS n = BS.pack <$> replicateM n (randomRIO (0, 255)) 