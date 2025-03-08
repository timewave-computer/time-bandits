{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Adapters.Network.QUICAdapter
Description : QUIC-based implementation of the NetworkAdapter
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides a QUIC-based implementation of the NetworkAdapter interface
for efficient, secure P2P communication in real-world networks.
-}
module Adapters.Network.QUICAdapter
  ( -- * Adapter Creation
    createQUICAdapter
  , QUICAdapter
    
    -- * Configuration
  , QUICConfig(..)
  , defaultQUICConfig
  
    -- * TLS Support
  , TLSConfig(..)
  , generateSelfSignedCert
  ) where

import Control.Concurrent (MVar, ThreadId, forkIO, killThread, newMVar, putMVar, readMVar, modifyMVar, modifyMVar_, takeMVar, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (catch, try, SomeException)
import Control.Monad (forM, forM_, forever, void, when)
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as PKCS15
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Network.Socket (SockAddr(..), Socket)
import qualified Network.Socket as Socket
import System.Random (randomIO, randomRIO)

-- Import from TimeBandits modules
import Adapters.NetworkAdapter
import Types.Network
import Adapters.Network.Protocol
import Adapters.Network.Serialization
import Adapters.Network.Discovery
import Adapters.Network.TLS.Certificate (generateSelfSignedCert)

-- | QUIC-based network adapter
data QUICAdapter = QUICAdapter
  { connections :: MVar (Map PeerId QUICConnection)  -- ^ Active QUIC connections
  , subscriptions :: MVar (Map SubscriptionId (MessageType, PeerId -> ByteString -> IO ()))  -- ^ Message subscriptions
  , config :: QUICConfig  -- ^ Adapter configuration
  , localPeerId :: PeerId  -- ^ ID of this node
  , running :: MVar Bool  -- ^ Whether the adapter is running
  , backgroundThreads :: MVar [ThreadId]  -- ^ Background processing threads
  , serverSocket :: MVar (Maybe Socket)  -- ^ Listening socket for incoming connections
  , tlsContext :: MVar TLSContext  -- ^ TLS context for secure connections
  }

-- | QUIC connection to a peer
data QUICConnection = QUICConnection
  { connectionId :: UUID  -- ^ Unique connection ID
  , peerId :: PeerId  -- ^ Remote peer ID
  , peerAddress :: SockAddr  -- ^ Remote peer address
  , socket :: Socket  -- ^ Connection socket
  , status :: MVar ConnectionStatus  -- ^ Connection status
  , incoming :: Chan ByteString  -- ^ Channel for incoming messages
  , outgoing :: Chan ByteString  -- ^ Channel for outgoing messages
  , lastActivity :: MVar UTCTime  -- ^ Last activity time
  }

-- | QUIC configuration
data QUICConfig = QUICConfig
  { listenAddress :: SockAddr  -- ^ Address to listen on
  , bootstrapPeers :: [SockAddr]  -- ^ Initial peers to connect to
  , maxConnections :: Int  -- ^ Maximum number of concurrent connections
  , connectionTimeout :: Int  -- ^ Connection timeout in milliseconds
  , keepAliveInterval :: Int  -- ^ Keep-alive interval in milliseconds
  , tlsConfig :: TLSConfig  -- ^ TLS configuration
  , peerDiscoveryConfig :: DiscoveryConfig  -- ^ Peer discovery configuration
  }

-- | Default QUIC configuration
defaultQUICConfig :: QUICConfig
defaultQUICConfig = QUICConfig
  { listenAddress = Socket.SockAddrInet 8443 (Socket.tupleToHostAddress (127, 0, 0, 1))
  , bootstrapPeers = []
  , maxConnections = 50
  , connectionTimeout = 5000
  , keepAliveInterval = 15000
  , tlsConfig = defaultTLSConfig
  , peerDiscoveryConfig = defaultDiscoveryConfig
  }

-- | TLS configuration
data TLSConfig = TLSConfig
  { certPath :: Maybe FilePath  -- ^ Path to TLS certificate (generated if None)
  , keyPath :: Maybe FilePath  -- ^ Path to TLS private key (generated if None)
  , caCertPath :: Maybe FilePath  -- ^ Path to CA certificate for validation
  , requireClientCert :: Bool  -- ^ Whether to require client certificates
  }

-- | Default TLS configuration
defaultTLSConfig :: TLSConfig
defaultTLSConfig = TLSConfig
  { certPath = Nothing  -- Generate a self-signed cert
  , keyPath = Nothing  -- Generate a private key
  , caCertPath = Nothing  -- No CA cert validation
  , requireClientCert = False  -- Don't require client certs
  }

-- | TLS context (certificates, keys, etc.)
data TLSContext = TLSContext
  { serverCert :: ByteString  -- ^ Server certificate
  , serverKey :: RSA.PrivateKey  -- ^ Server private key
  , clientCert :: Maybe ByteString  -- ^ Client certificate
  , clientKey :: Maybe RSA.PrivateKey  -- ^ Client private key
  , caCerts :: [ByteString]  -- ^ CA certificates
  }

-- | Connection status
data ConnectionStatus
  = Connected  -- ^ Active connection
  | Connecting  -- ^ Connection in progress
  | Disconnected  -- ^ Connection closed
  | Failed Text  -- ^ Connection failed with error
  deriving (Eq, Show)

-- | Create a new QUIC-based network adapter
createQUICAdapter :: QUICConfig -> IO QUICAdapter
createQUICAdapter config = do
  connectionsVar <- newMVar Map.empty
  subsVar <- newMVar Map.empty
  localId <- PeerId . pack . show <$> nextRandom
  runningVar <- newMVar False
  threadsVar <- newMVar []
  socketVar <- newMVar Nothing
  
  -- Set up TLS context
  tlsContext <- setupTLS (tlsConfig config)
  tlsContextVar <- newMVar tlsContext
  
  let adapter = QUICAdapter
        { connections = connectionsVar
        , subscriptions = subsVar
        , config = config
        , localPeerId = localId
        , running = runningVar
        , backgroundThreads = threadsVar
        , serverSocket = socketVar
        , tlsContext = tlsContextVar
        }
  
  return adapter

-- | Set up TLS context
setupTLS :: TLSConfig -> IO TLSContext
setupTLS tlsConfig = do
  -- Load or generate server certificate and key
  (serverCert, serverKey) <- case (certPath tlsConfig, keyPath tlsConfig) of
    (Just certFile, Just keyFile) -> do
      -- Load from file
      cert <- BS.readFile certFile
      key <- loadPrivateKey keyFile
      return (cert, key)
    _ -> do
      -- Generate self-signed
      (cert, key) <- generateSelfSignedCert "TimeBandits Server" 365
      return (cert, key)
  
  -- Load CA certificates if specified
  caCerts <- case caCertPath tlsConfig of
    Just caFile -> (: []) <$> BS.readFile caFile
    Nothing -> return []
  
  return TLSContext
    { serverCert = serverCert
    , serverKey = serverKey
    , clientCert = Nothing
    , clientKey = Nothing
    , caCerts = caCerts
    }

-- | Placeholder for loading a private key
loadPrivateKey :: FilePath -> IO RSA.PrivateKey
loadPrivateKey path = do
  -- In a real implementation, this would load and parse the key
  -- For now, we generate a dummy key
  error "Not implemented: loadPrivateKey"

-- | Start the adapter
startQUICAdapter :: QUICAdapter -> IO ()
startQUICAdapter adapter = do
  isRunning <- readMVar (running adapter)
  when (not isRunning) $ do
    -- Mark as running
    modifyMVar_ (running adapter) (const $ return True)
    
    -- Start server for incoming connections
    serverThread <- forkIO $ serverLoop adapter
    
    -- Start connection manager
    connectionManagerThread <- forkIO $ connectionManager adapter
    
    -- Start discovery manager
    discoveryCallback peerId = do
      -- Try to connect to the new peer
      _ <- connectToPeer adapter peerId
      return ()
    
    discoveryThread <- maintainPeerList adapter (peerDiscoveryConfig $ config adapter) discoveryCallback
    
    -- Record threads
    modifyMVar_ (backgroundThreads adapter) $ \threads ->
      return $ serverThread : connectionManagerThread : discoveryThread : threads

-- | Stop the adapter
stopQUICAdapter :: QUICAdapter -> IO ()
stopQUICAdapter adapter = do
  -- Mark as not running
  modifyMVar_ (running adapter) (const $ return False)
  
  -- Kill all background threads
  threads <- takeMVar (backgroundThreads adapter)
  mapM_ killThread threads
  putMVar (backgroundThreads adapter) []
  
  -- Close server socket
  modifyMVar_ (serverSocket adapter) $ \mbSocket -> do
    case mbSocket of
      Just sock -> Socket.close sock
      Nothing -> return ()
    return Nothing
  
  -- Close all connections
  connections <- takeMVar (connections adapter)
  forM_ (Map.elems connections) $ \conn -> do
    modifyMVar_ (status conn) (const $ return Disconnected)
    Socket.close (socket conn)
  putMVar (connections adapter) Map.empty

-- | Server loop for handling incoming connections
serverLoop :: QUICAdapter -> IO ()
serverLoop adapter = do
  -- Create socket
  sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  
  -- Allow reusing the address
  Socket.setSocketOption sock Socket.ReuseAddr 1
  
  -- Bind to the listen address
  Socket.bind sock (listenAddress $ config adapter)
  
  -- Listen for connections
  Socket.listen sock 5
  
  -- Store socket
  modifyMVar_ (serverSocket adapter) (const $ return $ Just sock)
  
  -- Accept loop
  forever $ do
    -- Check if we should continue running
    isRunning <- readMVar (running adapter)
    when (not isRunning) $ return ()
    
    -- Accept a connection
    (clientSock, clientAddr) <- Socket.accept sock
    
    -- Handle in a new thread
    forkIO $ handleIncomingConnection adapter clientSock clientAddr
  
  where
    handleIncomingConnection :: QUICAdapter -> Socket -> SockAddr -> IO ()
    handleIncomingConnection adapter sock addr = do
      -- Perform TLS handshake
      -- In a real implementation, this would use a proper QUIC/TLS library
      
      -- Create channels for communication
      inChan <- newChan
      outChan <- newChan
      
      -- Create connection object (with temporary peer ID)
      tempPeerId <- PeerId . pack . show <$> nextRandom
      connId <- nextRandom
      now <- getCurrentTime
      statusVar <- newMVar Connecting
      lastActivityVar <- newMVar now
      
      let conn = QUICConnection
            { connectionId = connId
            , peerId = tempPeerId
            , peerAddress = addr
            , socket = sock
            , status = statusVar
            , incoming = inChan
            , outgoing = outChan
            , lastActivity = lastActivityVar
            }
      
      -- Start reader and writer threads
      readerThread <- forkIO $ socketReader conn
      writerThread <- forkIO $ socketWriter conn
      
      -- Wait for peer identification (first message should contain peer ID)
      -- In a real implementation, this would use the QUIC/TLS authentication
      
      -- For now, we just accept the connection with the temporary ID
      modifyMVar_ (connections adapter) $ \conns ->
        return $ Map.insert tempPeerId conn conns
      
      modifyMVar_ (status conn) (const $ return Connected)

-- | Connection manager for maintaining connections
connectionManager :: QUICAdapter -> IO ()
connectionManager adapter = forever $ do
  -- Check if we should continue running
  isRunning <- readMVar (running adapter)
  when (not isRunning) $ return ()
  
  -- Clean up timed out connections
  now <- getCurrentTime
  connectionsMap <- readMVar (connections adapter)
  
  forM_ (Map.toList connectionsMap) $ \(peerId, conn) -> do
    lastAct <- readMVar (lastActivity conn)
    let timeSinceActivity = diffUTCTime now lastAct
        timeoutSecs = fromIntegral (connectionTimeout $ config adapter) / 1000
    
    when (timeSinceActivity > timeoutSecs) $ do
      -- Connection timed out, close it
      modifyMVar_ (status conn) (const $ return Disconnected)
      Socket.close (socket conn)
      
      -- Remove from connections map
      modifyMVar_ (connections adapter) $ \conns ->
        return $ Map.delete peerId conns
  
  -- Connect to bootstrap peers if needed
  when (Map.size connectionsMap < 1) $ do
    forM_ (bootstrapPeers $ config adapter) $ \addr -> do
      -- Create a peer ID from the address (in reality, we'd get this during handshake)
      let peerId = PeerId $ pack $ show addr
      connectToPeer adapter peerId
  
  -- Sleep for a bit
  threadDelay 1000000  -- 1 second

-- | Reader thread for a connection
socketReader :: QUICConnection -> IO ()
socketReader conn = forever $ do
  -- Check connection status
  status <- readMVar (status conn)
  when (status /= Connected) $ return ()
  
  -- Read data from socket
  -- In a real implementation, this would use proper QUIC/TLS APIs
  -- For now, we just simulate
  bytes <- BS.pack <$> replicateM 100 (randomRIO (0, 255))
  
  -- Update last activity
  now <- getCurrentTime
  modifyMVar_ (lastActivity conn) (const $ return now)
  
  -- Write to incoming channel
  writeChan (incoming conn) bytes
  
  -- Small delay to avoid busy-waiting
  threadDelay 100000  -- 100ms

-- | Writer thread for a connection
socketWriter :: QUICConnection -> IO ()
socketWriter conn = forever $ do
  -- Check connection status
  status <- readMVar (status conn)
  when (status /= Connected) $ return ()
  
  -- Read from outgoing channel
  bytes <- readChan (outgoing conn)
  
  -- Write to socket
  -- In a real implementation, this would use proper QUIC/TLS APIs
  -- For now, we just simulate
  
  -- Update last activity
  now <- getCurrentTime
  modifyMVar_ (lastActivity conn) (const $ return now)
  
  -- Small delay to avoid busy-waiting
  threadDelay 100000  -- 100ms

-- | Connect to a peer
connectToPeer :: QUICAdapter -> PeerId -> IO (Either NetworkError QUICConnection)
connectToPeer adapter peerId = do
  -- Check if we're already connected
  connections <- readMVar (connections adapter)
  case Map.lookup peerId connections of
    Just conn -> do
      status <- readMVar (status conn)
      if status == Connected
        then return $ Right conn
        else return $ Left $ ConnectionError "Connection not active"
    
    Nothing -> do
      -- For now, we can't connect without knowing the address
      -- In a real implementation, we would use a discovery service
      return $ Left $ PeerNotFound peerId

-- | Implements NetworkAdapter for QUICAdapter
instance NetworkAdapter QUICAdapter where
  -- Core communication functions
  broadcastMessage adapter msgType payload = do
    msgId <- newMessageId
    timestamp <- getCurrentTime
    
    let message = Message
          { messageId = msgId
          , messageType = msgType
          , sender = localPeerId adapter
          , recipient = Nothing  -- Broadcast
          , timestamp = timestamp
          , payload = payload
          , signature = Nothing
          }
    
    -- Serialize message
    let bytes = serializeMessage defaultFormat message
    
    -- Broadcast to all connected peers
    conns <- readMVar (connections adapter)
    forM_ (Map.elems conns) $ \conn -> do
      connStatus <- readMVar (status conn)
      when (connStatus == Connected) $ do
        writeChan (outgoing conn) bytes
    
    return $ Right msgId
  
  sendDirectMessage adapter targetPeerId msgType payload = do
    msgId <- newMessageId
    timestamp <- getCurrentTime
    
    let message = Message
          { messageId = msgId
          , messageType = msgType
          , sender = localPeerId adapter
          , recipient = Just targetPeerId
          , timestamp = timestamp
          , payload = payload
          , signature = Nothing
          }
    
    -- Serialize message
    let bytes = serializeMessage defaultFormat message
    
    -- Find the target connection
    conns <- readMVar (connections adapter)
    case Map.lookup targetPeerId conns of
      Nothing ->
        return $ Left $ PeerNotFound targetPeerId
      
      Just conn -> do
        connStatus <- readMVar (status conn)
        if connStatus /= Connected
          then return $ Left $ ConnectionError "Peer not connected"
          else do
            writeChan (outgoing conn) bytes
            return $ Right msgId
  
  -- Subscription management
  subscribeToMessages adapter msgType callback = do
    subId <- newSubscriptionId
    modifyMVar_ (subscriptions adapter) $ \subs ->
      return $ Map.insert subId (msgType, callback) subs
    return subId
  
  unsubscribeFromMessages adapter subId = do
    modifyMVar_ (subscriptions adapter) $ \subs ->
      return $ Map.delete subId subs
  
  -- Peer management
  discoverPeers adapter = do
    -- Use the discovery module
    discoverPeers adapter (peerDiscoveryConfig $ config adapter) []
  
  getPeerStatus adapter peerId = do
    conns <- readMVar (connections adapter)
    case Map.lookup peerId conns of
      Nothing -> return Nothing
      Just conn -> do
        connStatus <- readMVar (status conn)
        return $ Just $ case connStatus of
          Connected -> Connected
          Connecting -> Connecting
          Disconnected -> Disconnected
          Failed _ -> Unreachable
  
  -- Lifecycle management
  startAdapter = startQUICAdapter
  stopAdapter = stopQUICAdapter

-- Helper functions

-- | Helper for repeating an action n times
replicateM :: Monad m => Int -> m a -> m [a]
replicateM n action
  | n <= 0 = return []
  | otherwise = do
      x <- action
      xs <- replicateM (n-1) action
      return (x:xs) 