{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Network.Manager
Description : Network manager for Time Bandits
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module implements the network manager for Time Bandits, which handles
peer connections, discovery, and messaging using the QUIC protocol.
-}
module Network.Manager
  ( -- * Network Manager
    NetworkManager
  , NetworkConfig(..)
  , PeerConnection
  
    -- * Setup and Configuration
  , createNetworkManager
  , startNetworkManager
  , stopNetworkManager
  , withNetworkManager
  
    -- * Peer Management
  , connectToPeer
  , disconnectPeer
  , listConnectedPeers
  , findPeerByID
  
    -- * Messaging
  , sendMessage
  , broadcastMessage
  , subscribe
  , unsubscribe
  ) where

import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar, modifyMVar_, forkIO, ThreadId)
import Control.Monad (forM_, void, when, unless)
import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import GHC.Generics (Generic)
import Network.Socket (SockAddr)
import qualified Network.Socket as NS
import System.Random (randomRIO)

import Core.Log
import Core.Hashing (computeNodeScore)
import Network.Discovery.PeerDiscovery
import Network.Protocol.Version
import Data.Either (isRight)

-- | Network configuration
data NetworkConfig = NetworkConfig
  { networkNodeId :: Text          -- ^ This node's ID
  , networkListenAddr :: SockAddr  -- ^ Address to listen on
  , networkBootstrapPeers :: [PeerId]  -- ^ Initial peers to connect to
  , networkMaxPeers :: Int         -- ^ Maximum number of connected peers
  , networkLogStore :: LogStore    -- ^ Log store for network events
  , networkDiscoveryInterval :: Int -- ^ Milliseconds between peer discoveries
  , networkProtocolVersion :: ProtocolVersion  -- ^ Protocol version for this node
  }

-- | Represents a connection to a peer
data PeerConnection = PeerConnection
  { peerID :: PeerId              -- ^ Peer identifier
  , peerConnectedAt :: UTCTime    -- ^ When the connection was established
  , peerLastMessage :: UTCTime    -- ^ When the last message was received
  , peerSubscriptions :: [Text]   -- ^ Topics this peer is subscribed to
  , peerProtocolVersion :: ProtocolVersion  -- ^ Protocol version of this peer
  }

-- | Message with topic and payload
data NetworkMessage = NetworkMessage
  { messageTopic :: Text          -- ^ Topic of the message
  , messagePayload :: ByteString  -- ^ Message payload
  , messageSender :: Text         -- ^ Sender node ID
  , messageTimestamp :: UTCTime   -- ^ When the message was created
  } deriving (Show, Generic)

instance ToJSON NetworkMessage
instance FromJSON NetworkMessage

-- | Subscription callback type
type MessageCallback = NetworkMessage -> IO ()

-- | Network manager state
data NetworkManager = NetworkManager
  { networkConfig :: NetworkConfig                    -- ^ Network configuration
  , connectedPeers :: MVar (Map Text PeerConnection)  -- ^ Currently connected peers
  , subscriptions :: MVar (Map Text [MessageCallback]) -- ^ Topic subscriptions
  , discoveryConfig :: DiscoveryConfig                -- ^ Peer discovery configuration
  , discoveryThread :: MVar (Maybe ThreadId)          -- ^ Discovery background thread
  }

-- | Create a new network manager
createNetworkManager :: NetworkConfig -> IO NetworkManager
createNetworkManager config = do
  -- Initialize the MVars
  peersVar <- newMVar Map.empty
  subsVar <- newMVar Map.empty
  discoveryThreadVar <- newMVar Nothing
  
  -- Log creation
  _ <- logComponentInfo (networkLogStore config) "NetworkManager" 
      "Creating network manager"
  
  -- Create the discovery configuration
  let discConfig = DiscoveryConfig
        { bootstrapPeers = networkBootstrapPeers config
        , queryInterval = networkDiscoveryInterval config
        , peerTimeout = 60000  -- 60 seconds
        , maxKnownPeers = networkMaxPeers config * 2
        , maxPeersPerQuery = 3
        , logStore = networkLogStore config
        }
  
  -- Create and return the manager
  return NetworkManager
    { networkConfig = config
    , connectedPeers = peersVar
    , subscriptions = subsVar
    , discoveryConfig = discConfig
    , discoveryThread = discoveryThreadVar
    }

-- | Start the network manager
startNetworkManager :: NetworkManager -> IO ()
startNetworkManager manager = do
  -- Log startup
  _ <- logComponentInfo (networkLogStore $ networkConfig manager) "NetworkManager" 
      "Starting network manager"
  
  -- Start peer discovery in the background
  discoveryThread <- maintainPeerList (discoveryConfig manager) (handleNewPeer manager)
  
  -- Store the thread ID
  modifyMVar_ (discoveryThread manager) $ \_ -> return (Just discoveryThread)
  
  -- Log successful startup
  _ <- logComponentInfo (networkLogStore $ networkConfig manager) "NetworkManager" 
      "Network manager started successfully"
  
  return ()

-- | Stop the network manager
stopNetworkManager :: NetworkManager -> IO ()
stopNetworkManager manager = do
  -- Log shutdown
  _ <- logComponentInfo (networkLogStore $ networkConfig manager) "NetworkManager" 
      "Stopping network manager"
  
  -- TODO: Implement proper thread cancellation
  -- For now, we just clear the thread ID
  modifyMVar_ (discoveryThread manager) $ \_ -> return Nothing
  
  -- Disconnect all peers
  peers <- readMVar (connectedPeers manager)
  forM_ (Map.keys peers) $ \peerId ->
    disconnectPeer manager (T.pack peerId)
  
  -- Log shutdown complete
  _ <- logComponentInfo (networkLogStore $ networkConfig manager) "NetworkManager" 
      "Network manager stopped"
  
  return ()

-- | Use the network manager within a scope and ensure proper cleanup
withNetworkManager :: NetworkConfig -> (NetworkManager -> IO a) -> IO a
withNetworkManager config action = do
  manager <- createNetworkManager config
  startNetworkManager manager
  result <- action manager
  stopNetworkManager manager
  return result

-- | Connect to a peer
connectToPeer :: NetworkManager -> PeerId -> IO Bool
connectToPeer manager peer = do
  -- Log attempt
  _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
      "connect_attempt" (unPeerId peer) Nothing
  
  -- Check if we're already connected
  peers <- readMVar (connectedPeers manager)
  if Map.member (unPeerId peer) peers
    then do
      -- Already connected
      _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
          "connect_skip" (unPeerId peer) (Just "Already connected")
      return True
    else do
      -- Check if we have room for more peers
      if Map.size peers >= networkMaxPeers (networkConfig manager)
        then do
          -- Too many peers
          _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
              "connect_fail" (unPeerId peer) (Just "Too many peers")
          return False
        else do
          -- Simulate connection establishment
          -- In a real implementation, this would involve network communication
          success <- simulateConnectionEstablishment
          
          if success
            then do
              -- Simulate receiving peer's protocol version
              -- In a real implementation, this would be part of the handshake
              peerVersion <- simulateVersionExchange (networkProtocolVersion $ networkConfig manager)
              
              -- Validate protocol version compatibility
              let ourVersion = networkProtocolVersion $ networkConfig manager
                  versionCheck = validateVersion ourVersion peerVersion
              
              case versionCheck of
                Left errorMsg -> do
                  -- Incompatible version
                  _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
                      "connect_fail" (unPeerId peer) (Just $ "Incompatible protocol version: " <> errorMsg)
                  return False
                
                Right _ -> do
                  -- Version is compatible, create the connection
                  now <- getCurrentTime
                  let connection = PeerConnection
                        { peerID = peer
                        , peerConnectedAt = now
                        , peerLastMessage = now
                        , peerSubscriptions = []
                        , peerProtocolVersion = peerVersion
                        }
                  
                  -- Add to our peer map
                  modifyMVar_ (connectedPeers manager) $ \peers ->
                    return $ Map.insert (unPeerId peer) connection peers
                  
                  -- Log successful connection and protocol version
                  _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
                      "connect_success" (unPeerId peer) 
                      (Just $ "Protocol version: " <> T.pack (show peerVersion))
                  
                  return True
            else do
              -- Log failed connection
              _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
                  "connect_fail" (unPeerId peer) (Just "Connection failed")
              
              return False

-- | Disconnect from a peer
disconnectPeer :: NetworkManager -> Text -> IO Bool
disconnectPeer manager peerId = do
  -- Check if we're connected
  peers <- readMVar (connectedPeers manager)
  if Map.member peerId peers
    then do
      -- Log attempt
      _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
          "disconnect" peerId Nothing
      
      -- Remove from our peer map
      modifyMVar_ (connectedPeers manager) $ \peers ->
        return $ Map.delete peerId peers
      
      return True
    else do
      -- Not connected
      _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
          "disconnect_skip" peerId (Just "Not connected")
      
      return False

-- | List all connected peers
listConnectedPeers :: NetworkManager -> IO [PeerId]
listConnectedPeers manager = do
  peers <- readMVar (connectedPeers manager)
  return $ map (peerID . snd) (Map.toList peers)

-- | Find a peer by ID
findPeerByID :: NetworkManager -> Text -> IO (Maybe PeerConnection)
findPeerByID manager peerId = do
  peers <- readMVar (connectedPeers manager)
  return $ Map.lookup peerId peers

-- | Send a message to a specific peer
sendMessage :: NetworkManager -> Text -> Text -> ByteString -> IO Bool
sendMessage manager peerId topic payload = do
  -- Check if we're connected to this peer
  peers <- readMVar (connectedPeers manager)
  case Map.lookup peerId peers of
    Nothing -> do
      -- Not connected
      _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
          "send_fail" peerId (Just $ "Not connected to " <> peerId)
      
      return False
    
    Just conn -> do
      -- Check if the peer is subscribed to this topic
      if topic `elem` peerSubscriptions conn
        then do
          -- Create the message
          now <- getCurrentTime
          let message = NetworkMessage
                { messageTopic = topic
                , messagePayload = payload
                , messageSender = networkNodeId (networkConfig manager)
                , messageTimestamp = now
                }
          
          -- Simulate sending the message
          -- In a real implementation, this would involve network communication
          success <- simulateMessageSend
          
          if success
            then do
              -- Log successful send
              _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
                  "send_success" peerId (Just topic)
              
              return True
            else do
              -- Log failed send
              _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
                  "send_fail" peerId (Just $ "Failed to send to " <> peerId)
              
              return False
        else do
          -- Peer not subscribed
          _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
              "send_skip" peerId (Just $ peerId <> " not subscribed to " <> topic)
          
          return False

-- | Broadcast a message to all peers subscribed to a topic
broadcastMessage :: NetworkManager -> Text -> ByteString -> IO Int
broadcastMessage manager topic payload = do
  -- Find all peers subscribed to this topic
  peers <- readMVar (connectedPeers manager)
  let subscribedPeers = Map.keys $ Map.filter (\conn -> topic `elem` peerSubscriptions conn) peers
  
  -- Log broadcast attempt
  _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
      "broadcast" topic (Just $ "To " <> T.pack (show (length subscribedPeers)) <> " peers")
  
  -- Send to each peer
  sentCount <- foldl (\countIO peerId -> do
    count <- countIO
    success <- sendMessage manager peerId topic payload
    return $ if success then count + 1 else count
    ) (return 0) subscribedPeers
  
  -- Log broadcast results
  _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
      "broadcast_complete" topic (Just $ "Sent to " <> T.pack (show sentCount) <> " peers")
  
  return sentCount

-- | Subscribe to a topic
subscribe :: NetworkManager -> Text -> MessageCallback -> IO ()
subscribe manager topic callback = do
  -- Add to subscriptions
  modifyMVar_ (subscriptions manager) $ \subs ->
    return $ Map.insertWith (++) topic [callback] subs
  
  -- Log subscription
  _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
      "subscribe" topic Nothing
  
  return ()

-- | Unsubscribe from a topic
unsubscribe :: NetworkManager -> Text -> MessageCallback -> IO ()
unsubscribe manager topic callback = do
  -- Remove from subscriptions
  -- This is a simplistic implementation that doesn't actually remove the exact callback
  -- because function equality is tricky in Haskell
  -- In a real implementation, you might use a subscription ID
  modifyMVar_ (subscriptions manager) $ \subs ->
    return $ Map.update (\callbacks -> 
      let remaining = tail callbacks  -- This is simplistic!
      in if null remaining then Nothing else Just remaining
      ) topic subs
  
  -- Log unsubscription
  _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
      "unsubscribe" topic Nothing
  
  return ()

-- | Handle a newly discovered peer
handleNewPeer :: NetworkManager -> PeerId -> IO ()
handleNewPeer manager peer = do
  -- Log discovery
  _ <- logNetworkEvent (networkLogStore $ networkConfig manager) 
      "peer_discovered" (unPeerId peer) Nothing
  
  -- Check current peer count
  peers <- readMVar (connectedPeers manager)
  when (Map.size peers < networkMaxPeers (networkConfig manager)) $ do
    -- Try to connect
    void $ connectToPeer manager peer

-- | Simulate connection establishment
-- In a real implementation, this would involve network communication
simulateConnectionEstablishment :: IO Bool
simulateConnectionEstablishment = do
  -- Succeed 80% of the time
  r <- randomRIO (1, 100 :: Int)
  return (r <= 80)

-- | Simulate sending a message
-- In a real implementation, this would involve network communication
simulateMessageSend :: IO Bool
simulateMessageSend = do
  -- Succeed 95% of the time
  r <- randomRIO (1, 100 :: Int)
  return (r <= 95)

-- | Simulate protocol version exchange during connection establishment
-- In a real implementation, this would involve network communication
simulateVersionExchange :: ProtocolVersion -> IO ProtocolVersion
simulateVersionExchange ourVersion = do
  -- Most of the time, return a compatible version
  r <- randomRIO (1, 10 :: Int)
  if r <= 8  -- 80% of the time
    then return $ ProtocolVersion 
            (majorVersion ourVersion)
            (minorVersion ourVersion)
            (patchVersion ourVersion)
            (featureFlags ourVersion)
    else if r == 9  -- 10% of the time, return newer patch version
      then return $ ProtocolVersion 
              (majorVersion ourVersion)
              (minorVersion ourVersion)
              (patchVersion ourVersion + 1)
              (featureFlags ourVersion)
      else  -- 10% of the time, return incompatible version
        return $ ProtocolVersion 
                (majorVersion ourVersion + 1)
                (minorVersion ourVersion)
                (patchVersion ourVersion)
                (featureFlags ourVersion) 