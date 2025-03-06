{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
This module adapts the QUIC-based network implementation for use with Time Bandits' P2P networking.
It provides a layer that connects the core P2P network interface with the QUIC transport implementation.
-}
module TimeBandits.NetworkAdapter
  ( -- * Network Adaptation Functions
    adaptNetworkToQuic
  , createQuicNetworkEffect
  
  -- * Configuration Conversion
  , p2pConfigToQuicConfig
  , quicConfigToP2pConfig
  
  -- * Network Mode Functions
  , interpretNetworkForGeoDistributed
  ) where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO, modifyTVarIO)
import Control.Monad (forM, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (getCurrentTime)
import Network.Socket (SockAddr(..), PortNumber)
import Polysemy
import Polysemy.Error
import Polysemy.State qualified as PS
import Polysemy.Trace qualified as PT

-- Import from TimeBandits modules
import TimeBandits.Core (ActorHash, Hash(..))
import TimeBandits.Network
  ( P2PNetwork
  , P2PConfig(..)
  , P2PNode(..)
  , P2PCapability(..)
  , NetworkStats(..)
  , MessageDeliveryStatus(..)
  , MessageType(..)
  )
import TimeBandits.NetworkQUIC
  ( QuicConfig(..)
  , QuicPeer(..)
  , QuicConnection(..)
  , QuicMessage(..)
  , defaultQuicConfig
  , startQuicServer
  , connectToQuicPeer
  , broadcastQuicMessage
  , sendQuicMessage
  , receiveQuicMessage
  , discoverQuicPeers
  , registerWithBootstrapPeers
  )
import Core.Types
  ( Actor(..)
  , AppError(..)
  , PubKey(..)
  )

-- | Convert a P2P network configuration to a QUIC configuration
p2pConfigToQuicConfig :: P2PConfig -> QuicConfig
p2pConfigToQuicConfig p2pConfig = 
  defaultQuicConfig
    { qcBindAddress = pcBindAddress p2pConfig
    , qcBindPort = case pcBindAddress p2pConfig of
        SockAddrInet port _ -> port
        _ -> 8443  -- Default port if not specified
    , qcBootstrapPeers = pcBootstrapNodes p2pConfig
    , qcMaxPeers = pcMaxConnections p2pConfig
    , qcKeepAliveInterval = pcHeartbeatInterval p2pConfig
    , qcDiscoveryInterval = pcDiscoveryInterval p2pConfig
    , qcConnectionTimeout = 5000  -- Default value
    }

-- | Convert a QUIC configuration back to a P2P network configuration
quicConfigToP2pConfig :: QuicConfig -> P2PConfig
quicConfigToP2pConfig quicConfig =
  P2PConfig
    { pcBindAddress = qcBindAddress quicConfig
    , pcBootstrapNodes = qcBootstrapPeers quicConfig
    , pcMaxConnections = qcMaxPeers quicConfig
    , pcHeartbeatInterval = qcKeepAliveInterval quicConfig
    , pcDiscoveryInterval = qcDiscoveryInterval quicConfig
    , pcNodeCapabilities = [CanRoute, CanStore, CanCreateTimelines]  -- Default capabilities
    }

-- | Convert a QuicPeer to a P2PNode
quicPeerToP2PNode :: QuicPeer -> P2PNode
quicPeerToP2PNode peer =
  P2PNode
    { nodeId = qpId peer
    , nodeAddress = qpAddress peer
    , nodePubKey = qpPublicKey peer
    , nodeCapabilities = qpCapabilities peer
    , nodeLastSeen = qpLastSeen peer
    , nodeStats = NetworkStats
        { nsMessagesSent = qsPacketsSent (qpStats peer)
        , nsMessagesReceived = qsPacketsReceived (qpStats peer)
        , nsMessagesDropped = qsPacketsLost (qpStats peer)
        , nsLastActivity = qsLastActivity (qpStats peer)
        , nsAvgLatency = Just (qsRTT (qpStats peer))
        }
    }
  where
    qsPacketsSent stats = 0  -- Placeholder, replace with actual field
    qsPacketsReceived stats = 0  -- Placeholder, replace with actual field
    qsPacketsLost stats = 0  -- Placeholder, replace with actual field
    qsLastActivity stats = qpLastSeen peer  -- Placeholder, replace with actual field
    qsRTT stats = 0  -- Placeholder, replace with actual field

-- | Convert a P2PNode to a QuicPeer (partial conversion, needs additional data)
p2pNodeToQuicPeer :: P2PNode -> UTCTime -> ByteString -> QuicPeer
p2pNodeToQuicPeer node now connId =
  QuicPeer
    { qpId = nodeId node
    , qpAddress = nodeAddress node
    , qpPublicKey = nodePubKey node
    , qpCapabilities = nodeCapabilities node
    , qpLastSeen = nodeLastSeen node
    , qpConnectionId = connId
    , qpStats = QuicStats
        { qsRTT = fromMaybe 100.0 (nsAvgLatency (nodeStats node))
        , qsPacketsSent = nsMessagesSent (nodeStats node)
        , qsPacketsReceived = nsMessagesReceived (nodeStats node)
        , qsPacketsLost = nsMessagesDropped (nodeStats node)
        , qsBytesTransferred = 0  -- Not tracked in P2PNode
        , qsLastActivity = nsLastActivity (nodeStats node)
        }
    }

-- | Adapt the P2P Network effect to use QUIC transport
adaptNetworkToQuic ::
  (Member (Embed IO) r, Member (Error AppError) r, Member PT.Trace r) =>
  P2PConfig ->
  Actor ->
  PubKey ->
  (P2PNetwork ~> Sem r)
adaptNetworkToQuic p2pConfig self pubKey = interpret \case
  TimeBandits.Network.ConnectToNode nodeAddr -> do
    PT.trace $ "Connecting to node via QUIC: " <> show nodeAddr
    
    -- Convert P2P config to QUIC config
    let quicConfig = p2pConfigToQuicConfig p2pConfig
    
    -- Connect to the peer using QUIC
    maybeConn <- connectToQuicPeer quicConfig nodeAddr
    case maybeConn of
      Just conn -> do
        PT.trace "Connection established"
        return $ Just $ quicPeerToP2PNode (qcPeer conn)
      Nothing -> do
        PT.trace "Connection failed"
        return Nothing
        
  TimeBandits.Network.DiscoverNodes -> do
    PT.trace "Discovering nodes via QUIC network"
    
    -- Convert P2P config to QUIC config
    let quicConfig = p2pConfigToQuicConfig p2pConfig
    
    -- Create a local peer representation
    now <- embed getCurrentTime
    connId <- embed $ BS.pack <$> replicateM 16 (randomRIO (0, 255))
    
    let localNode = P2PNode
          { nodeId = actorId self
          , nodeAddress = pcBindAddress p2pConfig
          , nodePubKey = pubKey
          , nodeCapabilities = pcNodeCapabilities p2pConfig
          , nodeLastSeen = now
          , nodeStats = NetworkStats
              { nsMessagesSent = 0
              , nsMessagesReceived = 0
              , nsMessagesDropped = 0
              , nsLastActivity = now
              , nsAvgLatency = Nothing
              }
          }
        localPeer = p2pNodeToQuicPeer localNode now connId
    
    -- Connect to bootstrap peers
    connections <- registerWithBootstrapPeers quicConfig localPeer
    
    -- If we have connections, discover more peers
    if null connections
      then do
        PT.trace "No bootstrap connections established"
        return []
      else do
        peers <- discoverQuicPeers quicConfig connections
        return $ map quicPeerToP2PNode peers
        
  TimeBandits.Network.BroadcastMessage msgType payload -> do
    PT.trace $ "Broadcasting message via QUIC: " <> show msgType
    
    -- Convert P2P config to QUIC config
    let quicConfig = p2pConfigToQuicConfig p2pConfig
    
    -- Simulate having connections (in a real implementation, we would maintain these)
    now <- embed getCurrentTime
    connId <- embed $ BS.pack <$> replicateM 16 (randomRIO (0, 255))
    peers <- TimeBandits.Network.DiscoverNodes
    
    -- Create simulated connections
    connections <- fmap catMaybes $ forM peers $ \peer -> do
      let peerAddr = nodeAddress peer
      connectToQuicPeer quicConfig peerAddr
    
    -- Broadcast the message
    successes <- broadcastQuicMessage connections payload
    
    if successes > 0
      then return PartialDelivery
      else return DeliveryFailed
        
  TimeBandits.Network.SendDirectMessage targetNode msgType payload -> do
    PT.trace $ "Sending direct message via QUIC to: " <> show (nodeId targetNode) <> " type: " <> show msgType
    
    -- Convert P2P config to QUIC config
    let quicConfig = p2pConfigToQuicConfig p2pConfig
    
    -- Connect to the target node
    maybeConn <- connectToQuicPeer quicConfig (nodeAddress targetNode)
    case maybeConn of
      Just conn -> do
        -- Send the message
        success <- sendQuicMessage conn payload
        if success
          then return DeliveryConfirmed
          else return DeliveryFailed
      Nothing ->
        return DeliveryFailed
        
  TimeBandits.Network.GetNetworkStats -> do
    PT.trace "Getting network stats from QUIC connections"
    
    -- In a real implementation, we would track these stats
    -- For now, we return placeholder values
    now <- embed getCurrentTime
    return NetworkStats
      { nsMessagesSent = 0
      , nsMessagesReceived = 0
      , nsMessagesDropped = 0
      , nsLastActivity = now
      , nsAvgLatency = Nothing
      }

-- | Create a QUIC-based P2P network effect interpreter
createQuicNetworkEffect ::
  (Member (Embed IO) r, Member (Error AppError) r, Member PT.Trace r) =>
  P2PConfig ->
  Actor ->
  PubKey ->
  Sem (P2PNetwork ': r) a ->
  Sem r a
createQuicNetworkEffect config self pubKey =
  interpret (adaptNetworkToQuic config self pubKey)

-- | Specialized interpreter for Geo-Distributed mode
interpretNetworkForGeoDistributed ::
  P2PConfig ->
  Actor ->
  PubKey ->
  IORef [P2PNode] ->  -- ^ Node storage
  Sem (P2PNetwork ': r) a ->
  Sem r a
interpretNetworkForGeoDistributed config self pubKey nodesRef = interpret \case
  TimeBandits.Network.ConnectToNode nodeAddr -> do
    -- Delegate to QUIC implementation
    adaptNetworkToQuic config self pubKey (TimeBandits.Network.ConnectToNode nodeAddr)
  
  TimeBandits.Network.DiscoverNodes -> do
    -- Discover nodes using QUIC
    newNodes <- adaptNetworkToQuic config self pubKey TimeBandits.Network.DiscoverNodes
    
    -- Update the nodes storage
    embed $ atomically $ do
      currentNodes <- readTVar nodesRef
      let allNodes = currentNodes ++ newNodes
          -- Remove duplicates by nodeId
          uniqueNodes = Map.elems $ Map.fromList [(nodeId n, n) | n <- allNodes]
      writeTVar nodesRef uniqueNodes
    
    -- Return the discovered nodes
    return newNodes
  
  TimeBandits.Network.BroadcastMessage msgType payload -> do
    -- Delegate to QUIC implementation
    adaptNetworkToQuic config self pubKey (TimeBandits.Network.BroadcastMessage msgType payload)
  
  TimeBandits.Network.SendDirectMessage targetNode msgType payload -> do
    -- Delegate to QUIC implementation
    adaptNetworkToQuic config self pubKey (TimeBandits.Network.SendDirectMessage targetNode msgType payload)
  
  TimeBandits.Network.GetNetworkStats -> do
    -- Delegate to QUIC implementation
    adaptNetworkToQuic config self pubKey TimeBandits.Network.GetNetworkStats

-- Helper functions

-- | Return a default value if Nothing
fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x

-- | Generate a random byte
randomRIO :: (Integral a) => (a, a) -> IO a
randomRIO (lo, hi) = do
  r <- randomIO
  return $ lo + fromIntegral (abs r `mod` fromIntegral (hi - lo + 1))

-- | Random number generator (would be properly imported in real implementation)
randomIO :: IO Int
randomIO = return 0  -- Placeholder, replace with actual implementation

-- | Atomic modification of a TVar
modifyTVarIO :: TVar a -> (a -> a) -> IO ()
modifyTVarIO var f = atomically $ modifyTVar var f

-- Missing imports (would be included in real implementation)
import Control.Concurrent.STM (atomically, readTVar, modifyTVar, writeTVar)
import Data.IORef (IORef)
import qualified Data.ByteString.Char8 as BS8 