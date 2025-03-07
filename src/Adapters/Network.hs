{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- |

This module provides the P2P networking layer for the Time Bandits application.
It implements:

1. Rendezvous Hashing for deterministic node selection
2. Node discovery and connection management
3. Message routing and delivery
4. Integration with the existing effect system

The P2P layer is responsible for:
- Finding the appropriate nodes to store and retrieve data
- Routing messages between actors
- Maintaining connections to other nodes
- Replicating data according to the configured replication factor
-}
module Adapters.Network
  ( -- * P2P Effects
    P2PNetwork (..),
    interpretP2PNetwork,
    
    -- * Node Selection
    selectNodesForKey,
    getPeerHealth,
    
    -- * Message Routing
    routeMessage,
    broadcastToTimeline,
    
    -- * Network Management
    discoverPeers,
    refreshPeerList,
    prunePeers,

    -- * Connection Management
    connectToPeer,
    disconnectFromPeer,
    
    -- * Rendezvous Hashing
    computeNodeScore,
    
    -- * Types
    P2PNode (..),
    P2PMessage (..),
    P2PConfig (..),
    P2PStats (..),
    PeerHealth (..),
    P2PCapability (..),
    defaultP2PConfig,
    
    -- * Re-exports
    RouteResult (..),
  ) where

import Control.Monad ()
import Crypto.Hash.SHA256()
import Data.Binary ()
import Data.ByteString ()
import Data.ByteString qualified as BS
import Data.ByteString.Lazy()
import Data.ByteString.Char8 qualified as BS8
import Data.IORef()
import Data.List ()
import Data.Map.Strict ()
import Data.Map.Strict qualified as Map
import Data.Maybe ()
import Data.Ord ()
import Data.Set qualified as Set
import Data.Text (pack)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Word ()
import GHC.Generics ()
import Network.Socket (SockAddr(..), tupleToHostAddress)
import Polysemy
import Polysemy.Error
import Polysemy.State qualified as PS
import Polysemy.Trace qualified as PT
import System.Random (randomIO)
import Relude ()
import Relude.Extra.Tuple (fmapToFst)

import Core qualified as Core
import Core.Types

-- | Information about a P2P node in the network
data P2PNode = P2PNode
  { pnId :: !ActorHash
  -- ^ Unique identifier (hash of public key)
  , pnActor :: !Actor
  -- ^ The actor associated with this node
  , pnAddress :: !SockAddr
  -- ^ Network address
  , pnLastSeen :: !UTCTime
  -- ^ Last time this node was seen
  , pnCapabilities :: ![P2PCapability]
  -- ^ Capabilities this node supports
  , pnPublicKey :: !PubKey
  -- ^ Public key for authentication
  , pnLoad :: !Double
  -- ^ Current load factor (0.0-1.0)
  , pnStats :: !P2PStats
  -- ^ Statistics for this peer
  }
  deriving stock (Show, Eq, Generic)

-- | Capabilities a P2P node can support
data P2PCapability
  = CanRoute
  -- ^ Can route messages
  | CanStore
  -- ^ Can store resources
  | CanValidate
  -- ^ Can validate transactions
  | CanObserve
  -- ^ Read-only capability
  | FullNode
  -- ^ Has all capabilities
  deriving stock (Show, Eq, Ord, Generic)

-- | Statistics for a P2P node
data P2PStats = P2PStats
  { psMessagesRouted :: !Int
  -- ^ Number of messages routed through this node
  , psDataStored :: !Int
  -- ^ Amount of data stored on this node (in bytes)
  , psSuccessRate :: !Double
  -- ^ Success rate for operations (0.0-1.0)
  , psResponseTime :: !Double
  -- ^ Average response time in milliseconds
  , psUptime :: !Double
  -- ^ Uptime in seconds
  }
  deriving stock (Show, Eq, Generic)

-- | P2P message types for inter-node communication
data P2PMessage
  = NodeDiscovery
    { pmSenderNode :: !P2PNode
    , pmRequestingPeers :: !Bool
    }
  | NodeResponse
    { pmRespondingNode :: !P2PNode
    , pmKnownPeers :: ![P2PNode]
    }
  | ResourceLookup
    { pmRequestingNode :: !P2PNode
    , pmResourceHash :: !ResourceHash
    }
  | ResourceResponse
    { pmRespondingNode :: !P2PNode
    , pmResource :: !(Maybe Resource)
    }
  | MessageRouting
    { pmRoutingNode :: !P2PNode
    , pmMessage :: AuthenticatedMessage ByteString
    }
  | TimelineUpdate
    { pmUpdatingNode :: !P2PNode
    , pmTimelineEvents :: ![LogEntry TimelineEventType]
    }
  | HealthCheck
    { pmCheckingNode :: !P2PNode
    , pmTimestamp :: !UTCTime
    }
  | HealthResponse
    { pmHealthyNode :: !P2PNode
    , pmNodeLoad :: !Double
    , pmOriginalTimestamp :: !UTCTime
    }
  deriving stock (Show, Eq, Generic)

-- | Result of a routing operation
data RouteResult a
  = Delivered !a
  -- ^ Successfully delivered
  | PartiallyDelivered !a !Text
  -- ^ Delivered to some nodes with issues
  | FailedDelivery !Text
  -- ^ Failed to deliver
  deriving stock (Show, Eq, Generic)

-- | Health status of a peer
data PeerHealth
  = Healthy
  -- ^ Node is responsive and healthy
  | Degraded !Text
  -- ^ Node is responsive but has issues
  | Unhealthy !Text
  -- ^ Node is unresponsive or has serious issues
  | Unknown
  -- ^ Health status is unknown
  deriving stock (Show, Eq, Generic)

-- | Configuration for the P2P network
data P2PConfig = P2PConfig
  { pcBindAddress :: !SockAddr
  -- ^ Address to bind to
  , pcSeedNodes :: ![SockAddr]
  -- ^ Initial nodes to connect to
  , pcReplicationFactor :: !Int
  -- ^ Number of nodes to replicate data to
  , pcNodeCapacity :: !Int
  -- ^ Maximum number of peers to maintain
  , pcRefreshInterval :: !Int
  -- ^ Interval for peer list refresh (seconds)
  , pcHealthCheckInterval :: !Int
  -- ^ Interval for peer health checks (seconds)
  , pcConnectionTimeout :: !Int
  -- ^ Connection timeout (milliseconds)
  , pcMaxMessageSize :: !Int
  -- ^ Maximum message size (bytes)
  }
  deriving stock (Show, Eq, Generic)

-- | Default P2P configuration
defaultP2PConfig :: P2PConfig
defaultP2PConfig = P2PConfig
  { pcBindAddress = SockAddrInet 8888 (tupleToHostAddress (127, 0, 0, 1)) -- Default to localhost:8888
  , pcSeedNodes = []
  , pcReplicationFactor = 3
  , pcNodeCapacity = 50
  , pcRefreshInterval = 300
  , pcHealthCheckInterval = 60
  , pcConnectionTimeout = 5000
  , pcMaxMessageSize = 1024 * 1024
  }

-- | P2P network effect
data P2PNetwork m a where
  -- Node discovery and management
  DiscoverPeers :: P2PNetwork m [P2PNode]
  RefreshPeerList :: P2PNetwork m Int
  GetPeerHealth :: P2PNode -> P2PNetwork m PeerHealth
  PrunePeers :: P2PNetwork m Int
  
  -- Connection management
  ConnectToPeer :: SockAddr -> P2PNetwork m (Maybe P2PNode)
  DisconnectFromPeer :: P2PNode -> P2PNetwork m Bool
  
  -- Node selection
  SelectNodesForKey :: ByteString -> Int -> P2PNetwork m [P2PNode]
  
  -- Message routing
  RouteMessage :: AuthenticatedMessage ByteString -> P2PNetwork m (RouteResult ByteString)
  BroadcastToTimeline :: TimelineHash -> ByteString -> P2PNetwork m (RouteResult ())

makeSem ''P2PNetwork

-- | Compute a score for a node using rendezvous hashing
-- Returns the node's score for the given key
computeNodeScore :: P2PNode -> ByteString -> Word64
computeNodeScore node = Core.computeNodeScore (pnId node)

-- | Interpret the P2P Network effect
-- Provides concrete implementations for all P2P network operations, including
-- peer discovery, message routing, connection management, and data synchronization.
-- This interpreter uses the actor's identity to authenticate network operations
-- and maintains a peer list through the State effect.
-- 
-- In production, this would interface with real network protocols like libp2p
-- for establishing secure peer-to-peer connections across the internet.
interpretP2PNetwork :: 
  (Members '[Embed IO, Error AppError, PS.State [P2PNode], PT.Trace] r) =>
  P2PConfig ->      -- ^ Configuration for P2P network behavior
  Actor ->          -- ^ The local actor's identity
  PubKey ->         -- ^ The local actor's public key for message signing
  Sem (P2PNetwork ': r) a ->
  Sem r a
interpretP2PNetwork config self pubKey = interpret \case
  DiscoverPeers -> do
    PT.trace "Discovering peers in the P2P network"
    -- Get current list of peers
    currentPeers <- PS.get
    
    -- Connect to seed nodes if we have no peers
    newPeers <- if null currentPeers
      then do
        PT.trace $ "Connecting to " <> show (length (pcSeedNodes config)) <> " seed nodes"
        results <- mapM connectToSeedNode (pcSeedNodes config)
        return $ catMaybes results
      else do
        -- Select a subset of current peers to ask for more peers
        let sampleSize = min 5 (length currentPeers)
        timestamp <- embed getCurrentTime
        let sortedPeers = take sampleSize $ sortOn pnLastSeen currentPeers
        
        -- Request peers from each sample peer
        PT.trace $ "Requesting peers from " <> show sampleSize <> " existing peers"
        results <- mapM requestPeersFromNode sortedPeers
        
        -- Flatten and deduplicate results
        let allPeers = concat results
            uniquePeers = nubPeersByHash allPeers
        
        -- Filter out peers we already know
        let knownHashes = Set.fromList $ map pnId currentPeers
            newUniquePeers = filter (\p -> not $ Set.member (pnId p) knownHashes) uniquePeers
        
        PT.trace $ "Discovered " <> show (length newUniquePeers) <> " new peers"
        return newUniquePeers
    
    -- Update peer list
    let updatedPeers = nubPeersByHash (currentPeers ++ newPeers)
        prunedPeers = take (pcNodeCapacity config) updatedPeers
    
    PS.put prunedPeers
    return prunedPeers
  
  RefreshPeerList -> do
    PT.trace "Refreshing peer list"
    currentPeers <- PS.get
    
    -- Check health of all peers
    timestamp <- embed getCurrentTime
    healthStatuses <- mapM (\p -> do
      health <- checkPeerHealth p timestamp
      return (p, health)) currentPeers
    
    -- Keep healthy and degraded peers, remove unhealthy ones
    let healthyPeers = [p | (p, h) <- healthStatuses, h == Healthy || case h of Degraded _ -> True; _ -> False]
        removedCount = length currentPeers - length healthyPeers
    
    -- Discover new peers if needed
    updatedPeers <- if length healthyPeers < pcNodeCapacity config `div` 2
      then do
        PT.trace "Discovering peers in the P2P network"
        -- Get current list of peers
        -- currentPeers <- PS.get  -- Removing redundant PS.get since we already have healthyPeers
        
        -- Connect to seed nodes if we have no peers
        newPeers <- if null healthyPeers
          then do
            PT.trace $ "Connecting to " <> show (length (pcSeedNodes config)) <> " seed nodes"
            results <- mapM connectToSeedNode (pcSeedNodes config)
            return $ catMaybes results
          else do
            -- Select a subset of current peers to ask for more peers
            let sampleSize = min 5 (length healthyPeers)
            timestamp <- embed getCurrentTime
            let sortedPeers = take sampleSize $ sortOn pnLastSeen healthyPeers
            
            -- Request peers from each sample peer
            PT.trace $ "Requesting peers from " <> show sampleSize <> " existing peers"
            results <- mapM requestPeersFromNode sortedPeers
            
            -- Flatten and deduplicate results
            let allPeers = concat results
                uniquePeers = nubPeersByHash allPeers
            
            -- Filter out peers we already know
            let knownHashes = Set.fromList $ map pnId healthyPeers
                newUniquePeers = filter (\p -> not $ Set.member (pnId p) knownHashes) uniquePeers
            
            PT.trace $ "Discovered " <> show (length newUniquePeers) <> " new peers"
            return newUniquePeers
        
        return $ nubPeersByHash (healthyPeers ++ newPeers)
      else return healthyPeers
    
    -- Update peer list
    PS.put updatedPeers
    return removedCount
  
  GetPeerHealth node -> do
    timestamp <- embed getCurrentTime
    checkPeerHealth node timestamp
  
  PrunePeers -> do
    PT.trace "Pruning peer list"
    currentPeers <- PS.get
    timestamp <- embed getCurrentTime
    
    -- Check health and last seen time to identify active peers
    -- We remove peers that haven't been seen for more than an hour
    let maxAge = 3600 -- 1 hour in seconds
        isRecentlyActive p = diffUTCTime timestamp (pnLastSeen p) < maxAge
        activePeers = filter isRecentlyActive currentPeers
    
    -- Sort by health and keep best peers up to capacity
    -- This implements a quality-based selection mechanism that retains
    -- the most reliable peers when network capacity is constrained
    healthStatuses <- mapM (\p -> do
      health <- checkPeerHealth p timestamp
      return (p, scoreNodeHealth health p)) activePeers
    
    -- Sort peers by their health score (highest first) and limit to capacity
    -- This ensures we maintain connections to the most reliable peers
    -- while respecting the node capacity constraints
    let sortedPeers = map fst $ sortWith (Down . snd) healthStatuses
        prunedPeers = take (pcNodeCapacity config) sortedPeers
        removedCount = length currentPeers - length prunedPeers
    
    -- Update the peer list with the pruned set of peers
    PS.put prunedPeers
    return removedCount
  
  ConnectToPeer addr -> do
    PT.trace $ "Connecting to peer at " <> show addr
    -- Simulate connection attempt
    -- In a real implementation, this would establish a network connection
    result <- embed $ simulateConnectionAttempt addr
    case result of
      Just node -> do
        -- Add to peer list if not already present
        -- This prevents duplicate connections to the same peer
        currentPeers <- PS.get
        let updatedPeers = if any (\p -> pnId p == pnId node) currentPeers
              then currentPeers
              else node : currentPeers
        PS.put updatedPeers
        PT.trace $ "Successfully connected to " <> show (pnId node)
        return $ Just node
      Nothing -> do
        PT.trace $ "Failed to connect to " <> show addr
        return Nothing
  
  DisconnectFromPeer node -> do
    PT.trace $ "Disconnecting from peer " <> show (pnId node)
    -- Remove from peer list
    -- This terminates the network connection and removes the peer from tracking
    currentPeers <- PS.get
    let updatedPeers = filter (\p -> pnId p /= pnId node) currentPeers
    PS.put updatedPeers
    return $ length currentPeers /= length updatedPeers
  
  SelectNodesForKey key count -> do
    PT.trace $ "Selecting " <> show count <> " nodes for key " <> BS8.unpack key
    currentPeers <- PS.get
    
    -- Compute score for each node using rendezvous hashing
    -- This implements a consistent hashing algorithm that assigns keys to nodes
    -- in a deterministic way that minimizes reassignments when nodes join/leave
    let scoredNodes = map (\node -> (node, Core.computeNodeScore (pnId node) key)) currentPeers
        -- Sort by score (highest first)
        sortedNodes = map fst $ sortOn (Down . snd) scoredNodes
        -- Take required number of nodes
        selectedNodes = take count sortedNodes
    
    PT.trace $ "Selected " <> show (length selectedNodes) <> " nodes"
    return selectedNodes
  
  RouteMessage msg -> do
    PT.trace "Routing message"
    -- Get destination (if specified)
    case amDestination msg of
      Just destHash -> do
        -- Find nodes that can route to this destination
        currentPeers <- PS.get
        let destNodes = filter (\p -> hasRoutingCapability p && 
                                   (pnId p == destHash || 
                                    canRouteToDestination p destHash)) currentPeers
        
        if null destNodes
          then do
            PT.trace "No route to destination"
            return $ FailedDelivery "No route to destination"
          else do
            -- Try to send to first available node
            case viaNonEmpty head destNodes of
              Just firstNode -> do
                result <- sendMessageToNode firstNode msg
                case result of
                  Right response -> do
                    PT.trace "Message delivered successfully"
                    return $ Delivered response
                  Left err -> do
                    let errMsg = pack (show err)
                    PT.trace $ "Failed to deliver message: " <> toString errMsg
                    return $ FailedDelivery errMsg
              Nothing ->
                return $ FailedDelivery "No route to destination"
      
      -- Broadcast to all nodes if no destination specified
      Nothing -> do
        PT.trace "Broadcasting message to all nodes"
        currentPeers <- PS.get
        let routingNodes = filter hasRoutingCapability currentPeers
        
        if null routingNodes
          then do
            PT.trace "No routing nodes available"
            return $ FailedDelivery "No routing nodes available"
          else do
            results <- mapM (`sendMessageToNode` msg) routingNodes
            let successes = rights results
                failures = lefts results
            
            if null successes
              then case viaNonEmpty head failures of
                Just firstFailure -> return $ FailedDelivery $ show firstFailure
                Nothing -> return $ FailedDelivery "Unknown error"
              else case viaNonEmpty head successes of
                Just firstSuccess -> return $ Delivered firstSuccess
                Nothing -> return $ FailedDelivery "No successful deliveries"
  
  BroadcastToTimeline timelineHash content -> do
    PT.trace $ "Broadcasting to timeline " <> show timelineHash
    currentPeers <- PS.get
    
    -- Find nodes that participate in this timeline
    timestamp <- embed getCurrentTime
    let timelineNodes = filter (participatesInTimeline timelineHash) currentPeers
    
    if null timelineNodes
      then do
        PT.trace "No nodes for this timeline"
        return $ FailedDelivery "No nodes available for this timeline"
      else do
        -- Send to all timeline nodes
        results <- forM timelineNodes $ \node -> do
          let msg = createTimelineMessage self timelineHash content timestamp
          sendMessageToNode node msg
        
        let successes = length [() | Right _ <- results]
            failures = length (lefts results)
        
        if successes == 0
          then return $ FailedDelivery "Failed to deliver to any timeline nodes"
          else if failures == 0
            then return $ Delivered ()
            else return $ PartiallyDelivered () 
                  (pack $ "Delivered to " <> show successes <> 
                           " of " <> show (length timelineNodes) <> " nodes")

-- Helper functions

-- | Create a message for timeline broadcast
createTimelineMessage :: Actor -> TimelineHash -> ByteString -> UTCTime -> AuthenticatedMessage ByteString
createTimelineMessage sender timeline content timestamp =
  AuthenticatedMessage
    { amHash = Core.computeMessageHash content
    , amSender = sender
    , amDestination = Nothing
    , amPayload = ContentAddressedMessage (Core.computeMessageHash content) content
    , amSignature = Signature "simulated-signature" -- In a real system, this would be properly signed
    }

-- | Check if a node participates in a timeline
participatesInTimeline :: TimelineHash -> P2PNode -> Bool
participatesInTimeline _ _ = True  -- Simplified version, would check actual timeline participation

-- | Check if a node has routing capability
hasRoutingCapability :: P2PNode -> Bool
hasRoutingCapability node =
  CanRoute `elem` pnCapabilities node || FullNode `elem` pnCapabilities node

-- | Check if a node can route to a specific destination
canRouteToDestination :: P2PNode -> ActorHash -> Bool
canRouteToDestination _ _ = True  -- Simplified version, would check actual routing tables

-- | Send a message to a node
sendMessageToNode :: (Member (Embed IO) r) => 
                     P2PNode -> AuthenticatedMessage ByteString -> Sem r (Either AppError ByteString)
sendMessageToNode _ _ = do
  -- Simulate successful message delivery
  embed $ do
    success <- (>0.8) <$> (randomIO :: IO Double)
    if success
      then return $ Right "message-delivered"
      else return $ Left $ NetworkError "Failed to deliver message"

-- | Connect to a seed node
connectToSeedNode :: (Member (Embed IO) r) => SockAddr -> Sem r (Maybe P2PNode)
connectToSeedNode addr = do
  -- Simulate connection attempt
  embed $ simulateConnectionAttempt addr

-- | Request peers from a node
requestPeersFromNode :: (Member (Embed IO) r) => P2PNode -> Sem r [P2PNode]
requestPeersFromNode _ = do
  -- Simulate peer discovery
  embed $ do
    count <- customRandomRIO (0, 5) :: IO Int
    mapM (const generateRandomNode) [1..count]

-- Utility functions

-- | Generate a random node for simulation
generateRandomNode :: IO P2PNode
generateRandomNode = do
  timestamp <- getCurrentTime
  nodeId <- randomBS 32
  let actorHash = EntityHash (Hash nodeId)
      actor = Actor actorHash TimeTraveler
  
  return P2PNode
    { pnId = actorHash
    , pnActor = actor
    , pnAddress = error "Random address"
    , pnLastSeen = timestamp
    , pnCapabilities = [CanRoute, CanStore]
    , pnPublicKey = PubKey nodeId
    , pnLoad = 0.5
    , pnStats = P2PStats 100 1024 0.95 50 3600
    }

-- | Generate random ByteString
randomBS :: Int -> IO ByteString
randomBS len = BS.pack <$> sequence [randomIO | _ <- [1..len]]

-- | Generate random number in range
customRandomRIO :: (Integral a) => (a, a) -> IO a
customRandomRIO (lo, hi) = do
  r <- (randomIO :: IO Int)
  return $ lo + fromIntegral (r `mod` fromIntegral (hi - lo + 1))

-- | Simulate a connection attempt
simulateConnectionAttempt :: SockAddr -> IO (Maybe P2PNode)
simulateConnectionAttempt _ = do
  success <- (>0.7) <$> (randomIO :: IO Double)
  if success
    then Just <$> generateRandomNode
    else return Nothing

-- | Remove duplicate peers by hash
nubPeersByHash :: [P2PNode] -> [P2PNode]
nubPeersByHash = Map.elems . Map.fromList . fmapToFst pnId

-- | Check peer health
checkPeerHealth :: (Member (Embed IO) r) => P2PNode -> UTCTime -> Sem r PeerHealth
checkPeerHealth node timestamp = do
  -- Simulate health check
  embed $ do
    let age = diffUTCTime timestamp (pnLastSeen node)
    if age > 3600
      then return $ Unhealthy "Node not seen recently"
      else do
        rand <- randomIO :: IO Double
        if rand > 0.9
          then return $ Unhealthy "Connection failed"
          else if rand > 0.7
            then return $ Degraded "High latency"
            else return Healthy

-- | Score a node's health
scoreNodeHealth :: PeerHealth -> P2PNode -> Double
scoreNodeHealth health node =
  let baseScore = case health of
        Healthy -> 1.0
        Degraded _ -> 0.6
        Unhealthy _ -> 0.1
        Unknown -> 0.3
      -- Adjust by stats
      statsScore = (psSuccessRate (pnStats node) * 0.4) +
                   (1.0 - min 1.0 (psResponseTime (pnStats node) / 200.0)) * 0.4 +
                   min 1.0 (psUptime (pnStats node) / 86400.0) * 0.2
  in baseScore * 0.7 + statsScore * 0.3 