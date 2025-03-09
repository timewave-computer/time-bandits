{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Adapters.Network.Discovery
Description : Peer discovery for Time Bandits P2P network
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module implements peer discovery for the Time Bandits P2P network,
using rendezvous hashing for deterministic peer selection and discovery.
-}
module Adapters.Network.Discovery
  ( -- * Peer Discovery
    discoverPeersExt
  , maintainPeerList
  
    -- * Configuration
  , DiscoveryConfig(..)
  , defaultDiscoveryConfig
  
    -- * Peer Exchange
  , exchangePeers
  , announcePeer
  
    -- * Rendezvous Hashing
  , selectPeersToQuery
  , computePeerScore
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad (forever, forM, when)
import Data.Functor (void)
import qualified Data.Functor as Functor
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as TE
import Crypto.Hash (SHA256, Digest, hash)
import qualified Crypto.Hash as Hash
import Network.Socket (SockAddr)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Word (Word8)
import qualified Crypto.Hash as Crypto
import qualified Control.Concurrent as Concurrent

-- Import from TimeBandits modules
import Types.Network (PeerId(..), NetworkConfig(..), MessageType(..))
import Adapters.NetworkAdapter (NetworkAdapter(..))

-- | Configuration for peer discovery
data DiscoveryConfig = DiscoveryConfig
  { bootstrapPeers :: [SockAddr]      -- ^ Initial peers to connect to
  , discoveryInterval :: Int          -- ^ Interval between discovery cycles (seconds)
  , maxPeersPerQuery :: Int           -- ^ Maximum peers to query at once
  , targetPeerCount :: Int            -- ^ Target number of peers to maintain
  , peerExchangeEnabled :: Bool       -- ^ Whether to exchange peer lists
  , peerAnnounceEnabled :: Bool       -- ^ Whether to announce self to network
  , peerTimeoutSeconds :: Int         -- ^ Seconds after which a peer is considered timed out
  }

-- | Default peer discovery configuration
defaultDiscoveryConfig :: DiscoveryConfig
defaultDiscoveryConfig = DiscoveryConfig
  { bootstrapPeers = []
  , discoveryInterval = 60
  , maxPeersPerQuery = 5
  , targetPeerCount = 20
  , peerExchangeEnabled = True
  , peerAnnounceEnabled = True
  , peerTimeoutSeconds = 300
  }

-- | Extended peer information for discovery
data PeerInfo = PeerInfo
  { peerId :: PeerId               -- ^ Peer ID
  , peerAddress :: SockAddr        -- ^ Network address
  , lastSeen :: UTCTime            -- ^ Last time peer was seen
  , lastQueried :: Maybe UTCTime   -- ^ Last time peer was queried for peers
  , discoveredFrom :: Maybe PeerId -- ^ Peer that told us about this peer
  }

-- | Discover peers using rendezvous hashing
discoverPeersExt :: NetworkAdapter a => a -> DiscoveryConfig -> [PeerId] -> IO [PeerId]
discoverPeersExt adapter config knownPeers = do
  if null knownPeers
    then discoverFromBootstrap adapter config
    else discoverFromNetwork adapter config knownPeers

-- | Discover peers from bootstrap nodes
discoverFromBootstrap :: NetworkAdapter a => a -> DiscoveryConfig -> IO [PeerId]
discoverFromBootstrap adapter config = do
  -- For each bootstrap peer, try to connect and get peer list
  -- This is a simplified implementation that would be expanded in the real code
  return []  -- Placeholder

-- | Discover peers from the network using existing peers
discoverFromNetwork :: NetworkAdapter a => a -> DiscoveryConfig -> [PeerId] -> IO [PeerId]
discoverFromNetwork adapter config knownPeers = do
  -- Generate a random query key
  queryKey <- generateRandomKey
  
  -- Select peers to query based on rendezvous hashing
  let queryKeyText = TE.decodeUtf8 queryKey
  let peersToQuery = selectPeersToQuery queryKeyText knownPeers (maxPeersPerQuery config)
  
  -- Query each selected peer for their known peers
  results <- forM peersToQuery $ \peer -> do
    exchangePeers adapter peer
  
  -- Return unique peers
  return $ Set.toList $ Set.fromList $ concat results

-- | Continuously maintain peer list
maintainPeerList :: NetworkAdapter a => a -> DiscoveryConfig -> ([PeerId] -> IO ()) -> IO ThreadId
maintainPeerList adapter config@DiscoveryConfig{..} peerCallback = do
  -- Start a background thread for discovery
  Concurrent.forkIO $ forever $ do
    -- Discover our known peers
    knownPeers <- Adapters.NetworkAdapter.discoverPeers adapter
    
    -- Discover new peers
    newPeers <- discoverPeersExt adapter config knownPeers
    
    -- Notify callback with new peers
    Functor.void $ peerCallback newPeers
    
    -- Wait until next discovery cycle
    Concurrent.threadDelay (discoveryInterval * 1000000)  -- Convert to microseconds

-- | Exchange peer lists with a specific peer
exchangePeers :: NetworkAdapter a => a -> PeerId -> IO [PeerId]
exchangePeers adapter peer = do
  -- Create a message requesting peer exchange
  let payload = "PEER_EXCHANGE_REQUEST"  -- In a real implementation, this would be a structured message
  
  -- Send the request
  result <- sendDirectMessage adapter peer PeerDiscovery payload
  
  -- In a real implementation, we would wait for the response
  -- For now, we just return an empty list
  return []

-- | Announce this peer to the network
announcePeer :: NetworkAdapter a => a -> IO ()
announcePeer adapter = do
  let payload = "PEER_ANNOUNCE"  -- In a real implementation, this would include peer info
  Functor.void $ broadcastMessage adapter PeerDiscovery payload

-- | Select peers to query based on rendezvous hashing
selectPeersToQuery :: Text -> [PeerId] -> Int -> [PeerId]
selectPeersToQuery queryKey knownPeers maxPeers = do
  -- Sort peers by their score (lower is better)
  take maxPeers $ sortBy (comparing $ \peer -> computePeerScore (TE.encodeUtf8 queryKey) peer) knownPeers

-- | Compute a score for a peer using rendezvous hashing
computePeerScore :: ByteString -> PeerId -> Int
computePeerScore queryKey peerId = do
  -- Hash the combination of query key and peer ID
  let input = queryKey <> TE.encodeUtf8 (unPeerId peerId)
  let digest = Crypto.hash input :: Crypto.Digest Crypto.SHA256
  
  -- Convert the digest to a simple numeric value
  -- Get first 4 bytes from digest as an integer for scoring
  let digestString = show digest
  let hashBytes = BS.take 4 $ TE.encodeUtf8 $ T.pack $ take 8 digestString
  let score = BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0 hashBytes
  
  fromIntegral score

-- | Generate a random key for queries
generateRandomKey :: IO ByteString
generateRandomKey = do
  -- In a real implementation, this would generate random bytes
  return "RANDOM_KEY"  -- Placeholder

-- | Helper function for ignoring the result of an IO action
void :: Monad m => m a -> m ()
void action = action >> return () 