{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Network.Discovery.PeerDiscovery
Description : Peer discovery mechanism using rendezvous hashing
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module implements a peer discovery mechanism for the Time Bandits network
using rendezvous hashing for deterministic peer selection. It avoids the need
for a DHT while still providing consistent and deterministic peer discovery.
-}
module Network.Discovery.PeerDiscovery
  ( -- * Types
    DiscoveryConfig(..)
  , PeerId(..)
  
    -- * Discovery Operations
  , discoverPeers
  , maintainPeerList
  , selectPeersToQuery
  
    -- * Helper Functions
  , addBootstrapPeers
  , updateKnownPeers
  , scoreNodesForQuery
  ) where

import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import Control.Monad (forever, when, forM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Network.Socket (SockAddr)
import System.Random (randomRIO)

import Core.Log
import Core.Hashing (computeNodeScore)

-- | Peer identifier with network address
data PeerId = PeerId
  { unPeerId :: Text            -- ^ String representation of peer ID
  , peerAddr :: Maybe SockAddr  -- ^ Network address if known
  , lastSeen :: Maybe UTCTime   -- ^ When we last had contact with this peer
  } deriving (Eq, Generic)

instance Show PeerId where
  show (PeerId id _ _) = T.unpack id

instance ToJSON PeerId
instance FromJSON PeerId

-- | Configuration for peer discovery
data DiscoveryConfig = DiscoveryConfig
  { bootstrapPeers :: [PeerId]        -- ^ Initial peers to connect to
  , queryInterval :: Int              -- ^ Milliseconds between peer queries
  , peerTimeout :: Int                -- ^ Milliseconds before a peer is considered stale
  , maxKnownPeers :: Int              -- ^ Maximum number of peers to track
  , maxPeersPerQuery :: Int           -- ^ Maximum peers to query at once
  , logStore :: LogStore              -- ^ Log store for discovery events
  }

-- | Discover peers using the given configuration and existing peer list
discoverPeers :: DiscoveryConfig -> [PeerId] -> IO [PeerId]
discoverPeers config knownPeers = do
  -- Log the discovery attempt
  _ <- logComponentInfo (logStore config) "PeerDiscovery" $
    "Discovering peers from " <> T.pack (show (length knownPeers)) <> " known peers"
  
  -- If we have no peers, use bootstrap peers
  activePeers <- if null knownPeers
    then do
      _ <- logComponentInfo (logStore config) "PeerDiscovery" 
        "No known peers, using bootstrap peers"
      return $ bootstrapPeers config
    else return knownPeers
  
  -- If we still have no peers, return empty list
  when (null activePeers) $ do
    _ <- logComponentWarning (logStore config) "PeerDiscovery" 
      "No bootstrap peers configured"
    return []
  
  -- Create a random query key
  queryKey <- generateQueryKey
  
  -- Select peers to query based on the key
  let peersToQuery = selectPeersToQuery activePeers queryKey (maxPeersPerQuery config)
  
  -- Log the selected peers
  _ <- logComponentInfo (logStore config) "PeerDiscovery" $
    "Selected " <> T.pack (show (length peersToQuery)) <> " peers to query"
  
  -- Query each peer for their known peers
  -- In a real implementation, this would involve network communication
  -- Here we just simulate it
  now <- getCurrentTime
  newPeers <- forM peersToQuery $ \peer -> do
    -- Simulate getting peers from this peer
    simulatedPeers <- simulateGetPeersFromPeer peer
    
    -- Log the result
    _ <- logComponentInfo (logStore config) "PeerDiscovery" $
      "Got " <> T.pack (show (length simulatedPeers)) <> " peers from " <> unPeerId peer
    
    -- Update last seen timestamp
    let updatedPeer = peer { lastSeen = Just now }
    
    -- Return the peers we got plus the updated peer
    return $ updatedPeer : simulatedPeers
  
  -- Flatten and deduplicate the results
  let allPeers = concat newPeers
      uniquePeers = removeDuplicatePeers allPeers
      
  -- Return the unique peers, limited to maxKnownPeers
  return $ take (maxKnownPeers config) uniquePeers

-- | Maintain a peer list in the background
maintainPeerList :: DiscoveryConfig -> (PeerId -> IO ()) -> IO ThreadId
maintainPeerList config peerCallback = do
  -- Create an MVar to hold the known peers
  knownPeersMVar <- newMVar (bootstrapPeers config)
  
  -- Start a background thread
  forkIO $ forever $ do
    -- Read current known peers
    currentPeers <- readMVar knownPeersMVar
    
    -- Try to discover new peers
    newPeers <- discoverPeers config currentPeers
    
    -- Update the known peers list
    modifyMVar_ knownPeersMVar $ \_ -> return newPeers
    
    -- Call the callback for each new peer
    mapM_ peerCallback newPeers
    
    -- Wait for the next interval
    threadDelay (queryInterval config * 1000)  -- convert to microseconds

-- | Select a subset of peers to query using rendezvous hashing
selectPeersToQuery :: [PeerId] -> ByteString -> Int -> [PeerId]
selectPeersToQuery knownPeers queryKey maxPeers = 
  take maxPeers $ sortBy (comparing (computeScore queryKey)) knownPeers
  where
    computeScore key peer = computeNodeScore key (TE.encodeUtf8 $ unPeerId peer)

-- | Add bootstrap peers to the known peers list
addBootstrapPeers :: [PeerId] -> [PeerId] -> [PeerId]
addBootstrapPeers knownPeers bootstrapPeers = 
  removeDuplicatePeers (knownPeers ++ bootstrapPeers)

-- | Update the list of known peers with new discoveries
updateKnownPeers :: [PeerId] -> [PeerId] -> Int -> [PeerId]
updateKnownPeers knownPeers newPeers maxPeers = 
  take maxPeers $ removeDuplicatePeers (knownPeers ++ newPeers)

-- | Score nodes for a specific query using rendezvous hashing
scoreNodesForQuery :: [PeerId] -> ByteString -> [(PeerId, Double)]
scoreNodesForQuery peers queryKey = 
  map (\peer -> (peer, computeNodeScore queryKey (TE.encodeUtf8 $ unPeerId peer))) peers

-- | Generate a random query key
generateQueryKey :: IO ByteString
generateQueryKey = do
  -- Generate a random number and convert to string
  r <- randomRIO (1, 1000000 :: Int)
  return $ C8.pack $ "query-" ++ show r

-- | Remove duplicate peers based on peer ID
removeDuplicatePeers :: [PeerId] -> [PeerId]
removeDuplicatePeers = foldr addIfNotPresent []
  where
    addIfNotPresent peer acc = 
      if any (\p -> unPeerId p == unPeerId peer) acc
        then acc
        else peer : acc

-- | Simulate getting peers from a peer
-- In a real implementation, this would involve network communication
simulateGetPeersFromPeer :: PeerId -> IO [PeerId]
simulateGetPeersFromPeer peer = do
  -- Generate a random number of peers
  numPeers <- randomRIO (2, 5)
  
  -- Generate random peer IDs
  now <- getCurrentTime
  forM [1..numPeers] $ \i -> do
    let peerId = "peer-" <> unPeerId peer <> "-" <> T.pack (show i)
    return $ PeerId peerId Nothing (Just now) 