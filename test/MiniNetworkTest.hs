{-# LANGUAGE OverloadedStrings #-}

-- A minimal standalone test for Network peer discovery, with minimal dependencies

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sortOn)
import Data.Word (Word32)
import Data.Maybe (catMaybes)
import Data.Bits (xor)

-- Simplified Network types
newtype PeerId = PeerId Text
  deriving (Show, Eq, Ord)

data PeerInfo = PeerInfo
  { peerId :: PeerId
  , peerAddress :: Text
  , peerLastSeen :: Int  -- Unix timestamp
  , peerScore :: Double  -- Reliability score
  } deriving (Show, Eq)

data DiscoveryConfig = DiscoveryConfig
  { bootstrapNodes :: [PeerInfo]
  , maxPeers :: Int
  , minScore :: Double
  , queryTimeout :: Int  -- seconds
  } deriving (Show, Eq)

-- Simplified hash function for rendezvous hashing
hashPeer :: PeerId -> Word32
hashPeer (PeerId id) = 
  -- Simple hash function for testing only
  fromIntegral $ sum $ map fromEnum $ T.unpack id

-- Simplified rendezvous hashing
scoreForRanking :: PeerId -> PeerId -> Double
scoreForRanking targetId peerId =
  let h1 = hashPeer targetId
      h2 = hashPeer peerId
  in 1.0 / fromIntegral (h1 `xor` h2 + 1)

-- Simplified peer selection based on rendezvous hashing
selectPeersToQuery :: PeerId -> [PeerInfo] -> Int -> [PeerInfo]
selectPeersToQuery targetId peers maxCount =
  let withScores = [(peer, scoreForRanking targetId (peerId peer)) | peer <- peers]
      sorted = sortOn (negate . snd) withScores  -- Sort by score descending
      selected = take maxCount sorted
  in map fst selected  -- Return the peers without scores

-- Simplified function to update known peers
updateKnownPeers :: Map PeerId PeerInfo -> [PeerInfo] -> Map PeerId PeerInfo
updateKnownPeers knownPeers newPeers =
  -- In a real implementation, this would merge the lists more intelligently
  -- updating timestamps and scores for existing peers
  foldl (\m p -> Map.insert (peerId p) p m) knownPeers newPeers

-- Test peer discovery
testPeerSelection :: IO ()
testPeerSelection = do
  putStrLn "Testing peer selection using rendezvous hashing..."
  
  let peers = 
        [ PeerInfo (PeerId "peer1") "192.168.1.1:8000" 1615000000 0.95
        , PeerInfo (PeerId "peer2") "192.168.1.2:8000" 1615000100 0.92
        , PeerInfo (PeerId "peer3") "192.168.1.3:8000" 1615000200 0.88
        , PeerInfo (PeerId "peer4") "192.168.1.4:8000" 1615000300 0.91
        , PeerInfo (PeerId "peer5") "192.168.1.5:8000" 1615000400 0.94
        ]
        
  let targetId = PeerId "target123"
  let selected = selectPeersToQuery targetId peers 3
  
  putStrLn $ "Target peer: " ++ show targetId
  putStrLn $ "Selected " ++ show (length selected) ++ " peers:"
  mapM_ (\p -> putStrLn $ "  " ++ T.unpack (peerAddress p)) selected
  
  if length selected == 3
    then putStrLn "Correct number of peers selected!"
    else putStrLn $ "ERROR: Expected 3 peers, but got " ++ show (length selected)

testPeerUpdates :: IO ()
testPeerUpdates = do
  putStrLn "\nTesting peer list updates..."
  
  let initialPeers = Map.fromList
        [ (PeerId "peer1", PeerInfo (PeerId "peer1") "192.168.1.1:8000" 1615000000 0.90)
        , (PeerId "peer2", PeerInfo (PeerId "peer2") "192.168.1.2:8000" 1615000100 0.85)
        ]
        
  let newPeers = 
        [ PeerInfo (PeerId "peer2") "192.168.1.2:8000" 1615001000 0.95  -- Updated
        , PeerInfo (PeerId "peer3") "192.168.1.3:8000" 1615001100 0.88  -- New
        ]
        
  let updatedPeers = updateKnownPeers initialPeers newPeers
  
  putStrLn $ "Initial peer count: " ++ show (Map.size initialPeers)
  putStrLn $ "Updated peer count: " ++ show (Map.size updatedPeers)
  
  if Map.size updatedPeers == 3
    then putStrLn "Correct number of peers after update!"
    else putStrLn $ "ERROR: Expected 3 peers, but got " ++ show (Map.size updatedPeers)

main :: IO ()
main = do
  putStrLn "Running simplified Network peer discovery tests"
  testPeerSelection
  testPeerUpdates 