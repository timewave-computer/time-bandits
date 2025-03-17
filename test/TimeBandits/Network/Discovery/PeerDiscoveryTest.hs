{-# LANGUAGE OverloadedStrings #-}

module TimeBandits.Network.Discovery.PeerDiscoveryTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent (threadDelay, newEmptyMVar, putMVar, takeMVar, forkIO)
import Control.Monad (forM_, replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

import TimeBandits.Core.Log
import Core.Hashing
import Network.Discovery.PeerDiscovery

-- | Main test group
tests :: TestTree
tests = testGroup "Peer Discovery Tests"
  [ testCase "Select peers using rendezvous hashing" testSelectPeers
  , testCase "Add bootstrap peers" testAddBootstrapPeers
  , testCase "Discover peers with bootstrap" testDiscoverPeersWithBootstrap
  , testCase "Maintain peer list" testMaintainPeerList
  , testCase "Score nodes for query" testScoreNodesForQuery
  ]

-- | Create test peers
createTestPeers :: Int -> IO [PeerId]
createTestPeers count = do
  now <- getCurrentTime
  return $ map (\i -> PeerId (T.pack $ "peer-" ++ show i) Nothing (Just now)) [1..count]

-- | Create a test discovery config
createTestConfig :: [PeerId] -> LogStore -> DiscoveryConfig
createTestConfig bootstraps store = DiscoveryConfig
  { bootstrapPeers = bootstraps
  , queryInterval = 100  -- fast for testing
  , peerTimeout = 5000
  , maxKnownPeers = 100
  , maxPeersPerQuery = 3
  , logStore = store
  }

-- | Test selecting peers using rendezvous hashing
testSelectPeers :: Assertion
testSelectPeers = do
  -- Create test peers
  peers <- createTestPeers 10
  
  -- Create a query key
  let queryKey1 = C8.pack "test-query-1"
      queryKey2 = C8.pack "test-query-2"
  
  -- Select peers for the first query
  let selected1 = selectPeersToQuery peers queryKey1 3
  
  -- Select peers for the second query
  let selected2 = selectPeersToQuery peers queryKey2 3
  
  -- Verify we got the right number
  length selected1 @?= 3
  length selected2 @?= 3
  
  -- Run the same query again and verify determinism
  let selected1Again = selectPeersToQuery peers queryKey1 3
  
  -- The same query should select the same peers
  selected1 @?= selected1Again
  
  -- Different queries should select different peers
  selected1 /= selected2 @? "Different queries should select different peers"

-- | Test adding bootstrap peers
testAddBootstrapPeers :: Assertion
testAddBootstrapPeers = do
  -- Create test peers
  knownPeers <- createTestPeers 5
  bootstraps <- createTestPeers 3
  
  -- Add bootstrap peers
  let combined = addBootstrapPeers knownPeers bootstraps
  
  -- Verify the result
  length combined @?= 8  -- Should have all 8 peers
  
  -- Add the same bootstrap peers again
  let combinedAgain = addBootstrapPeers combined bootstraps
  
  -- Should still have 8 peers (no duplicates)
  length combinedAgain @?= 8

-- | Test discovering peers with bootstrap peers
testDiscoverPeersWithBootstrap :: Assertion
testDiscoverPeersWithBootstrap = withSystemLogging $ \store -> do
  -- Create bootstrap peers
  bootstraps <- createTestPeers 3
  
  -- Create a discovery config
  let config = createTestConfig bootstraps store
  
  -- Discover peers with no known peers (should use bootstrap)
  discovered <- discoverPeers config []
  
  -- Should have discovered peers
  (not $ null discovered) @? "Should discover peers using bootstrap"
  
  -- Try again with known peers
  morePeers <- discoverPeers config discovered
  
  -- Should have discovered even more peers
  length morePeers >= length discovered @? "Should discover more peers"

-- | Test maintaining a peer list
testMaintainPeerList :: Assertion
testMaintainPeerList = withSystemLogging $ \store -> do
  -- Create bootstrap peers
  bootstraps <- createTestPeers 3
  
  -- Create a discovery config with a short interval for testing
  let config = createTestConfig bootstraps store
                 { queryInterval = 100 }  -- 100ms
  
  -- Create an MVar to collect peers from the callback
  collectedPeers <- newEmptyMVar
  
  -- Create a callback that puts peers into the MVar
  let callback peer = do
        peers <- takeMVar collectedPeers
        putMVar collectedPeers (peer : peers)
  
  -- Initialize the MVar
  putMVar collectedPeers []
  
  -- Start the peer list maintenance
  threadId <- maintainPeerList config callback
  
  -- Wait a bit for it to run
  threadDelay 500000  -- 500ms
  
  -- Get the collected peers
  peers <- takeMVar collectedPeers
  
  -- Should have collected some peers
  (not $ null peers) @? "Should collect peers from maintenance"

-- | Test scoring nodes for a query
testScoreNodesForQuery :: Assertion
testScoreNodesForQuery = do
  -- Create test peers
  peers <- createTestPeers 5
  
  -- Create a query key
  let queryKey = C8.pack "test-query"
  
  -- Score the peers
  let scores = scoreNodesForQuery peers queryKey
  
  -- Verify we have scores for all peers
  length scores @?= 5
  
  -- Each score should be between 0 and 1
  forM_ scores $ \(_, score) -> do
    score >= 0 && score <= 1 @? "Score should be between 0 and 1"
  
  -- Scores should vary
  let allSame = all (\(_, s) -> s == snd (head scores)) scores
  (not allSame) @? "Not all scores should be the same" 