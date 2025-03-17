{-# LANGUAGE OverloadedStrings #-}

module TimeBandits.Network.ManagerTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent (threadDelay, newEmptyMVar, putMVar, takeMVar, forkIO)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import Control.Monad (forM_, void, replicateM, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Network.Socket (SockAddr(..))
import qualified Network.Socket as NS
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

import TimeBandits.Core.Log
import Network.Discovery.PeerDiscovery
import Network.Manager
import Network.Protocol.Version

-- | Main test group
tests :: TestTree
tests = testGroup "Network Manager Tests"
  [ testCase "Create and start network manager" testCreateAndStart
  , testCase "Connect to peers" testConnectToPeers
  , testCase "Send and receive messages" testSendMessages
  , testCase "Topic subscriptions" testSubscriptions
  , testCase "Peer discovery integration" testPeerDiscovery
  , testCase "Protocol versioning" testProtocolVersioning
  ]

-- | Create a mock socket address for testing
createMockSockAddr :: IO SockAddr
createMockSockAddr = do
  addr <- NS.inet_addr "127.0.0.1"
  return $ NS.SockAddrInet 8000 addr

-- | Create test peers
createTestPeers :: Int -> IO [PeerId]
createTestPeers count = do
  now <- getCurrentTime
  addr <- createMockSockAddr
  return $ map (\i -> PeerId (T.pack $ "peer-" ++ show i) (Just addr) (Just now)) [1..count]

-- | Create a test network configuration
createTestConfig :: [PeerId] -> LogStore -> IO NetworkConfig
createTestConfig bootstraps store = do
  addr <- createMockSockAddr
  return $ NetworkConfig
    { networkNodeId = "test-node"
    , networkListenAddr = addr
    , networkBootstrapPeers = bootstraps
    , networkMaxPeers = 10
    , networkLogStore = store
    , networkDiscoveryInterval = 100  -- fast for testing
    , networkProtocolVersion = currentVersion  -- Use the current protocol version
    }

-- | Test creating and starting the network manager
testCreateAndStart :: Assertion
testCreateAndStart = withSystemLogging $ \store -> do
  -- Create bootstrap peers
  bootstraps <- createTestPeers 3
  
  -- Create a network config
  config <- createTestConfig bootstraps store
  
  -- Create the network manager
  manager <- createNetworkManager config
  
  -- Start it
  startNetworkManager manager
  
  -- Wait a bit for it to start
  threadDelay 100000  -- 100ms
  
  -- Check that we have a discovery thread
  threadId <- readMVar (discoveryThread manager)
  assertBool "Should have a discovery thread" (threadId /= Nothing)
  
  -- Clean up
  stopNetworkManager manager

-- | Test connecting to peers
testConnectToPeers :: Assertion
testConnectToPeers = withSystemLogging $ \store -> do
  -- Create bootstrap peers
  bootstraps <- createTestPeers 3
  
  -- Create a network config
  config <- createTestConfig bootstraps store
  
  withNetworkManager config $ \manager -> do
    -- Try to connect to the bootstrap peers manually
    successes <- mapM (connectToPeer manager) bootstraps
    
    -- Most should succeed (but some might fail due to the simulation)
    let successCount = length $ filter id successes
    successCount > 0 @? "Should connect to at least one peer"
    
    -- List connected peers
    connectedPeers <- listConnectedPeers manager
    
    -- Should have connected to some peers
    length connectedPeers > 0 @? "Should have connected peers"
    
    -- Disconnect from a peer
    case connectedPeers of
      [] -> return ()  -- Shouldn't happen, but just in case
      (peer:_) -> do
        success <- disconnectPeer manager (unPeerId peer)
        success @? "Should disconnect successfully"
        
        -- Check it's disconnected
        remainingPeers <- listConnectedPeers manager
        length remainingPeers < length connectedPeers @? "Should have fewer connected peers"

-- | Test sending and receiving messages
testSendMessages :: Assertion
testSendMessages = withSystemLogging $ \store -> do
  -- Create bootstrap peers
  bootstraps <- createTestPeers 5
  
  -- Create a network config
  config <- createTestConfig bootstraps store
  
  withNetworkManager config $ \manager -> do
    -- Connect to the bootstrap peers manually
    mapM_ (connectToPeer manager) bootstraps
    
    -- List connected peers
    connectedPeers <- listConnectedPeers manager
    
    -- Should have connected to some peers
    length connectedPeers > 0 @? "Should have connected peers"
    
    -- We need to simulate peer subscriptions
    -- In a real system, peers would send subscription messages
    forM_ connectedPeers $ \peer -> do
      -- Simulate subscribing to topics
      let topics = ["topic1", "topic2"]
      
      -- Get the peer connection and update subscriptions
      mConn <- findPeerByID manager (unPeerId peer)
      case mConn of
        Nothing -> return ()
        Just conn -> do
          -- Modify the subscriptions
          -- Note: In a real system, this would happen via message exchange
          -- For testing, we're directly modifying the state
          modifyMVar_ (connectedPeers manager) $ \peers ->
            return $ Map.insert (unPeerId peer) (conn { peerSubscriptions = topics }) peers
    
    -- Try to send a message to the first peer
    case connectedPeers of
      [] -> return ()  -- Shouldn't happen, but just in case
      (peer:_) -> do
        -- Send a message
        success <- sendMessage manager (unPeerId peer) "topic1" "Hello, world!"
        success @? "Should send message successfully"
        
        -- Try a topic the peer isn't subscribed to
        badTopic <- sendMessage manager (unPeerId peer) "unknown-topic" "Should fail"
        not badTopic @? "Should fail to send to unsubscribed topic"
        
        -- Try to broadcast
        sentCount <- broadcastMessage manager "topic1" "Broadcast message"
        sentCount > 0 @? "Should broadcast to at least one peer"

-- | Test topic subscriptions
testSubscriptions :: Assertion
testSubscriptions = withSystemLogging $ \store -> do
  -- Create a network config with no bootstrap peers
  config <- createTestConfig [] store
  
  withNetworkManager config $ \manager -> do
    -- Create a message receiver
    receivedMessages <- newMVar ([] :: [NetworkMessage])
    
    -- Create a callback
    let callback msg = modifyMVar_ receivedMessages $ \msgs ->
          return (msg : msgs)
    
    -- Subscribe to a topic
    subscribe manager "test-topic" callback
    
    -- Try to broadcast (there are no peers, but it should work)
    sentCount <- broadcastMessage manager "test-topic" "Broadcast message"
    sentCount @?= 0  -- No peers to send to
    
    -- Unsubscribe
    unsubscribe manager "test-topic" callback
    
    -- Get the received messages (should be empty since we have no peers)
    messages <- readMVar receivedMessages
    null messages @? "Should have no messages"

-- | Test peer discovery integration
testPeerDiscovery :: Assertion
testPeerDiscovery = withSystemLogging $ \store -> do
  -- Create bootstrap peers
  bootstraps <- createTestPeers 3
  
  -- Create a network config
  config <- createTestConfig bootstraps store
  
  withNetworkManager config $ \manager -> do
    -- Wait for peer discovery to run
    threadDelay 500000  -- 500ms
    
    -- List connected peers
    connectedPeers <- listConnectedPeers manager
    
    -- Should have discovered and connected to some peers
    length connectedPeers > 0 @? "Should discover and connect to peers"

-- | Test protocol versioning
testProtocolVersioning :: Assertion
testProtocolVersioning = withSystemLogging $ \store -> do
  -- Create bootstrap peers
  bootstraps <- createTestPeers 5
  
  -- Create a network config
  config <- createTestConfig bootstraps store
  
  withNetworkManager config $ \manager -> do
    -- Connect to some peers (this will involve protocol version checking)
    results <- mapM (connectToPeer manager) bootstraps
    
    -- Some connections should succeed
    or results @? "At least one connection should succeed"
    
    -- Get connected peers
    connectedPeers <- listConnectedPeers manager
    when (not $ null connectedPeers) $ do
      -- Check the first connected peer
      let peerId = head connectedPeers
      mConn <- findPeerByID manager (unPeerId peerId)
      
      -- Verify protocol version is set
      case mConn of
        Nothing -> assertFailure "Should have found the peer connection"
        Just conn -> do
          -- The peer version should be present and compatible with ours
          let peerVersion = peerProtocolVersion conn
              ourVersion = networkProtocolVersion config
          
          majorVersion peerVersion @?= majorVersion ourVersion
          isCompatible ourVersion peerVersion @? "Peer should have compatible version" 