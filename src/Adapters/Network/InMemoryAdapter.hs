{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Adapters.Network.InMemoryAdapter
Description : In-memory implementation of NetworkAdapter for testing
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides an in-memory implementation of the NetworkAdapter interface
for testing and local simulations. It simulates a network environment using
in-memory channels and data structures.
-}
module Adapters.Network.InMemoryAdapter
  ( -- * Adapter Creation
    createInMemoryAdapter
  , InMemoryAdapter
    
    -- * Configuration
  , InMemoryConfig(..)
  , defaultInMemoryConfig
  
    -- * Testing Utilities
  , getActivePeers
  , getMessageHistory
  , simulatePeerFailure
  , simulatePeerRecovery
  ) where

import Control.Concurrent (MVar, ThreadId, forkIO, killThread, newMVar, putMVar, readMVar, modifyMVar, modifyMVar_, takeMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (catch, SomeException)
import Control.Monad (forM, forM_, forever, void, when)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import System.Random (randomIO, randomRIO)

-- Import from TimeBandits modules
import Adapters.NetworkAdapter
import Types.Network

-- | In-memory adapter state
data InMemoryAdapter = InMemoryAdapter
  { peers :: MVar (Map PeerId PeerChannel)  -- ^ Active peers and their channels
  , subscriptions :: MVar (Map SubscriptionId (MessageType, PeerId -> ByteString -> IO ()))  -- ^ Message subscriptions
  , messageHistory :: MVar [Message]  -- ^ History of messages for testing
  , config :: InMemoryConfig  -- ^ Adapter configuration
  , localPeerId :: PeerId  -- ^ ID of this node
  , running :: MVar Bool  -- ^ Whether the adapter is running
  , backgroundThreads :: MVar [ThreadId]  -- ^ Background processing threads
  }

-- | Communication channel for a peer
data PeerChannel = PeerChannel
  { peerId :: PeerId  -- ^ Peer ID
  , incoming :: Chan Message  -- ^ Channel for incoming messages
  , outgoing :: Chan Message  -- ^ Channel for outgoing messages
  , status :: MVar PeerStatus  -- ^ Current peer status
  , lastSeen :: MVar UTCTime  -- ^ Last time the peer was seen
  }

-- | Configuration for the in-memory adapter
data InMemoryConfig = InMemoryConfig
  { maxPeers :: Int  -- ^ Maximum number of connected peers
  , messageDelayMillis :: Int  -- ^ Simulated message delay in milliseconds
  , dropMessages :: Double  -- ^ Percentage of messages to drop (0.0-1.0)
  , peerFailureProbability :: Double  -- ^ Probability of random peer failure (0.0-1.0)
  }

-- | Default configuration for in-memory adapter
defaultInMemoryConfig :: InMemoryConfig
defaultInMemoryConfig = InMemoryConfig
  { maxPeers = 20
  , messageDelayMillis = 0
  , dropMessages = 0.0
  , peerFailureProbability = 0.0
  }

-- | Create a new in-memory network adapter
createInMemoryAdapter :: InMemoryConfig -> IO InMemoryAdapter
createInMemoryAdapter config = do
  peersVar <- newMVar Map.empty
  subsVar <- newMVar Map.empty
  historyVar <- newMVar []
  localId <- PeerId . pack . show <$> nextRandom
  runningVar <- newMVar True
  threadsVar <- newMVar []
  
  let adapter = InMemoryAdapter
        { peers = peersVar
        , subscriptions = subsVar
        , messageHistory = historyVar
        , config = config
        , localPeerId = localId
        , running = runningVar
        , backgroundThreads = threadsVar
        }
  
  return adapter

-- | Start the adapter
startAdapter :: InMemoryAdapter -> IO ()
startAdapter adapter = do
  isRunning <- readMVar (running adapter)
  when (not isRunning) $ do
    -- Mark as running
    modifyMVar_ (running adapter) (const $ return True)
    
    -- Start message processing thread
    processingThread <- forkIO $ messageProcessor adapter
    
    -- Start peer failure simulation thread if configured
    failureSimThread <- if peerFailureProbability (config adapter) > 0
                        then Just <$> forkIO (peerFailureSimulator adapter)
                        else return Nothing
    
    -- Record threads
    modifyMVar_ (backgroundThreads adapter) $ \threads ->
      return $ processingThread : (maybe threads (:threads) failureSimThread)

-- | Stop the adapter
stopAdapter :: InMemoryAdapter -> IO ()
stopAdapter adapter = do
  -- Mark as not running
  modifyMVar_ (running adapter) (const $ return False)
  
  -- Kill all background threads
  threads <- takeMVar (backgroundThreads adapter)
  mapM_ killThread threads
  putMVar (backgroundThreads adapter) []
  
  -- Disconnect from all peers
  peers <- takeMVar (peers adapter)
  forM_ (Map.elems peers) $ \channel -> do
    modifyMVar_ (status channel) (const $ return Disconnected)
  putMVar (peers adapter) Map.empty

-- | Internal message processor
messageProcessor :: InMemoryAdapter -> IO ()
messageProcessor adapter = forever $ do
  -- Check if we should continue running
  isRunning <- readMVar (running adapter)
  when (not isRunning) $ return ()
  
  -- Process messages from each peer
  peerMap <- readMVar (peers adapter)
  forM_ (Map.elems peerMap) $ \channel -> do
    -- Check if there are messages in the incoming channel
    -- We use tryReadChan (not blocking) in a real implementation
    catch (do
      message <- readChan (incoming channel)
      
      -- Record in history for testing
      modifyMVar_ (messageHistory adapter) $ \history ->
        return (message : history)
      
      -- Deliver to subscribers
      subs <- readMVar (subscriptions adapter)
      forM_ (Map.elems subs) $ \(msgType, callback) ->
        when (messageType message == msgType) $
          callback (sender message) (payload message)
      
    ) (\(_ :: SomeException) -> return ())

-- | Simulate peer failures if configured
peerFailureSimulator :: InMemoryAdapter -> IO ()
peerFailureSimulator adapter = forever $ do
  -- Check if we should continue running
  isRunning <- readMVar (running adapter)
  when (not isRunning) $ return ()
  
  -- Sleep for a random interval (1-10 seconds)
  sleepMillis <- randomRIO (1000, 10000) :: IO Int
  threadDelay (sleepMillis * 1000)
  
  -- Maybe cause a random peer failure
  failureRoll <- randomRIO (0.0, 1.0) :: IO Double
  when (failureRoll < peerFailureProbability (config adapter)) $ do
    peerMap <- readMVar (peers adapter)
    if Map.null peerMap
      then return ()
      else do
        -- Choose a random peer
        peerIndex <- randomRIO (0, Map.size peerMap - 1)
        let (peerId, channel) = Map.toList peerMap !! peerIndex
        
        -- Mark as unreachable
        modifyMVar_ (status channel) (const $ return Unreachable)

-- | Implements NetworkAdapter for InMemoryAdapter
instance NetworkAdapter InMemoryAdapter where
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
    
    -- Record in history
    modifyMVar_ (messageHistory adapter) $ \history ->
      return (message : history)
    
    -- Broadcast to all peers
    peerMap <- readMVar (peers adapter)
    forM_ (Map.elems peerMap) $ \channel -> do
      peerStatus <- readMVar (status channel)
      when (peerStatus == Connected) $ do
        -- Check if we should drop the message
        shouldDrop <- randomRIO (0.0, 1.0) :: IO Double
        when (shouldDrop > dropMessages (config adapter)) $ do
          -- Simulate network delay
          when (messageDelayMillis (config adapter) > 0) $
            threadDelay (messageDelayMillis (config adapter) * 1000)
          
          -- Send the message
          writeChan (outgoing channel) message
    
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
    
    -- Record in history
    modifyMVar_ (messageHistory adapter) $ \history ->
      return (message : history)
    
    -- Find the target peer
    peerMap <- readMVar (peers adapter)
    case Map.lookup targetPeerId peerMap of
      Nothing ->
        return $ Left $ PeerNotFound targetPeerId
        
      Just channel -> do
        peerStatus <- readMVar (status channel)
        if peerStatus /= Connected
          then return $ Left $ ConnectionError "Peer not connected"
          else do
            -- Check if we should drop the message
            shouldDrop <- randomRIO (0.0, 1.0) :: IO Double
            if shouldDrop < dropMessages (config adapter)
              then return $ Left $ MessageError "Message dropped (simulated)"
              else do
                -- Simulate network delay
                when (messageDelayMillis (config adapter) > 0) $
                  threadDelay (messageDelayMillis (config adapter) * 1000)
                
                -- Send the message
                writeChan (outgoing channel) message
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
    peerMap <- readMVar (peers adapter)
    return $ Map.keys peerMap
  
  getPeerStatus adapter peerId = do
    peerMap <- readMVar (peers adapter)
    case Map.lookup peerId peerMap of
      Nothing -> return Nothing
      Just channel -> do
        status <- readMVar (status channel)
        return $ Just status
  
  -- Lifecycle management
  startAdapter = startAdapter
  stopAdapter = stopAdapter

-- | Create a new peer channel
createPeerChannel :: PeerId -> IO PeerChannel
createPeerChannel id = do
  inChan <- newChan
  outChan <- newChan
  statusVar <- newMVar Connected
  now <- getCurrentTime
  lastSeenVar <- newMVar now
  return PeerChannel
    { peerId = id
    , incoming = inChan
    , outgoing = outChan
    , status = statusVar
    , lastSeen = lastSeenVar
    }

-- | Connect to a simulated peer
connectToPeer :: InMemoryAdapter -> PeerId -> IO (Either NetworkError ())
connectToPeer adapter peerId = do
  peerMap <- readMVar (peers adapter)
  
  -- Check if we're already at max peers
  when (Map.size peerMap >= maxPeers (config adapter)) $
    return $ Left $ ConnectionError "Maximum peer limit reached"
  
  -- Check if we're already connected to this peer
  case Map.lookup peerId peerMap of
    Just _ -> return $ Right ()  -- Already connected
    Nothing -> do
      -- Create a new peer channel
      channel <- createPeerChannel peerId
      
      -- Add to peer map
      modifyMVar_ (peers adapter) $ \peers ->
        return $ Map.insert peerId channel peers
      
      return $ Right ()

-- Testing utilities

-- | Get list of active peers (for testing)
getActivePeers :: InMemoryAdapter -> IO [PeerId]
getActivePeers adapter = do
  peerMap <- readMVar (peers adapter)
  connectedPeers <- forM (Map.toList peerMap) $ \(id, channel) -> do
    peerStatus <- readMVar (status channel)
    return $ if peerStatus == Connected then Just id else Nothing
  return $ catMaybes connectedPeers

-- | Get message history (for testing)
getMessageHistory :: InMemoryAdapter -> IO [Message]
getMessageHistory = readMVar . messageHistory

-- | Simulate a peer failure (for testing)
simulatePeerFailure :: InMemoryAdapter -> PeerId -> IO Bool
simulatePeerFailure adapter peerId = do
  peerMap <- readMVar (peers adapter)
  case Map.lookup peerId peerMap of
    Nothing -> return False
    Just channel -> do
      modifyMVar_ (status channel) (const $ return Unreachable)
      return True

-- | Simulate a peer recovery (for testing)
simulatePeerRecovery :: InMemoryAdapter -> PeerId -> IO Bool
simulatePeerRecovery adapter peerId = do
  peerMap <- readMVar (peers adapter)
  case Map.lookup peerId peerMap of
    Nothing -> return False
    Just channel -> do
      modifyMVar_ (status channel) (const $ return Connected)
      now <- getCurrentTime
      modifyMVar_ (lastSeen channel) (const $ return now)
      return True

-- Helper functions

-- | Thread delay in microseconds
threadDelay :: Int -> IO ()
threadDelay micros = return ()  -- In real code, this would use Control.Concurrent.threadDelay 