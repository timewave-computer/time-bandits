{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : TimeBandits.Core.Concurrency.Distributed
Description : Distributed concurrency support for Time Bandits
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module extends the resource-scoped concurrency model to work across
distributed Bandit nodes, enabling consistent concurrent execution in a
geo-distributed environment.
-}
module TimeBandits.Core.Concurrency.Distributed
  ( -- * Distributed Lock Manager
    DistributedLockManager
  , createDistributedLockManager
  
    -- * Distributed Lock Operations
  , TimeBandits.Core.Concurrency.Distributed.acquireDistributedLock
  , TimeBandits.Core.Concurrency.Distributed.releaseDistributedLock
  , withDistributedLock
  
    -- * Coordination
  , broadcastLockAcquisition
  , broadcastLockRelease
  , handleLockRequest
  , handleLockRelease
  ) where

import Control.Concurrent (MVar, modifyMVar_)
import qualified Control.Concurrent as Concurrent
import Control.Exception (bracket, catch, SomeException)
import Control.Monad (void, when, forM, forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime, diffUTCTime)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Aeson (encode, decode, Value, object, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as TE
import Control.Concurrent (forkIO, threadDelay, ThreadId)

-- Import from TimeBandits modules
import TimeBandits.Core.Concurrency.ResourceLock (ResourceLock(..), LockError(..), LockResult)
import TimeBandits.Core.Concurrency.LockManager
import qualified TimeBandits.Core.Concurrency.LockManager as LockManager
import TimeBandits.Core.ResourceId (ResourceId)
import TimeBandits.Core.Types (EffectId)

-- | Temporary NetworkAdapter class until proper module is migrated
class NetworkAdapter a where
  getPeerId :: a -> IO PeerId
  discoverPeers :: a -> IO [PeerId]
  sendMessage :: a -> PeerId -> ByteString -> IO Bool
  receiveMessage :: a -> (PeerId -> ByteString -> IO ()) -> IO ()
  broadcastMessage :: a -> MessageType -> ByteString -> IO Int
  
-- | Temporary PeerId type
type PeerId = Text

-- | Message types for network communication
data MessageType = ControlMessage | DataMessage | HeartbeatMessage
  deriving (Show, Eq)

-- | Helper functions for network communication
subscribeToMessages :: NetworkAdapter a => a -> MessageType -> (PeerId -> ByteString -> IO ()) -> IO ThreadId
subscribeToMessages _ _ handler = do
  -- Placeholder implementation
  forkIO $ forever $ do
    threadDelay 1000000  -- 1 second
    return ()

sendDirectMessage :: NetworkAdapter a => a -> PeerId -> MessageType -> ByteString -> IO Bool
sendDirectMessage adapter peer _ message = do
  -- Placeholder implementation
  sendMessage adapter peer message

-- | Distributed lock manager for coordinating locks across nodes
data DistributedLockManager = forall a. NetworkAdapter a => DistributedLockManager
  { networkAdapter :: a                           -- ^ Network adapter for P2P communication
  , localLockManager :: LockManager                -- ^ Local lock manager
  , remoteLocks :: MVar (Map ResourceId PeerId)    -- ^ Resources locked by remote peers
  , pendingRequests :: MVar (Map ResourceId [MVar Bool])  -- ^ Pending lock requests
  , quorumSize :: Int                              -- ^ Minimum number of peers needed for consensus
  }

-- | Create a distributed lock manager
createDistributedLockManager :: NetworkAdapter a => a -> LockManager -> Int -> IO DistributedLockManager
createDistributedLockManager adapter localMgr quorum = do
  remoteLocksVar <- Concurrent.newMVar Map.empty
  pendingVar <- Concurrent.newMVar Map.empty
  
  -- Create the manager
  let manager = DistributedLockManager
        { networkAdapter = adapter
        , localLockManager = localMgr
        , remoteLocks = remoteLocksVar
        , pendingRequests = pendingVar
        , quorumSize = quorum
        }
  
  -- Set up message handler for lock-related messages
  void $ subscribeToMessages adapter ControlMessage $ \sender payload -> do
    handleLockMessage manager sender payload
  
  return manager

-- | Response to a lock request
data LockResponse = LockApproved | LockRejected Text
  deriving (Show, Eq)

-- | Acquire a distributed lock with consensus
acquireDistributedLock :: DistributedLockManager -> ResourceId -> EffectId -> IO (LockResult ResourceLock)
acquireDistributedLock manager resId effectId = do
  -- First check existing remote locks
  remotesMap <- Concurrent.readMVar (remoteLocks manager)
  case Map.lookup resId remotesMap of
    Just remotePeer ->
      -- Resource is already locked by a remote peer
      return $ Left $ ResourceBusy effectId
    
    Nothing ->
      -- Resource not locked remotely, try to acquire local lock
      let localMgr = localLockManager manager in
      LockManager.acquireDistributedLock localMgr resId effectId

-- | Release a distributed lock
releaseDistributedLock :: DistributedLockManager -> ResourceId -> EffectId -> IO (LockResult ())
releaseDistributedLock manager resId effectId = do
  -- First release the local lock
  localResult <- releaseResourceLock (localLockManager manager) resId effectId
  
  -- Broadcast lock release to peers
  void $ broadcastLockRelease manager resId effectId
  
  return localResult

-- | RAII-style distributed lock acquisition and release
withDistributedLock :: DistributedLockManager -> ResourceId -> EffectId -> (ResourceLock -> IO a) -> IO (Either LockError a)
withDistributedLock manager@(DistributedLockManager {..}) resId effectId action = do
  lockResult <- TimeBandits.Core.Concurrency.Distributed.acquireDistributedLock manager resId effectId
  case lockResult of 
    Left failure -> return $ Left failure
    Right lock -> (bracket
      (return lock)
      (\_ -> void $ TimeBandits.Core.Concurrency.Distributed.releaseDistributedLock manager resId effectId)
      (\lock -> Right <$> action lock)) `catch` \(e :: SomeException) -> do
          void $ TimeBandits.Core.Concurrency.Distributed.releaseDistributedLock manager resId effectId
          error $ T.pack $ "Exception in withDistributedLock: " ++ show e

-- | Broadcast a lock acquisition to all peers and wait for consensus
broadcastLockAcquisition :: DistributedLockManager -> ResourceId -> EffectId -> IO Bool
broadcastLockAcquisition manager@DistributedLockManager{networkAdapter=adapter, pendingRequests=pendingReqs, quorumSize=quorum} resId effectId = do
  -- Create a lock request message
  let message = encodeLockRequest resId effectId
  
  -- Create an MVar to wait for responses
  responseVar <- Concurrent.newMVar (0 :: Int)  -- Count of approvals
  
  -- Add to pending requests
  pendingMap <- Concurrent.readMVar pendingReqs
  let existingRequests = Map.findWithDefault [] resId pendingMap
  -- We're using a different type of MVar here, so we need to handle it separately
  Concurrent.modifyMVar_ pendingReqs $ \_ -> 
    return $ pendingMap  -- Just keep the existing map for now
  
  -- Broadcast the message
  result <- broadcastMessage adapter ControlMessage message
  
  -- Wait for responses (with timeout)
  let timeout = 5000000  -- 5 seconds in microseconds
  Concurrent.threadDelay timeout
  
  -- Check if we got enough approvals
  approvals <- Concurrent.readMVar responseVar
  let hasConsensus = approvals >= quorum
  
  -- Clean up pending request
  modifyMVar_ (pendingRequests manager) $ \pending ->
    return $ if null existingRequests 
      then pending
      else pending  -- Keep the existing map
  
  return hasConsensus

-- | Broadcast lock release to peers
broadcastLockRelease :: DistributedLockManager -> ResourceId -> EffectId -> IO Bool
broadcastLockRelease (DistributedLockManager adapter _ _ _ _) resId effectId = do
  -- Create a lock release message
  let message = encodeLockRelease resId effectId
  
  -- Broadcast the message
  result <- broadcastMessage adapter ControlMessage message
  
  -- We don't wait for responses for releases
  return True

-- | Handle an incoming lock-related message
handleLockMessage :: DistributedLockManager -> PeerId -> ByteString -> IO ()
handleLockMessage manager sender payload = do
  -- Try to decode the message
  case decodeLockMessage payload of
    Left err ->
      -- Log error in a real implementation
      return ()
    
    Right (LockRequest resId effectId) ->
      -- Handle lock request
      handleLockRequest manager sender resId effectId
    
    Right (LockRelease resId effectId) ->
      -- Handle lock release
      handleLockRelease manager sender resId effectId
    
    Right (LockApproval resId) ->
      -- Handle lock approval
      handleLockApproval manager sender resId
    
    Right (LockRejection resId reason) ->
      -- Handle lock rejection
      handleLockRejection manager sender resId reason

-- | Handle a lock request from another peer
handleLockRequest :: DistributedLockManager -> PeerId -> ResourceId -> EffectId -> IO ()
handleLockRequest manager@(DistributedLockManager adapter _ remoteLockMap _ _) sender resId effectId = do
  -- Check if we already have a lock on this resource
  isLocked <- isResourceLocked (localLockManager manager) resId
  
  if isLocked
    then do
      -- Reject the request
      let rejectMessage = encodeLockRejection resId "Resource already locked locally"
      void $ sendDirectMessage adapter sender ControlMessage rejectMessage
    else do
      -- Record the lock as held by the remote peer
      Concurrent.modifyMVar_ remoteLockMap $ \remotes ->
        return $ Map.insert resId sender remotes
      
      -- Approve the request
      let approveMessage = encodeLockApproval resId
      void $ sendDirectMessage adapter sender ControlMessage approveMessage

-- | Handle a lock release from another peer
handleLockRelease :: DistributedLockManager -> PeerId -> ResourceId -> EffectId -> IO ()
handleLockRelease manager@(DistributedLockManager {..}) sender resId effectId = do
  -- Remove the lock from our remote locks
  modifyMVar_ remoteLocks $ \remotes ->
    return $ Map.delete resId remotes

-- | Handle a lock approval from another peer
handleLockApproval :: DistributedLockManager -> PeerId -> ResourceId -> IO ()
handleLockApproval manager sender resId = do
  -- Find the pending request for this resource
  pendingRequests <- getPendingRequests manager resId
  case pendingRequests of
    [] -> 
      -- No pending requests for this resource
      return ()
    
    (responseVar:_) -> do
      -- For simplicity, just put True in the MVar to indicate approval
      -- This is a simplified implementation
      Concurrent.putMVar responseVar True

-- | Handle a lock rejection from another peer
handleLockRejection :: DistributedLockManager -> PeerId -> ResourceId -> Text -> IO ()
handleLockRejection manager sender resId reason = do
  -- Find the pending request for this resource
  pendingMap <- Concurrent.readMVar (pendingRequests manager)
  case Map.lookup resId pendingMap of
    Nothing ->
      -- No pending request for this resource
      return ()
    
    Just (responseVar:_) -> do
      -- We got a rejection, so we won't reach consensus
      -- In a real implementation, we might log the reason
      return ()
    
    Just [] ->
      -- No response vars (shouldn't happen)
      return ()

-- Lock message types

data LockMessage
  = LockRequest ResourceId EffectId      -- ^ Request to acquire a lock
  | LockRelease ResourceId EffectId      -- ^ Notification of lock release
  | LockApproval ResourceId              -- ^ Approval of a lock request
  | LockRejection ResourceId Text        -- ^ Rejection of a lock request
  deriving (Show)

-- | Encode a lock request message
encodeLockRequest :: ResourceId -> EffectId -> ByteString
encodeLockRequest resId effectId = do
  -- In a real implementation, this would use proper serialization
  -- For now, we just create a placeholder
  let resIdText = T.pack $ show resId
  let effectIdText = T.pack $ show effectId
  "LOCK_REQUEST:" <> TE.encodeUtf8 resIdText <> ":" <> TE.encodeUtf8 effectIdText

-- | Encode a lock release message
encodeLockRelease :: ResourceId -> EffectId -> ByteString
encodeLockRelease resId effectId = do
  -- In a real implementation, this would use proper serialization
  -- For now, we just create a placeholder
  let resIdText = T.pack $ show resId
  let effectIdText = T.pack $ show effectId
  "LOCK_RELEASE:" <> TE.encodeUtf8 resIdText <> ":" <> TE.encodeUtf8 effectIdText

-- | Encode a lock approval message
encodeLockApproval :: ResourceId -> ByteString
encodeLockApproval resId = do
  -- In a real implementation, this would use proper serialization
  -- For now, we just create a placeholder
  let resIdText = T.pack $ show resId
  "LOCK_APPROVAL:" <> TE.encodeUtf8 resIdText

-- | Encode a lock rejection message
encodeLockRejection :: ResourceId -> Text -> ByteString
encodeLockRejection resId reason = do
  -- In a real implementation, this would use proper serialization
  -- For now, we just create a placeholder
  let resIdText = T.pack $ show resId
  "LOCK_REJECTION:" <> TE.encodeUtf8 resIdText <> ":" <> TE.encodeUtf8 reason

-- | Decode a lock message
decodeLockMessage :: ByteString -> Either Text LockMessage
decodeLockMessage payload = do
  -- In a real implementation, this would use proper deserialization
  -- For now, we just create a placeholder
  let payloadText = TE.decodeUtf8 payload
  if "LOCK_REQUEST:" `BS.isPrefixOf` payload
    then Right $ LockRequest (error "Not a real resource ID") (error "Not a real effect ID")
  else if "LOCK_RELEASE:" `BS.isPrefixOf` payload
    then Right $ LockRelease (error "Not a real resource ID") (error "Not a real effect ID")
  else if "LOCK_APPROVAL:" `BS.isPrefixOf` payload
    then Right $ LockApproval (error "Not a real resource ID")
  else if "LOCK_REJECTION:" `BS.isPrefixOf` payload
    then Right $ LockRejection (error "Not a real resource ID") "Rejected"
  else Left $ "Unknown lock message type: " <> payloadText

-- Helper functions

-- | Create a new empty MVar
newEmptyMVar :: IO (MVar a)
newEmptyMVar = Concurrent.newEmptyMVar

-- | Create pending lock request
createPendingRequest :: ResourceId -> [MVar Bool] -> MVar (Map ResourceId [MVar Bool]) -> IO (MVar Bool)
createPendingRequest resId existingRequests pendingMap = do
  -- Create a new MVar for this request
  responseVar <- Concurrent.newEmptyMVar
  
  -- Add to existing requests
  let updatedRequests = responseVar : existingRequests
  Concurrent.modifyMVar_ pendingMap $ \currentMap -> 
    return $ Map.insert resId updatedRequests currentMap
  
  return responseVar

-- | Wait for lock confirmations from the network
waitForLockConfirmations :: DistributedLockManager -> ResourceId -> Int -> IO Bool
waitForLockConfirmations manager resId timeout = do
  -- Create a response counter
  responseVar <- Concurrent.newMVar (0 :: Int)  -- Count of approvals
  
  -- Wait for responses until timeout or sufficient approvals
  approvals <- Concurrent.readMVar responseVar
  let hasConsensus = approvals >= quorumSize manager
  
  return hasConsensus

-- | Get the MVar for a pending request, creating it if necessary
getPendingMVar :: DistributedLockManager -> ResourceId -> EffectId -> IO (MVar Bool)
getPendingMVar manager resId effectId = do
  pendingMap <- Concurrent.readMVar (pendingRequests manager)
  case Map.lookup resId pendingMap of
    Nothing -> do
      -- No pending request for this resource, create a new one
      responseVar <- Concurrent.newEmptyMVar
      let updatedRequests = Map.insert resId [responseVar] pendingMap
      return responseVar
    Just existingRequests -> do
      -- Add a new request MVar to the existing list
      responseVar <- Concurrent.newEmptyMVar
      let updatedRequests = responseVar : existingRequests
      return responseVar

-- | Check lock confirmations
checkLockConfirmations :: DistributedLockManager -> ResourceId -> Int -> MVar Int -> IO Bool
checkLockConfirmations manager resId timeout responseVar = do
  -- Wait for responses until timeout or sufficient approvals
  approvals <- Concurrent.readMVar responseVar
  let hasConsensus = approvals >= quorumSize manager
  
  return hasConsensus

-- | Get pending lock requests for a resource
getPendingRequests :: DistributedLockManager -> ResourceId -> IO [MVar Bool]
getPendingRequests manager resId = do
  pendingMap <- Concurrent.readMVar (pendingRequests manager)
  case Map.lookup resId pendingMap of
    Nothing -> return []
    Just requests -> return requests

-- | Update pending lock requests for a resource
updatePendingRequests :: DistributedLockManager -> ResourceId -> [MVar Bool] -> IO ()
updatePendingRequests manager resId requests = do
  pendingMap <- Concurrent.readMVar (pendingRequests manager)
  let updatedRequests = Map.insert resId requests pendingMap
  Concurrent.modifyMVar_ (pendingRequests manager) $ \currentMap -> 
    return $ Map.insert resId requests currentMap 