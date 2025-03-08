{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Core.Concurrency.Distributed
Description : Distributed concurrency support for Time Bandits
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module extends the resource-scoped concurrency model to work across
distributed Bandit nodes, enabling consistent concurrent execution in a
geo-distributed environment.
-}
module Core.Concurrency.Distributed
  ( -- * Distributed Lock Manager
    DistributedLockManager
  , createDistributedLockManager
  
    -- * Distributed Lock Operations
  , acquireDistributedLock
  , releaseDistributedLock
  , withDistributedLock
  
    -- * Coordination
  , broadcastLockAcquisition
  , broadcastLockRelease
  , handleLockRequest
  , handleLockRelease
  ) where

import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar, modifyMVar_, takeMVar, putMVar, threadDelay)
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

-- Import from TimeBandits modules
import Core.Concurrency.ResourceLock
import Core.Concurrency.LockManager
import Core.ResourceId (ResourceId)
import Core.Types (EffectId)
import Adapters.NetworkAdapter

-- | Distributed lock manager for coordinating locks across nodes
data DistributedLockManager = DistributedLockManager
  { networkAdapter :: NetworkAdapter               -- ^ Network adapter for P2P communication
  , localLockManager :: LockManager                -- ^ Local lock manager
  , remoteLocks :: MVar (Map ResourceId PeerId)    -- ^ Resources locked by remote peers
  , pendingRequests :: MVar (Map ResourceId [MVar Bool])  -- ^ Pending lock requests
  , quorumSize :: Int                              -- ^ Minimum number of peers needed for consensus
  }

-- | Create a distributed lock manager
createDistributedLockManager :: NetworkAdapter -> LockManager -> Int -> IO DistributedLockManager
createDistributedLockManager adapter localMgr quorum = do
  remoteLocksVar <- newMVar Map.empty
  pendingVar <- newMVar Map.empty
  
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

-- | Acquire a distributed lock
acquireDistributedLock :: DistributedLockManager -> ResourceId -> EffectId -> IO (LockResult ResourceLock)
acquireDistributedLock manager resId effectId = do
  -- First check if the resource is locked by a remote peer
  remotesMap <- readMVar (remoteLocks manager)
  case Map.lookup resId remotesMap of
    Just remotePeer ->
      -- Resource is locked by a remote peer
      return $ Left $ ResourceBusy effectId
    
    Nothing -> do
      -- Try to acquire the local lock
      localResult <- acquireResourceLock (localLockManager manager) resId effectId
      case localResult of
        Left err -> return $ Left err
        Right localLock -> do
          -- Broadcast lock acquisition to peers
          consensusResult <- broadcastLockAcquisition manager resId effectId
          
          if consensusResult
            then return $ Right localLock  -- We have consensus
            else do
              -- We didn't get consensus, release the local lock
              void $ releaseResourceLock (localLockManager manager) resId effectId
              return $ Left $ InternalLockError "Failed to acquire distributed lock consensus"

-- | Release a distributed lock
releaseDistributedLock :: DistributedLockManager -> ResourceId -> EffectId -> IO (LockResult ())
releaseDistributedLock manager resId effectId = do
  -- First release the local lock
  localResult <- releaseResourceLock (localLockManager manager) resId effectId
  
  -- Broadcast lock release to peers
  void $ broadcastLockRelease manager resId effectId
  
  return localResult

-- | Perform an operation with a distributed lock
withDistributedLock :: DistributedLockManager
                    -> ResourceId  -- ^ Resource to lock
                    -> EffectId    -- ^ Effect requesting the lock
                    -> IO a        -- ^ Operation to perform with lock
                    -> IO (LockResult a)
withDistributedLock manager resId effectId action = do
  -- Acquire the distributed lock
  lockResult <- acquireDistributedLock manager resId effectId
  case lockResult of
    Left err -> return $ Left err
    Right lock -> do
      -- Perform the action and release the lock
      result <- bracket
        (return ())  -- No additional setup
        (\_ -> releaseDistributedLock manager resId effectId >> return ())  -- Always release lock
        (\_ -> action)  -- Run the action
        `catch` \(e :: SomeException) -> do
          -- Release the lock on exception
          void $ releaseDistributedLock manager resId effectId
          error $ "Exception in withDistributedLock: " ++ show e
      
      return $ Right result

-- | Broadcast lock acquisition to peers
broadcastLockAcquisition :: DistributedLockManager -> ResourceId -> EffectId -> IO Bool
broadcastLockAcquisition manager resId effectId = do
  -- Create a lock request message
  let message = encodeLockRequest resId effectId
  
  -- Create an MVar to wait for responses
  responseVar <- newMVar (0 :: Int)  -- Count of approvals
  
  -- Add to pending requests
  modifyMVar_ (pendingRequests manager) $ \pending ->
    let existingRequests = Map.findWithDefault [] resId pending
        updatedRequests = newEmptyMVar : existingRequests
    in return $ Map.insert resId updatedRequests pending
  
  -- Broadcast the message
  result <- broadcastMessage (networkAdapter manager) ControlMessage message
  
  -- Wait for responses (with timeout)
  let timeout = 5000000  -- 5 seconds in microseconds
  threadDelay timeout
  
  -- Check if we got enough approvals
  approvals <- readMVar responseVar
  let hasConsensus = approvals >= quorumSize manager
  
  -- Clean up pending request
  modifyMVar_ (pendingRequests manager) $ \pending ->
    let existingRequests = Map.findWithDefault [] resId pending
        updatedRequests = tail existingRequests  -- Remove our request
    in if null updatedRequests
         then Map.delete resId pending
         else Map.insert resId updatedRequests pending
  
  return hasConsensus

-- | Broadcast lock release to peers
broadcastLockRelease :: DistributedLockManager -> ResourceId -> EffectId -> IO Bool
broadcastLockRelease manager resId effectId = do
  -- Create a lock release message
  let message = encodeLockRelease resId effectId
  
  -- Broadcast the message
  result <- broadcastMessage (networkAdapter manager) ControlMessage message
  
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
handleLockRequest manager sender resId effectId = do
  -- Check if we already have a lock on this resource
  isLocked <- isResourceLocked (localLockManager manager) resId
  
  if isLocked
    then do
      -- Reject the request
      let rejectMessage = encodeLockRejection resId "Resource already locked locally"
      void $ sendDirectMessage (networkAdapter manager) sender ControlMessage rejectMessage
    else do
      -- Record the lock as held by the remote peer
      modifyMVar_ (remoteLocks manager) $ \remotes ->
        return $ Map.insert resId sender remotes
      
      -- Approve the request
      let approveMessage = encodeLockApproval resId
      void $ sendDirectMessage (networkAdapter manager) sender ControlMessage approveMessage

-- | Handle a lock release from another peer
handleLockRelease :: DistributedLockManager -> PeerId -> ResourceId -> EffectId -> IO ()
handleLockRelease manager sender resId effectId = do
  -- Remove the lock from our remote locks
  modifyMVar_ (remoteLocks manager) $ \remotes ->
    return $ Map.delete resId remotes

-- | Handle a lock approval from another peer
handleLockApproval :: DistributedLockManager -> PeerId -> ResourceId -> IO ()
handleLockApproval manager sender resId = do
  -- Find the pending request for this resource
  pendingMap <- readMVar (pendingRequests manager)
  case Map.lookup resId pendingMap of
    Nothing ->
      -- No pending request for this resource
      return ()
    
    Just (responseVar:_) -> do
      -- Increment the approval count
      modifyMVar_ responseVar $ \count -> return (count + 1)
    
    Just [] ->
      -- No response vars (shouldn't happen)
      return ()

-- | Handle a lock rejection from another peer
handleLockRejection :: DistributedLockManager -> PeerId -> ResourceId -> Text -> IO ()
handleLockRejection manager sender resId reason = do
  -- Find the pending request for this resource
  pendingMap <- readMVar (pendingRequests manager)
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
  "LOCK_REQUEST:" <> BS.pack (show resId) <> ":" <> BS.pack (show effectId)

-- | Encode a lock release message
encodeLockRelease :: ResourceId -> EffectId -> ByteString
encodeLockRelease resId effectId = do
  -- In a real implementation, this would use proper serialization
  -- For now, we just create a placeholder
  "LOCK_RELEASE:" <> BS.pack (show resId) <> ":" <> BS.pack (show effectId)

-- | Encode a lock approval message
encodeLockApproval :: ResourceId -> ByteString
encodeLockApproval resId = do
  -- In a real implementation, this would use proper serialization
  -- For now, we just create a placeholder
  "LOCK_APPROVAL:" <> BS.pack (show resId)

-- | Encode a lock rejection message
encodeLockRejection :: ResourceId -> Text -> ByteString
encodeLockRejection resId reason = do
  -- In a real implementation, this would use proper serialization
  -- For now, we just create a placeholder
  "LOCK_REJECTION:" <> BS.pack (show resId) <> ":" <> BS.pack (T.unpack reason)

-- | Decode a lock message
decodeLockMessage :: ByteString -> Either Text LockMessage
decodeLockMessage payload = do
  -- In a real implementation, this would use proper deserialization
  -- For now, we just create a placeholder
  let payloadStr = BS.unpack payload
  if "LOCK_REQUEST:" `BS.isPrefixOf` payload
    then Right $ LockRequest (error "Not a real resource ID") (error "Not a real effect ID")
  else if "LOCK_RELEASE:" `BS.isPrefixOf` payload
    then Right $ LockRelease (error "Not a real resource ID") (error "Not a real effect ID")
  else if "LOCK_APPROVAL:" `BS.isPrefixOf` payload
    then Right $ LockApproval (error "Not a real resource ID")
  else if "LOCK_REJECTION:" `BS.isPrefixOf` payload
    then Right $ LockRejection (error "Not a real resource ID") "Rejected"
  else Left $ "Unknown lock message type: " <> T.pack payloadStr

-- Helper functions

-- | Create a new empty MVar
newEmptyMVar :: MVar Int
newEmptyMVar = error "Not implemented: newEmptyMVar" 