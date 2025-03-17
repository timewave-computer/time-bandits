{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : TimeBandits.Core.Concurrency.LockManager
Description : Resource lock manager for concurrency control
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module implements a resource lock manager for the Time Bandits concurrency model,
enabling the safe management of locks across multiple resources and effects.
-}
module TimeBandits.Core.Concurrency.LockManager
  ( -- * Lock Manager
    LockManager(..)
  , createLockManager
  , destroyLockManager
  
    -- * Lock Operations
  , acquireResourceLock
  , releaseResourceLock
  , withResourceLock
  
    -- * Lock Queries
  , getResourceLockInfo
  , isResourceLocked
  , getLockedResources
  , getEffectLocks
  
    -- * Distributed Locking
  , acquireDistributedLock
  , releaseDistributedLock
  ) where

-- Use qualified imports to avoid ambiguity
import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar, modifyMVar_, takeMVar, putMVar, threadDelay)
import qualified Control.Concurrent as Concurrent
import Control.Exception (bracket, catch, SomeException)
import Control.Monad (when, forM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime, UTCTime)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

-- Import from TimeBandits modules
import TimeBandits.Core.Concurrency.ResourceLock (ResourceLock(..), LockError(..), LockResult, 
                                     acquireLock, releaseLock, isLockOwner, 
                                     getLockInfo, isLocked, tryAcquireLock)
-- Use qualified import for ResourceLock to avoid conflicting lockTimeout 
import qualified TimeBandits.Core.Concurrency.ResourceLock as ResourceLock
import TimeBandits.Core.ResourceId (ResourceId)
import TimeBandits.Core.Types (EffectId)

-- | Peer identifier type (temporary definition until NetworkAdapter is migrated)
type PeerId = Text

-- | Default lock timeout
defaultLockTimeout :: NominalDiffTime
defaultLockTimeout = 5  -- 5 seconds

-- | Manages locks for multiple resources
data LockManager = LockManager
  { locks :: MVar (Map ResourceId ResourceLock)  -- ^ Currently held locks
  , locksByEffect :: MVar (Map EffectId (Set ResourceId))  -- ^ Resources locked by each effect
  , lockManagerTimeout :: NominalDiffTime  -- ^ Default lock timeout
  , distributedLockEnabled :: Bool  -- ^ Whether distributed locking is enabled
  , networkPeers :: Maybe [PeerId]  -- ^ Peers for distributed locking
  }

-- | Create a new lock manager
createLockManager :: NominalDiffTime -> Bool -> IO LockManager
createLockManager timeout distributed = do
  locksVar <- Concurrent.newMVar Map.empty
  byEffectVar <- Concurrent.newMVar Map.empty
  return LockManager
    { locks = locksVar
    , locksByEffect = byEffectVar
    , lockManagerTimeout = timeout
    , distributedLockEnabled = distributed
    , networkPeers = Nothing
    }

-- | Destroy a lock manager, releasing all locks
destroyLockManager :: LockManager -> IO ()
destroyLockManager manager = do
  -- Take all locks and clear them
  modifyMVar_ (locks manager) $ \_ -> return Map.empty
  modifyMVar_ (locksByEffect manager) $ \_ -> return Map.empty

-- | Acquire a lock on a resource
acquireResourceLock :: LockManager -> ResourceId -> EffectId -> IO (LockResult ResourceLock)
acquireResourceLock manager resId effectId = do
  -- Check if this is a distributed resource
  if distributedLockEnabled manager
    then acquireDistributedLock manager resId effectId
    else acquireLocalLock manager resId effectId

-- | Acquire a lock on a local resource
acquireLocalLock :: LockManager -> ResourceId -> EffectId -> IO (LockResult ResourceLock)
acquireLocalLock manager resId effectId = do
  -- Check if lock already exists
  locksMap <- Concurrent.readMVar (locks manager)
  case Map.lookup resId locksMap of
    Just existingLock -> do
      -- Lock exists, try to acquire it
      result <- tryAcquireLock existingLock effectId
      case result of
        Left err -> return $ Left err
        Right acquiredLock -> do
          -- Update locks map
          updateLocks manager resId acquiredLock effectId
          return $ Right acquiredLock
    
    Nothing -> do
      -- Lock doesn't exist, create a new one
      result <- acquireLock resId effectId (lockManagerTimeout manager)
      case result of
        Left err -> return $ Left err
        Right newLock -> do
          -- Update locks map
          updateLocks manager resId newLock effectId
          return $ Right newLock

-- | Update the locks maps after acquiring a lock
updateLocks :: LockManager -> ResourceId -> ResourceLock -> EffectId -> IO ()
updateLocks manager resId lock effectId = do
  -- Update locks map
  modifyMVar_ (locks manager) $ \locksMap ->
    return $ Map.insert resId lock locksMap
  
  -- Update locks by effect map
  modifyMVar_ (locksByEffect manager) $ \byEffectMap ->
    let resourceSet = Map.findWithDefault Set.empty effectId byEffectMap
        newResourceSet = Set.insert resId resourceSet
    in return $ Map.insert effectId newResourceSet byEffectMap

-- | Release a lock on a resource
releaseResourceLock :: LockManager -> ResourceId -> EffectId -> IO (LockResult ())
releaseResourceLock manager resId effectId = do
  if distributedLockEnabled manager
    then releaseDistributedLock manager resId effectId
    else releaseLocalLock manager resId effectId

-- | Release a local resource lock
releaseLocalLock :: LockManager -> ResourceId -> EffectId -> IO (LockResult ())
releaseLocalLock manager resId effectId = do
  -- Get the current locks
  locksMap <- Concurrent.readMVar (locks manager)
  case Map.lookup resId locksMap of
    Just existingLock -> do
      -- Lock exists, try to release it
      result <- releaseLock existingLock effectId
      case result of
        Left err -> return $ Left err
        Right () -> do
          -- Update locks maps
          updateLocksOnRelease manager resId effectId
          return $ Right ()
    
    Nothing ->
      -- Lock doesn't exist
      return $ Right ()

-- | Update the locks maps after releasing a lock
updateLocksOnRelease :: LockManager -> ResourceId -> EffectId -> IO ()
updateLocksOnRelease manager resId effectId = do
  -- Update locks map (keep the lock object but clear owner)
  modifyMVar_ (locks manager) $ \locksMap ->
    case Map.lookup resId locksMap of
      Just lock ->
        let updatedLock = lock { owner = Nothing }
        in return $ Map.insert resId updatedLock locksMap
      Nothing ->
        return locksMap
  
  -- Update locks by effect map
  modifyMVar_ (locksByEffect manager) $ \byEffectMap ->
    case Map.lookup effectId byEffectMap of
      Just resourceSet ->
        let newResourceSet = Set.delete resId resourceSet
        in if Set.null newResourceSet
             then return $ Map.delete effectId byEffectMap
             else return $ Map.insert effectId newResourceSet byEffectMap
      Nothing ->
        return byEffectMap

-- | Perform an operation with a resource lock
withResourceLock :: LockManager
                 -> ResourceId  -- ^ Resource to lock
                 -> EffectId    -- ^ Effect requesting the lock
                 -> IO a        -- ^ Operation to perform with lock
                 -> IO (LockResult a)
withResourceLock manager resId effectId action = do
  -- Acquire the lock
  lockResult <- acquireResourceLock manager resId effectId
  case lockResult of
    Left err -> return $ Left err
    Right lock -> do
      -- Execute the action
      result <- action `catch` \(e :: SomeException) -> do
        -- Release the lock on exception
        _ <- releaseResourceLock manager resId effectId
        return $ error $ T.pack $ "Exception in withResourceLock: " ++ show e
      
      return $ Right result

-- | Get information about a resource lock
getResourceLockInfo :: LockManager -> ResourceId -> IO (Maybe (ResourceId, Maybe EffectId, UTCTime))
getResourceLockInfo manager resId = do
  -- Get the resource lock
  locksMap <- Concurrent.readMVar (locks manager)
  return $ fmap getLockInfo $ Map.lookup resId locksMap

-- | Check if a resource is locked
isResourceLocked :: LockManager -> ResourceId -> IO Bool
isResourceLocked manager resId = do
  locksMap <- Concurrent.readMVar (locks manager)
  case Map.lookup resId locksMap of
    Just lock -> isLocked lock
    Nothing -> return False

-- | Get all locked resources
getLockedResources :: LockManager -> IO [(ResourceId, EffectId)]
getLockedResources manager = do
  -- Get all locks
  locksMap <- Concurrent.readMVar (locks manager)
  
  -- Extract resource IDs and their locking effects
  let lockedResources = map extractLockInfo $ Map.toList locksMap
  
  -- Return only the valid lock entries
  return $ Maybe.catMaybes lockedResources

-- | Get all resources locked by an effect
getEffectLocks :: LockManager -> EffectId -> IO [ResourceId]
getEffectLocks manager effectId = do
  -- Get the effect -> resources map
  byEffectMap <- Concurrent.readMVar (locksByEffect manager)
  return $ maybe [] Set.toList $ Map.lookup effectId byEffectMap

-- | Acquire a distributed lock
acquireDistributedLock :: LockManager -> ResourceId -> EffectId -> IO (LockResult ResourceLock)
acquireDistributedLock manager resId effectId = do
  -- First, acquire the local lock
  localResult <- acquireLocalLock manager resId effectId
  case localResult of
    Left err -> return $ Left err
    Right localLock -> do
      -- If we have network peers, coordinate with them
      case networkPeers manager of
        Just peers | not (null peers) -> do
          -- In a real implementation, this would use network communication to
          -- coordinate with peers using a distributed lock algorithm like Redlock
          
          -- For now, we just simulate success
          return $ Right localLock
        
        _ -> return $ Right localLock

-- | Release a distributed lock
releaseDistributedLock :: LockManager -> ResourceId -> EffectId -> IO (LockResult ())
releaseDistributedLock manager resId effectId = do
  -- First release the local lock
  localResult <- releaseLocalLock manager resId effectId
  case localResult of
    Left err -> return $ Left err
    Right () -> do
      -- If we have network peers, coordinate with them
      case networkPeers manager of
        Just peers | not (null peers) -> do
          -- In a real implementation, this would use network communication to
          -- coordinate with peers using a distributed lock algorithm like Redlock
          
          -- For now, we just simulate success
          return $ Right ()
        
        _ -> return $ Right ()

-- | Helper for catMaybes - renamed to avoid conflict with the Prelude version
ourCatMaybes :: [Maybe a] -> [a]
ourCatMaybes [] = []
ourCatMaybes (Nothing : xs) = ourCatMaybes xs
ourCatMaybes (Just x : xs) = x : ourCatMaybes xs

-- | Extract lock information from a lock entry
extractLockInfo :: (ResourceId, ResourceLock) -> Maybe (ResourceId, EffectId)
extractLockInfo (resId, lock) = 
  case owner lock of
    Just ownerId -> Just (resId, ownerId)
    Nothing -> Nothing 