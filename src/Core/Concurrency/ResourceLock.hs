{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Core.Concurrency.ResourceLock
Description : Resource-scoped locks for concurrent effect application
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module implements resource-scoped locks for the Time Bandits concurrency model,
enabling atomic effect application while maintaining causal consistency.
-}
module Core.Concurrency.ResourceLock
  ( -- * Resource Locks
    ResourceLock(..)
  , LockError(..)
  , LockResult
    
    -- * Lock Operations
  , acquireLock
  , releaseLock
  , withResourceLock
  
    -- * Lock Utilities
  , isLockOwner
  , getLockInfo
  , isLocked
  ) where

import Control.Exception (bracket, catch, SomeException)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import Control.Concurrent (MVar)
import qualified Control.Concurrent as Concurrent
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, readTVar, writeTVar, check)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, isJust)

-- Import from TimeBandits modules
import Core.ResourceId (ResourceId)
import Core.Types (EffectId)

-- | Resource lock for atomic effect application
data ResourceLock = ResourceLock
  { resourceId :: ResourceId       -- ^ ID of the resource being locked
  , owner :: Maybe EffectId        -- ^ Current owner effect ID (if locked)
  , acquiredAt :: UTCTime          -- ^ When the lock was acquired
  , lockTimeout :: NominalDiffTime -- ^ Lock timeout (to prevent deadlocks)
  , lockMVar :: MVar ()            -- ^ The actual lock mechanism
  }

-- | Errors that can occur during lock operations
data LockError
  = ResourceBusy EffectId         -- ^ Resource is locked by another effect
  | LockTimeout ResourceId        -- ^ Lock acquisition timed out
  | LockNotOwned ResourceId       -- ^ Attempted to release a lock not owned
  | InternalLockError Text        -- ^ Internal error
  deriving (Eq, Show)

-- | Result of a lock operation
type LockResult a = Either LockError a

-- | Create a new resource lock
createResourceLock :: ResourceId -> NominalDiffTime -> IO ResourceLock
createResourceLock resId timeout = do
  lockVar <- Concurrent.newMVar ()
  now <- getCurrentTime
  return ResourceLock
    { resourceId = resId
    , owner = Nothing
    , acquiredAt = now
    , lockTimeout = timeout
    , lockMVar = lockVar
    }

-- | Acquire a lock for a resource
acquireLock :: ResourceId      -- ^ Resource to lock
            -> EffectId        -- ^ Effect requesting the lock
            -> NominalDiffTime -- ^ Lock timeout
            -> IO (LockResult ResourceLock)
acquireLock resId effectId timeout = do
  -- Create a new lock if one doesn't exist
  lock <- createResourceLock resId timeout
  
  -- Try to acquire the lock
  result <- tryAcquireLock lock effectId
  case result of
    Left err -> return $ Left err
    Right acquiredLock -> return $ Right acquiredLock

-- | Try to acquire an existing lock
tryAcquireLock :: ResourceLock -> EffectId -> IO (LockResult ResourceLock)
tryAcquireLock lock effectId = do
  result <- tryTakeMVarWithTimeout (lockMVar lock) timeout
  case result of
    Left err -> return $ Left err
    Right () -> do
      -- Got the lock, update its state
      now <- getCurrentTime
      return $ Right $ lock
        { owner = Just effectId
        , acquiredAt = now
        }
  where
    timeout = lockTimeout lock

-- | Try to take an MVar with a timeout
tryTakeMVarWithTimeout :: MVar () -> NominalDiffTime -> IO (LockResult ())
tryTakeMVarWithTimeout mvar timeout = do
  startTime <- getCurrentTime
  let tryLock = do
        -- Try to take the MVar (non-blocking)
        result <- tryReadMVarCustom mvar
        case result of
          Nothing -> do
            -- MVar is empty, take it
            Concurrent.takeMVar mvar
            return $ Right ()
          Just _ -> do
            -- MVar is full, check timeout
            now <- getCurrentTime
            let elapsed = diffUTCTime now startTime
            if elapsed > timeout
              then return $ Left $ LockTimeout (error "Resource ID not available in tryTakeMVar")
              else do
                -- Sleep briefly and retry
                Concurrent.threadDelay 10000  -- 10ms
                tryLock
  tryLock

-- | Release a lock
releaseLock :: ResourceLock -> EffectId -> IO (LockResult ())
releaseLock lock effectId = do
  -- Check if the effect owns the lock
  if owner lock == Just effectId
    then do
      -- Release the lock
      Concurrent.putMVar (lockMVar lock) ()
      return $ Right ()
    else
      return $ Left $ LockNotOwned (resourceId lock)

-- | Perform an operation with a resource lock
withResourceLock :: ResourceId      -- ^ Resource to lock
                 -> EffectId        -- ^ Effect requesting the lock
                 -> NominalDiffTime -- ^ Lock timeout
                 -> IO a            -- ^ Operation to perform with the lock
                 -> IO (LockResult a)
withResourceLock resId effectId timeout action = do
  -- Acquire the lock
  lockResult <- acquireLock resId effectId timeout
  case lockResult of
    Left err -> return $ Left err
    Right lock -> do
      -- Perform the action and release the lock
      result <- bracket
        (return ())  -- No additional setup
        (\_ -> releaseLock lock effectId >> return ())  -- Always release lock
        (\_ -> action)  -- Run the action
        `catch` \(e :: SomeException) -> do
          -- Release the lock on exception
          _ <- releaseLock lock effectId
          return $ error $ T.pack $ "Exception in withResourceLock: " ++ show e
      
      return $ Right result

-- | Check if an effect is the owner of a lock
isLockOwner :: ResourceLock -> EffectId -> Bool
isLockOwner lock effectId = owner lock == Just effectId

-- | Get information about a lock
getLockInfo :: ResourceLock -> (ResourceId, Maybe EffectId, UTCTime)
getLockInfo lock = (resourceId lock, owner lock, acquiredAt lock)

-- | Check if a lock is currently held
isLocked :: ResourceLock -> IO Bool
isLocked lock = do
  isEmpty <- isEmptyMVar (lockMVar lock)
  return (not isEmpty)

-- | Check if an MVar is empty
isEmptyMVar :: MVar a -> IO Bool
isEmptyMVar mvar = do
  result <- tryReadMVarCustom mvar
  return (Data.Maybe.isNothing result)

-- | Try to read an MVar without taking it (custom implementation)
tryReadMVarCustom :: MVar a -> IO (Maybe a)
tryReadMVarCustom mvar = do
  tryTakeMVarNow mvar >>= \case
    Nothing -> return Nothing
    Just x -> do
      Concurrent.putMVar mvar x
      return (Just x)

-- | Try to take an MVar immediately (non-blocking)
tryTakeMVarNow :: MVar a -> IO (Maybe a)
tryTakeMVarNow mvar = error "Not implemented: tryTakeMVarNow"

-- | Check if a Maybe value is Nothing
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False 