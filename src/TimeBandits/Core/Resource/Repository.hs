{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : TimeBandits.Core.Resource.Repository
Description : Resource repository interface
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module defines the core resource repository interface for creating,
transferring, and managing resources. It provides a clean abstraction over
the underlying resource management system.
-}
module TimeBandits.Core.Resource.Repository
  ( -- * Resource Operations
    createResource
  , transferResource
  , consumeResource
  , verifyResourceOwnership
  
  -- * Escrow Operations
  , escrowResource
  , claimResource
  , releaseResource
  , verifyEscrowStatus
  
  -- * Effect Log Operations
  , getResourceEffectLog
  , applyEffectToResource
  , getResourceEffectHistory

  -- * Resource Lookup
  , findResourceById
  , getAllResources
  , getResourcesByOwner
  
  -- * Repository Management
  , newResourceRepository
  , ResourceRepository
  ) where

import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar, newTVarIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad (when, unless)

-- Internal imports
import TimeBandits.Core.Resource.Types
  ( Resource(..)
  , ResourceId
  , Escrow(..)
  , EscrowStatus(..)
  , ClaimCondition(..)
  )
import TimeBandits.Core.Types (LamportTime, Effect, EffectId)
import TimeBandits.Core.ResourceId (ResourceId)
import TimeBandits.Core.ProgramId (ProgramId)
import TimeBandits.Core.Concurrency.EffectLog (EffectLog, EffectEntry)
import qualified TimeBandits.Core.Concurrency.EffectLog as EffectLog

-- | Resource repository for managing resources
data ResourceRepository = ResourceRepository
  { repositoryResources :: TVar (Map ResourceId Resource)
  , repositoryOwnership :: TVar (Map ResourceId ProgramId)
  , repositoryEscrows :: TVar (Map ResourceId Escrow)
  , repositoryEffectLogs :: TVar (Map ResourceId EffectLog)
  }

-- | Create a new resource repository
newResourceRepository :: MonadIO m => m ResourceRepository
newResourceRepository = liftIO $ do
  resources <- newTVarIO Map.empty
  ownership <- newTVarIO Map.empty
  escrows <- newTVarIO Map.empty
  effectLogs <- newTVarIO Map.empty
  return $ ResourceRepository resources ownership escrows effectLogs

-- | Create a new resource
createResource :: MonadIO m 
               => ResourceRepository
               -> Resource 
               -> ProgramId  -- ^ Owner
               -> m ResourceId
createResource repo resource ownerId = liftIO $ do
  resourceId <- generateResourceId resource
  
  atomically $ do
    -- Add resource to repository
    resources <- readTVar (repositoryResources repo)
    writeTVar (repositoryResources repo) (Map.insert resourceId resource resources)
    
    -- Set ownership
    ownership <- readTVar (repositoryOwnership repo)
    writeTVar (repositoryOwnership repo) (Map.insert resourceId ownerId ownership)
  
  -- Create effect log for the resource
  effectLog <- EffectLog.createEffectLog
  atomically $ do
    effectLogs <- readTVar (repositoryEffectLogs repo)
    writeTVar (repositoryEffectLogs repo) (Map.insert resourceId effectLog effectLogs)
  
  return resourceId

-- | Transfer a resource to a new owner
transferResource :: MonadIO m
                 => ResourceRepository
                 -> ResourceId
                 -> ProgramId  -- ^ Current owner
                 -> ProgramId  -- ^ New owner
                 -> m Bool
transferResource repo resourceId currentOwner newOwner = liftIO $ do
  -- Verify current ownership
  owned <- verifyResourceOwnership repo resourceId currentOwner
  
  if not owned
    then return False
    else atomically $ do
      -- Update ownership
      ownership <- readTVar (repositoryOwnership repo)
      writeTVar (repositoryOwnership repo) (Map.insert resourceId newOwner ownership)
      return True

-- | Consume (remove) a resource
consumeResource :: MonadIO m
                => ResourceRepository
                -> ResourceId
                -> ProgramId  -- ^ Owner
                -> m Bool
consumeResource repo resourceId ownerId = liftIO $ do
  -- Verify ownership
  owned <- verifyResourceOwnership repo resourceId ownerId
  
  if not owned
    then return False
    else atomically $ do
      -- Remove resource and ownership records
      resources <- readTVar (repositoryResources repo)
      writeTVar (repositoryResources repo) (Map.delete resourceId resources)
      
      ownership <- readTVar (repositoryOwnership repo)
      writeTVar (repositoryOwnership repo) (Map.delete resourceId ownership)
      
      return True

-- | Verify resource ownership
verifyResourceOwnership :: MonadIO m
                        => ResourceRepository
                        -> ResourceId
                        -> ProgramId  -- ^ Claimed owner
                        -> m Bool
verifyResourceOwnership repo resourceId claimedOwner = liftIO $ atomically $ do
  ownership <- readTVar (repositoryOwnership repo)
  return $ case Map.lookup resourceId ownership of
    Just actualOwner -> actualOwner == claimedOwner
    Nothing -> False

-- | Escrow a resource
escrowResource :: MonadIO m
               => ResourceRepository
               -> ResourceId
               -> ProgramId  -- ^ Current owner
               -> ProgramId  -- ^ Beneficiary
               -> ClaimCondition
               -> m (Maybe EscrowId)
escrowResource repo resourceId currentOwner beneficiary condition = liftIO $ do
  -- Verify ownership
  owned <- verifyResourceOwnership repo resourceId currentOwner
  
  if not owned
    then return Nothing
    else do
      -- Get the resource
      mResource <- findResourceById repo resourceId
      case mResource of
        Nothing -> return Nothing
        Just resource -> do
          -- Create escrow record
          escrowId <- generateEscrowId resourceId
          timestamp <- getCurrentTime
          
          let escrow = Escrow
                { escrowId = escrowId
                , escrowedResource = resource
                , originalOwner = ownerToAddress currentOwner
                , beneficiary = ownerToAddress beneficiary
                , claimCondition = condition
                , escrowStatus = Active
                , escrowTimestamp = timestamp
                }
          
          -- Update repository
          atomically $ do
            escrows <- readTVar (repositoryEscrows repo)
            writeTVar (repositoryEscrows repo) (Map.insert resourceId escrow escrows)
            
            -- Transfer to escrow ownership (special case)
            ownership <- readTVar (repositoryOwnership repo)
            writeTVar (repositoryOwnership repo) (Map.delete resourceId ownership)
          
          return (Just escrowId)

-- | Claim an escrowed resource
claimResource :: MonadIO m
              => ResourceRepository
              -> EscrowId
              -> ProgramId  -- ^ Claimant
              -> m Bool
claimResource repo escrowId claimant = liftIO $ do
  -- Find the escrow
  mEscrow <- findEscrowById repo escrowId
  case mEscrow of
    Nothing -> return False
    Just (resourceId, escrow) -> do
      -- Verify claim conditions
      validClaim <- validateClaim escrow (ownerToAddress claimant)
      
      if not validClaim
        then return False
        else atomically $ do
          -- Update escrow status
          let updatedEscrow = escrow { escrowStatus = Claimed }
          escrows <- readTVar (repositoryEscrows repo)
          writeTVar (repositoryEscrows repo) (Map.insert resourceId updatedEscrow escrows)
          
          -- Transfer ownership to claimant
          ownership <- readTVar (repositoryOwnership repo)
          writeTVar (repositoryOwnership repo) (Map.insert resourceId claimant ownership)
          
          return True

-- | Release an escrowed resource back to original owner
releaseResource :: MonadIO m
                => ResourceRepository
                -> EscrowId
                -> m Bool
releaseResource repo escrowId = liftIO $ do
  -- Find the escrow
  mEscrow <- findEscrowById repo escrowId
  case mEscrow of
    Nothing -> return False
    Just (resourceId, escrow) -> do
      -- Only active escrows can be released
      if escrowStatus escrow /= Active
        then return False
        else do
          -- Get original owner
          let origOwnerAddr = originalOwner escrow
              origOwner = addressToOwner origOwnerAddr
          
          atomically $ do
            -- Update escrow status
            let updatedEscrow = escrow { escrowStatus = Released }
            escrows <- readTVar (repositoryEscrows repo)
            writeTVar (repositoryEscrows repo) (Map.insert resourceId updatedEscrow escrows)
            
            -- Transfer ownership back to original owner
            ownership <- readTVar (repositoryOwnership repo)
            writeTVar (repositoryOwnership repo) (Map.insert resourceId origOwner ownership)
            
            return True

-- | Check escrow status
verifyEscrowStatus :: MonadIO m
                   => ResourceRepository
                   -> EscrowId
                   -> m (Maybe EscrowStatus)
verifyEscrowStatus repo escrowId = liftIO $ do
  mEscrow <- findEscrowById repo escrowId
  return $ escrowStatus . snd <$> mEscrow

-- | Get the effect log for a resource
getResourceEffectLog :: MonadIO m
                     => ResourceRepository
                     -> ResourceId
                     -> m (Maybe EffectLog)
getResourceEffectLog repo resourceId = liftIO $ atomically $ do
  effectLogs <- readTVar (repositoryEffectLogs repo)
  return $ Map.lookup resourceId effectLogs

-- | Apply an effect to a resource
applyEffectToResource :: MonadIO m
                      => ResourceRepository
                      -> ResourceId
                      -> Effect
                      -> m Bool
applyEffectToResource repo resourceId effect = liftIO $ do
  mEffectLog <- getResourceEffectLog repo resourceId
  case mEffectLog of
    Nothing -> return False
    Just effectLog -> do
      success <- EffectLog.appendEffect effectLog effect
      return success

-- | Get effect history for a resource
getResourceEffectHistory :: MonadIO m
                         => ResourceRepository
                         -> ResourceId
                         -> m [EffectEntry]
getResourceEffectHistory repo resourceId = liftIO $ do
  mEffectLog <- getResourceEffectLog repo resourceId
  case mEffectLog of
    Nothing -> return []
    Just effectLog -> EffectLog.getEffectHistory effectLog

-- | Find a resource by ID
findResourceById :: MonadIO m
                 => ResourceRepository
                 -> ResourceId
                 -> m (Maybe Resource)
findResourceById repo resourceId = liftIO $ atomically $ do
  resources <- readTVar (repositoryResources repo)
  return $ Map.lookup resourceId resources

-- | Get all resources
getAllResources :: MonadIO m
                => ResourceRepository
                -> m [(ResourceId, Resource)]
getAllResources repo = liftIO $ atomically $ do
  resources <- readTVar (repositoryResources repo)
  return $ Map.toList resources

-- | Get resources by owner
getResourcesByOwner :: MonadIO m
                    => ResourceRepository
                    -> ProgramId
                    -> m [(ResourceId, Resource)]
getResourcesByOwner repo ownerId = liftIO $ atomically $ do
  resources <- readTVar (repositoryResources repo)
  ownership <- readTVar (repositoryOwnership repo)
  
  let ownedIds = Map.keys $ Map.filter (== ownerId) ownership
      ownedResources = mapMaybe (\id -> (,) id <$> Map.lookup id resources) ownedIds
  
  return ownedResources

-- | Find an escrow by ID
findEscrowById :: MonadIO m
               => ResourceRepository
               -> EscrowId
               -> m (Maybe (ResourceId, Escrow))
findEscrowById repo targetEscrowId = liftIO $ atomically $ do
  escrows <- readTVar (repositoryEscrows repo)
  let matches = Map.toList $ Map.filter (\e -> escrowId e == targetEscrowId) escrows
  return $ case matches of
    (resourceId, escrow):_ -> Just (resourceId, escrow)
    [] -> Nothing

-- Helper functions

-- | Generate a unique resource ID
generateResourceId :: Resource -> IO ResourceId
generateResourceId = undefined  -- Implementation to be provided

-- | Generate an escrow ID
generateEscrowId :: ResourceId -> IO EscrowId
generateEscrowId = undefined  -- Implementation to be provided

-- | Get current Lamport time
getCurrentTime :: IO LamportTime
getCurrentTime = undefined  -- Implementation to be provided

-- | Convert owner ID to address
ownerToAddress :: ProgramId -> Address
ownerToAddress = undefined  -- Implementation to be provided

-- | Convert address to owner ID
addressToOwner :: Address -> ProgramId
addressToOwner = undefined  -- Implementation to be provided

-- | Validate a claim against escrow conditions
validateClaim :: Escrow -> Address -> IO Bool
validateClaim = undefined  -- Implementation to be provided

-- | Helper for working with Maybe
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
  Nothing -> mapMaybe f xs
  Just y  -> y : mapMaybe f xs 