{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : TimeBandits.Core.Resource.Ledger
Description : Implementation of the resource ledger
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides the implementation of the resource ledger, which tracks
ownership of resources and ensures that resource transfers comply with the
system's rules.
-}
module TimeBandits.Core.Resource.Ledger
  ( -- * Ledger Operations
    createLedger
  , addResource
  , removeResource
  , updateResourceState
  , getResourceState
  , lockResource
  , unlockResource
  , isResourceLocked
  
  -- * Ownership Operations
  , changeOwnership
  , getOwnershipRecord
  , getResourcesOwnedBy
  , recordOwnershipChange
  , getOwnershipHistory
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, throwError, catchError)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (STM, TVar, readTVar, writeTVar, modifyTVar, newTVar)
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Prelude hiding (id)
import qualified Data.Text.Encoding as TE

import TimeBandits.Common.ContentAddress (ContentAddress)
import TimeBandits.Core.Resource.Types
  ( Resource(..)
  , ResourceId
  , TokenId
  , ResourceLedger(ResourceLedger, resources, ownerIndex, lockTable)
  , ResourceState(..)
  , OwnershipRecord(..)
  , OwnershipChange(..)
  , OwnershipError(..)
  , LockTable
  , LockStatus(..)
  , OwnerId
  )
import TimeBandits.Core.Common.SharedTypes (ResourceId(..), EffectId)

-- | Create a new empty resource ledger
createLedger :: IO (TVar ResourceLedger)
createLedger = STM.atomically $ newTVar $ ResourceLedger
  { resources = Map.empty
  , ownerIndex = Map.empty
  , lockTable = Map.empty
  }

-- | Add a resource to the ledger with initial ownership
addResource 
  :: TVar ResourceLedger
  -> Resource
  -> OwnerId
  -> IO (Either OwnershipError ResourceId)
addResource ledgerVar resource owner = do
  -- Get current time outside the STM transaction
  time <- getCurrentTime
  
  -- Now run the STM transaction
  STM.atomically do
    ledger <- readTVar ledgerVar
    
    -- Generate a unique ID for the resource
    let resourceId = case resource of
          TokenBalanceResource tid _ _ -> ResourceId tid
          EscrowReceiptResource eid -> ResourceId eid
          ContractWitnessResource cid _ -> ResourceId cid
          SyntheticInternalMarker txt -> ResourceId $ TE.encodeUtf8 txt
    
    -- Check if the resource already exists
    if Map.member resourceId (resources ledger) then
      return (Left OwnershipErrorResourceExists)
    else do
      -- Create ownership record (not used in this implementation but kept for reference)
      let record = OwnershipRecord
            { ownedResource = resource
            , currentOwner = owner
            , ownershipTimestamp = time
            , ownershipStatus = "ACTIVE"
            }
      
      -- Add the resource to the ledger
      let newResources = Map.insert resourceId resource (resources ledger)
      
      -- Update owner index
      let newOwnerIndex = case Map.lookup owner (ownerIndex ledger) of
            Nothing -> Map.insert owner [resourceId] (ownerIndex ledger)
            Just rs -> Map.insert owner (resourceId : rs) (ownerIndex ledger)
      
      writeTVar ledgerVar ledger {resources = newResources, ownerIndex = newOwnerIndex}
      return (Right resourceId)

-- | Remove a resource from the ledger
removeResource
  :: TVar ResourceLedger
  -> ResourceId
  -> OwnerId  -- ^ Owner must be the current owner
  -> IO (Either OwnershipError ())
removeResource ledgerVar resourceId requestingOwner = STM.atomically do
  ledger <- readTVar ledgerVar
  
  -- Check if the resource exists
  case Map.lookup resourceId (resources ledger) of
    Nothing -> return (Left $ OwnershipErrorResourceNotFound)
    Just resource -> do
      -- Check if the requestingOwner is the current owner
      let owner = findOwnerForResource resourceId (ownerIndex ledger)
      case owner of 
        Nothing -> return (Left $ OwnershipErrorNotOwner)
        Just currentOwner ->
          if currentOwner /= requestingOwner
            then return (Left $ OwnershipErrorNotOwner)
            else do
              -- Check if the resource is locked
              let lockMap = lockTable ledger
              case Map.lookup resourceId lockMap of
                Just (LockedBy _) -> return (Left $ OwnershipErrorAlreadyLocked)
                _ -> do
                  -- Remove from resources
                  let newResources = Map.delete resourceId (resources ledger)
                  
                  -- Remove from owner index
                  let newOwnerIndex = case Map.lookup requestingOwner (ownerIndex ledger) of
                        Nothing -> ownerIndex ledger  -- Should never happen
                        Just rs -> Map.insert requestingOwner (filter (/= resourceId) rs) (ownerIndex ledger)
                  
                  -- Save changes
                  writeTVar ledgerVar ledger
                    { resources = newResources
                    , ownerIndex = newOwnerIndex
                    }
                  
                  return (Right ())

-- | Update the state of a resource
-- This simply replaces the resource with a new one
updateResourceState
  :: TVar ResourceLedger
  -> ResourceId
  -> (Resource -> Resource)
  -> IO (Either OwnershipError ())
updateResourceState ledgerVar resourceId updateFn = STM.atomically do
  ledger <- readTVar ledgerVar
  
  -- Check if the resource exists
  case Map.lookup resourceId (resources ledger) of
    Nothing -> return (Left $ OwnershipErrorResourceNotFound)
    Just resource -> do
      -- Apply the update
      let newResource = updateFn resource
      
      -- Save changes
      let newResources = Map.insert resourceId newResource (resources ledger)
      writeTVar ledgerVar ledger { resources = newResources }
      
      return (Right ())

-- | Get the current state of a resource
getResourceState
  :: TVar ResourceLedger
  -> ResourceId
  -> IO (Either OwnershipError Resource)
getResourceState ledgerVar resourceId = STM.atomically do
  ledger <- readTVar ledgerVar
  
  -- Check if the resource exists
  case Map.lookup resourceId (resources ledger) of
    Nothing -> return (Left $ OwnershipErrorResourceNotFound)
    Just resource -> return (Right resource)

-- | Lock a resource to prevent concurrent access
lockResource 
  :: TVar ResourceLedger
  -> ResourceId
  -> EffectId
  -> IO (Either OwnershipError ())
lockResource ledgerVar resourceId effectId = STM.atomically do
  ledger <- readTVar ledgerVar
  let lockMap = lockTable ledger
  case Map.lookup resourceId lockMap of
    Just Unlocked -> do
      let newLockMap = Map.insert resourceId (LockedBy effectId) lockMap
      writeTVar ledgerVar ledger { lockTable = newLockMap }
      return (Right ())
    Just (LockedBy _) -> return (Left $ OwnershipErrorAlreadyLocked)
    Nothing -> return (Left $ OwnershipErrorResourceNotFound)

-- | Unlock a resource
unlockResource 
  :: TVar ResourceLedger
  -> ResourceId
  -> EffectId
  -> IO (Either OwnershipError ())
unlockResource ledgerVar resourceId effectId = STM.atomically do
  ledger <- readTVar ledgerVar
  let lockMap = lockTable ledger
  case Map.lookup resourceId lockMap of
    Just (LockedBy lockedEffectId) | lockedEffectId == effectId -> do
      let newLockMap = Map.insert resourceId Unlocked lockMap
      writeTVar ledgerVar ledger { lockTable = newLockMap }
      return (Right ())
    Just (LockedBy _) -> return (Left $ OwnershipErrorAlreadyLocked)
    Just Unlocked -> return (Right ())
    Nothing -> return (Left $ OwnershipErrorResourceNotFound)

-- | Check if a resource is locked
isResourceLocked 
  :: TVar ResourceLedger
  -> ResourceId
  -> IO (Either OwnershipError Bool)
isResourceLocked ledgerVar resourceId = STM.atomically do
  ledger <- readTVar ledgerVar
  let lockMap = lockTable ledger
  case Map.lookup resourceId lockMap of
    Just status -> return (Right (status /= Unlocked))
    Nothing -> return (Left $ OwnershipErrorResourceNotFound)

-- | Change ownership of a resource
changeOwnership
  :: TVar ResourceLedger
  -> ResourceId
  -> OwnerId  -- ^ Current owner
  -> OwnerId  -- ^ New owner
  -> IO (Either OwnershipError ())
changeOwnership ledgerVar resourceId currentOwnerId newOwnerId = STM.atomically do
  ledger <- readTVar ledgerVar
  
  -- Check if the resource exists
  case Map.lookup resourceId (resources ledger) of
    Nothing -> return (Left $ OwnershipErrorResourceNotFound)
    Just resource -> do
      -- Check if currentOwnerId is the actual current owner
      let owner = findOwnerForResource resourceId (ownerIndex ledger)
      case owner of
        Nothing -> return (Left $ OwnershipErrorNotOwner)
        Just actualOwner ->
          if actualOwner /= currentOwnerId
            then return (Left $ OwnershipErrorNotOwner)
            else do
              -- Check if the resource is locked
              let lockMap = lockTable ledger
              case Map.lookup resourceId lockMap of
                Just (LockedBy _) -> return (Left $ OwnershipErrorAlreadyLocked)
                _ -> do
                  -- Update owner index - remove from current owner
                  let newOwnerIndex1 = case Map.lookup currentOwnerId (ownerIndex ledger) of
                        Nothing -> ownerIndex ledger  -- Should never happen
                        Just rs -> Map.insert currentOwnerId (filter (/= resourceId) rs) (ownerIndex ledger)
                  
                  -- Update owner index - add to new owner
                  let newOwnerIndex2 = case Map.lookup newOwnerId newOwnerIndex1 of
                        Nothing -> Map.insert newOwnerId [resourceId] newOwnerIndex1
                        Just rs -> Map.insert newOwnerId (resourceId : rs) newOwnerIndex1
                  
                  -- Save changes
                  writeTVar ledgerVar ledger
                    { ownerIndex = newOwnerIndex2
                    }
                  
                  return (Right ())

-- | Get the ownership record for a resource
getOwnershipRecord
  :: TVar ResourceLedger
  -> ResourceId
  -> IO (Either OwnershipError OwnershipRecord)
getOwnershipRecord ledgerVar resourceId = do
  -- Get current time outside the STM transaction
  time <- getCurrentTime
  
  -- Now run the STM transaction
  STM.atomically do
    ledger <- readTVar ledgerVar
    
    -- Check if the resource exists
    case Map.lookup resourceId (resources ledger) of
      Nothing -> return (Left $ OwnershipErrorResourceNotFound)
      Just resource -> do
        -- Find the owner from the ownerIndex
        let owner = findOwnerForResource resourceId (ownerIndex ledger)
        case owner of
          Nothing -> return (Left $ OwnershipErrorResourceNotFound)
          Just ownerId -> 
            return (Right $ OwnershipRecord
              { ownedResource = resource
              , currentOwner = ownerId
              , ownershipTimestamp = time
              , ownershipStatus = "ACTIVE"
              })

-- Helper function to find the owner of a resource
findOwnerForResource :: ResourceId -> Map.Map OwnerId [ResourceId] -> Maybe OwnerId
findOwnerForResource resId ownerMap =
  Map.foldrWithKey (\ownerId resIds acc -> 
    if resId `elem` resIds 
      then Just ownerId 
      else acc) Nothing ownerMap

-- | Get all resources owned by a specific owner
getResourcesOwnedBy
  :: TVar ResourceLedger
  -> OwnerId
  -> IO [ResourceId]
getResourcesOwnedBy ledgerVar ownerId = STM.atomically do
  ledger <- readTVar ledgerVar
  
  -- Get resources from owner index
  case Map.lookup ownerId (ownerIndex ledger) of
    Nothing -> return []
    Just resources -> return resources

-- | Record a change in ownership (without actually changing ownership)
-- This is now a no-op since we're not storing ownership changes in the resource
recordOwnershipChange
  :: TVar ResourceLedger
  -> ResourceId
  -> OwnershipChange
  -> IO (Either OwnershipError ())
recordOwnershipChange ledgerVar resourceId change = STM.atomically do
  ledger <- readTVar ledgerVar
  
  -- Check if the resource exists
  case Map.lookup resourceId (resources ledger) of
    Nothing -> return (Left $ OwnershipErrorResourceNotFound)
    Just _ -> return (Right ())  -- We don't actually record the change

-- | Get the ownership history for a resource
getOwnershipHistory
  :: TVar ResourceLedger
  -> ResourceId
  -> IO (Either OwnershipError [OwnershipChange])
getOwnershipHistory ledgerVar resourceId = STM.atomically do
  ledger <- readTVar ledgerVar
  
  -- For now this is a stub that returns an empty history
  -- The actual history would need to be stored somewhere
  case Map.lookup resourceId (resources ledger) of
    Nothing -> return (Left $ OwnershipErrorResourceNotFound)
    Just _ -> return (Right []) 