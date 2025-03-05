{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
This module provides the ResourceLedger, which is responsible for tracking
ownership of resources across the Time Bandits system. It enforces the
single-owner invariant, ensuring that each resource has exactly one owner
at any given time.

The ResourceLedger:
1. Tracks which program owns each resource
2. Validates resource transfers between programs
3. Prevents double-spending of resources
4. Maintains a history of ownership changes
-}
module TimeBandits.Core.ResourceLedger 
  ( -- * Core Types
    ResourceLedger(..)
  , OwnershipRecord(..)
  , OwnershipError(..)
  
  -- * Ledger Operations
  , createLedger
  , registerResource
  , transferResource
  , verifyOwnership
  , getResourceOwner
  
  -- * Ownership History
  , getOwnershipHistory
  , verifyOwnershipChain
  ) where

-- Standard library imports
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Data.Serialize (Serialize)
import Polysemy (Sem, Member, embed)
import Polysemy.Error (Error, throw)
import Polysemy.Embed (Embed)

-- Import from TimeBandits modules
import TimeBandits.Core.Core (Hash(..), EntityHash(..), computeSha256)
import TimeBandits.Core.Types
  ( AppError(..)
  , ResourceErrorType(..)
  , ResourceErrorType(ResourceNotFound)
  )
import TimeBandits.Core.Resource
  ( Resource
  , ResourceHash
  )
import TimeBandits.Core.Timeline (TimelineHash)

-- | Type alias for program identifiers
type ProgramId = ByteString

-- | Error types specific to resource ownership
data OwnershipError
  = OwnershipResourceNotFound ResourceHash
  | ResourceAlreadyOwned ResourceHash ProgramId
  | UnauthorizedTransfer ResourceHash ProgramId ProgramId
  | InvalidOwnershipProof ResourceHash
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Record of resource ownership
data OwnershipRecord = OwnershipRecord
  { orResource :: ResourceHash
  , orOwner :: ProgramId
  , orTimestamp :: UTCTime
  , orTimeline :: TimelineHash
  , orPreviousOwner :: Maybe ProgramId
  , orTransferHash :: Hash
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | The ResourceLedger tracks ownership of all resources
data ResourceLedger = ResourceLedger
  { -- | Current ownership mapping
    currentOwners :: Map.Map ResourceHash ProgramId
    -- | History of ownership changes
  , ownershipHistory :: Map.Map ResourceHash [OwnershipRecord]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new resource ledger
createLedger :: 
  (Member (Error AppError) r) => 
  Sem r ResourceLedger
createLedger = do
  -- Create an empty ledger
  pure $ ResourceLedger
    { currentOwners = Map.empty
    , ownershipHistory = Map.empty
    }

-- | Get the hash of a resource
getResourceHash :: Resource -> ResourceHash
getResourceHash resource = EntityHash $ computeSha256 $ show resource

-- | Register a new resource with an initial owner
registerResource :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ResourceLedger -> 
  Resource -> 
  ProgramId -> 
  TimelineHash -> 
  Sem r ResourceLedger
registerResource ledger resource owner timeline = do
  -- Get the resource hash
  let resourceHash = getResourceHash resource
  
  -- Check if the resource is already registered
  case Map.lookup resourceHash (currentOwners ledger) of
    Just _ ->
      throw $ ResourceError $ ResourceAlreadyExists resourceHash
    Nothing -> do
      -- Get current time for the record
      now <- embed $ getCurrentTime
      
      -- Create a transfer hash
      let transferData = show resourceHash <> show owner <> show timeline
          transferHash = computeSha256 transferData
      
      -- Create the ownership record
      let record = OwnershipRecord
            { orResource = resourceHash
            , orOwner = owner
            , orTimestamp = now
            , orTimeline = timeline
            , orPreviousOwner = Nothing
            , orTransferHash = transferHash
            }
      
      -- Update the ledger
      let newOwners = Map.insert resourceHash owner (currentOwners ledger)
          newHistory = Map.insert resourceHash [record] (ownershipHistory ledger)
      
      pure $ ledger
        { currentOwners = newOwners
        , ownershipHistory = newHistory
        }

-- | Transfer ownership of a resource to a new program
transferResource :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ResourceLedger -> 
  ResourceHash -> 
  ProgramId -> 
  ProgramId -> 
  TimelineHash -> 
  Sem r ResourceLedger
transferResource ledger resourceHash currentOwner newOwner timeline = do
  -- Verify current ownership
  actualOwner <- case Map.lookup resourceHash (currentOwners ledger) of
    Just owner -> pure owner
    Nothing -> throw $ ResourceError $ ResourceNotFound resourceHash
  
  -- Verify the current owner matches the expected owner
  when (actualOwner /= currentOwner) $
    throw $ ResourceError $ InvalidResourceState $ 
      "Unauthorized transfer attempt: resource " <> show resourceHash <> 
      " owned by " <> show actualOwner <> 
      ", but transfer attempted by " <> show currentOwner
  
  -- Get current time for the record
  now <- embed $ getCurrentTime
  
  -- Create a transfer hash
  let transferData = show resourceHash <> show currentOwner <> show newOwner <> show timeline
      transferHash = computeSha256 transferData
  
  -- Create the ownership record
  let record = OwnershipRecord
        { orResource = resourceHash
        , orOwner = newOwner
        , orTimestamp = now
        , orTimeline = timeline
        , orPreviousOwner = Just currentOwner
        , orTransferHash = transferHash
        }
  
  -- Get the existing history
  let history = Map.findWithDefault [] resourceHash (ownershipHistory ledger)
      newHistory = record : history
  
  -- Update the ledger
  let newOwners = Map.insert resourceHash newOwner (currentOwners ledger)
      updatedHistory = Map.insert resourceHash newHistory (ownershipHistory ledger)
  
  pure $ ledger
    { currentOwners = newOwners
    , ownershipHistory = updatedHistory
    }

-- | Verify that a program owns a resource
verifyOwnership :: 
  (Member (Error AppError) r) => 
  ResourceLedger -> 
  ResourceHash -> 
  ProgramId -> 
  Sem r Bool
verifyOwnership ledger resourceHash expectedOwner = do
  -- Look up the current owner
  case Map.lookup resourceHash (currentOwners ledger) of
    Just actualOwner -> pure $ actualOwner == expectedOwner
    Nothing -> throw $ ResourceError $ ResourceNotFound resourceHash

-- | Get the current owner of a resource
getResourceOwner :: 
  (Member (Error AppError) r) => 
  ResourceLedger -> 
  ResourceHash -> 
  Sem r ProgramId
getResourceOwner ledger resourceHash = do
  -- Look up the current owner
  case Map.lookup resourceHash (currentOwners ledger) of
    Just owner -> pure owner
    Nothing -> throw $ ResourceError $ ResourceNotFound resourceHash

-- | Get the ownership history for a resource
getOwnershipHistory :: 
  (Member (Error AppError) r) => 
  ResourceLedger -> 
  ResourceHash -> 
  Sem r [OwnershipRecord]
getOwnershipHistory ledger resourceHash = do
  -- Look up the history
  case Map.lookup resourceHash (ownershipHistory ledger) of
    Just history -> pure history
    Nothing -> throw $ ResourceError $ ResourceNotFound resourceHash

-- | Verify the ownership chain for a resource
verifyOwnershipChain :: 
  (Member (Error AppError) r) => 
  ResourceLedger -> 
  ResourceHash -> 
  Sem r Bool
verifyOwnershipChain ledger resourceHash = do
  -- Get the ownership history
  history <- getOwnershipHistory ledger resourceHash
  
  -- Verify the chain is valid
  -- In a real implementation, this would check that each transfer
  -- correctly references the previous owner and has valid signatures
  
  -- For now, just check that the history is non-empty
  pure $ not (null history) 