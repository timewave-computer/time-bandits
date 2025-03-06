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
Module: Core.ResourceLedger
Description: Pure types for tracking resource ownership

This module defines the core types for the ResourceLedger, which tracks ownership
of resources across the Time Bandits system. It enforces the single-owner invariant,
ensuring that each resource has exactly one owner at any given time.

Note: This module contains ONLY the pure data types. All business logic related to
resource ledger operations has been moved to Execution.ResourceLedger.
-}
module Core.ResourceLedger 
  ( -- * Core Types
    ResourceLedger
  , ResourceState(..)
  , OwnershipRecord(..)
  , OwnershipChange(..)
  , OwnershipError(..)
  , LockTable
  , LockStatus(..)
  
  -- * Type Aliases
  , ResourceOwnership
  ) where

-- Standard library imports
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

-- Local imports
import Core.ResourceId (ResourceId)
import Core.ProgramId (ProgramId)
import Core.Effect (EffectId)

-- | ResourceLedger is a map from ResourceId to (ProgramId, ResourceState)
type ResourceLedger = Map.Map ResourceId (ProgramId, ResourceState)

-- | ResourceState represents the current state of a resource
data ResourceState = ResourceState
  { value :: Integer  -- ^ The numeric value of the resource
  , metadata :: Map.Map String String  -- ^ Additional metadata about the resource
  , lastUpdated :: UTCTime  -- ^ When the resource was last updated
  }
  deriving stock (Show, Generic)

-- | Type alias for the map of resource ownership
type ResourceOwnership = Map.Map ResourceId ProgramId

-- | OwnershipRecord tracks the full history of a resource's ownership
data OwnershipRecord = OwnershipRecord
  { resource :: ResourceId
  , currentOwner :: ProgramId
  , ownershipHistory :: [OwnershipChange]
  }
  deriving stock (Show, Generic)

-- | OwnershipChange records a single transfer of ownership
data OwnershipChange = OwnershipChange
  { fromProgram :: ProgramId
  , toProgram :: ProgramId
  , timestamp :: UTCTime
  , effectId :: EffectId
  }
  deriving stock (Show, Generic)

-- | OwnershipError represents errors that can occur during ownership operations
data OwnershipError =
    ResourceNotFound ResourceId
  | NotOwner ProgramId ResourceId
  | AlreadyOwned ProgramId ResourceId
  | ResourceLocked ResourceId LockStatus
  | InvalidTransfer ResourceId ProgramId ProgramId
  deriving stock (Show, Generic)

-- | LockTable tracks which resources are currently locked
type LockTable = Map.Map ResourceId LockStatus

-- | LockStatus indicates whether a resource is locked and by whom
data LockStatus =
    Unlocked
  | LockedBy EffectId
  deriving stock (Eq, Show, Generic) 