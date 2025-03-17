{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: TimeBandits.Core.Common.SharedTypes
-- Description: Common types shared between Resource and Effect modules
--
-- This module defines the common types that are used by both Resource and Effect modules,
-- helping to resolve circular dependencies between these modules.
module TimeBandits.Core.Common.SharedTypes
  ( -- * Resource Identifiers
    ResourceId(..)
  , EffectId
  , EffectID
  
  -- * Basic Effect Types 
  , EffectStatus(..)
  , EffectResult(..)
  
  -- * Common Error Types
  , ResourceErrorType(..)
  , OwnershipError(..)
  
  -- * Resource Types
  , OwnerId
  , ResourceTag
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

import TimeBandits.Core.Common.Serialize 

-- | Resource identifier type, moved here to avoid circular dependencies
-- This was previously defined in Core.ResourceId
newtype ResourceId = ResourceId { unResourceId :: ByteString }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialize)

-- | Effect identifier type aliases, to avoid ambiguity between modules
type EffectId = ByteString
type EffectID = EffectId  -- For backward compatibility

-- | Owner identifier type
type OwnerId = Text

-- | Resource tag type
type ResourceTag = Text

-- | Status of effects as they are processed
data EffectStatus = 
    EffectPending 
  | EffectStarted UTCTime
  | EffectCompleted UTCTime
  | EffectErrored UTCTime Text
  | EffectApplied      -- ^ Effect has been successfully applied (from Effect module)
  | EffectRejected     -- ^ Effect application was rejected (from Effect module)
  | EffectReverted     -- ^ Effect was applied but later reverted (from Effect module)
  deriving (Show, Eq, Generic, Serialize)

-- | Result of an effect execution
data EffectResult = 
    EffectSuccess ByteString
  | EffectError Text
  deriving (Show, Eq, Generic, Serialize)

-- | Error types related to resource operations
data ResourceErrorType =
    ResourceNotFound
  | ResourceInvalidState
  | ResourceAccessDenied
  | ResourceTemporarilyUnavailable
  | ResourcePermanentlyUnavailable
  | ResourceAlreadyExists
  deriving (Show, Eq, Generic, Serialize)

-- | Errors related to resource ownership
data OwnershipError =
    OwnershipErrorResourceNotFound
  | OwnershipErrorResourceExists
  | OwnershipErrorNotOwner
  | OwnershipErrorAlreadyLocked
  | OwnershipErrorNotLocked
  | OwnershipErrorInvalidToken
  deriving (Show, Eq, Generic, Serialize) 