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
Module: Programs.Types
Description: Shared types for program-related modules

This module provides common types used across the Programs component,
to avoid cyclic dependencies between modules.
-}
module Programs.Types 
  ( -- * Memory Types
    MemorySlot(..)
  , ProgramMemory(..)
  
  -- * Time Types
  , TimeMap(..)
  
  -- * Resource Types
  , ResourceClaim(..)
  
  -- * Program ID Types
  , ProgramId
  , ProgramOwner
  
  -- * Function Types
  , ProgramFunction
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Time (UTCTime)

-- Import from Core modules
import Core.Common (EntityHash(..))
import Core.Resource (Resource, Address, EscrowId)
import Core.Timeline (TimelineHash, BlockHeader)
import Core.Types (LamportTime)

-- Forward declaration for Program
data Program

-- | Unique identifier for a Program
type ProgramId = EntityHash Program

-- | Program owner address
type ProgramOwner = Address

-- | A memory slot is a named container for resources within program memory
newtype MemorySlot = MemorySlot Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialize)

-- | Program memory tracks the runtime state of program-owned resources
newtype ProgramMemory = ProgramMemory
  { slots :: Map.Map MemorySlot (Maybe Resource) }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Time map is a composed view of multiple timelines
data TimeMap = TimeMap
  { timelines :: Map.Map TimelineHash LamportTime
  , observedHeads :: Map.Map TimelineHash BlockHeader
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Resource claim tracks ownership of resources by programs
data ResourceClaim = ResourceClaim
  { claimedResource :: Resource
  , claimEscrowId :: EscrowId
  , claimTimestamp :: LamportTime
  , claimExpiryTime :: Maybe LamportTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A named program function (entrypoint)
type ProgramFunction = [ByteString] -- Placeholder, will be replaced with actual type 