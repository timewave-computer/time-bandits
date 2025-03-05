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
Description: Shared type definitions for the Programs subsystem.

This module contains type definitions shared between other modules in the Programs subsystem.
It helps break circular dependencies by providing a common foundation of types that can be
imported by other modules without creating import cycles.

The types defined here include:
- Guard conditions for effects
- Basic effect structure
- Memory and program-related types

By centralizing these definitions, we maintain a clean dependency structure where:
- Types.hs depends only on Core modules
- Other Program modules depend on Types.hs
- No circular dependencies are created between Program modules
-}
module Programs.Types 
  ( -- * Core Types
    Guard(..)
  , GuardedEffect(..)
  , Effect(..)
  , MemorySlot(..)
  , ProgramMemory(..)
  , TimeMap(..)
  , ResourceClaim(..)
  , FunctionName
  , Capability
  
  -- * Memory Types
  , SlotSpec(..)
  , ResourceRequirement(..)
  , MemoryContract(..)
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)

-- Import from TimeBandits modules
import Core 
  ( Hash(..)
  , EntityHash(..)
  )
import Core.Types
  ( LamportTime(..)
  )
import Core.Resource 
  ( Resource
  , Address
  , EscrowId
  , TokenId
  , ContractId
  , ResourceHash
  )
import Core.Timeline (TimelineHash, BlockHeader)

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

-- | Type alias for program function names
type FunctionName = Text

-- | Type alias for capability identifiers
type Capability = Text

-- | Specification for a memory slot in a contract
data SlotSpec = SlotSpec
  { slotName :: MemorySlot
  , slotRequired :: Bool
  , slotDescription :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Resource requirement for a program step
data ResourceRequirement = ResourceRequirement
  { requiredSlot :: MemorySlot
  , requiredResourceType :: Text
  , isOptional :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Memory contract defines what resources a program expects
data MemoryContract = MemoryContract
  { slotSpecs :: [SlotSpec]
  , stepRequirements :: Map.Map Int [ResourceRequirement]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Guard conditions for effects
data Guard
  = BalanceAtLeast TokenId Address Integer    -- Check minimum balance
  | EscrowExists EscrowId                    -- Check if escrow exists
  | ContractInState ContractId ByteString    -- Check contract state
  | ActorAuthorized Address Capability       -- Check actor capability
  | TimeAfter LamportTime                    -- Check time condition
  | OwnershipVerified ResourceHash Address   -- Check resource ownership
  | ProgramOwnershipVerified ByteString Address  -- Check program ownership
  | ResourceInSlot MemorySlot Resource       -- Check resource in memory slot
  | SlotEmpty MemorySlot                     -- Check slot is empty
  | EscrowClaimable EscrowId Address         -- Check escrow can be claimed
  | Always                                   -- Always satisfied
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Effect represents primitive operations
data Effect
  = UpdateMemory MemorySlot (Maybe Resource)  -- Update or clear a memory slot
  | TransferResource ResourceHash Address     -- Transfer resource ownership
  | CreateResource Text ByteString            -- Create a new resource
  | QueryTimeline TimelineHash ByteString     -- Query a timeline
  | LogEvent Text ByteString                  -- Log an event
  | InvokeContract TimelineHash ContractId ByteString  -- Call a contract
  | CreateEscrow ResourceHash Address Address LamportTime  -- Put in escrow
  | ClaimEscrow EscrowId Address             -- Claim from escrow
  | ReleaseEscrow EscrowId                   -- Release from escrow
  | SendMessage Address ByteString           -- Send message to an actor
  | Sleep LamportTime                        -- Wait for a period of time
  | Noop                                     -- No operation
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A guarded effect pairs a precondition with an effect
data GuardedEffect = GuardedEffect
  { guard :: Guard
  , effect :: Effect
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize) 