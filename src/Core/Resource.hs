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
This module provides the Resource abstraction and related functionality.
It encapsulates all resource interactions within program memory.

Resources are internal program-owned representations of external assets or
synthetic program-specific values that live in program memory. They are:
- Created, moved, consumed within a program's memory
- Assigned to a memory slot inside a program
- Scoped to the program's lifecycle
- Pointed to directly in program memory
-}
module Core.Resource 
  ( -- * Core Types
    Resource(..)
  , ResourceId
  , TokenId
  , Address
  , Amount
  , EscrowId
  , ContractId
  , Escrow(..)
  , EscrowStatus(..)
  , ClaimCondition(..)
  
  -- * Resource Operations
  , createResource
  , transferResource
  , consumeResource
  , verifyResourceOwnership
  
  -- * Escrow Operations
  , escrowResource
  , claimResource
  , releaseResource
  , verifyEscrowStatus
  
  -- * Re-exports from Types
  , ResourceHash
  , ResourceEvent(..)
  , ResourceEventType(..)
  , ResourceCapability(..)

  -- * Adapter functions for backward compatibility with old Effects interface
  , adaptCreateResource
  , adaptTransferResource
  , adaptConsumeResource
  , adaptVerifyResource

  -- * Address types
  , Address
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import Data.Word (Word64)

-- Import from TimeBandits modules
import Core.Common (Hash(..))
import Core.Types
  ( ResourceHash
  , ResourceEvent(..)
  , ResourceEventType(..)
  , ResourceCapability(..)
  , AppError(..)
  , ResourceErrorType(..)
  , ActorHash
  , TimelineHash
  , EntityHash(..)
  )

import Core.Serialize ()  -- Import Serialize instances

-- | Unique identifier for a Resource
type ResourceId = Text

-- | Token identifier
type TokenId = ByteString

-- | Address on a timeline (e.g., wallet address)
type Address = Text

-- | Token amount
type Amount = Integer

-- | Escrow identifier
type EscrowId = Text

-- | Contract identifier
type ContractId = ByteString

-- | Resource is an internal program-owned representation of an external asset
-- or a synthetic program-specific value
data Resource 
  = TokenBalanceResource TokenId Address Amount 
  | EscrowReceiptResource EscrowId 
  | ContractWitnessResource ContractId ByteString 
  | SyntheticInternalMarker Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Status of an escrowed resource
data EscrowStatus
  = Active        -- Actively held in escrow
  | Claimed       -- Claimed by the beneficiary
  | Released      -- Released back to the original owner
  | Expired       -- Claim period expired
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Condition that must be met for a claim to succeed
data ClaimCondition
  = TimeBasedClaim Word64                 -- Must be claimed before this time
  | SignatureRequiredClaim ByteString     -- Must be signed by this key
  | PredicateBasedClaim Text ByteString   -- Must satisfy a custom predicate
  | AlwaysAllowed                         -- Can be claimed without condition
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Escrow represents a resource held in escrow
data Escrow = Escrow
  { escrowId :: EscrowId
  , escrowedResource :: Resource
  , originalOwner :: Address
  , beneficiary :: Address
  , claimCondition :: ClaimCondition
  , escrowStatus :: EscrowStatus
  , escrowTimestamp :: Word64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new resource
createResource :: 
  (Member (Error AppError) r) => 
  Resource -> 
  Sem r ResourceId
createResource resource = do
  -- In a real implementation, this would create a resource in the system
  -- For now, just return a dummy resource ID
  pure "dummy-resource-id"

-- | Transfer a resource to a new owner
transferResource ::
  (Member (Error AppError) r) =>
  ResourceId ->
  Address ->  -- New owner
  Sem r ResourceId
transferResource resourceId newOwner = do
  -- In a real implementation, this would update the resource's ownership
  -- For now, just return the original resource ID
  pure resourceId

-- | Consume a resource (mark as spent)
consumeResource ::
  (Member (Error AppError) r) =>
  ResourceId ->
  Sem r ()
consumeResource resourceId = do
  -- In a real implementation, this would mark the resource as spent
  -- For now, do nothing
  pure ()

-- | Verify resource ownership
verifyResourceOwnership ::
  (Member (Error AppError) r) =>
  ResourceId ->
  Address ->  -- Expected owner
  Sem r Bool
verifyResourceOwnership resourceId expectedOwner = do
  -- In a real implementation, this would check if the address owns the resource
  -- For now, just return True
  pure True

-- | Escrow a resource for potential claim by another address
escrowResource ::
  (Member (Error AppError) r) =>
  Resource ->            -- Resource to escrow
  Address ->             -- Original owner
  Address ->             -- Beneficiary who can claim
  ClaimCondition ->      -- Condition for claiming
  Sem r Escrow
escrowResource resource owner beneficiaryAddr claimCond = do
  -- Generate a unique escrow ID
  let escrowId = Text.pack $ "escrow-" ++ show (hash resource)
      
      -- Create the escrow with Active status
      escrow = Escrow
        { escrowId = escrowId
        , escrowedResource = resource
        , originalOwner = owner
        , beneficiary = beneficiaryAddr
        , claimCondition = claimCond
        , escrowStatus = Active
        , escrowTimestamp = 0  -- Would be current time in real impl
        }
  
  -- In a real implementation, this would store the escrow in a persistent store
  -- For now, just return the created escrow
  pure escrow

-- | Claim an escrowed resource as the beneficiary
claimResource ::
  (Member (Error AppError) r) =>
  EscrowId ->          -- ID of the escrow to claim
  Address ->           -- Address claiming the resource
  ByteString ->        -- Proof data (e.g., signature) if needed
  Sem r Resource
claimResource escrowId claimantAddr proofData = do
  -- In a real implementation, this would:
  -- 1. Look up the escrow by ID
  -- 2. Verify the claimant is the beneficiary
  -- 3. Verify the claim condition is met
  -- 4. Update the escrow status to Claimed
  -- 5. Return the claimed resource
  
  -- For now, create a synthetic resource to represent a claimed resource
  pure $ SyntheticInternalMarker "claimed-resource"

-- | Release an escrowed resource back to the original owner
releaseResource ::
  (Member (Error AppError) r) =>
  EscrowId ->          -- ID of the escrow to release
  Address ->           -- Address releasing the resource (must be owner or authorized)
  Sem r Resource
releaseResource escrowId releaserAddr = do
  -- In a real implementation, this would:
  -- 1. Look up the escrow by ID
  -- 2. Verify the releaser is authorized
  -- 3. Update the escrow status to Released
  -- 4. Return the released resource to the original owner
  
  -- For now, create a synthetic resource to represent a released resource
  pure $ SyntheticInternalMarker "released-resource"

-- | Verify the status of an escrow
verifyEscrowStatus ::
  (Member (Error AppError) r) =>
  EscrowId ->        -- ID of the escrow to check
  Sem r EscrowStatus
verifyEscrowStatus escrowId = do
  -- In a real implementation, this would look up the escrow and return its status
  -- For now, just return Active as a placeholder
  pure Active

-- | Helper function to hash a resource into a ByteString
hash :: Resource -> ByteString
hash resource = BS.pack $ show resource

-- | Adapter functions for backward compatibility with old Effects interface

-- | Adapter for compatibility with old ResourceOps interface
adaptCreateResource :: 
  (Member (Error AppError) r) => 
  ByteString -> 
  ActorHash -> 
  TimelineHash -> 
  Sem r (Either AppError Resource)
adaptCreateResource metadata owner timeline = do
  -- Create a resource based on metadata and owner
  let resource = SyntheticInternalMarker (Text.pack $ BS.unpack metadata)
  -- In a real implementation, this would create a resource appropriate for the metadata
  rid <- createResource resource
  -- Wrap the result with Right since the old interface returned Either
  pure $ Right resource

-- | Adapter for compatibility with old ResourceOps interface
adaptTransferResource :: 
  (Member (Error AppError) r) => 
  Resource -> 
  ActorHash -> 
  TimelineHash -> 
  Sem r (Either AppError Resource)
adaptTransferResource resource newOwner timeline = do
  -- Convert ActorHash to Address (implementation detail)
  let newOwnerAddr = Text.pack $ show $ unEntityHash newOwner
  -- Transfer the resource
  rid <- transferResource "dummy-resource-id" newOwnerAddr
  -- Return the resource with Right
  pure $ Right resource

-- | Adapter for compatibility with old ResourceOps interface
adaptConsumeResource :: 
  (Member (Error AppError) r) => 
  Resource -> 
  Sem r (Either AppError Resource)
adaptConsumeResource resource = do
  -- Consume the resource
  consumeResource "dummy-resource-id"
  -- Return the resource with Right
  pure $ Right resource

-- | Adapter for compatibility with old ResourceOps interface
adaptVerifyResource :: 
  (Member (Error AppError) r) => 
  Resource -> 
  Sem r (Either AppError Bool)
adaptVerifyResource resource = do
  -- Verify resource ownership
  result <- verifyResourceOwnership "dummy-resource-id" Text.empty
  pure $ Right result 