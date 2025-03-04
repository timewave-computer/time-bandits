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
module TimeBandits.Resource 
  ( -- * Core Types
    Resource(..)
  , ResourceId
  , TokenId
  , Address
  , Amount
  , EscrowId
  , ContractId
  
  -- * Resource Operations
  , createResource
  , transferResource
  , consumeResource
  , verifyResourceOwnership
  
  -- * Re-exports from Types
  , ResourceHash
  , ResourceEvent(..)
  , ResourceEventType(..)
  , ResourceCapability(..)
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import TimeBandits.Types
  ( ResourceHash
  , ResourceEvent(..)
  , ResourceEventType(..)
  , ResourceCapability(..)
  , LamportTime(..)
  , AppError(..)
  , ResourceErrorType(..)
  )

-- | Unique identifier for a Resource
type ResourceId = EntityHash Resource

-- | Token identifier
type TokenId = ByteString

-- | Address on a timeline (e.g., wallet address)
type Address = ByteString

-- | Token amount
type Amount = Integer

-- | Escrow identifier
type EscrowId = ByteString

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

-- | Create a new resource
createResource :: 
  (Member (Error AppError) r) => 
  Resource -> 
  Sem r ResourceId
createResource resource = do
  -- In a real implementation, this would create a resource in the system
  -- For now, just return a dummy resource ID
  pure $ EntityHash $ Hash "dummy-resource-id"

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