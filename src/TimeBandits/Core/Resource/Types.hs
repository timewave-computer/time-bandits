{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : TimeBandits.Core.Resource.Types
Description : Core resource type definitions
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module defines the core types used in the resource management system.
It provides the fundamental data structures for resources, their ownership,
and the ledger that tracks them.
-}
module TimeBandits.Core.Resource.Types
  ( -- * Core Resource Types
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
  , OwnerId
  
  -- * Ledger Types
  , ResourceLedger(..)
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
import Control.Monad (when, forM)
import qualified Data.Map.Strict as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Time (UTCTime)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import qualified Data.ByteString.Base64 as B64

-- Internal imports
import qualified TimeBandits.Core.Types as CT
import TimeBandits.Core.Common.Types (LamportTime(..), EntityHash(..), Hash(..))
import TimeBandits.Core.ProgramId (ProgramId)
-- Import common types to avoid circular dependencies
import TimeBandits.Core.Common.SharedTypes
  ( ResourceId
  , EffectId
  , OwnershipError(..)
  , ResourceErrorType(..)
  , OwnerId
  )

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

-- Manual Serialize instance to avoid Text serialization ambiguity
instance Serialize Resource where
  put (TokenBalanceResource tid addr amt) = do
    S.put (0 :: Word8)  -- Tag for TokenBalanceResource
    S.put tid
    S.put addr
    S.put amt
  put (EscrowReceiptResource eid) = do
    S.put (1 :: Word8)  -- Tag for EscrowReceiptResource
    S.put eid
  put (ContractWitnessResource cid bs) = do
    S.put (2 :: Word8)  -- Tag for ContractWitnessResource
    S.put cid
    S.put bs
  put (SyntheticInternalMarker txt) = do
    S.put (3 :: Word8)  -- Tag for SyntheticInternalMarker
    S.put (TE.encodeUtf8 txt)
    
  get = do
    tag <- S.get :: S.Get Word8
    case tag of
      0 -> TokenBalanceResource <$> S.get <*> S.get <*> S.get
      1 -> EscrowReceiptResource <$> S.get
      2 -> ContractWitnessResource <$> S.get <*> S.get
      3 -> do
        bs <- S.get
        return $ SyntheticInternalMarker (TE.decodeUtf8 bs)
      _ -> fail "Invalid Resource tag"

-- | Status of an escrowed resource
data EscrowStatus
  = Active        -- Actively held in escrow
  | Claimed       -- Claimed by the beneficiary
  | Released      -- Released back to the original owner
  | Expired       -- Claim period expired
  deriving stock (Eq, Show, Generic)

-- Standalone deriving instance
deriving instance Serialize EscrowStatus

-- | Condition that must be met for a claim to succeed
data ClaimCondition
  = TimeBasedClaim LamportTime            -- Must be claimed before this time
  | SignatureRequiredClaim ByteString     -- Must be signed by this key
  | PredicateBasedClaim Text ByteString   -- Must satisfy a custom predicate
  | AlwaysAllowed                         -- Can be claimed without condition
  deriving stock (Eq, Show, Generic)

-- Manual Serialize instance to avoid Text serialization ambiguity
instance Serialize ClaimCondition where
  put (TimeBasedClaim time) = do
    S.put (0 :: Word8)  -- Tag for TimeBasedClaim
    S.put time
  put (SignatureRequiredClaim bs) = do
    S.put (1 :: Word8)  -- Tag for SignatureRequiredClaim
    S.put bs
  put (PredicateBasedClaim txt bs) = do
    S.put (2 :: Word8)  -- Tag for PredicateBasedClaim
    S.put (TE.encodeUtf8 txt)
    S.put bs
  put AlwaysAllowed = 
    S.put (3 :: Word8)  -- Tag for AlwaysAllowed
    
  get = do
    tag <- S.get :: S.Get Word8
    case tag of
      0 -> TimeBasedClaim <$> S.get
      1 -> SignatureRequiredClaim <$> S.get
      2 -> do
        bs <- S.get
        predBs <- S.get
        return $ PredicateBasedClaim (TE.decodeUtf8 bs) predBs
      3 -> return AlwaysAllowed
      _ -> fail "Invalid ClaimCondition tag"

-- | Escrow represents a resource held in escrow
data Escrow = Escrow
  { escrowId :: EscrowId
  , escrowedResource :: Resource
  , originalOwner :: Address
  , beneficiary :: Address
  , claimCondition :: ClaimCondition
  , escrowStatus :: EscrowStatus
  , escrowTimestamp :: LamportTime
  }
  deriving stock (Eq, Show, Generic)

-- Standalone deriving instance
deriving instance Serialize Escrow

-- | ResourceLedger tracks ownership and state of resources
data ResourceLedger = ResourceLedger
  { resources :: Map.Map ResourceId Resource
  , ownerIndex :: Map.Map OwnerId [ResourceId]
  , lockTable :: LockTable
  }
  deriving stock (Eq, Show, Generic)

-- | ResourceState represents the current state of a resource
data ResourceState = ResourceState
  { ownershipRecord :: OwnershipRecord  -- ^ The ownership record for this resource
  , transactionHistory :: [OwnershipChange]  -- ^ History of ownership transfers
  }
  deriving (Show, Generic)

-- | Type alias for the map of resource ownership
type ResourceOwnership = Map.Map ResourceId ProgramId

-- | OwnershipRecord tracks ownership of a resource
data OwnershipRecord = OwnershipRecord
  { ownedResource :: Resource  -- ^ The resource being owned
  , currentOwner :: OwnerId  -- ^ Current owner of the resource
  , ownershipTimestamp :: UTCTime  -- ^ When ownership was established
  , ownershipStatus :: Text  -- ^ Status of the ownership (e.g., "ACTIVE")
  }
  deriving (Show, Generic)

-- | OwnershipChange records a single transfer of ownership
data OwnershipChange = OwnershipChange
  { fromProgram :: ProgramId
  , toProgram :: ProgramId
  , timestamp :: UTCTime
  , effectId :: TimeBandits.Core.Common.SharedTypes.EffectId
  }
  deriving (Show, Generic)

-- | LockTable tracks which resources are currently locked
type LockTable = Map.Map ResourceId LockStatus

-- | LockStatus indicates whether a resource is locked and by whom
data LockStatus =
    Unlocked
  | LockedBy TimeBandits.Core.Common.SharedTypes.EffectId
  | LockStatusUnlocked  -- ^ Alias for Unlocked
  | LockStatusLocked    -- ^ Alias for LockedBy with empty effectId
  deriving (Eq, Show, Generic) 