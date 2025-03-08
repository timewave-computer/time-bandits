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
  
  -- * Effect Log Operations
  , getResourceEffectLog
  , applyEffectToResource
  , getResourceEffectHistory
  
  -- * Re-exports from Types
  , ResourceHash
  , ResourceEvent(..)
  , ResourceEventType(..)
  , ResourceCapability(..)

  -- * Serialization
  , resourceToByteString

  -- * Adapter functions for backward compatibility with old Effects interface
  , adaptCreateResource
  , adaptTransferResource
  , adaptConsumeResource
  , adaptVerifyResource
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Text (Text)
import Data.Text qualified as Text
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import Control.Concurrent qualified as Concurrent
import Control.Monad (when, forM)
import Data.Time (UTCTime)
import System.IO.Unsafe (unsafePerformIO)
import Data.Word (Word8)

-- Import from TimeBandits modules
import Core.Core (Hash(..), EntityHash(..))
import Core.ResourceId (ResourceId)
import qualified Core.ResourceId as ResourceId
import Core.Types
  ( ResourceHash
  , ResourceEvent(..)
  , ResourceEventType(..)
  , ResourceCapability(..)
  , LamportTime(..)
  , AppError(..)
  , ResourceErrorType(..)
  , ActorHash
  , TimelineHash
  , EffectId, Effect
  )
import Types.EffectTypes (EffectType)

-- Import Serialize instances from Core.Serialize to avoid ambiguity with Types.EffectBase
import Core.Serialize ()

-- Import the new EffectLog module
import Core.Concurrency.EffectLog
  ( createEffectLog
  , appendEffect
  , getEffectHistory
  , EffectLog
  , EffectEntry
  , EffectLogError
  )
import qualified Core.Concurrency.EffectLog as EffectLog
import Core.Concurrency.ResourceLock

-- | Unique identifier for a Resource (using Text for simplicity)
-- type ResourceId = Text

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

-- | Resource effect log map (resource ID -> effect log)
{-# NOINLINE resourceEffectLogs #-}
resourceEffectLogs :: Concurrent.MVar (Map.Map ResourceId EffectLog)
resourceEffectLogs = unsafePerformIO $ Concurrent.newMVar Map.empty

-- | Get or create the effect log for a resource
getResourceEffectLog :: ResourceId -> IO EffectLog
getResourceEffectLog resId = do
  logs <- Concurrent.readMVar resourceEffectLogs
  case Map.lookup resId logs of
    Just log -> return log
    Nothing -> do
      -- Create a new log
      newLog <- createEffectLog resId
      -- Add it to the map
      Concurrent.modifyMVar_ resourceEffectLogs $ \logs' ->
        return $ Map.insert resId newLog logs'
      return newLog

-- | Apply an effect to a resource
applyEffectToResource :: ResourceId -> Effect -> IO (Either EffectLogError EffectEntry)
applyEffectToResource resId effect = do
  -- Get or create the resource's effect log
  log <- getResourceEffectLog resId
  
  -- Append the effect to the log
  result <- appendEffect log effect
  
  -- In a real implementation, this would also update the resource state
  -- based on the effect, but for now we just return the result
  return result

-- | Get the effect history for a resource
getResourceEffectHistory :: ResourceId -> Maybe TimeRange -> IO [EffectEntry]
getResourceEffectHistory resId _ = do
  -- Get the resource's effect log
  log <- getResourceEffectLog resId
  
  -- Get the effect history without time range filtering for now
  getEffectHistory log Nothing

-- | TimeRange for querying effects
data TimeRange = TimeRange
  { startTime :: UTCTime  -- ^ Start time (inclusive)
  , endTime   :: UTCTime  -- ^ End time (inclusive)
  } deriving (Eq, Show)

-- | Create a new resource
createResource :: 
  (Member (Error AppError) r) => 
  Resource -> 
  Sem r ResourceId
createResource resource = do
  -- In a real implementation, this would create a resource in the system
  -- For now, just return a dummy resource ID
  case ResourceId.fromText "dummy-resource-id" of
    Right resId -> pure resId
    Left _ -> error "Failed to create resource ID"

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
  let escrowId = BS.pack $ "escrow-" ++ show (hash resource)
      
      -- Create the escrow with Active status
      escrow = Escrow
        { escrowId = escrowId
        , escrowedResource = resource
        , originalOwner = owner
        , beneficiary = beneficiaryAddr
        , claimCondition = claimCond
        , escrowStatus = Active
        , escrowTimestamp = LamportTime 0  -- Would be current time in real impl
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
  let newOwnerAddr = BS.pack $ show $ unEntityHash newOwner
      dummyResId = case ResourceId.fromText "dummy-resource-id" of
        Right resId -> resId
        Left _ -> error "Failed to create resource ID"
  -- Transfer the resource
  rid <- transferResource dummyResId newOwnerAddr
  -- Return the resource with Right
  pure $ Right resource

-- | Adapter for compatibility with old ResourceOps interface
adaptConsumeResource :: 
  (Member (Error AppError) r) => 
  Resource -> 
  Sem r (Either AppError Resource)
adaptConsumeResource resource = do
  -- Consume the resource
  let dummyResId = case ResourceId.fromText "dummy-resource-id" of
        Right resId -> resId
        Left _ -> error "Failed to create resource ID"
  consumeResource dummyResId
  -- Return the resource with Right
  pure $ Right resource

-- | Adapter for compatibility with old ResourceOps interface
adaptVerifyResource :: 
  (Member (Error AppError) r) => 
  Resource -> 
  Sem r (Either AppError Bool)
adaptVerifyResource resource = do
  -- Verify the resource
  let dummyResId = case ResourceId.fromText "dummy-resource-id" of
        Right resId -> resId
        Left _ -> error "Failed to create resource ID"
  result <- verifyResourceOwnership dummyResId BS.empty
  -- Return the result with Right
  pure $ Right result

-- | Convert a resource to a ByteString for serialization/hashing
resourceToByteString :: Resource -> ByteString
resourceToByteString = S.encode 