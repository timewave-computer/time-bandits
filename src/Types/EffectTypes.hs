{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module: Types.EffectTypes
Description: Types for effects in the Time Bandits system

This module provides the core types for the effect system in Time Bandits.
It defines the fundamental types needed for representing effects, their payload,
guards, and the DAG structure that effects form.
-}
module Types.EffectTypes (
    -- * Re-export from Types.EffectBase
    EffectId(..),
    FactId(..),
    FactValue(..),
    ObservationProof(..),
    ObservedFact(..),
    FactSnapshot,
    TimeMapId(..),
    emptyFactSnapshot,
    Types.EffectBase.calculateEffectHash,
    
    -- * Effect Status
    EffectStatus(..),
    
    -- * Effect Result
    EffectResult(..),
    
    -- * Effect Type
    EffectType(..),
    
    -- * Type Aliases
    ResourceKey,
    Condition,
    Capability,
    Trigger,
    Expiry,
    FunctionName,
    Value
) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime)
import Core.Common (Hash(..), EntityHash(..), computeHash, LamportTime)
import qualified Data.ByteString as BS
import Data.Word (Word64)

-- Re-export from Types.EffectBase
import Types.EffectBase

-- Type aliases
type ResourceKey = ByteString
type Condition = Text
type Capability = Text
type Trigger = Text
type Expiry = LamportTime
type FunctionName = Text
type Value = Text

-- | Status of an effect
data EffectStatus
    = Proposed    -- ^ Effect has been proposed but not yet applied
    | Validated   -- ^ Effect has been validated but not yet applied
    | Applied     -- ^ Effect has been successfully applied
    | Failed Text -- ^ Effect application failed with reason
    | Rejected    -- ^ Effect was rejected
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialize)

-- | Result of applying an effect
data EffectResult
    = Success EffectId                 -- ^ Effect was successfully applied
    | Failure Text                     -- ^ Effect failed with reason
    | ResponseRequired EffectId        -- ^ Effect requires a response
    | EffectChain [EffectId]           -- ^ Effect triggered a chain of effects
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialize)

-- | Types of effects that can be applied
data EffectType
    = DepositEffect                   -- ^ Deposit resources into a program
    | WithdrawEffect                  -- ^ Withdraw resources from a program
    | TransferEffect                  -- ^ Transfer resources between programs
    | UpdateStateEffect               -- ^ Update program state
    | ObserveEffect                   -- ^ Observe an external fact
    | CallEffect                      -- ^ Call another program
    | EmitEffect                      -- ^ Emit an event
    | TimeoutEffect                   -- ^ Set a timeout
    | RetryEffect                     -- ^ Retry a failed effect
    | ConditionalEffect               -- ^ Execute conditionally
    | BatchEffect                     -- ^ Execute multiple effects as batch
    | SystemEffect                    -- ^ System-level effect (upgrades, etc.)
    | EvolveSchema                    -- ^ Schema evolution effect
        { oldSchemaBytes :: ByteString   -- ^ Previous schema serialized
        , newSchemaBytes :: ByteString   -- ^ New schema serialized
        , evolutionSuccessful :: Bool    -- ^ Whether evolution succeeded
        }
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialize)

-- This function is now imported from Types.EffectBase
-- | Calculate a hash for an effect based on its content with timestamp
calculateEffectHashWithTimestamp :: ByteString -> [EffectId] -> ByteString -> Hash
calculateEffectHashWithTimestamp payload parentIds timestamp = 
  let combinedBytes = mconcat 
        [ payload
        , mconcat (map (\(EffectId h) -> unHash h) parentIds)
        , timestamp
        ]
  in computeHash combinedBytes