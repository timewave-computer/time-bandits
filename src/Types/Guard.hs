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
Module: Types.Guard
Description: Guard types for conditional effect execution

This module defines the guard types used to implement conditional effect
execution. Guards represent preconditions that must be satisfied before
an effect can be applied.

Guards are used to:
- Implement conditional execution
- Enforce security constraints
- Implement timeouts and retries
- Gate effects based on external state
-}
module Types.Guard 
  ( -- * Guard Types
    Guard(..)
  , GuardedEffect(..)
  
  -- * Guard Operations
  , createGuardedEffect
  
  -- * Condition Types
  , Condition(..)
  , Trigger(..)
  , Expiry(..)
  
  -- * Re-exports
  , Effect
  ) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime)

-- Import from Types modules
import Types.Effect (Effect)
import Types.Core (FactId)

-- | A condition that must be met for a guard to trigger
data Condition
  = FactCondition FactId               -- ^ Condition based on a fact
  | TimeCondition UTCTime              -- ^ Condition based on time
  | ResourceCondition Text ByteString  -- ^ Condition based on a resource
  | AndCondition [Condition]           -- ^ All conditions must be met
  | OrCondition [Condition]            -- ^ Any condition must be met
  | NotCondition Condition             -- ^ Condition must not be met
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | What happens when a guard condition is met
data Trigger
  = ApplyEffect                        -- ^ Apply the guarded effect
  | CancelEffect                       -- ^ Cancel the guarded effect
  | ReplaceEffect Effect               -- ^ Replace with another effect
  | NotifyOnly                         -- ^ Just notify, don't apply
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | When a guard expires
data Expiry
  = ExpiryTime UTCTime                 -- ^ Expires at a specific time
  | ExpiryDuration Integer             -- ^ Expires after duration (ms)
  | ExpiryCondition Condition          -- ^ Expires when condition is met
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A guard is a condition that must be met for an effect to apply
data Guard = Guard
  { guardCondition :: Condition        -- ^ The condition to check
  , guardTrigger :: Trigger            -- ^ What to do when the condition is met
  , guardExpiry :: Maybe Expiry        -- ^ When the guard expires
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A guarded effect pairs an effect with its guard
data GuardedEffect = GuardedEffect
  { effectGuard :: Guard           -- ^ The guard condition
  , guardedEffect :: Effect         -- ^ The effect to apply when the guard is met
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a guarded effect
createGuardedEffect :: Guard -> Effect -> GuardedEffect
createGuardedEffect g e = GuardedEffect
  { effectGuard = g
  , guardedEffect = e
  } 