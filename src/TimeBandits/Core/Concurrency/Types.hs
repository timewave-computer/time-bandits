{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : TimeBandits.Core.Concurrency.Types
Description : Common types for the Time Bandits concurrency system
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module defines common types used across the TimeBandits concurrency system.
It's designed to avoid cyclic module dependencies by providing type-only definitions
that don't depend on implementation details from other modules.
-}
module TimeBandits.Core.Concurrency.Types
  ( 
  -- * Re-exports from other modules
    ResourceId
  , EffectId
  
  -- * Forward declarations to break cycles
  , Effect(..)
  , EffectType(..)
  
  -- * Lock types
  , LockError(..)
  , LockResult
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import TimeBandits.Core.ResourceId (ResourceId)
import GHC.Generics (Generic)

-- | Forward declaration of EffectId to avoid circular imports
-- The actual type is defined in TimeBandits.Core.Effect
type EffectId = ByteString 

-- | Basic effect types for forward declaration
data EffectType = 
    CreateEffect 
  | TransferEffect
  | ConsumeEffect
  | CustomEffect Text
  deriving (Eq, Show, Generic)

-- | Forward declaration of Effect type to avoid circular imports
-- This is a placeholder for the real Effect type
data Effect = Effect ResourceId EffectType ByteString ByteString
  deriving (Eq, Show, Generic)

-- | Errors that can occur during lock operations
data LockError
  = ResourceBusy EffectId         -- ^ Resource is locked by another effect
  | LockTimeout ResourceId        -- ^ Lock acquisition timed out
  | LockNotOwned ResourceId       -- ^ Attempted to release a lock not owned
  | InternalLockError Text        -- ^ Internal error
  deriving (Eq, Show)

-- | Result of a lock operation
type LockResult a = Either LockError a 