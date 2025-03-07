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
Module: Types.EffectPayload
Description: Effect payload types for the effect DAG model

This module defines the effect payload types that are used in the effect system.
By separating these from the core Effect type, we can avoid circular dependencies
between Core and Programs modules.

The effect payload types represent different operations that can be performed
in a program's lifecycle, such as:
- Resource management
- Timeline interactions
- State management
- Time-sensitive operations
-}
module Types.EffectPayload 
  ( -- * Effect Payload
    EffectPayload(..)
  
  -- * Re-exports from EffectTypes
  , EffectId
  , FactId
  , FactValue
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- Import from Types.EffectTypes
import Types.EffectTypes
  ( EffectId
  , FactId
  , FactValue
  )

-- | The EffectPayload type separates the content of an effect from its metadata
data EffectPayload
  = -- | Core effects for resource management
    CreateResourcePayload Text ByteString    -- ^ Create a new resource
  | UpdateResourcePayload Text ByteString    -- ^ Update an existing resource
  | DeleteResourcePayload Text               -- ^ Delete a resource
  | TransferResourcePayload Text Text        -- ^ Transfer a resource to another program
  
    -- | Timeline interaction effects
  | ObserveTimelinePayload Text ByteString   -- ^ Observe a timeline state
  | PublishToTimelinePayload Text ByteString -- ^ Publish data to a timeline
  | VerifyTimelinePayload Text ByteString    -- ^ Verify data on a timeline
  
    -- | State management effects
  | ReadStatePayload Text                    -- ^ Read program state
  | WriteStatePayload Text ByteString        -- ^ Write program state
  | DeleteStatePayload Text                  -- ^ Delete program state
  
    -- | Program interaction effects
  | InvokePayload Text Text [ByteString]     -- ^ Invoke a function on another program
  | CallbackPayload Text ByteString          -- ^ Callback from another program
  
    -- | Time-sensitive effects
  | SchedulePayload FactId EffectPayload     -- ^ Schedule an effect for later execution
  | CancelPayload FactId                     -- ^ Cancel a scheduled effect
  
    -- | Composite effects
  | SequencePayload [EffectPayload]          -- ^ Execute effects in sequence
  | ParallelPayload [EffectPayload]          -- ^ Execute effects in parallel
  | ConditionalPayload FactId EffectPayload EffectPayload -- ^ If-then-else
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize) 