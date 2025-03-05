{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module: Core.Effect
Description: Defines the core effect system that enables time-travel operations

This module defines the fundamental Effect type and related functions that form the
foundation of the Time-Bandits system. Effects represent atomic, verifiable operations
that can be performed on timelines.

Effects have several key properties:
- They are composable, allowing programs to be built from smaller effect primitives
- They are deterministic, ensuring consistent behavior when replayed
- They can be cryptographically verified for security
- They provide an abstraction over different timeline implementations

The Effect type is used throughout the system as the primary building block for
programs that operate across timelines.
-}
module Core.Effect 
  ( -- * Core Effect Types
    Effect(..)
  , EffectId
  , EffectResult(..)
  
  -- * Effect Metadata
  , EffectMetadata(..)
  , EffectStatus(..)
  
  -- * Effect Preconditions
  , Precondition(..)
  , PreconditionType(..)
  
  -- * Effect Creation
  , createEffect
  , effectId
  
  -- * Effect Validation
  , validateEffect
  , effectPreconditions
  , effectPostconditions
  
  -- * Effect Serialization
  , serializeEffect
  , deserializeEffect
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Serialize (Serialize, encode, decode)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)

-- Import from Core modules only
import Core.Common (Hash, computeHash)
import Core.ProgramId (ProgramId)
import Core.ResourceId (ResourceId)
import Core.TimelineId (TimelineId)

-- | Unique identifier for effects
type EffectId = Hash

-- | Status of an effect in the system
data EffectStatus
  = EffectPending     -- ^ Effect has been created but not applied
  | EffectApplied     -- ^ Effect has been successfully applied
  | EffectRejected    -- ^ Effect application was rejected
  | EffectReverted    -- ^ Effect was applied but later reverted
  deriving (Show, Eq, Generic, Serialize)

-- | Metadata associated with an effect
data EffectMetadata = EffectMetadata
  { effectId :: EffectId                -- ^ Unique identifier for the effect
  , effectCreator :: ProgramId          -- ^ Program that created the effect
  , effectCreatedAt :: UTCTime          -- ^ When the effect was created
  , effectStatus :: EffectStatus        -- ^ Current status of the effect
  , effectPreconditions :: [Precondition] -- ^ Preconditions that must be satisfied
  }
  deriving (Show, Eq, Generic, Serialize)

-- | Type of precondition for effect application
data PreconditionType
  = ResourceOwnership ResourceId ProgramId  -- ^ A resource must be owned by the specified program
  | TimelineState TimelineId ByteString     -- ^ Timeline must be in specific state
  | TimeCondition UTCTime                   -- ^ Time-based condition
  | LogicalCondition Text                   -- ^ Logical condition expressed as code
  deriving (Show, Eq, Generic, Serialize)

-- | Precondition that must be satisfied for an effect to be applied
data Precondition = Precondition
  { preconditionType :: PreconditionType  -- ^ Type of precondition
  , preconditionDescription :: Text       -- ^ Human-readable description
  }
  deriving (Show, Eq, Generic, Serialize)

-- | Result of attempting to apply an effect
data EffectResult
  = EffectSuccess ByteString              -- ^ Effect was successfully applied with result data
  | EffectFailure Text                    -- ^ Effect application failed with error message
  | EffectDeferred UTCTime                -- ^ Effect will be applied at a later time
  deriving (Show, Eq, Generic, Serialize)

-- | The core Effect type representing atomic operations
data Effect
  = ResourceEffect ResourceId ByteString  -- ^ Effect on a resource
  | TimelineEffect TimelineId ByteString  -- ^ Effect on a timeline
  | ProgramEffect ProgramId ByteString    -- ^ Effect on a program
  | CompositeEffect [Effect]              -- ^ Composition of multiple effects
  deriving (Show, Eq, Generic, Serialize)

-- | Create a new effect with metadata
createEffect :: ProgramId -> [Precondition] -> Effect -> IO (Effect, EffectMetadata)
createEffect programId preconditions effect = do
  currentTime <- getCurrentTime
  let effectHash = computeHash $ encode effect
      metadata = EffectMetadata
        { effectId = effectHash
        , effectCreator = programId
        , effectCreatedAt = currentTime
        , effectStatus = EffectPending
        , effectPreconditions = preconditions
        }
  pure (effect, metadata)

-- | Get the unique identifier for an effect
effectId :: Effect -> EffectId
effectId = computeHash . encode

-- | Validate that an effect can be applied
validateEffect :: Effect -> [Precondition] -> Bool
validateEffect _ [] = True  -- No preconditions means it's always valid
validateEffect _ _ = True   -- Actual validation is performed by the interpreter

-- | Get all preconditions required for an effect
effectPreconditions :: Effect -> [Precondition]
effectPreconditions (ResourceEffect _ _) = []  -- Placeholder, actual implementation depends on effect
effectPreconditions (TimelineEffect _ _) = []  -- Placeholder, actual implementation depends on effect
effectPreconditions (ProgramEffect _ _) = []   -- Placeholder, actual implementation depends on effect
effectPreconditions (CompositeEffect effects) = 
  concatMap effectPreconditions effects        -- Combine preconditions from all sub-effects

-- | Get all postconditions guaranteed by an effect
effectPostconditions :: Effect -> [Precondition]
effectPostconditions (ResourceEffect _ _) = []  -- Placeholder, actual implementation depends on effect
effectPostconditions (TimelineEffect _ _) = []  -- Placeholder, actual implementation depends on effect
effectPostconditions (ProgramEffect _ _) = []   -- Placeholder, actual implementation depends on effect
effectPostconditions (CompositeEffect effects) = 
  concatMap effectPostconditions effects        -- Combine postconditions from all sub-effects

-- | Serialize an effect to bytes
serializeEffect :: Effect -> ByteString
serializeEffect = encode

-- | Deserialize an effect from bytes
deserializeEffect :: ByteString -> Either String Effect
deserializeEffect = decode 