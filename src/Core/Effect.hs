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
  , getEffectId
  
  -- * Effect Validation
  , validateEffect
  , getEffectPreconditions
  , effectPostconditions
  
  -- * Effect Serialization
  , serializeEffect
  , deserializeEffect
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Serialize (Serialize, encode, decode)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import qualified Data.Text.Encoding as TE

-- Import from Core modules only
import Core.Common (Hash)
import Core.ProgramId (ProgramId)
import Core.ResourceId (ResourceId)
import Core.TimelineId (TimelineId)
import Core.ActorId (ActorId)
import Core.AccountProgram (AccountMessage)
import Core.Utils (computeContentHashSimple)

-- | Unique identifier for effects
type EffectId = Hash

-- | Status of an effect in the system
data EffectStatus
  = EffectPending     -- ^ Effect has been created but not applied
  | EffectApplied     -- ^ Effect has been successfully applied
  | EffectRejected    -- ^ Effect application was rejected
  | EffectReverted    -- ^ Effect was applied but later reverted
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | Metadata associated with an effect
data EffectMetadata = EffectMetadata
  { effectId :: EffectId                -- ^ Unique identifier for the effect
  , effectCreator :: ProgramId          -- ^ Program that created the effect
  , effectCreatedAt :: UTCTime          -- ^ When the effect was created
  , effectStatus :: EffectStatus        -- ^ Current status of the effect
  , effectPreconditions :: [Precondition] -- ^ Preconditions that must be satisfied
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | Type of precondition for effect application
data PreconditionType
  = ResourceOwnership ResourceId ProgramId  -- ^ A resource must be owned by the specified program
  | TimelineState TimelineId ByteString     -- ^ Timeline must be in specific state
  | TimeCondition UTCTime                   -- ^ Time-based condition
  | LogicalCondition Text                   -- ^ Logical condition expressed as code
  deriving stock (Show, Eq, Generic)

-- Manual instance for PreconditionType
instance Serialize PreconditionType where
  put (ResourceOwnership rid pid) = do
    Serialize.putWord8 0
    Serialize.put rid
    Serialize.put pid
  put (TimelineState tid bs) = do
    Serialize.putWord8 1
    Serialize.put tid
    Serialize.put bs
  put (TimeCondition utc) = do
    Serialize.putWord8 2
    Serialize.put utc
  put (LogicalCondition txt) = do
    Serialize.putWord8 3
    Serialize.put (TE.encodeUtf8 txt)
  
  get = do
    tag <- Serialize.getWord8
    case tag of
      0 -> ResourceOwnership <$> Serialize.get <*> Serialize.get
      1 -> TimelineState <$> Serialize.get <*> Serialize.get
      2 -> TimeCondition <$> Serialize.get
      3 -> LogicalCondition . TE.decodeUtf8 <$> Serialize.get
      _ -> fail $ "Invalid PreconditionType tag: " ++ show tag

-- | Precondition that must be satisfied for an effect to be applied
data Precondition = Precondition
  { preconditionType :: PreconditionType  -- ^ Type of precondition
  , preconditionDescription :: Text       -- ^ Human-readable description
  }
  deriving stock (Show, Eq, Generic)

-- Manual instance for Precondition
instance Serialize Precondition where
  put (Precondition ptype pdesc) = do
    Serialize.put ptype
    Serialize.put (TE.encodeUtf8 pdesc)
  
  get = do
    ptype <- Serialize.get
    pdesc <- TE.decodeUtf8 <$> Serialize.get
    return $ Precondition ptype pdesc

-- | Result of attempting to apply an effect
data EffectResult
  = EffectSuccess ByteString              -- ^ Effect was successfully applied with result data
  | EffectFailure Text                    -- ^ Effect application failed with error message
  | EffectDeferred UTCTime                -- ^ Effect will be applied at a later time
  deriving stock (Show, Eq, Generic)

-- Manual instance for EffectResult
instance Serialize EffectResult where
  put (EffectSuccess bs) = do
    Serialize.putWord8 0
    Serialize.put bs
  put (EffectFailure txt) = do
    Serialize.putWord8 1
    Serialize.put (TE.encodeUtf8 txt)
  put (EffectDeferred utc) = do
    Serialize.putWord8 2
    Serialize.put utc
  
  get = do
    tag <- Serialize.getWord8
    case tag of
      0 -> EffectSuccess <$> Serialize.get
      1 -> EffectFailure . TE.decodeUtf8 <$> Serialize.get
      2 -> EffectDeferred <$> Serialize.get
      _ -> fail $ "Invalid EffectResult tag: " ++ show tag

-- | The core Effect type representing atomic operations
data Effect
  = ResourceEffect ResourceId ByteString  -- ^ Effect on a resource
  | TimelineEffect TimelineId ByteString  -- ^ Effect on a timeline
  | ProgramEffect ProgramId ByteString    -- ^ Effect on a program
  | CompositeEffect [Effect]              -- ^ Composition of multiple effects
  | AccountMessageEffect ActorId AccountMessage  -- ^ Message to an account program
  deriving stock (Show, Eq, Generic)

-- Manual Serialize instance for Effect
instance Serialize Effect where
  put (ResourceEffect rid bs) = do
    Serialize.putWord8 0
    Serialize.put rid
    Serialize.put bs
  put (TimelineEffect tid bs) = do
    Serialize.putWord8 1
    Serialize.put tid
    Serialize.put bs
  put (ProgramEffect pid bs) = do
    Serialize.putWord8 2
    Serialize.put pid
    Serialize.put bs
  put (CompositeEffect effects) = do
    Serialize.putWord8 3
    Serialize.put effects
  put (AccountMessageEffect actorId _) = do
    -- For now, we don't fully serialize AccountMessage
    -- This should be implemented properly in a real application
    Serialize.putWord8 4
    Serialize.put actorId
    -- We'd serialize the AccountMessage here if it had a Serialize instance
    
  get = do
    tag <- Serialize.getWord8
    case tag of
      0 -> ResourceEffect <$> Serialize.get <*> Serialize.get
      1 -> TimelineEffect <$> Serialize.get <*> Serialize.get
      2 -> ProgramEffect <$> Serialize.get <*> Serialize.get
      3 -> CompositeEffect <$> Serialize.get
      4 -> do
           actorId <- Serialize.get
           -- For now, we return a placeholder AccountMessage
           -- This should be implemented properly in a real application
           return $ AccountMessageEffect actorId (error "AccountMessage deserialization not implemented")
      _ -> fail $ "Invalid Effect tag: " ++ show tag

-- | Create a new effect with metadata
createEffect :: ProgramId -> [Precondition] -> Effect -> IO (Effect, EffectMetadata)
createEffect programId preconditions effect = do
  currentTime <- getCurrentTime
  let effectHash = computeContentHashSimple effect
      metadata = EffectMetadata
        { effectId = effectHash
        , effectCreator = programId
        , effectCreatedAt = currentTime
        , effectStatus = EffectPending
        , effectPreconditions = preconditions
        }
  pure (effect, metadata)

-- | Get the unique identifier for an effect
getEffectId :: Effect -> EffectId
getEffectId = computeContentHashSimple

-- | Validate that an effect can be applied
validateEffect :: Effect -> [Precondition] -> Bool
validateEffect _ [] = True  -- No preconditions means it's always valid
validateEffect _ _ = True   -- Actual validation is performed by the interpreter

-- | Get all preconditions required for an effect
getEffectPreconditions :: Effect -> [Precondition]
getEffectPreconditions (ResourceEffect _ _) = []  -- Placeholder, actual implementation depends on effect
getEffectPreconditions (TimelineEffect _ _) = []  -- Placeholder, actual implementation depends on effect
getEffectPreconditions (ProgramEffect _ _) = []   -- Placeholder, actual implementation depends on effect
getEffectPreconditions (CompositeEffect effects) = 
  concatMap getEffectPreconditions effects        -- Combine preconditions from all sub-effects
getEffectPreconditions (AccountMessageEffect _ _) = []  -- Placeholder, actual implementation depends on message type

-- | Get all postconditions guaranteed by an effect
effectPostconditions :: Effect -> [Precondition]
effectPostconditions (ResourceEffect _ _) = []  -- Placeholder, actual implementation depends on effect
effectPostconditions (TimelineEffect _ _) = []  -- Placeholder, actual implementation depends on effect
effectPostconditions (ProgramEffect _ _) = []   -- Placeholder, actual implementation depends on effect
effectPostconditions (CompositeEffect effects) = 
  concatMap effectPostconditions effects        -- Combine postconditions from all sub-effects
effectPostconditions (AccountMessageEffect _ _) = []  -- Placeholder, actual implementation depends on message type

-- | Serialize an effect to bytes
serializeEffect :: Effect -> ByteString
serializeEffect = encode

-- | Deserialize an effect from bytes
deserializeEffect :: ByteString -> Either String Effect
deserializeEffect = decode 