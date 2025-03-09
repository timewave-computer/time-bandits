{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
Module: Types.EffectBase
Description: Foundation types for the effect system

This module provides the most basic shared types for the effect system that
need to be imported by multiple modules without creating circular dependencies.
This module should have minimal imports itself to avoid introducing new cycles.
-}
module Types.EffectBase
  ( -- * Basic Effect Types
    EffectId(..)
  , FactId(..)
  , FactValue(..)
  , TimeMapId(..)
  , FactSnapshot
  , emptyFactSnapshot
  , ObservedFact(..)
  , ObservationProof(..)
  , calculateEffectHash
  
  -- * Simple Type Definitions  
  , ProtocolVersion
  , ProgramState
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, nominalDay)
import qualified Data.Time.Clock as Time
import Data.Time.Calendar (fromGregorian)
import Data.Serialize (Serialize(..))
import qualified Data.Serialize as S
import GHC.Generics (Generic)
import Data.Version (Version)
import Data.Aeson (Value)
import Control.DeepSeq (NFData)

-- Import only core primitives
import Core.Common (Hash(..), EntityHash(..), computeHash)

-- Import the Serialize Text instance from Core.SerializeInstances
import Core.SerializeInstances ()

-- | Epoch time reference (start of 1970)
epochUTCTime :: UTCTime
epochUTCTime = Time.UTCTime (fromGregorian 1970 1 1) 0

-- | Manual Serialize instance for UTCTime
instance Serialize UTCTime where
  put time = do
    -- Serialize as seconds since epoch (rational number)
    S.put (realToFrac (Time.diffUTCTime time epochUTCTime) :: Double)
  get = do
    -- Deserialize from seconds since epoch
    secs <- S.get :: S.Get Double
    return $ Time.addUTCTime (realToFrac secs) epochUTCTime

-- | The ID of an effect (hash of its content)
newtype EffectId = EffectId { unEffectId :: Hash }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize, NFData)

-- | Ord instance for EffectId
instance Ord EffectId where
  compare (EffectId h1) (EffectId h2) = compare h1 h2

-- | The ID of a fact (content-addressed)
data FactId = FactId
  { factSource :: Text   -- ^ Source of the fact
  , factName :: Text     -- ^ Name of the fact
  , factHash :: Hash     -- ^ Hash of the fact content
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Ord instance for FactId
instance Ord FactId where
  compare (FactId s1 n1 h1) (FactId s2 n2 h2) = 
    case compare s1 s2 of
      EQ -> case compare n1 n2 of
              EQ -> compare h1 h2
              o -> o
      o -> o

-- Manual instance for Serialize
instance Serialize FactId where
  put (FactId source name hash) = do
    S.put source
    S.put name
    S.put hash
  get = do
    source <- S.get
    name <- S.get
    hash <- S.get
    return $ FactId source name hash

-- | Values that a fact can hold
data FactValue
  = IntValue Int
  | TextValue Text
  | BoolValue Bool
  | BytesValue ByteString
  | TimeValue UTCTime
  | HashValue Hash
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- Manual instance for Serialize
instance Serialize FactValue where
  put (IntValue i) = do
    S.putWord8 0
    S.put i
  put (TextValue t) = do
    S.putWord8 1
    S.put t
  put (BoolValue b) = do
    S.putWord8 2
    S.put b
  put (BytesValue bs) = do
    S.putWord8 3
    S.put bs
  put (TimeValue time) = do
    S.putWord8 4
    S.put time
  put (HashValue h) = do
    S.putWord8 5
    S.put h
  
  get = do
    tag <- S.getWord8
    case tag of
      0 -> IntValue <$> S.get
      1 -> TextValue <$> S.get
      2 -> BoolValue <$> S.get
      3 -> BytesValue <$> S.get
      4 -> TimeValue <$> S.get
      5 -> HashValue <$> S.get
      _ -> fail $ "Invalid FactValue tag: " ++ show tag

-- | An observed fact from an external system
data ObservedFact = ObservedFact
  { ofId :: FactId
  , ofValue :: FactValue
  , ofObservedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- Manual instance for Serialize
instance Serialize ObservedFact where
  put (ObservedFact factId value time) = do
    S.put factId
    S.put value
    S.put time
  
  get = do
    factId <- S.get
    value <- S.get
    time <- S.get
    return $ ObservedFact factId value time

-- | Cryptographic proof of a fact observation
data ObservationProof = ObservationProof
  { opFact :: FactId
  , opProofBytes :: ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- Manual instance for Serialize
instance Serialize ObservationProof where
  put (ObservationProof factId proofBytes) = do
    S.put factId
    S.put proofBytes
  
  get = do
    factId <- S.get
    proofBytes <- S.get
    return $ ObservationProof factId proofBytes

-- | A snapshot of observed facts
type FactSnapshot = Map FactId FactValue

-- | An empty fact snapshot
emptyFactSnapshot :: FactSnapshot
emptyFactSnapshot = Map.empty

-- | ID for a time map
newtype TimeMapId = TimeMapId { unTimeMapId :: Hash }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize, NFData)

-- | Protocol version type (reexported)
type ProtocolVersion = Version

-- | The program state type (simplified) (reexported)
type ProgramState = Map Text Value

-- | Calculate a hash for an effect based on its payload and parents
calculateEffectHash :: ByteString -> [EffectId] -> Hash
calculateEffectHash payloadBytes parentEffects =
  let parentHashes = map (\(EffectId h) -> h) parentEffects
      combinedBytes = mconcat [payloadBytes, S.encode parentHashes]
  in computeHash combinedBytes 