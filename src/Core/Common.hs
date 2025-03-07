{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}

{- |
Module: Core.Common
Description: Common utility types for the Time Bandits system

This module provides common utility types and functions that are used throughout
the Time Bandits system. These include basic data types for cryptographic hashes,
addresses, assets, and other primitive values.

This is the canonical source for fundamental types like Hash, Signature,
EntityHash, etc. to avoid duplication and import conflicts.
-}
module Core.Common
  ( -- * Cryptographic Primitives
    Hash(..)
  , EntityHash(..)
  , Signature(..)
  , SignatureError(..)
  , VerificationError(..)
  , PubKey(..)
  , PrivKey(..)
  
  -- * Entity Hash Type Aliases
  , ActorHash
  , ResourceHash
  , TimelineHash
  
  -- * Phantom Types for Entities
  , Actor
  , Resource
  , Timeline
  
  -- * Asset Representation
  , AssetType(..)
  , AssetAmount(..)
  , AssetId(..)
  , Asset(..)
  
  -- * Time Representation
  , LamportTime(..)
  , TimeMapEntry(..)
  , TimeMap(..)
  , emptyTimeMap
  
  -- * Map Utility Functions
  , getValueByMaxKey
  , tryGetValueByKey
  , findMapEntryByValue
  
  -- * Address Types
  , Address(..)
  , AddressType(..)
  
  -- * Simulation Modes
  , SimulationMode(..)
  
  -- * Basic Utilities
  , computeHash
  , computeSha256
  , generateEntityHash
  ) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Data.Time (Day (..), DiffTime, UTCTime (..))
import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import Control.Monad (when)
import Core.Serialize () -- Import Serialize instances

-- | Instance for serializing UTCTime
instance Serialize UTCTime where
  put (UTCTime day time) = do
    S.put (toModifiedJulianDay day)
    S.put (realToFrac time :: Double)
  get = do
    day <- fmap ModifiedJulianDay (S.get :: S.Get Integer)
    time <- fmap (realToFrac :: Double -> DiffTime) (S.get :: S.Get Double)
    pure $ UTCTime day time

-- | Cryptographic hash for content-addressable data
newtype Hash = Hash { unHash :: ByteString }
  deriving (Eq, Ord, Generic)
  deriving anyclass (Serialize)
  deriving stock (Show)

instance IsString Hash where
  fromString = Hash . TE.encodeUtf8 . T.pack

-- | A type-safe wrapper for hashes of different entities
-- This provides type safety when dealing with different entity hashes
newtype EntityHash a = EntityHash { unEntityHash :: Hash }
  deriving (Eq, Ord, Generic)
  deriving anyclass (Serialize)
  deriving stock (Show)

-- | Phantom types for type-safe entity references

-- | Phantom type for Actor entity
data Actor

-- | Phantom type for Resource entity
data Resource

-- | Phantom type for Timeline entity
data Timeline

-- | Type aliases for common entity hashes
type ActorHash = EntityHash Actor
type ResourceHash = EntityHash Resource
type TimelineHash = EntityHash Timeline

-- | Represents a logical timestamp in a distributed system
newtype LamportTime = LamportTime Int
  deriving (Eq, Ord, Generic)
  deriving anyclass (Serialize)
  deriving stock (Show)

-- | Represents a public key, which uniquely identifies actors.
newtype PubKey = PubKey ByteString
  deriving (Eq, Ord, Generic)
  deriving anyclass (Serialize)
  deriving stock (Show)

-- | Represents a private key, used for signing messages.
newtype PrivKey = PrivKey ByteString
  deriving (Eq, Ord, Generic)
  deriving anyclass (Serialize)
  deriving stock (Show)

-- | Digital signature for data authentication
newtype Signature = Signature { unSignature :: ByteString }
  deriving (Eq, Ord, Generic)
  deriving anyclass (Serialize)
  deriving stock (Show)

-- | Types of errors that can occur during signature creation
data SignatureError
  = InvalidSigningKey Text
  | SigningDataError Text
  deriving (Show, Eq)

-- | Types of errors that can occur during signature verification
data VerificationError
  = InvalidVerificationKey Text
  | VerificationDataError Text
  | SignatureMismatch
  deriving (Show, Eq)

-- | Types of assets in the system
data AssetType
  = NativeToken    -- ^ Native token of a timeline
  | ERC20Token     -- ^ ERC-20 compatible token
  | NFT            -- ^ Non-fungible token
  | GenericAsset   -- ^ Generic asset type
  deriving (Show, Eq, Generic, Serialize)

-- | Amount of an asset
newtype AssetAmount = AssetAmount { unAssetAmount :: Integer }
  deriving (Eq, Ord, Generic)
  deriving anyclass (Serialize)
  deriving stock (Show)

-- | Unique identifier for an asset
type AssetId = ByteString

-- | An asset with metadata
data Asset = Asset
  { assetType :: AssetType       -- ^ Type of the asset
  , assetId :: AssetId           -- ^ Unique identifier
  , assetAmount :: AssetAmount   -- ^ Amount of the asset
  }
  deriving (Show, Eq, Generic, Serialize)

-- | Types of addresses in the system
data AddressType
  = EOA           -- ^ Externally Owned Account
  | Contract      -- ^ Smart Contract
  | Timelock      -- ^ Timelock address
  | Multisig      -- ^ Multisignature address
  deriving (Show, Eq, Generic, Serialize)

-- | An address in a timeline
data Address = Address
  { addressType :: AddressType        -- ^ Type of address
  , addressBytes :: ByteString        -- ^ Raw address bytes
  }
  deriving (Show, Eq, Generic, Serialize)

-- | Simulation modes for the system
data SimulationMode
  = InMemory          -- ^ Run everything in memory (simplest mode)
  | LocalProcesses    -- ^ Run actors as separate local processes
  | GeoDistributed    -- ^ Run as geo-distributed system
  deriving (Show, Eq, Generic, Serialize)

-- | Compute a hash of a ByteString
computeHash :: ByteString -> Hash
computeHash = Hash . SHA256.hash

-- | Compute a SHA-256 hash directly returning ByteString
computeSha256 :: ByteString -> ByteString
computeSha256 = SHA256.hash

-- | Generate an entity hash for a serializable value
generateEntityHash :: Serialize a => a -> EntityHash b
generateEntityHash = EntityHash . computeHash . S.encode

-- | Represents a time map entry
data TimeMapEntry = TimeMapEntry
  { timeMapEntryKey :: LamportTime
  , timeMapEntryValue :: AssetAmount
  }
  deriving (Show, Eq, Generic, Serialize)

-- | Represents a time map
data TimeMap = TimeMap
  { timeMapEntries :: Map.Map LamportTime TimeMapEntry }
  deriving (Show, Eq, Generic, Serialize)

-- | Represents an empty time map
emptyTimeMap :: TimeMap
emptyTimeMap = TimeMap Map.empty

-- | Gets the value associated with the maximum key in a time map
getValueByMaxKey :: TimeMap -> Maybe AssetAmount
getValueByMaxKey (TimeMap entries) =
  case Map.lookupMax entries of
    Nothing -> Nothing
    Just (_, entry) -> Just (timeMapEntryValue entry)

-- | Tries to get the value associated with a key in a time map
tryGetValueByKey :: LamportTime -> TimeMap -> Maybe AssetAmount
tryGetValueByKey key (TimeMap entries) = 
  case Map.lookup key entries of
    Nothing -> Nothing
    Just entry -> Just (timeMapEntryValue entry)

-- | Finds the map entry associated with a value in a time map
findMapEntryByValue :: AssetAmount -> TimeMap -> Maybe TimeMapEntry
findMapEntryByValue value (TimeMap entries) = 
  case Map.toList (Map.filter (\entry -> timeMapEntryValue entry == value) entries) of
    [] -> Nothing
    (_, entry):_ -> Just entry
