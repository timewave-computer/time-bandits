{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Core.Common
Description: Common utility types for the Time Bandits system

This module provides common utility types and functions that are used throughout
the Time Bandits system. These include basic data types for cryptographic hashes,
addresses, assets, and other primitive values.
-}
module Core.Common
  ( -- * Cryptographic Primitives
    Hash(..)
  , Signature(..)
  , SignatureError(..)
  , VerificationError(..)
  
  -- * Asset Representation
  , Asset(..)
  , AssetType(..)
  , AssetAmount(..)
  , AssetId
  
  -- * Address Types
  , Address(..)
  , AddressType(..)
  
  -- * Entity Identifiers
  , EntityHash(..)
  , EntityId
  
  -- * Key Types
  , PubKey(..)
  , PrivKey(..)
  
  -- * Utility Functions
  , generateEntityHash
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Serialize (Serialize)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import qualified Crypto.Hash.SHA256 as SHA256

-- | Cryptographic hash for content-addressable data
newtype Hash = Hash { unHash :: ByteString }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Serialize)

instance IsString Hash where
  fromString = Hash . TE.encodeUtf8 . T.pack

-- | Digital signature for data authentication
newtype Signature = Signature { unSignature :: ByteString }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Serialize)

instance IsString Signature where
  fromString = Signature . TE.encodeUtf8 . T.pack

-- | Types of errors that can occur during signature creation
data SignatureError
  = InvalidSigningKey Text
  | SigningDataError Text
  deriving stock (Show, Eq)

-- | Types of errors that can occur during signature verification
data VerificationError
  = InvalidVerificationKey Text
  | VerificationDataError Text
  | SignatureMismatch
  deriving stock (Show, Eq)

-- | Types of assets in the system
data AssetType
  = NativeToken    -- ^ Native token of a timeline
  | ERC20Token     -- ^ ERC-20 compatible token
  | NFT            -- ^ Non-fungible token
  | GenericAsset   -- ^ Generic asset type
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | Amount of an asset
newtype AssetAmount = AssetAmount { unAssetAmount :: Integer }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Serialize)

-- | Unique identifier for an asset
type AssetId = ByteString

-- | Asset representation
data Asset = Asset
  { assetId     :: AssetId
  , assetType   :: AssetType
  , assetAmount :: AssetAmount
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | Types of addresses in the system
data AddressType
  = EthereumAddress
  | SolanaAddress
  | GenericAddress
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | Address on a timeline
data Address = Address
  { addressType  :: AddressType
  , addressValue :: ByteString
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | Entity hash for content-addressable entities
newtype EntityHash a = EntityHash { unEntityHash :: Hash }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialize)

-- | Unique identifier for an entity
type EntityId = ByteString

-- | Public key for cryptographic operations
newtype PubKey = PubKey { unPubKey :: ByteString }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Serialize)

-- | Private key for cryptographic operations
newtype PrivKey = PrivKey { unPrivKey :: ByteString }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Serialize)

-- | Generate an entity hash from a ByteString
generateEntityHash :: ByteString -> Text
generateEntityHash bs = T.pack $ show $ Hash $ SHA256.hash bs

-- | Convert a ByteString to a hexadecimal string
bytesToHex :: ByteString -> String
bytesToHex bs = concatMap (\c -> [hexDigit (ord c `div` 16), hexDigit (ord c `mod` 16)]) (BS.unpack bs)
  where
    ord :: Char -> Int
    ord = fromEnum
    
    hexDigit :: Int -> Char
    hexDigit i
      | i < 10    = toEnum (i + fromEnum '0')
      | otherwise = toEnum (i - 10 + fromEnum 'a') 