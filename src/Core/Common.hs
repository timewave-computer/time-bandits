{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
  
  -- * Basic Utilities
  , computeHash
  , computeSha256
  ) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize (Serialize)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

-- | Cryptographic hash for content-addressable data
newtype Hash = Hash { unHash :: ByteString }
  deriving (Eq, Ord, Generic, Serialize)

instance Show Hash where
  show (Hash h) = "Hash:" ++ show (BS.take 8 h) ++ "..."

instance IsString Hash where
  fromString = Hash . TE.encodeUtf8 . T.pack

-- | Digital signature for data authentication
newtype Signature = Signature { unSignature :: ByteString }
  deriving (Eq, Ord, Generic, Serialize)

instance Show Signature where
  show (Signature s) = "Sig:" ++ show (BS.take 8 s) ++ "..."

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
  deriving (Eq, Ord, Generic, Serialize)

instance Show AssetAmount where
  show (AssetAmount amt) = show amt

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

-- | Compute a hash of a ByteString
computeHash :: ByteString -> Hash
computeHash = Hash . SHA256.hash

-- | Compute a SHA-256 hash directly returning ByteString
computeSha256 :: ByteString -> ByteString
computeSha256 = SHA256.hash
