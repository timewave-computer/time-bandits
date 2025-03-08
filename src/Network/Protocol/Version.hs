{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Network.Protocol.Version
Description : Protocol version handling for Time Bandits
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides protocol versioning functionality for ensuring compatibility
between different Time Bandits nodes on the network.
-}
module Network.Protocol.Version
  ( -- * Protocol Version
    ProtocolVersion(..)
  , VersionRange(..)
  , FeatureFlag(..)
  
    -- * Version Validation
  , isCompatible
  , validateVersion
  , supportedFeatures
  
    -- * Current Version
  , currentVersion
  , minimumCompatibleVersion
  , requiredFeatures
  
    -- * Serialization
  , versionToBytes
  , bytesToVersion
  ) where

import Data.Bits ((.|.), (.&.), shift, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- | Protocol version with major, minor, and patch numbers
data ProtocolVersion = ProtocolVersion
  { majorVersion :: Word32     -- ^ Major version (incompatible changes)
  , minorVersion :: Word32     -- ^ Minor version (backward compatible changes)
  , patchVersion :: Word32     -- ^ Patch version (bug fixes)
  , featureFlags :: Word64     -- ^ Feature flags bitmap
  } deriving (Eq, Generic)

instance Show ProtocolVersion where
  show (ProtocolVersion major minor patch _) =
    show major ++ "." ++ show minor ++ "." ++ show patch

instance ToJSON ProtocolVersion
instance FromJSON ProtocolVersion

-- | Range of compatible protocol versions
data VersionRange = VersionRange
  { minVersion :: ProtocolVersion  -- ^ Minimum compatible version
  , maxVersion :: ProtocolVersion  -- ^ Maximum compatible version
  } deriving (Show, Eq, Generic)

instance ToJSON VersionRange
instance FromJSON VersionRange

-- | Feature flags for the protocol
data FeatureFlag
  = BasicMessaging       -- ^ Basic peer-to-peer messaging
  | PubSubSupport        -- ^ Topic-based publish/subscribe
  | ContentAddressing    -- ^ Content-addressed messaging
  | RendezvousDiscovery  -- ^ Rendezvous hashing for peer discovery
  | SecureTransport      -- ^ Encrypted transport (TLS/QUIC)
  | PeerAuthentication   -- ^ Peer authentication
  | NATTraversal         -- ^ NAT traversal capabilities
  | CompressedMessages   -- ^ Message compression
  | BinaryEncoding       -- ^ Binary message encoding
  | JSONEncoding         -- ^ JSON message encoding
  deriving (Show, Eq, Enum, Bounded)

-- | Current protocol version of this node
currentVersion :: ProtocolVersion
currentVersion = ProtocolVersion 0 1 0 (featuresToFlags requiredFeatures)

-- | Minimum protocol version compatible with this node
minimumCompatibleVersion :: ProtocolVersion
minimumCompatibleVersion = ProtocolVersion 0 1 0 (featuresToFlags essentialFeatures)

-- | Features required by this node
requiredFeatures :: [FeatureFlag]
requiredFeatures =
  [ BasicMessaging
  , PubSubSupport
  , RendezvousDiscovery
  , SecureTransport
  , BinaryEncoding
  ]

-- | Essential features for basic operation
essentialFeatures :: [FeatureFlag]
essentialFeatures =
  [ BasicMessaging
  , RendezvousDiscovery
  , SecureTransport
  ]

-- | Check if two protocol versions are compatible
isCompatible :: ProtocolVersion -> ProtocolVersion -> Bool
isCompatible ourVersion theirVersion =
  -- Major versions must match
  majorVersion ourVersion == majorVersion theirVersion &&
  -- Their minor version must be at least our minimum
  minorVersion theirVersion >= minorVersion minimumCompatibleVersion &&
  -- They must support all essential features
  hasRequiredFeatures theirVersion essentialFeatures

-- | More comprehensive version validation with detailed results
validateVersion :: ProtocolVersion -> ProtocolVersion -> Either Text Text
validateVersion ourVersion theirVersion
  | majorVersion ourVersion /= majorVersion theirVersion =
      Left $ "Incompatible major version: " <> showVersion theirVersion <> 
             ", expected: " <> showVersion ourVersion
  | minorVersion theirVersion < minorVersion minimumCompatibleVersion =
      Left $ "Incompatible minor version: " <> showVersion theirVersion <> 
             ", minimum required: " <> showVersion minimumCompatibleVersion
  | not (hasRequiredFeatures theirVersion essentialFeatures) =
      Left $ "Missing required features: " <> 
             T.intercalate ", " (map (T.pack . show) missingFeatures)
  | otherwise =
      Right $ "Compatible version: " <> showVersion theirVersion
  where
    showVersion v = T.pack (show v)
    missingFeatures = filter (not . hasFeature theirVersion) essentialFeatures

-- | Get list of features supported by a version
supportedFeatures :: ProtocolVersion -> [FeatureFlag]
supportedFeatures version =
  filter (hasFeature version) [minBound .. maxBound]

-- | Check if a version has a specific feature
hasFeature :: ProtocolVersion -> FeatureFlag -> Bool
hasFeature version feature =
  testBit (featureFlags version) (fromEnum feature)

-- | Check if a version has all required features
hasRequiredFeatures :: ProtocolVersion -> [FeatureFlag] -> Bool
hasRequiredFeatures version required =
  all (hasFeature version) required

-- | Convert feature list to flags bitmap
featuresToFlags :: [FeatureFlag] -> Word64
featuresToFlags = foldr setBit 0
  where
    setBit feature acc = acc .|. bit (fromEnum feature)
    bit n = 1 `shift` n

-- | Convert version to binary representation
versionToBytes :: ProtocolVersion -> ByteString
versionToBytes (ProtocolVersion major minor patch flags) =
  word32ToBytes major <>
  word32ToBytes minor <>
  word32ToBytes patch <>
  word64ToBytes flags

-- | Convert binary representation to version
bytesToVersion :: ByteString -> Maybe ProtocolVersion
bytesToVersion bs
  | BS.length bs >= 16 =
      let (majorBytes, rest1) = BS.splitAt 4 bs
          (minorBytes, rest2) = BS.splitAt 4 rest1
          (patchBytes, flagsBytes) = BS.splitAt 4 rest2
          major = bytesToWord32 majorBytes
          minor = bytesToWord32 minorBytes
          patch = bytesToWord32 patchBytes
          flags = bytesToWord64 flagsBytes
      in Just $ ProtocolVersion major minor patch flags
  | otherwise = Nothing

-- | Convert Word32 to bytes
word32ToBytes :: Word32 -> ByteString
word32ToBytes w =
  BS.pack [ fromIntegral (w `shift` (-24) .&. 0xFF)
          , fromIntegral (w `shift` (-16) .&. 0xFF)
          , fromIntegral (w `shift` (-8) .&. 0xFF)
          , fromIntegral (w .&. 0xFF)
          ]

-- | Convert bytes to Word32
bytesToWord32 :: ByteString -> Word32
bytesToWord32 bs
  | BS.length bs >= 4 =
      let [b1, b2, b3, b4] = map fromIntegral $ BS.unpack $ BS.take 4 bs
      in (b1 `shift` 24) .|. (b2 `shift` 16) .|. (b3 `shift` 8) .|. b4
  | otherwise = 0

-- | Convert Word64 to bytes
word64ToBytes :: Word64 -> ByteString
word64ToBytes w =
  BS.pack [ fromIntegral (w `shift` (-56) .&. 0xFF)
          , fromIntegral (w `shift` (-48) .&. 0xFF)
          , fromIntegral (w `shift` (-40) .&. 0xFF)
          , fromIntegral (w `shift` (-32) .&. 0xFF)
          , fromIntegral (w `shift` (-24) .&. 0xFF)
          , fromIntegral (w `shift` (-16) .&. 0xFF)
          , fromIntegral (w `shift` (-8) .&. 0xFF)
          , fromIntegral (w .&. 0xFF)
          ]

-- | Convert bytes to Word64
bytesToWord64 :: ByteString -> Word64
bytesToWord64 bs
  | BS.length bs >= 8 =
      let bytes = map fromIntegral $ BS.unpack $ BS.take 8 bs
          shifts = [56, 48, 40, 32, 24, 16, 8, 0]
      in foldr (.|.) 0 (zipWith (\b s -> fromIntegral b `shift` s) bytes shifts)
  | otherwise = 0 