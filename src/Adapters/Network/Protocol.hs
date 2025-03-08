{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Adapters.Network.Protocol
Description : Network protocol definition for Time Bandits P2P communication
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module defines the network protocol for Time Bandits P2P communication,
including message envelopes, protocol versioning, and serialization.
-}
module Adapters.Network.Protocol
  ( -- * Protocol Versioning
    ProtocolVersion(..)
  , currentProtocolVersion
  , isCompatibleVersion
    
    -- * Message Envelope
  , MessageEnvelope(..)
  , createEnvelope
  , validateEnvelope
  
    -- * Serialization
  , serializeEnvelope
  , deserializeEnvelope
  
    -- * Signatures
  , Signature(..)
  , signEnvelope
  , verifyEnvelope
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Binary (Binary, encode, decode)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as JSON
import Crypto.Hash (SHA256, Digest, hash)
import qualified Crypto.Hash as Hash
import Control.Monad (when)

-- Import from TimeBandits modules
import Types.Network

-- | Protocol version for network messages
data ProtocolVersion
  = ProtocolV1  -- ^ Version 1 (current)
  | ProtocolV2  -- ^ Version 2 (future)
  deriving (Eq, Ord, Show, Generic)

instance Binary ProtocolVersion
instance ToJSON ProtocolVersion
instance FromJSON ProtocolVersion

-- | Current protocol version
currentProtocolVersion :: ProtocolVersion
currentProtocolVersion = ProtocolV1

-- | Check if a protocol version is compatible with the current version
isCompatibleVersion :: ProtocolVersion -> Bool
isCompatibleVersion ProtocolV1 = True
isCompatibleVersion ProtocolV2 = False  -- Not yet implemented

-- | Digital signature
newtype Signature = Signature { unSignature :: ByteString }
  deriving (Eq, Show, Generic)

instance Binary Signature
instance ToJSON Signature
instance FromJSON Signature

-- | Message envelope for network communication
data MessageEnvelope = MessageEnvelope
  { version :: ProtocolVersion    -- ^ Protocol version
  , sender :: PeerId              -- ^ Sender node ID
  , messageType :: MessageType    -- ^ Type of message
  , timestamp :: UTCTime          -- ^ Time the message was created
  , payload :: ByteString         -- ^ Message content
  , signature :: Maybe Signature  -- ^ Optional signature
  , messageId :: MessageId        -- ^ Unique message ID
  } deriving (Show, Generic)

instance Binary MessageEnvelope
instance ToJSON MessageEnvelope
instance FromJSON MessageEnvelope

-- | Create a message envelope from a message
createEnvelope :: Message -> MessageEnvelope
createEnvelope msg = MessageEnvelope
  { version = currentProtocolVersion
  , sender = sender msg
  , messageType = messageType msg
  , timestamp = timestamp msg
  , payload = payload msg
  , signature = Nothing  -- Will be added by signEnvelope if needed
  , messageId = messageId msg
  }

-- | Validate a message envelope
validateEnvelope :: MessageEnvelope -> Either Text MessageEnvelope
validateEnvelope env = do
  -- Check protocol version
  when (not $ isCompatibleVersion $ version env) $
    Left "Unsupported protocol version"
  
  -- Verify signature if present
  when (isJust $ signature env) $ do
    isValid <- verifyEnvelope env
    when (not isValid) $
      Left "Invalid message signature"
  
  -- All checks passed
  return env

-- | Serialize a message envelope to binary format
serializeEnvelope :: MessageEnvelope -> ByteString
serializeEnvelope = LBS.toStrict . encode

-- | Deserialize a message envelope from binary format
deserializeEnvelope :: ByteString -> Either String MessageEnvelope
deserializeEnvelope bs = 
  Right $ runGet decode (LBS.fromStrict bs)
  `catch` \(e :: SomeException) -> 
    Left $ "Error deserializing envelope: " ++ show e

-- | Sign a message envelope
signEnvelope :: PrivateKey -> MessageEnvelope -> IO MessageEnvelope
signEnvelope key env = do
  -- Simple placeholder implementation
  -- In a real implementation, this would use cryptographic signatures
  let hashable = serializeEnvelope env { signature = Nothing }
      sig = Signature $ Hash.digestToByteString (hash hashable :: Digest SHA256)
  return env { signature = Just sig }

-- | Verify a message envelope signature
verifyEnvelope :: MessageEnvelope -> Bool
verifyEnvelope env =
  case signature env of
    Nothing -> True  -- No signature to verify
    Just sig -> do
      -- Simple placeholder implementation
      -- In a real implementation, this would verify cryptographic signatures
      let hashable = serializeEnvelope env { signature = Nothing }
          expectedSig = Signature $ Hash.digestToByteString (hash hashable :: Digest SHA256)
      sig == expectedSig

-- Helper functions

-- | Check if a Maybe value is Just
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

-- | Try to run an action, catching any exception
catch :: a -> (e -> a) -> a
catch a _ = a  -- Simplified version, real code would use Control.Exception.catch 