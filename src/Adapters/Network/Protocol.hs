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
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import GHC.Generics (Generic)
import Data.Binary (Binary, encode, decode)
import qualified Data.Binary as Binary
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), Value(..))
import qualified Data.Aeson as JSON
import Crypto.Hash (SHA256, Digest, hash)
import qualified Crypto.Hash as Hash
import Control.Monad (when)
import qualified Data.Maybe as Maybe
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as B64
import qualified Data.UUID as UUID

-- Import from TimeBandits modules
import qualified Types.Network as Network
import Types.Network (MessageType(..), PeerId(..), MessageId(..), NetworkConfig(..))

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
createEnvelope :: Network.Message -> MessageEnvelope
createEnvelope msg = MessageEnvelope
  { version = currentProtocolVersion
  , sender = Network.sender msg
  , messageType = Network.messageType msg
  , timestamp = Network.timestamp msg
  , payload = Network.payload msg
  , signature = Nothing  -- Will be added by signEnvelope if needed
  , messageId = Network.messageId msg
  }

-- | Validate a message envelope
validateEnvelope :: MessageEnvelope -> Either Text MessageEnvelope
validateEnvelope env = do
  -- Check protocol version
  when (not $ isCompatibleVersion $ version env) $
    Left "Unsupported protocol version"
  
  -- Verify signature if present
  when (Maybe.isJust $ signature env) $ do
    let valid = verifyEnvelope env
    if valid
      then return ()
      else Left "Invalid signature"
  
  -- All checks passed
  return env

-- | Serialize a message envelope to binary format
serializeEnvelope :: MessageEnvelope -> ByteString
serializeEnvelope = LBS.toStrict . encode

-- | Deserialize a message envelope from binary format
deserializeEnvelope :: ByteString -> Either String MessageEnvelope
deserializeEnvelope bs = 
  -- Use a try/catch block to handle decoding errors
  let decoded = decode (LBS.fromStrict bs) :: MessageEnvelope
  in Right decoded
  `catch` \(e :: SomeException) -> 
    Left $ "Error deserializing envelope: " ++ show e

-- | Private key for signing (simplified placeholder)
newtype PrivateKey = PrivateKey { unPrivateKey :: ByteString }
  deriving (Eq, Show, Generic)

instance Binary PrivateKey

-- | Sign a message envelope
signEnvelope :: PrivateKey -> MessageEnvelope -> IO MessageEnvelope
signEnvelope key env = do
  -- Simple placeholder implementation
  -- In a real implementation, this would use cryptographic signatures
  let hashable = serializeEnvelope env { signature = Nothing }
      -- Convert digest to ByteString using show and BS.pack
      digest = hash hashable :: Digest SHA256
      digestStr = show digest
      sig = Signature $ BS.pack $ map (fromIntegral . fromEnum) digestStr
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
          digest = hash hashable :: Digest SHA256
          digestStr = show digest
          expectedSig = Signature $ BS.pack $ map (fromIntegral . fromEnum) digestStr
      sig == expectedSig

-- Helper functions

-- | Check if a Maybe value is Just
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

-- | Try to run an action, catching any exception
catch :: a -> (e -> a) -> a
catch a _ = a  -- Simplified version, real code would use Control.Exception.catch 

-- ByteString instances using Base64 encoding for JSON
instance ToJSON ByteString where
  toJSON bs = JSON.String $ TE.decodeUtf8 $ B64.encode bs

instance FromJSON ByteString where
  parseJSON (JSON.String txt) = pure $ B64.decodeLenient $ TE.encodeUtf8 txt
  parseJSON _ = fail "Expected String for ByteString"

-- PeerId instances
instance ToJSON PeerId where
  toJSON (PeerId txt) = JSON.String txt

instance FromJSON PeerId where
  parseJSON (JSON.String txt) = pure $ PeerId txt
  parseJSON _ = fail "Expected String for PeerId"

-- Binary instance for PeerId
instance Binary PeerId where
  put (PeerId txt) = Binary.put (TE.encodeUtf8 txt)
  get = PeerId . TE.decodeUtf8 <$> Binary.get

-- MessageType instances
instance Binary MessageType where
  put FactBroadcast = Binary.putWord8 0
  put EffectPropagation = Binary.putWord8 1
  put PeerDiscovery = Binary.putWord8 2
  put TimeMapUpdate = Binary.putWord8 3
  put ResourceUpdate = Binary.putWord8 4
  put ProgramSync = Binary.putWord8 5
  put ControlMessage = Binary.putWord8 6
  
  get = do
    tag <- Binary.getWord8
    case tag of
      0 -> pure FactBroadcast
      1 -> pure EffectPropagation
      2 -> pure PeerDiscovery
      3 -> pure TimeMapUpdate
      4 -> pure ResourceUpdate
      5 -> pure ProgramSync
      6 -> pure ControlMessage
      _ -> fail $ "Unknown MessageType tag: " ++ show tag

instance ToJSON MessageType where
  toJSON FactBroadcast = JSON.String "FactBroadcast"
  toJSON EffectPropagation = JSON.String "EffectPropagation"
  toJSON PeerDiscovery = JSON.String "PeerDiscovery"
  toJSON TimeMapUpdate = JSON.String "TimeMapUpdate"
  toJSON ResourceUpdate = JSON.String "ResourceUpdate"
  toJSON ProgramSync = JSON.String "ProgramSync"
  toJSON ControlMessage = JSON.String "ControlMessage"

instance FromJSON MessageType where
  parseJSON (JSON.String "FactBroadcast") = pure FactBroadcast
  parseJSON (JSON.String "EffectPropagation") = pure EffectPropagation
  parseJSON (JSON.String "PeerDiscovery") = pure PeerDiscovery
  parseJSON (JSON.String "TimeMapUpdate") = pure TimeMapUpdate
  parseJSON (JSON.String "ResourceUpdate") = pure ResourceUpdate
  parseJSON (JSON.String "ProgramSync") = pure ProgramSync
  parseJSON (JSON.String "ControlMessage") = pure ControlMessage
  parseJSON _ = fail "Expected String for MessageType"

-- MessageId instances
instance Binary MessageId where
  put (MessageId uuid) = Binary.put (UUID.toByteString uuid)
  get = do
    bs <- Binary.get
    case UUID.fromByteString bs of
      Just uuid -> pure (MessageId uuid)
      Nothing -> fail "Invalid UUID format"

instance ToJSON MessageId where
  toJSON (MessageId uuid) = JSON.String (UUID.toText uuid)

instance FromJSON MessageId where
  parseJSON (JSON.String txt) = case UUID.fromText txt of
    Just uuid -> pure (MessageId uuid)
    Nothing -> fail "Invalid UUID format for MessageId"
  parseJSON _ = fail "Expected String for MessageId"

-- UTCTime Binary instance
instance Binary UTCTime where
  put time = Binary.put (realToFrac (utcTimeToPOSIXSeconds time) :: Double)
  get = do
    seconds <- Binary.get :: Binary.Get Double
    return $ posixSecondsToUTCTime (realToFrac seconds)

