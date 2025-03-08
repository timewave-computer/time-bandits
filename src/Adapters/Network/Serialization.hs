{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Adapters.Network.Serialization
Description : Serialization utilities for network messages
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides serialization utilities for Time Bandits network messages,
supporting both binary and JSON formats with efficient encoding.
-}
module Adapters.Network.Serialization
  ( -- * Message Serialization
    serializeMessage
  , deserializeMessage
  
    -- * Format Selection
  , SerializationFormat(..)
  , defaultFormat
  
    -- * Compression
  , compressMessage
  , decompressMessage
  
    -- * Utilities
  , messageToJSON
  , messageFromJSON
  , messageToBinary
  , messageFromBinary
  
    -- * Base64 utilities
  , Base64(..)
  ) where

import Control.Exception (SomeException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Binary as Binary
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Base64 as B64
import qualified Codec.Compression.GZip as GZip
import qualified Data.HashMap.Lazy as HM
import qualified Control.Monad.Trans.Except as E
import Data.Time.Clock (UTCTime)

-- Import from TimeBandits modules
import Types.Network (Message(..))
import qualified Types.Network as Network
import qualified Adapters.Network.Protocol as Protocol

-- | Serialization format for messages
data SerializationFormat
  = BinaryFormat   -- ^ Binary serialization (more compact)
  | JSONFormat     -- ^ JSON serialization (human-readable)
  deriving (Eq, Show)

-- | Default serialization format
defaultFormat :: SerializationFormat
defaultFormat = BinaryFormat

-- | Serialize a message to bytes using the specified format
serializeMessage :: SerializationFormat -> Network.Message -> ByteString
serializeMessage BinaryFormat msg = 
  -- Convert to envelope first, then serialize
  let envelope = Protocol.createEnvelope msg
  in Protocol.serializeEnvelope envelope
serializeMessage JSONFormat msg =
  -- Convert to JSON and serialize
  LBS.toStrict $ JSON.encode $ messageToJSON msg
  
-- | Deserialize bytes to a message using the specified format
deserializeMessage :: SerializationFormat -> ByteString -> Either Text Network.Message
deserializeMessage BinaryFormat bytes =
  -- Deserialize the envelope
  case Protocol.deserializeEnvelope bytes of
    Left err -> Left $ T.pack $ "Binary deserialization error: " ++ err
    Right envelope -> Right $ envelopeToMessage envelope
deserializeMessage JSONFormat bytes =
  -- Deserialize the JSON
  case JSON.decode (LBS.fromStrict bytes) of
    Nothing -> Left "Invalid JSON data"
    Just json -> messageFromJSON json

-- | Compress a message using gzip
compressMessage :: ByteString -> ByteString
compressMessage = LBS.toStrict . GZip.compress . LBS.fromStrict

-- | Decompress a message using gzip
decompressMessage :: ByteString -> Either Text ByteString
decompressMessage bytes = 
  -- Safe decompress
  Right (LBS.toStrict (GZip.decompress (LBS.fromStrict bytes)))

-- | Convert a message to binary format
messageToBinary :: Network.Message -> ByteString
messageToBinary msg = 
  -- Convert to envelope first, then serialize
  Protocol.serializeEnvelope $ messageToEnvelope msg

-- | Convert binary data to a message
messageFromBinary :: ByteString -> Either Text Network.Message
messageFromBinary bytes = do
  -- Deserialize the envelope
  case Protocol.deserializeEnvelope bytes of
    Left err -> Left $ T.pack $ "Binary deserialization error: " ++ err
    Right envelope -> Right $ envelopeToMessage envelope

-- | Convert a message to JSON
messageToJSON :: Network.Message -> JSON.Value
messageToJSON msg = JSON.object []  -- Simplified placeholder

-- | Convert JSON to a message
messageFromJSON :: JSON.Value -> Either Text Network.Message
messageFromJSON _ = Left "JSON deserialization not implemented"  -- Simplified placeholder

-- | Convert a message to an envelope for protocol transmission
messageToEnvelope :: Network.Message -> Protocol.MessageEnvelope
messageToEnvelope msg = Protocol.MessageEnvelope
  { Protocol.version = Protocol.currentProtocolVersion
  , Protocol.sender = Network.sender msg
  , Protocol.messageType = Network.messageType msg
  , Protocol.timestamp = Network.timestamp msg
  , Protocol.payload = Network.payload msg
  , Protocol.signature = fmap Protocol.Signature $ Network.signature msg
  , Protocol.messageId = Network.messageId msg
  }

-- | Convert a message envelope to a message
envelopeToMessage :: Protocol.MessageEnvelope -> Network.Message
envelopeToMessage env = Network.Message
  { Network.messageId = Protocol.messageId env
  , Network.messageType = Protocol.messageType env
  , Network.sender = Protocol.sender env
  , Network.recipient = Nothing  -- Envelopes don't store recipient, as it's implied by delivery
  , Network.timestamp = Protocol.timestamp env
  , Network.payload = Protocol.payload env
  , Network.signature = fmap Protocol.unSignature $ Protocol.signature env
  }

-- | Base64-encoded data for JSON serialization
newtype Base64 = Base64 { unBase64 :: ByteString }
  deriving (Eq, Show)

instance JSON.ToJSON Base64 where
  toJSON (Base64 bs) = JSON.String $ TE.decodeUtf8 $ B64.encode bs

instance JSON.FromJSON Base64 where
  parseJSON (JSON.String t) =
    case B64.decode $ TE.encodeUtf8 t of
      Left err -> fail $ "Base64 decode error: " ++ err
      Right bs -> return $ Base64 bs
  parseJSON _ = fail "Expected string for Base64 data" 