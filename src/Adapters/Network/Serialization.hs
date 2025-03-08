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
  ) where

import Control.Exception (try, SomeException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Binary as Binary
import qualified Data.Aeson as JSON
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib

-- Import from TimeBandits modules
import Types.Network
import Adapters.Network.Protocol

-- | Serialization format for messages
data SerializationFormat
  = BinaryFormat     -- ^ Binary format (more efficient)
  | JSONFormat       -- ^ JSON format (more readable/debuggable)
  | CompressedBinary -- ^ Compressed binary format (smallest size)
  deriving (Eq, Show)

-- | Default serialization format
defaultFormat :: SerializationFormat
defaultFormat = CompressedBinary

-- | Serialize a message to bytes using the specified format
serializeMessage :: SerializationFormat -> Message -> ByteString
serializeMessage BinaryFormat msg =
  messageToBinary msg
serializeMessage JSONFormat msg =
  LBS.toStrict $ JSON.encode $ messageToJSON msg
serializeMessage CompressedBinary msg =
  compressMessage $ messageToBinary msg

-- | Deserialize a message from bytes using the specified format
deserializeMessage :: SerializationFormat -> ByteString -> Either Text Message
deserializeMessage BinaryFormat bytes =
  messageFromBinary bytes
deserializeMessage JSONFormat bytes =
  case JSON.eitherDecode (LBS.fromStrict bytes) of
    Left err -> Left $ T.pack $ "JSON deserialization error: " ++ err
    Right jsonVal -> messageFromJSON jsonVal
deserializeMessage CompressedBinary bytes =
  case decompressMessage bytes of
    Left err -> Left err
    Right decompressed -> messageFromBinary decompressed

-- | Convert a message to a binary representation
messageToBinary :: Message -> ByteString
messageToBinary msg = do
  -- Create an envelope and serialize it
  let envelope = createEnvelope msg
  serializeEnvelope envelope

-- | Convert a binary representation to a message
messageFromBinary :: ByteString -> Either Text Message
messageFromBinary bytes = do
  -- Deserialize the envelope
  case deserializeEnvelope bytes of
    Left err -> Left $ T.pack $ "Binary deserialization error: " ++ err
    Right envelope -> Right $ envelopeToMessage envelope

-- | Convert a message to a JSON value
messageToJSON :: Message -> JSON.Value
messageToJSON msg = JSON.object
  [ "id" . JSON.toJSON $ messageId msg
  , "type" . JSON.toJSON $ messageType msg
  , "sender" . JSON.toJSON $ sender msg
  , "recipient" . JSON.toJSON $ recipient msg
  , "timestamp" . JSON.toJSON $ timestamp msg
  , "payload" . JSON.toJSON $ Base64 $ payload msg
  , "signature" . JSON.toJSON $ fmap Base64 $ signature msg
  ]
  where
    (.) = (,)

-- | Convert a JSON value to a message
messageFromJSON :: JSON.Value -> Either Text Message
messageFromJSON json = do
  case JSON.fromJSON json of
    JSON.Error err -> Left $ T.pack $ "JSON parsing error: " ++ err
    JSON.Success obj -> do
      msgId <- getField obj "id"
      msgType <- getField obj "type"
      msgSender <- getField obj "sender"
      msgRecipient <- getField obj "recipient"
      msgTimestamp <- getField obj "timestamp"
      msgPayload <- getBase64Field obj "payload"
      msgSignature <- getOptionalBase64Field obj "signature"
      
      return Message
        { messageId = msgId
        , messageType = msgType
        , sender = msgSender
        , recipient = msgRecipient
        , timestamp = msgTimestamp
        , payload = msgPayload
        , signature = msgSignature
        }
  where
    getField :: JSON.FromJSON a => JSON.Object -> Text -> Either Text a
    getField obj key =
      case JSON.lookup key obj of
        Nothing -> Left $ "Missing field: " <> key
        Just v -> case JSON.fromJSON v of
          JSON.Error err -> Left $ "Error parsing field " <> key <> ": " <> T.pack err
          JSON.Success val -> Right val
    
    getBase64Field :: JSON.Object -> Text -> Either Text ByteString
    getBase64Field obj key = do
      base64 <- getField obj key :: Either Text Base64
      return $ unBase64 base64
    
    getOptionalBase64Field :: JSON.Object -> Text -> Either Text (Maybe ByteString)
    getOptionalBase64Field obj key =
      case JSON.lookup key obj of
        Nothing -> Right Nothing
        Just JSON.Null -> Right Nothing
        Just v -> do
          base64 <- JSON.fromJSON v :: Either Text (Maybe Base64)
          return $ fmap unBase64 base64

-- | Compress a message using zlib
compressMessage :: ByteString -> ByteString
compressMessage = LBS.toStrict . GZip.compress . LBS.fromStrict

-- | Decompress a message using zlib
decompressMessage :: ByteString -> Either Text ByteString
decompressMessage bytes = do
  result <- try $ LBS.toStrict . GZip.decompress . LBS.fromStrict $ bytes
  case result of
    Left (e :: SomeException) -> Left $ T.pack $ "Decompression error: " ++ show e
    Right decompressed -> Right decompressed

-- | Convert a message envelope to a message
envelopeToMessage :: MessageEnvelope -> Message
envelopeToMessage env = Message
  { messageId = messageId env
  , messageType = messageType env
  , sender = sender env
  , recipient = Nothing  -- Envelopes don't store recipient, as it's implied by delivery
  , timestamp = timestamp env
  , payload = payload env
  , signature = fmap unSignature $ signature env
  }

-- | Base64-encoded data for JSON serialization
newtype Base64 = Base64 { unBase64 :: ByteString }

instance JSON.ToJSON Base64 where
  toJSON (Base64 bs) = JSON.String $ decodeUtf8 $ BS.encode bs

instance JSON.FromJSON Base64 where
  parseJSON (JSON.String t) =
    case BS.decode $ encodeUtf8 t of
      Left err -> fail $ "Base64 decode error: " ++ err
      Right bs -> return $ Base64 bs
  parseJSON _ = fail "Expected string for Base64 data" 