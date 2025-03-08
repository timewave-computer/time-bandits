{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Serialize
  ( -- * Re-exports
    Serialize(..)
  , encode
  , decode
  
  -- * Serialization functions
  , serializeText
  , deserializeText
  , serializeToByteString
  , deserializeFromByteString
  , serializeToBase64
  , deserializeFromBase64
  ) where

import Data.Serialize (Serialize, encode, decode, Get, Put, putByteString, getByteString)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Control.Exception (try, SomeException)

-- Re-export the Serialize Text instance from Core.SerializeInstances
import Core.SerializeInstances ()

-- | Serialize Text to ByteString
serializeText :: Text -> Put
serializeText text = do
  let bs = TE.encodeUtf8 text
  S.put (BS.length bs :: Int)
  putByteString bs

-- | Deserialize Text from ByteString
deserializeText :: Get Text
deserializeText = do
  len <- S.get :: Get Int
  bs <- getByteString len
  pure $ TE.decodeUtf8 bs

-- | Serialize a value to a ByteString
serializeToByteString :: Serialize a => a -> ByteString
serializeToByteString = encode

-- | Deserialize a value from a ByteString
deserializeFromByteString :: Serialize a => ByteString -> Either String a
deserializeFromByteString = decode

-- | Serialize a value to a Base64 string
serializeToBase64 :: Serialize a => a -> ByteString
serializeToBase64 = B64.encode . encode

-- | Deserialize a value from a Base64 string
deserializeFromBase64 :: Serialize a => ByteString -> Either String a
deserializeFromBase64 bs =
  case B64.decode bs of
    Left err -> Left err
    Right decoded -> decode decoded 