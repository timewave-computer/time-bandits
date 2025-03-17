{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module: TimeBandits.Core.Common.Serialize
Description: Serialization utilities for the Time-Bandits system

This module provides serialization functions and utilities used throughout the 
Time-Bandits system. It centralizes serialization logic and provides consistent
interfaces for converting between different data formats.

@since 0.1.0
-}
module TimeBandits.Core.Common.Serialize
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

-- Import documentation of standard extensions
import TimeBandits.Core.Common.Extensions


-- External libraries
import Data.Serialize (Serialize, encode, decode, Get, Put, putByteString, getByteString)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Control.Exception (try, SomeException)
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as A
import Data.Time.Clock (UTCTime(..), diffUTCTime, addUTCTime)
import Data.Time.Calendar (fromGregorian)

-- Used in type signatures and orphan instances
import TimeBandits.Core.Common.Types (EntityHash(..), Hash(..))

-- | Epoch time reference (start of 1970)
epochUTCTime :: UTCTime
epochUTCTime = UTCTime (fromGregorian 1970 1 1) 0

-- | Standardized Serialize instance for UTCTime
-- Uses seconds since epoch for efficient binary representation
instance Serialize UTCTime where
  put time = do
    -- Serialize as seconds since epoch (rational number)
    S.put (realToFrac (diffUTCTime time epochUTCTime) :: Double)
  get = do
    -- Deserialize from seconds since epoch
    secs <- S.get :: S.Get Double
    return $ addUTCTime (realToFrac secs) epochUTCTime

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

-- | Serialize instance for Text
instance Serialize Text where
  put = serializeText
  get = deserializeText

-- | ToJSON instance for ByteString
-- Uses base64 encoding for JSON representation
instance ToJSON ByteString where
  toJSON = A.String . TE.decodeUtf8 . B64.encode

-- | FromJSON instance for ByteString
-- Decodes from base64 representation
instance FromJSON ByteString where
  parseJSON = A.withText "ByteString" $ \t ->
    case B64.decode (TE.encodeUtf8 t) of
      Left err -> fail $ "Invalid base64: " ++ err
      Right bs -> return bs 
