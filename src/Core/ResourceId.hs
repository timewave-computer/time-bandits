{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Core.ResourceId
Description: Strongly typed identifiers for resources

This module provides a strongly typed ResourceId newtype to prevent accidental
mixing of different identifier types. It includes functionality for creating,
validating, and working with resource identifiers.
-}
module Core.ResourceId
  ( -- * Types
    ResourceId(..)
  , ResourceIdError(..)
  
  -- * Construction
  , fromText
  , fromByteString
  , fromHash
  
  -- * Conversion
  , toText
  , toByteString
  , toHash
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize (Serialize)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

-- | Unique identifier for resources in the Time Bandits system
newtype ResourceId = ResourceId { unResourceId :: ByteString }
  deriving (Eq, Ord, Generic, Serialize)

instance Show ResourceId where
  show = T.unpack . toText

-- | Allows using string literals for ResourceId
instance IsString ResourceId where
  fromString = ResourceId . TE.encodeUtf8 . T.pack

-- | Errors that can occur when working with resource IDs
data ResourceIdError
  = InvalidResourceIdFormat Text
  | EmptyResourceId
  deriving (Show, Eq)

-- | Create a ResourceId from Text
fromText :: Text -> Either ResourceIdError ResourceId
fromText text
  | T.null text = Left EmptyResourceId
  | otherwise = Right $ ResourceId $ TE.encodeUtf8 text

-- | Create a ResourceId from ByteString
fromByteString :: ByteString -> Either ResourceIdError ResourceId
fromByteString bs
  | BS.null bs = Left EmptyResourceId
  | otherwise = Right $ ResourceId bs

-- | Create a ResourceId from a hash value
fromHash :: ByteString -> ResourceId
fromHash = ResourceId

-- | Convert a ResourceId to Text
toText :: ResourceId -> Text
toText = TE.decodeUtf8 . unResourceId

-- | Convert a ResourceId to ByteString
toByteString :: ResourceId -> ByteString
toByteString = unResourceId

-- | Convert a ResourceId to a hash representation (currently just the underlying ByteString)
toHash :: ResourceId -> ByteString
toHash = unResourceId
