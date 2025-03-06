{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Core.TimelineId
Description: Strongly typed identifiers for timelines

This module provides a strongly typed TimelineId newtype to prevent accidental
mixing of different identifier types. It includes functionality for creating,
validating, and working with timeline identifiers.
-}
module Core.TimelineId
  ( -- * Types
    TimelineId(..)
  , TimelineIdError(..)
  
  -- * Construction
  , fromText
  , fromByteString
  , fromHash
  
  -- * Conversion
  , timelineIdToText
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

-- | Unique identifier for timelines in the Time Bandits system
newtype TimelineId = TimelineId { unTimelineId :: ByteString }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Serialize)

-- | Allows using string literals for TimelineId
instance IsString TimelineId where
  fromString = TimelineId . TE.encodeUtf8 . T.pack

-- | Errors that can occur when working with timeline IDs
data TimelineIdError
  = InvalidTimelineIdFormat Text
  | EmptyTimelineId
  deriving (Show, Eq)

-- | Create a TimelineId from Text
fromText :: Text -> Either TimelineIdError TimelineId
fromText text
  | T.null text = Left EmptyTimelineId
  | otherwise = Right $ TimelineId $ TE.encodeUtf8 text

-- | Create a TimelineId from ByteString
fromByteString :: ByteString -> Either TimelineIdError TimelineId
fromByteString bs
  | BS.null bs = Left EmptyTimelineId
  | otherwise = Right $ TimelineId bs

-- | Create a TimelineId from a hash value
fromHash :: ByteString -> TimelineId
fromHash = TimelineId

-- | Convert a TimelineId to Text
timelineIdToText :: TimelineId -> Text
timelineIdToText = TE.decodeUtf8 . unTimelineId

-- | Convert a TimelineId to ByteString
toByteString :: TimelineId -> ByteString
toByteString = unTimelineId

-- | Convert a TimelineId to a hash representation (currently just the underlying ByteString)
toHash :: TimelineId -> ByteString
toHash = unTimelineId
