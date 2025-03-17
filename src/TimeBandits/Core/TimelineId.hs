{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: TimeBandits.Core.TimelineId
Description: Strongly typed identifiers for timelines

This module provides a strongly typed TimelineId newtype to prevent accidental
mixing of different identifier types. It includes functionality for creating,
validating, and working with timeline identifiers.

@since 0.1.0
-}
module TimeBandits.Core.TimelineId
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

-- Import documentation of standard extensions
import TimeBandits.Core.Common.Extensions


-- External libraries
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
  deriving (Eq, Ord, Generic)
  deriving anyclass (Serialize)
  deriving stock (Show)

-- | Helper function for text conversion
timelineIdToText :: TimelineId -> Text
timelineIdToText (TimelineId bs) = case TE.decodeUtf8' bs of
  Left _  -> T.pack $ "TimelineId:" ++ show (BS.take 8 bs) ++ "..."
  Right t -> t

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

-- | Convert a TimelineId to ByteString
toByteString :: TimelineId -> ByteString
toByteString = unTimelineId

-- | Convert a TimelineId to a hash representation (currently just the underlying ByteString)
toHash :: TimelineId -> ByteString
toHash = unTimelineId 
