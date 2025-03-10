{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize (Serialize(..), Put, Get)
import qualified Data.Serialize as S
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

-- | Serialize instance for Text
instance Serialize Text where
  put = S.put . TE.encodeUtf8
  get = TE.decodeUtf8 <$> S.get

-- | Newtype for timeline IDs
newtype TimelineId = TimelineId { unTimelineId :: ByteString }
  deriving (Eq, Ord, Generic)
  deriving anyclass (Serialize)

instance Show TimelineId where
  show (TimelineId bs) = "TimelineId:" ++ show (BS.take 8 bs) ++ "..."

-- | Conversion to Text
timelineIdToText :: TimelineId -> Text
timelineIdToText (TimelineId bs) = case TE.decodeUtf8' bs of
  Left _  -> T.pack $ "TimelineId:" ++ show (BS.take 8 bs) ++ "..."
  Right t -> t

-- | Allows using string literals for TimelineId
instance IsString TimelineId where
  fromString = TimelineId . TE.encodeUtf8 . T.pack

-- | Create a TimelineId from Text
fromText :: Text -> TimelineId
fromText = TimelineId . TE.encodeUtf8

-- | Create a TimelineId from ByteString
fromByteString :: ByteString -> TimelineId
fromByteString = TimelineId

-- | Hash type for cryptographic hashes
newtype Hash = Hash { unHash :: ByteString }
  deriving (Eq, Ord, Generic)
  deriving anyclass (Serialize)

instance Show Hash where
  show (Hash bs) = "Hash:" ++ show (BS.take 8 bs) ++ "..."

-- | TimelineHash is a simplified version for our standalone implementation
newtype TimelineHash = TimelineHash { unTimelineHash :: ByteString }
  deriving (Eq, Ord, Generic)
  deriving anyclass (Serialize)

instance Show TimelineHash where
  show (TimelineHash bs) = "TimelineHash:" ++ show (BS.take 8 bs) ++ "..."

-- | Create a TimelineHash from a ByteString
timelineHashFromBytes :: ByteString -> TimelineHash
timelineHashFromBytes = TimelineHash

-- | Simple logical time implementation
newtype LamportTime = LamportTime { unLamportTime :: Int }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Serialize) 