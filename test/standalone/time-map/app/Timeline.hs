{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Timeline 
  ( -- * Types
    Timeline(..)
  , TimelineClock(..)
  , Event(..)
  , BlockHeader(..)
  
  -- * Functions
  , createTimeline
  , timelineId
  , getTimelineName
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize (Serialize)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Types (TimelineId, LamportTime(..), TimelineHash, fromText, timelineHashFromBytes)

-- | A timeline in the Time-Bandits system
data Timeline = Timeline
  { _timelineId :: TimelineId        -- ^ Unique identifier
  , _timelineName :: Text            -- ^ Human-readable name
  , _timelineDescription :: Text     -- ^ Description
  , _timelineClock :: TimelineClock  -- ^ Timeline's clock
  , _timelineBlocks :: [BlockHeader] -- ^ Block headers
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | Clock for a timeline
data TimelineClock = TimelineClock
  { _clockHeight :: Int              -- ^ Current block height
  , _clockTimestamp :: LamportTime   -- ^ Current timestamp
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | A block header in a timeline
data BlockHeader = BlockHeader
  { _blockHash :: TimelineHash       -- ^ Block hash
  , _blockHeight :: Int              -- ^ Block height
  , _blockTimestamp :: LamportTime   -- ^ Block timestamp
  , _blockPrevHash :: TimelineHash   -- ^ Previous block hash
  , _blockRoot :: ByteString         -- ^ Merkle root of transactions
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | An event in a timeline
data Event = Event
  { _eventType :: Text               -- ^ Event type
  , _eventPayload :: ByteString      -- ^ Event data
  , _eventTimestamp :: LamportTime   -- ^ Event timestamp
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | Create a new timeline
createTimeline :: Text -> Text -> Timeline
createTimeline name description =
  let id = fromText name
      initialClock = TimelineClock 0 (LamportTime 0)
      genesisBlock = BlockHeader 
        { _blockHash = timelineHashFromBytes "genesis"
        , _blockHeight = 0
        , _blockTimestamp = LamportTime 0
        , _blockPrevHash = timelineHashFromBytes ""
        , _blockRoot = "genesis"
        }
  in Timeline
     { _timelineId = id
     , _timelineName = name
     , _timelineDescription = description
     , _timelineClock = initialClock
     , _timelineBlocks = [genesisBlock]
     }

-- | Get the timeline ID
timelineId :: Timeline -> TimelineId
timelineId = _timelineId

-- | Get the timeline name
getTimelineName :: Timeline -> Text
getTimelineName = _timelineName 