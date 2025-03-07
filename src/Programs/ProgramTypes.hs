{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Programs.ProgramTypes 
  ( -- * Common Types
    Program
  , MemorySlot(..)
  , ProgramState(..)
  , TimeMap(..)
  ) where

import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Core.Common (TimelineHash, EntityHash(..))
import Core.Serialize (Serialize)

-- | Unique identifier for a Program
type ProgramId = EntityHash Program

data Program

-- | Memory slot in program state
data MemorySlot = MemorySlot
  { slotName :: Text
  , slotValue :: Text
  }
  deriving stock (Eq, Show, Generic)

-- | Program state
data ProgramState = ProgramState
  { programCounter :: Int
  , programMemory :: Map Text MemorySlot
  , timeMap :: TimeMap
  }
  deriving stock (Eq, Show, Generic)

-- | Time map tracking timeline states
data TimeMap = TimeMap
  { timelineStates :: Map TimelineHash Int
  }
  deriving stock (Eq, Show, Generic)

-- Standalone deriving instances
deriving instance Serialize MemorySlot
deriving instance Serialize ProgramState
deriving instance Serialize TimeMap