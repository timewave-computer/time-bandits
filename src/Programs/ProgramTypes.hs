module Programs.ProgramTypes 
  ( -- * Common Types
    ProgramId
  , MemorySlot(..)
  , ProgramState(..)
  , TimeMap(..)
  ) where

import Core (EntityHash(..))
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Core.Timeline (TimelineHash)

-- | Unique identifier for a Program
type ProgramId = EntityHash Program

-- | Placeholder for Program type to avoid circular dependency
data Program

-- | Memory slot in program state
data MemorySlot = MemorySlot
  { slotName :: Text
  , slotValue :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Program state
data ProgramState = ProgramState
  { programCounter :: Int
  , programMemory :: Map Text MemorySlot
  , timeMap :: TimeMap
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Time map tracking timeline states
data TimeMap = TimeMap
  { timelineStates :: Map TimelineHash Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize) 