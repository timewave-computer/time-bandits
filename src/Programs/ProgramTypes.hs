{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Programs.ProgramTypes 
  ( -- * Common Types
    Program
  , ProgramId
  , MemorySlot(..)
  , ProgramType
  ) where

import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Core.Common (TimelineHash, EntityHash(..))
import Data.Serialize (Serialize)

-- Import Serialize instance for Text
import Types.EffectBase ()

-- | Phantom type for programs
data ProgramType

-- | Unique identifier for a Program
type ProgramId = EntityHash "Program"

data Program

-- | Memory slot in program state
data MemorySlot = MemorySlot
  { slotName :: Text
  , slotValue :: Text
  }
  deriving stock (Eq, Show, Generic)

-- Standalone deriving instances
deriving instance Serialize MemorySlot