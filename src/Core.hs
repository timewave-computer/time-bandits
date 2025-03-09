-- | Core module that re-exports all Core submodules in an organized way
module Core 
  ( -- * Fundamental Types from Core.Common
    EntityHash(..)
  , Hash(..)
  , ActorHash
  , ResourceHash
  , TimelineHash
  , LamportTime(..)
  , PubKey(..)
  , PrivKey(..)
  , Signature(..)
  , SimulationMode(..)
  , computeHash
  , generateEntityHash
  , computeNodeScore
  , computeMessageHash
    
    -- * Resource Management from Core.Resource and Core.ResourceId
  , Core.Resource.Resource(..)
  , Core.ResourceId.ResourceId
  , ResourceInfo(..)
  , ResourceCapability(..)
  , createResource
    
    -- * Effect System from Core.Effect
  , Core.Effect.Effect(..)
  , Core.Effect.EffectId
  , EffectStatus(..)
  , EffectResult(..)
  , EffectDAG(..)
  , EffectNode(..)
  , EffectMetadata(..)
  , Precondition(..)
  , PreconditionType(..)
  , Core.Effect.FactSnapshot(..)
  , FactSource(..)
  , ObservationMethod(..)
  , createEffect
  , Core.Effect.getEffectId
  , validateEffect
  , getEffectPreconditions
  
    -- * Timeline Management from Core.Timeline and Core.TimelineId
  , Core.Timeline.Timeline(..)
  , Core.TimelineId.TimelineId
  , BlockHeader(..)
  , createTimeline
  
  -- * Execution Log from Core.ExecutionLog
  , Core.ExecutionLog.LogEntry(..)
    
    -- * Actor and Program Identity
  , ActorId
  , ProgramId
  , AccountProgram(..)
  
  -- We're temporarily commenting out these exports since the modules don't exist
  -- -- * Events and Messages
  -- , Event(..)
  -- , Message(..)
  -- , Actor(..)
  ) where

-- Import all Core submodules
import Core.AccountProgram
import Core.ActorId
import Core.Common
import Core.Effect
import Core.Effects
import Core.Error
import Core.ExecutionLog
import Core.ProgramId
import Core.Resource
import Core.ResourceId
import Core.ResourceLedger
import Core.Serialize
import Core.TimeMap
import Core.Timeline
import Core.TimelineDescriptor
import Core.TimelineId
import Core.Types
import Core.Hashing (computeNodeScore)
import Core.Core (computeMessageHash)
-- Commenting out these imports as they don't exist in the codebase
-- import Core.Event (Event(..))
-- import Core.Message (Message(..))
-- import Core.Actor (Actor(..))

-- Additional imports for utility functions
import Data.Serialize (Serialize, encode)

-- | Generate a hash for an entity of a specific type
-- This is a utility function that wraps computeHash
generateEntityHash :: Serialize a => a -> EntityHash s
generateEntityHash a = EntityHash $ computeHash $ encode a
