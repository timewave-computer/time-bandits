-- | Core module that re-exports all Core submodules in an organized way
module Core 
  ( -- * Fundamental Types
    -- | Common primitive types used throughout the codebase
    module Core.Common
    
    -- * Resource Management
  , module Core.Resource
  , module Core.ResourceId
  , module Core.ResourceLedger
    
    -- * Effect System
    -- | The unified effect model for all operations
  , module Core.Effect
    
    -- * Polysemy Effects
    -- | Polysemy-based effect interfaces for composable operations
  , module Core.Effects
    
    -- * Timeline Management
  , module Core.Timeline
  , module Core.TimelineId
  , module Core.TimelineDescriptor
  , module Core.TimeMap
    
    -- * Actor and Program Identity
  , module Core.ActorId
  , module Core.ProgramId
  , module Core.AccountProgram
    
    -- * Core Types
    -- | Additional type definitions from Core.Types
    -- (primitives are from Core.Common)
  , module Core.Types
    
    -- * Utilities
  , module Core.Serialize
  , module Core.Error
  , module Core.ExecutionLog
  ) where

-- Re-export all Core submodules
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
