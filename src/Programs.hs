-- | Programs module that re-exports all Programs submodules
module Programs 
  ( -- * Program Types and Operations from Programs.Program
    module Programs.Program

    -- * Program State Management from Programs.ProgramState
    -- We re-export everything except programId to avoid conflicts
  , ProgramState(..)
  , ExecutionLog(..)
  , LogEntry(..)
  , createProgramState
  , updateProgramState
  , getMemorySlot
  , setMemorySlot
  , clearMemorySlot
  , claimResource
  , getExecutionLog
  , logExecution
  
    -- * Program Effects from Programs.ProgramEffect
  , module Programs.ProgramEffect
  
    -- * Preconditions from Programs.Preconditions
  , module Programs.Preconditions
  ) where

-- Import all Programs submodules
import Programs.Program
import Programs.ProgramState hiding (programId)
import Programs.ProgramEffect hiding (GuardedEffect)
import Programs.Preconditions
