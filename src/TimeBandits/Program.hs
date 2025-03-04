{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
This module provides the Program abstraction and related functionality.
It encapsulates program state and memory.

Programs are declarative state machines that operates within a time map, consisting of:
- Program Definition: Immutable sequence of GuardedEffects
- Memory Contract: Immutable declaration of per-step resource expectations
- Program State: Current execution counter and memory
-}
module TimeBandits.Program 
  ( -- * Core Types
    Program(..)
  , ProgramId
  , ProgramState(..)
  , ProgramMemory(..)
  , MemorySlot(..)
  , TimeMap(..)
  
  -- * Program Operations
  , createProgram
  , deployProgram
  , getProgramState
  , advanceProgramCounter
  
  -- * Memory Operations
  , lookupMemorySlot
  , updateMemorySlot
  , clearMemorySlot

  -- * Adapter functions for backward compatibility with old interfaces
  , adaptInitializeProgram
  , adaptExecuteProgramStep
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import TimeBandits.Types
  ( AppError(..)
  , LamportTime(..)
  )
import TimeBandits.Resource (Resource)
import TimeBandits.Timeline (TimelineHash, BlockHeader)

-- | Unique identifier for a Program
type ProgramId = EntityHash Program

-- | A memory slot is a named container for resources within program memory
newtype MemorySlot = MemorySlot Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialize)

-- | Program memory tracks the runtime state of program-owned resources
newtype ProgramMemory = ProgramMemory
  { slots :: Map.Map MemorySlot (Maybe Resource) }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Time map is a composed view of multiple timelines
data TimeMap = TimeMap
  { timelines :: Map.Map TimelineHash LamportTime
  , observedHeads :: Map.Map TimelineHash BlockHeader
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Program state represents the current execution state of a program
data ProgramState = ProgramState
  { programCounter :: Int
  , programMemory :: ProgramMemory
  , programTimeMap :: TimeMap
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A program is a declarative state machine operating within a time map
data Program = Program
  { programId :: ProgramId
  , executionState :: ProgramState
  , memory :: ProgramMemory
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new program
createProgram :: 
  (Member (Error AppError) r) => 
  TimeMap ->
  Sem r Program
createProgram timeMap = do
  -- Create a program ID
  let programId = EntityHash $ Hash "dummy-program-id"
      -- Initialize empty memory
      emptyMemory = ProgramMemory Map.empty
      -- Initialize program state
      initialState = ProgramState
        { programCounter = 0
        , programMemory = emptyMemory
        , programTimeMap = timeMap
        }
  
  pure $ Program
    { programId = programId
    , executionState = initialState
    , memory = emptyMemory
    }

-- | Deploy a program
deployProgram ::
  (Member (Error AppError) r) =>
  Program ->
  Sem r ProgramId
deployProgram program = do
  -- In a real implementation, this would deploy the program to the system
  -- For now, just return the program ID
  pure $ programId program

-- | Get the current state of a program
getProgramState ::
  (Member (Error AppError) r) =>
  ProgramId ->
  Sem r ProgramState
getProgramState programId = do
  -- In a real implementation, this would look up the program state
  -- For now, just return a dummy state
  let emptyMemory = ProgramMemory Map.empty
      emptyTimeMap = TimeMap Map.empty Map.empty
  pure $ ProgramState
    { programCounter = 0
    , programMemory = emptyMemory
    , programTimeMap = emptyTimeMap
    }

-- | Advance the program counter
advanceProgramCounter ::
  (Member (Error AppError) r) =>
  ProgramState ->
  Sem r ProgramState
advanceProgramCounter state = do
  -- Increment the program counter
  pure $ state { programCounter = programCounter state + 1 }

-- | Look up a resource in a memory slot
lookupMemorySlot ::
  (Member (Error AppError) r) =>
  ProgramMemory ->
  MemorySlot ->
  Sem r (Maybe Resource)
lookupMemorySlot memory slot = do
  -- Look up the slot in memory
  pure $ case Map.lookup slot (slots memory) of
    Just (Just resource) -> Just resource
    _ -> Nothing

-- | Update a memory slot with a resource
updateMemorySlot ::
  (Member (Error AppError) r) =>
  ProgramMemory ->
  MemorySlot ->
  Resource ->
  Sem r ProgramMemory
updateMemorySlot memory slot resource = do
  -- Update the slot in memory
  pure $ memory { slots = Map.insert slot (Just resource) (slots memory) }

-- | Clear a memory slot
clearMemorySlot ::
  (Member (Error AppError) r) =>
  ProgramMemory ->
  MemorySlot ->
  Sem r ProgramMemory
clearMemorySlot memory slot = do
  -- Clear the slot in memory
  pure $ memory { slots = Map.insert slot Nothing (slots memory) }

-- | Adapter functions for backward compatibility with old interfaces

-- | Adapter for initializing a program with default settings
adaptInitializeProgram :: 
  (Member (Error AppError) r) => 
  ByteString -> 
  Sem r Program
adaptInitializeProgram programName = do
  -- Create an empty time map
  let emptyTimeMap = TimeMap Map.empty Map.empty
  -- Create a program with the empty time map
  createProgram emptyTimeMap

-- | Adapter for executing a program step
adaptExecuteProgramStep :: 
  (Member (Error AppError) r) => 
  Program -> 
  Sem r Program
adaptExecuteProgramStep program = do
  -- Get the current state
  let state = executionState program
  -- Advance the program counter
  advancedState <- advanceProgramCounter state
  -- Return the updated program
  pure $ program { executionState = advancedState } 