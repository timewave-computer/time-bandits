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
Module: Programs.ProgramState
Description: Mutable state management for Time-Bandits programs.

This module provides types and functions for managing the mutable state of programs
during execution. It separates the dynamic, runtime state from the static program
definition.

ProgramState represents the current execution state of a program, including:
- Program counter tracking current execution position
- Current memory state with allocated resources
- Current time map view 
- Resource claims and ownership tracking

This separation of mutable state from static program definition enables:
1. Cleaner serialization and persistence of program state
2. Easier reasoning about program execution and effects
3. More explicit tracking of state changes during execution
4. Better support for program suspension and resumption
-}
module Programs.ProgramState 
  ( -- * Re-exports from Types
    ProgramMemory(..)
  , MemorySlot(..)
  , TimeMap(..)
  , ResourceClaim(..)
  
  -- * Core Types
  , ProgramState(..)
  
  -- * State Operations
  , createInitialState
  , advanceProgramCounter
  , updateTimeMap
  , addResourceClaim
  , removeResourceClaim
  
  -- * Memory Operations
  , lookupMemorySlot
  , updateMemorySlot
  , clearMemorySlot
  
  -- * Serialization
  , serializeProgramState
  , deserializeProgramState
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize, encode, decode)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)

-- Import from TimeBandits modules
import Core.Types
  ( AppError(..)
  , ProgramErrorType(..)
  )
import Core.Resource (Resource, EscrowId)

-- Import from Programs modules
import Programs.Types
  ( ProgramMemory(..)
  , MemorySlot(..)
  , TimeMap(..)
  , ResourceClaim(..)
  )

-- | Program state represents the current execution state of a program
data ProgramState = ProgramState
  { programCounter :: Int
  , programMemory :: ProgramMemory
  , programTimeMap :: TimeMap
  , programResourceClaims :: [ResourceClaim]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create an initial program state
createInitialState :: 
  (Member (Error AppError) r) => 
  TimeMap -> 
  Sem r ProgramState
createInitialState timeMap = do
  -- Initialize empty memory
  let emptyMemory = ProgramMemory Map.empty
  
  -- Initialize program state
  pure $ ProgramState
    { programCounter = 0
    , programMemory = emptyMemory
    , programTimeMap = timeMap
    , programResourceClaims = []
    }

-- | Advance the program counter to the next step
advanceProgramCounter :: 
  (Member (Error AppError) r) => 
  ProgramState -> 
  Sem r ProgramState
advanceProgramCounter state = do
  pure $ state { programCounter = programCounter state + 1 }

-- | Update the time map in program state
updateTimeMap :: 
  (Member (Error AppError) r) => 
  ProgramState -> 
  TimeMap -> 
  Sem r ProgramState
updateTimeMap state newTimeMap = do
  pure $ state { programTimeMap = newTimeMap }

-- | Add a resource claim to program state
addResourceClaim :: 
  (Member (Error AppError) r) => 
  ProgramState -> 
  ResourceClaim -> 
  Sem r ProgramState
addResourceClaim state claim = do
  pure $ state { programResourceClaims = claim : programResourceClaims state }

-- | Remove a resource claim from program state
removeResourceClaim :: 
  (Member (Error AppError) r) => 
  ProgramState -> 
  EscrowId -> 
  Sem r ProgramState
removeResourceClaim state escrowId = do
  let updatedClaims = filter (\c -> claimEscrowId c /= escrowId) (programResourceClaims state)
  pure $ state { programResourceClaims = updatedClaims }

-- | Look up a memory slot in program memory
lookupMemorySlot :: 
  (Member (Error AppError) r) => 
  ProgramState -> 
  MemorySlot -> 
  Sem r (Maybe Resource)
lookupMemorySlot state slot = do
  let memory = programMemory state
      slotMap = slots memory
  pure $ case Map.lookup slot slotMap of
    Just maybeResource -> maybeResource
    Nothing -> Nothing  -- Slot not found, return Nothing

-- | Update a memory slot with a new resource
updateMemorySlot :: 
  (Member (Error AppError) r) => 
  ProgramState -> 
  MemorySlot -> 
  Resource -> 
  Sem r ProgramState
updateMemorySlot state slot resource = do
  let memory = programMemory state
      slotMap = slots memory
      updatedSlots = Map.insert slot (Just resource) slotMap
      updatedMemory = memory { slots = updatedSlots }
  
  pure $ state { programMemory = updatedMemory }

-- | Clear a memory slot (set to Nothing)
clearMemorySlot :: 
  (Member (Error AppError) r) => 
  ProgramState -> 
  MemorySlot -> 
  Sem r ProgramState
clearMemorySlot state slot = do
  let memory = programMemory state
      slotMap = slots memory
      updatedSlots = Map.insert slot Nothing slotMap
      updatedMemory = memory { slots = updatedSlots }
  
  pure $ state { programMemory = updatedMemory }

-- | Serialize program state to a ByteString
serializeProgramState :: 
  (Member (Error AppError) r) => 
  ProgramState -> 
  Sem r ByteString
serializeProgramState state = do
  pure $ encode state

-- | Deserialize program state from a ByteString
deserializeProgramState :: 
  (Member (Error AppError) r) => 
  ByteString -> 
  Sem r ProgramState
deserializeProgramState bytes = do
  case decode bytes of
    Left err -> throw $ ProgramError $ DeserializationError $ T.pack err
    Right state -> pure state 