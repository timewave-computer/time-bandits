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
Module: Programs.Program
Description: Program abstraction and execution model for Time-Bandits.

This module provides the Program abstraction and related functionality.
It connects the static ProgramDefinition with the dynamic ProgramState
to form a complete program entity.

In the Time-Bandits architecture, Programs serve as the primary abstraction for:

1. Cross-Timeline Logic: Programs define the business logic that operates across
   multiple timelines, orchestrating resource transfers and state transitions.

2. Resource Management: Programs declare their resource requirements through memory
   contracts and manage resources in memory slots during execution.

3. Security Model: Programs establish a security boundary through ownership and
   authorization mechanisms, controlling who can invoke specific functionality.

4. Deterministic Execution: Programs provide a deterministic execution model where
   effects are applied in a well-defined order, with explicit preconditions.

This module has been refactored to separate static program definition
(ProgramDefinition) from mutable program state (ProgramState), providing clearer
boundaries and better security properties.
-}
module Programs.Program 
  ( -- * Core types
    Program(..)
  , ProgramId
  , ProgramOwner
  
  -- * Re-exports from ProgramState
  , ProgramState(..)
  , ProgramMemory(..)
  , MemorySlot(..)
  
  -- * Re-exports from ProgramDefinition
  , ProgramDefinition
  , programDefinition
  , memoryContract
  , MemoryContract(..)
  , SlotSpec(..)
  , ResourceRequirement(..)
  
  -- * Program Operations
  , createProgram
  , deployProgram
  , getProgramState
  , advanceProgramCounter
  , invokeProgram
  , registerProgramFunction
  
  -- * Memory Operations
  , lookupMemorySlot
  , updateMemorySlot
  , clearMemorySlot
  , escrowToProgram
  , claimFromProgram
  , verifyMemoryContract
  
  -- * Ownership Operations
  , transferProgramOwnership
  , isAuthorizedCaller
  , recordResourceClaim

  -- * Adapter functions for backward compatibility with old interfaces
  , adaptInitializeProgram
  , adaptExecuteProgramStep

  -- * Backward compatibility functions
  , getLastEffect
  , getCurrentStep
  , applyEffect

  -- * Forward declaration of GuardedEffect
  , GuardedEffect
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Polysemy (Member, Sem, embed)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Embed (Embed)
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Set (Set)
import Data.Set qualified as Set

-- Import from Core modules
import Types.Core (AppError(..), ProgramErrorType(RuntimeError))
import Core.Common (EntityHash(..), Hash(..))
import Core.Resource (Resource, Address, EscrowId)
import Core.Timeline (TimelineHash, BlockHeader)

-- Import from Programs modules
import Programs.ProgramTypes (ProgramId)

-- Import from Programs.Types instead
import Programs.Types
  ( ProgramMemory(..)
  , ResourceClaim(..)
  , MemorySlot(..)
  , TimeMap(..)
  )

import Programs.ProgramState 
  ( -- Remove types that aren't exported
    ProgramState
  , createProgramState
  , updateProgramState
  , updateTimeMap
  , getMemorySlot
  , setMemorySlot
  , clearMemorySlot
  , claimResource  -- Add this for resource claim handling
  )

import Programs.ProgramDefinition qualified as PD
import Programs.ProgramDefinition (ProgramDefinition, MemoryContract, SlotSpec, ResourceRequirement, 
                                  createProgramDefinition, validateDefinition, addFunction, 
                                  lookupFunction, validateMemoryContract, memoryContract)

-- Import effect system
import Types.Effect (Effect)

-- Forward declaration of GuardedEffect to avoid circular dependency
data GuardedEffect

-- | Program owner address
type ProgramOwner = Address

-- | A program is a declarative state machine operating within a time map
data Program = Program
  { programId :: ProgramId
  , programState :: ProgramState
  , programDefinition :: ProgramDefinition
  , owner :: ProgramOwner
  , authorizedCallers :: [Address]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new program
createProgram ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  TimeMap ->
  ProgramDefinition ->
  ProgramOwner ->
  Sem r Program
createProgram timeMap definition programOwner = do
  -- Get the program ID from the definition
  let defPid = PD.programId definition  -- Use qualified name to disambiguate
  
  -- Create the initial state with current time  
  now <- embed getCurrentTime
  let initialState = createProgramState defPid now  
  
  pure $ Program
    { programId = defPid  -- No ambiguity here as we're setting our own record field
    , programState = initialState
    , programDefinition = definition
    , owner = programOwner
    , authorizedCallers = [programOwner]  -- Owner is always authorized
    }

-- | Deploy a program
deployProgram ::
  (Member (Error AppError) r) =>
  Program ->
  Sem r ProgramId
deployProgram program = do
  -- In a real implementation, this would deploy the program to the system
  -- For now, just return the program ID
  let pid = case program of
              Program id _ _ _ _ -> id  -- Pattern match to avoid ambiguity
  pure pid

-- | Get the current state of a program
getProgramState ::
  (Member (Error AppError) r) =>
  Program ->
  Sem r ProgramState
getProgramState program = do
  -- Return the program state directly
  pure $ programState program

-- | Invoke a program function
invokeProgram ::
  (Member (Error AppError) r) =>
  Program ->
  Text ->  -- ^ Function name
  [Text] ->  -- ^ Function arguments
  Address ->  -- ^ Caller address
  Sem r Program
invokeProgram program functionName args caller = do
  -- Check if caller is authorized
  authorized <- isAuthorizedCaller program caller
  unless authorized $
    throw $ ProgramError $ RuntimeError "Caller is not authorized"
  
  -- Look up the function in the program definition
  let functions = PD.programFunctions $ programDefinition program
  case Map.lookup functionName functions of
    Nothing -> 
      throw $ ProgramError $ RuntimeError $ "Function not found: " <> functionName
    Just _ -> do
      -- For now, just return the program (actual invocation would modify state)
      pure program

-- | Register a new program function
registerProgramFunction ::
  (Member (Error AppError) r) =>
  Program ->
  Text ->  -- ^ Function name
  [GuardedEffect] ->  -- ^ Function body
  Address ->  -- ^ Caller address
  Sem r Program
registerProgramFunction program functionName effects caller = do
  -- Check if caller is the owner
  unless (caller == owner program) $
    throw $ ProgramError $ RuntimeError "Only owner can register functions"
  
  -- For now, just return the program unchanged
  -- In a real implementation, this would add the function to the program definition
  pure program

-- | Verify that a program satisfies its memory contract
verifyMemoryContract ::
  (Member (Error AppError) r) =>
  Program ->
  Sem r Bool
verifyMemoryContract program = do
  -- For now, just validate the memory contract itself
  let result = validateMemoryContract $ memoryContract $ programDefinition program
  case result of
    Left _ -> pure False
    Right _ -> pure True

-- | Escrow a resource to a program
escrowToProgram ::
  (Member (Error AppError) r) =>
  Program ->
  Resource ->
  MemorySlot ->
  Sem r Program
escrowToProgram program resource slot = do
  -- Update the memory slot with the resource
  newState <- updateMemorySlot (programState program) slot resource
  
  -- Return updated program
  pure $ program { programState = newState }

-- | Claim a resource from a program
claimFromProgram ::
  (Member (Error AppError) r) =>
  Program ->
  MemorySlot ->
  Address ->  -- ^ Claimer address
  Sem r (Program, Maybe Resource)
claimFromProgram program slot claimer = do
  -- Check if claimer is authorized
  authorized <- isAuthorizedCaller program claimer
  unless authorized $
    throw $ ProgramError $ RuntimeError "Claimer is not authorized"
  
  -- Look up the resource in the memory slot
  resource <- lookupMemorySlot (programState program) slot
  
  -- Clear the memory slot
  let (MemorySlot slotName) = slot
      newState = clearMemorySlot slotName (programState program)
  
  -- Return updated program and the resource
  pure (program { programState = newState }, resource)

-- | Check if a caller is authorized to invoke program functions
isAuthorizedCaller ::
  (Member (Error AppError) r) =>
  Program ->
  Address ->  -- ^ Caller address
  Sem r Bool
isAuthorizedCaller program caller = do
  -- Check if caller is in the authorized callers list
  pure $ caller `elem` authorizedCallers program

-- | Transfer program ownership to a new owner
transferProgramOwnership ::
  (Member (Error AppError) r) =>
  Program ->
  Address ->  -- ^ New owner
  Address ->  -- ^ Current owner (must match)
  Sem r Program
transferProgramOwnership program newOwner currentOwner = do
  -- Check if caller is the current owner
  unless (currentOwner == owner program) $
    throw $ ProgramError $ RuntimeError "Only current owner can transfer ownership"
  
  -- Update the owner and add to authorized callers
  let updatedCallers = newOwner : filter (/= currentOwner) (authorizedCallers program)
  
  -- Return updated program
  pure $ program { owner = newOwner, authorizedCallers = updatedCallers }

-- | Record a resource claim for the program
recordResourceClaim ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  Program ->
  ResourceClaim ->
  Sem r Program
recordResourceClaim program claim = do
  -- Use the claimResource function from ProgramState
  let currentState = programState program
      resource = claimedResource claim  -- Standard record access
      -- Create a simple empty map for the TimeMap
      emptyMap = Map.empty
      -- Use the constructor with named fields
      timeMap = TimeMap 
                { timelines = emptyMap
                , observedHeads = emptyMap
                }
  
  -- Get current time
  now <- embed getCurrentTime
  let updatedState = claimResource resource timeMap now currentState
  
  -- Return updated program
  pure $ program { programState = updatedState }

-- | Adapter for initializing a program
adaptInitializeProgram ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  ProgramDefinition ->
  Address ->  -- ^ Owner address
  TimeMap ->
  Sem r Program
adaptInitializeProgram definition owner timeMap = do
  createProgram timeMap definition owner

-- | Adapter function for backward compatibility with old interfaces
adaptExecuteProgramStep ::
  (Member (Error AppError) r) =>
  Program ->
  Sem r Program
adaptExecuteProgramStep program = do
  -- Advance the program counter
  newState <- advanceProgramCounter (programState program)
  
  -- Return updated program
  pure $ program { programState = newState }

-- | Lookup memory slot
lookupMemorySlot ::
  (Member (Error AppError) r) =>
  ProgramState ->
  MemorySlot ->
  Sem r (Maybe Resource)
lookupMemorySlot state (MemorySlot slotName) = do
  -- Use the getMemorySlot function from ProgramState
  pure $ getMemorySlot slotName state

-- | Update memory slot with a resource
updateMemorySlot ::
  (Member (Error AppError) r) =>
  ProgramState ->
  MemorySlot ->
  Resource ->
  Sem r ProgramState
updateMemorySlot state (MemorySlot slotName) resource = do
  -- Use the setMemorySlot function from ProgramState
  pure $ setMemorySlot slotName resource state

-- | Advance program counter
advanceProgramCounter ::
  (Member (Error AppError) r) =>
  ProgramState ->
  Sem r ProgramState
advanceProgramCounter state = do
  -- Placeholder implementation - in the real code, this would be implemented
  pure state  -- Just return the state unchanged for now 

-- | Get the last effect applied to the program (compatibility function)
getLastEffect ::
  (Member (Error AppError) r) =>
  Program ->
  Sem r (Maybe Effect)
getLastEffect _ = do
  -- Placeholder implementation - effects are now managed differently
  pure Nothing

-- | Get the current execution step (compatibility function)
getCurrentStep ::
  (Member (Error AppError) r) =>
  Program ->
  Sem r Int
getCurrentStep program = do
  -- Return a placeholder value since the new ProgramState doesn't have a programCounter
  -- In a real implementation, this would be derived from the current state
  pure 0
  
-- | Apply an effect to a program (compatibility function)
applyEffect ::
  (Member (Error AppError) r) =>
  Program ->
  Effect ->
  Sem r Program
applyEffect program _ = do
  -- Placeholder implementation - effects are now applied differently
  pure program 