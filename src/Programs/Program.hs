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
  ( -- * Core Types
    Program(..)
  , ProgramId
  , ProgramOwner
  
  -- * Re-exports from ProgramState
  , ProgramState(..)
  , ProgramMemory(..)
  , MemorySlot(..)
  
  -- * Re-exports from ProgramDefinition
  , ProgramDefinition(..)
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
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)

-- Import from Core modules
import Core.Types (AppError(..), ProgramErrorType(..))
import Core.Common (EntityHash(..), Hash(..))
import Core.Resource (Resource, Address, EscrowId)
import Core.Timeline (TimelineHash, BlockHeader)

-- Import from Programs modules
import Programs.ProgramState 
  ( ProgramState(..)
  , ProgramMemory(..)
  , MemorySlot(..)
  , TimeMap(..)
  , ResourceClaim(..)
  , createInitialState
  , advanceProgramCounter
  , updateTimeMap
  , lookupMemorySlot
  , updateMemorySlot
  , clearMemorySlot
  )

import Programs.ProgramDefinition
  ( ProgramDefinition(..)
  , MemoryContract(..)
  , SlotSpec(..)
  , ResourceRequirement(..)
  , createProgramDefinition
  , validateProgramDefinition
  , addProgramFunction
  , getProgramFunction
  , validateMemoryContract
  )

import Programs.ProgramEffect (GuardedEffect)

-- | Unique identifier for a Program
type ProgramId = EntityHash Program

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
  (Member (Error AppError) r) => 
  TimeMap ->
  ProgramDefinition ->
  ProgramOwner ->
  Sem r Program
createProgram timeMap definition programOwner = do
  -- Create initial program state
  initialState <- createInitialState timeMap
  
  -- Create a program ID
  let programId = EntityHash $ Hash "dummy-program-id"
  
  pure $ Program
    { programId = programId
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
  pure $ programId program

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
    throw $ ProgramError $ UnauthorizedCaller "Caller is not authorized"
  
  -- Look up the function in the program definition
  let functions = programFunctions $ programDefinition program
  case Map.lookup functionName functions of
    Nothing -> 
      throw $ ProgramError $ FunctionNotFound $ "Function not found: " <> functionName
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
    throw $ ProgramError $ UnauthorizedCaller "Only owner can register functions"
  
  -- Add the function to the program definition
  updatedDef <- addProgramFunction (programDefinition program) functionName effects
  
  -- Return updated program
  pure $ program { programDefinition = updatedDef }

-- | Verify that a program satisfies its memory contract
verifyMemoryContract ::
  (Member (Error AppError) r) =>
  Program ->
  Sem r Bool
verifyMemoryContract program = do
  -- For now, just validate the memory contract itself
  validateMemoryContract $ programMemoryContract $ programDefinition program

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
    throw $ ProgramError $ UnauthorizedCaller "Claimer is not authorized"
  
  -- Look up the resource in the memory slot
  resource <- lookupMemorySlot (programState program) slot
  
  -- Clear the memory slot
  newState <- clearMemorySlot (programState program) slot
  
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
    throw $ ProgramError $ UnauthorizedCaller "Only current owner can transfer ownership"
  
  -- Update the owner and add to authorized callers
  let updatedCallers = newOwner : filter (/= currentOwner) (authorizedCallers program)
  
  -- Return updated program
  pure $ program { owner = newOwner, authorizedCallers = updatedCallers }

-- | Record a resource claim in the program state
recordResourceClaim ::
  (Member (Error AppError) r) =>
  Program ->
  ResourceClaim ->
  Sem r Program
recordResourceClaim program claim = do
  -- Add the claim to the program state
  let currentState = programState program
      currentClaims = programResourceClaims currentState
      updatedState = currentState { programResourceClaims = claim : currentClaims }
  
  -- Return updated program
  pure $ program { programState = updatedState }

-- | Adapter function for backward compatibility with old interfaces
adaptInitializeProgram ::
  (Member (Error AppError) r) =>
  ProgramDefinition ->
  Address ->
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