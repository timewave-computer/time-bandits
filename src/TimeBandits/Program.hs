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
  , ProgramDefinition(..)
  , MemoryContract(..)
  , SlotSpec(..)
  , ResourceClaim(..)
  , ResourceRequirement(..)
  , ProgramOwner
  
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

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import TimeBandits.Types
  ( AppError(..)
  , LamportTime(..)
  )
import TimeBandits.Resource 
  ( Resource
  , Address
  , EscrowId
  )
import TimeBandits.Timeline (TimelineHash, BlockHeader)

-- Forward declaration for ProgramEffect
data GuardedEffect

-- | Unique identifier for a Program
type ProgramId = EntityHash Program

-- | Program owner address
type ProgramOwner = Address

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
  , programResourceClaims :: [ResourceClaim]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Specification for a memory slot in a contract
data SlotSpec = SlotSpec
  { slotName :: MemorySlot
  , slotRequired :: Bool
  , slotDescription :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Resource requirement for a program step
data ResourceRequirement = ResourceRequirement
  { requiredSlot :: MemorySlot
  , requiredResourceType :: Text
  , isOptional :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Memory contract defines what resources a program expects
data MemoryContract = MemoryContract
  { slotSpecs :: [SlotSpec]
  , stepRequirements :: Map.Map Int [ResourceRequirement]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Program definition contains the immutable sequence of guarded effects
data ProgramDefinition = ProgramDefinition
  { programName :: Text
  , programDescription :: Text
  , programEffects :: [GuardedEffect]
  , programFunctions :: Map.Map Text [GuardedEffect]
  , programMemoryContract :: MemoryContract
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Resource claim tracks ownership of resources by programs
data ResourceClaim = ResourceClaim
  { claimedResource :: Resource
  , claimEscrowId :: EscrowId
  , claimTimestamp :: LamportTime
  , claimExpiryTime :: Maybe LamportTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A program is a declarative state machine operating within a time map
data Program = Program
  { programId :: ProgramId
  , executionState :: ProgramState
  , programDefinition :: ProgramDefinition
  , memory :: ProgramMemory
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
  -- Create a program ID
  let programId = EntityHash $ Hash "dummy-program-id"
      -- Initialize empty memory
      emptyMemory = ProgramMemory Map.empty
      -- Initialize program state
      initialState = ProgramState
        { programCounter = 0
        , programMemory = emptyMemory
        , programTimeMap = timeMap
        , programResourceClaims = []
        }
  
  pure $ Program
    { programId = programId
    , executionState = initialState
    , programDefinition = definition
    , memory = emptyMemory
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
    , programResourceClaims = []
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

-- | Invoke a program function with arguments
invokeProgram ::
  (Member (Error AppError) r) =>
  Program ->
  Text ->      -- Function name
  [Resource] -> -- Arguments
  Address ->   -- Caller address
  Sem r ProgramState
invokeProgram program functionName args callerAddr = do
  -- Check if caller is authorized
  isAuthorized <- isAuthorizedCaller program callerAddr
  unless isAuthorized $
    throw $ ProgramError $ "Caller " <> show callerAddr <> " not authorized to invoke program"
  
  -- Look up the function in the program definition
  let functions = programFunctions $ programDefinition program
  case Map.lookup functionName functions of
    Nothing -> throw $ ProgramError $ "Function " <> show functionName <> " not found"
    Just effects -> do
      -- In a real implementation, this would execute the effects
      -- For now, just return the current state
      pure $ executionState program

-- | Register a new function in a program
registerProgramFunction ::
  (Member (Error AppError) r) =>
  Program ->
  Text ->           -- Function name
  [GuardedEffect] -> -- Function effects
  Address ->        -- Caller address (must be owner)
  Sem r Program
registerProgramFunction program functionName effects callerAddr = do
  -- Check if caller is the owner
  unless (owner program == callerAddr) $
    throw $ ProgramError "Only the owner can register functions"
  
  -- Add the function to the program definition
  let definition = programDefinition program
      updatedFunctions = Map.insert functionName effects (programFunctions definition)
      updatedDefinition = definition { programFunctions = updatedFunctions }
  
  -- Return the updated program
  pure $ program { programDefinition = updatedDefinition }

-- | Escrow a resource to a program's memory slot
escrowToProgram ::
  (Member (Error AppError) r) =>
  Program ->
  Resource ->      -- Resource to escrow
  MemorySlot ->    -- Target memory slot
  Address ->       -- Address escrowing the resource
  Sem r Program
escrowToProgram program resource slot escrowingAddr = do
  -- In a real implementation, this would create an escrow and update program memory
  -- For now, just update the memory slot
  let currentState = executionState program
      currentMemory = programMemory currentState
  
  updatedMemory <- updateMemorySlot currentMemory slot resource
  let updatedState = currentState { programMemory = updatedMemory }
  
  -- Return the updated program
  pure $ program { executionState = updatedState }

-- | Claim a resource from a program's memory slot
claimFromProgram ::
  (Member (Error AppError) r) =>
  Program ->
  MemorySlot ->    -- Source memory slot
  Address ->       -- Address claiming the resource
  Sem r (Program, Resource)
claimFromProgram program slot claimingAddr = do
  -- Check if there is a resource in the slot
  let currentState = executionState program
      currentMemory = programMemory currentState
  
  maybeResource <- lookupMemorySlot currentMemory slot
  case maybeResource of
    Nothing -> throw $ ProgramError $ "No resource in slot " <> show slot
    Just resource -> do
      -- Clear the slot
      updatedMemory <- clearMemorySlot currentMemory slot
      let updatedState = currentState { programMemory = updatedMemory }
      
      -- Return the updated program and the claimed resource
      pure (program { executionState = updatedState }, resource)

-- | Transfer program ownership to a new address
transferProgramOwnership ::
  (Member (Error AppError) r) =>
  Program ->
  Address ->       -- New owner address
  Address ->       -- Current owner address (must match)
  Sem r Program
transferProgramOwnership program newOwner currentOwnerAddr = do
  -- Check if caller is the current owner
  unless (owner program == currentOwnerAddr) $
    throw $ ProgramError "Only the current owner can transfer ownership"
  
  -- Update the owner and authorized callers
  let updatedAuthorizedCallers = newOwner : filter (/= currentOwnerAddr) (authorizedCallers program)
  
  -- Return the updated program
  pure $ program { owner = newOwner, authorizedCallers = updatedAuthorizedCallers }

-- | Check if an address is authorized to call the program
isAuthorizedCaller ::
  (Member (Error AppError) r) =>
  Program ->
  Address ->       -- Caller to check
  Sem r Bool
isAuthorizedCaller program callerAddr = do
  -- Check if the address is in the authorized callers list
  pure $ callerAddr `elem` authorizedCallers program

-- | Record a resource claim by the program
recordResourceClaim ::
  (Member (Error AppError) r) =>
  ProgramState ->
  ResourceClaim ->
  Sem r ProgramState
recordResourceClaim state claim = do
  -- Add the claim to the program's resource claims
  let updatedClaims = claim : programResourceClaims state
  
  -- Return the updated state
  pure $ state { programResourceClaims = updatedClaims }

-- | Verify that a program state satisfies its memory contract
verifyMemoryContract ::
  (Member (Error AppError) r) =>
  ProgramState ->
  MemoryContract ->
  Sem r Bool
verifyMemoryContract state contract = do
  -- Get the current step requirements
  let stepReqs = case Map.lookup (programCounter state) (stepRequirements contract) of
        Just reqs -> reqs
        Nothing -> []  -- No specific requirements for this step
  
  -- Check each requirement
  let memory = programMemory state
  forM stepReqs $ \req -> do
    let slot = requiredSlot req
    maybeResource <- lookupMemorySlot memory slot
    
    -- If the resource is required but not present, fail
    if isOptional req || isJust maybeResource
      then pure True
      else pure False
  
  -- All requirements must be satisfied
  pure True

-- Utility functions for Maybe and boolean operations

-- | Check if a Maybe value is Just
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

-- | Execute an action only if a condition is True
unless :: Applicative f => Bool -> f () -> f ()
unless condition action = if condition then pure () else action

-- | Execute multiple actions in sequence and return a list of results
forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m [b]
forM xs f = sequence (fmap f xs)

-- | Adapter functions for backward compatibility with old interfaces

-- | Adapter for initializing a program with default settings
adaptInitializeProgram :: 
  (Member (Error AppError) r) => 
  ByteString -> 
  Sem r Program
adaptInitializeProgram programName = do
  -- Create an empty time map
  let emptyTimeMap = TimeMap Map.empty Map.empty
      
      -- Create a dummy program definition
      emptyContract = MemoryContract [] Map.empty
      definition = ProgramDefinition
        { programName = "Legacy Program"
        , programDescription = "Automatically migrated from old system"
        , programEffects = []
        , programFunctions = Map.empty
        , programMemoryContract = emptyContract
        }
      
      -- Use a default owner address
      defaultOwner = "default-owner"
  
  -- Create a program with the empty time map and dummy definition
  createProgram emptyTimeMap definition defaultOwner

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