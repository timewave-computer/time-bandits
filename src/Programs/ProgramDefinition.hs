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
Module: Programs.ProgramDefinition
Description: Definition of the static, immutable structure of programs

This module defines the static structure of programs in the Time-Bandits architecture,
separating the immutable definition from the mutable state (found in ProgramState).

Separating the program definition from state provides several benefits:
1. Security - immutable definition prevents runtime modification of behavior
2. Serialization - simpler state serialization for persistence
3. Verification - enables formal verification of program logic
4. Upgrades - allows program logic to evolve while maintaining state

The module exports types for program definitions, memory contracts,
and functions for creating and validating definitions.
-}
module Programs.ProgramDefinition
  ( -- * Core types
    ProgramDefinition(..)
  , MemoryContract(..)
  , SlotSpec(..)
  , ResourceRequirement(..)
  
  -- * Creation functions
  , createProgramDefinition
  , validateDefinition
  
  -- * Function management
  , getProgramFunctions
  , lookupFunction
  , addFunction
  , removeFunction
  
  -- * Memory contract
  , validateMemoryContract
  , compileMemoryContract
  , contractRequiresSlot
  , getFunctionRequirements
  ) where

import Control.Monad (unless, void, when)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)

-- Import from core modules
import Core.Common (EntityHash, generateEntityHash)
import Core.Error (DefinitionError(..), Result)
import Core.Resource (Address)

-- Import from Programs modules
import Programs.Types (MemorySlot(..), ProgramFunction, ProgramId, ProgramOwner)

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

-- | Program definition contains the static, immutable definition of a program
data ProgramDefinition = ProgramDefinition
  { programId :: ProgramId
  , programOwner :: ProgramOwner
  , programName :: Text
  , programDescription :: Text
  , programCode :: ByteString
  , programFunctions :: Map.Map Text ProgramFunction
  , memoryContract :: MemoryContract
  , programVersion :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new program definition
createProgramDefinition :: ProgramOwner -> Text -> Text -> ByteString -> 
                         Map.Map Text ProgramFunction -> MemoryContract -> 
                         Result ProgramDefinition
createProgramDefinition owner name desc code functions contract = do
  -- Validate inputs
  when (name == "") (Left $ InvalidDefinition "Program name cannot be empty")
  when (Map.null functions) (Left $ InvalidDefinition "Program must have at least one function")
  
  -- Validate memory contract
  validateMemoryContract contract
  
  -- Create the definition
  let def = ProgramDefinition 
        { programId = undefined -- Will be set later
        , programOwner = owner
        , programName = name
        , programDescription = desc
        , programCode = code
        , programFunctions = functions
        , memoryContract = contract
        , programVersion = 1
        }
  
  -- Generate ID based on definition content
  let withId = def { programId = generateEntityHash def }
  
  pure withId

-- | Validate a program definition
validateDefinition :: ProgramDefinition -> Result ()
validateDefinition def = do
  -- Check for required fields
  when (programName def == "") (Left $ InvalidDefinition "Program name cannot be empty")
  when (Map.null $ programFunctions def) (Left $ InvalidDefinition "Program must have at least one function")
  
  -- Validate memory contract
  validateMemoryContract (memoryContract def)
  
  -- Verify ID matches content hash
  let expectedId = generateEntityHash def { programId = undefined }
  unless (programId def == expectedId) 
    (Left $ InvalidDefinition "Program ID does not match content hash")
  
  pure ()

-- | Validate a memory contract
validateMemoryContract :: MemoryContract -> Result ()
validateMemoryContract contract = do
  -- Check for duplicate slot names
  let slotNames = map slotName (slotSpecs contract)
  when (length slotNames /= length (Map.fromList $ zip slotNames [0..])) 
    (Left $ InvalidDefinition "Duplicate slot names in memory contract")
  
  -- Check step requirements refer to valid slots
  let slotSet = Map.fromList $ zip slotNames [0..]
  let allReqs = concatMap snd $ Map.toList $ stepRequirements contract
  programForM_ allReqs $ \req -> 
    unless (isJust $ Map.lookup (requiredSlot req) slotSet)
      (Left $ InvalidDefinition $ "Step requirement refers to undefined slot: " <> show (requiredSlot req))
  
  pure ()

-- | Check if a slot is required by a memory contract
contractRequiresSlot :: MemoryContract -> MemorySlot -> Bool
contractRequiresSlot contract slot =
  any (\spec -> slotName spec == slot && slotRequired spec) (slotSpecs contract)

-- | Compile a memory contract (placeholder for future expansion)
compileMemoryContract :: MemoryContract -> Result ByteString
compileMemoryContract _ = pure "compiled_contract"

-- | Get a list of all program functions
getProgramFunctions :: ProgramDefinition -> [Text]
getProgramFunctions = Map.keys . programFunctions

-- | Look up a function by name
lookupFunction :: Text -> ProgramDefinition -> Maybe ProgramFunction
lookupFunction name = Map.lookup name . programFunctions

-- | Add a function to a program definition (returns a new definition)
addFunction :: Text -> ProgramFunction -> ProgramDefinition -> Result ProgramDefinition
addFunction name func def = do
  when (Map.member name $ programFunctions def)
    (Left $ InvalidDefinition $ "Function already exists: " <> show name)
  
  let newFunctions = Map.insert name func (programFunctions def)
  let newDef = def { programFunctions = newFunctions }
  
  -- Regenerate ID after modification
  let withNewId = newDef { programId = generateEntityHash newDef { programId = undefined } }
  
  pure withNewId

-- | Remove a function from a program definition (returns a new definition)
removeFunction :: Text -> ProgramDefinition -> Result ProgramDefinition
removeFunction name def = do
  unless (Map.member name $ programFunctions def)
    (Left $ InvalidDefinition $ "Function does not exist: " <> show name)
  
  -- Ensure at least one function remains
  when (Map.size (programFunctions def) <= 1)
    (Left $ InvalidDefinition "Cannot remove the last function")
  
  let newFunctions = Map.delete name (programFunctions def)
  let newDef = def { programFunctions = newFunctions }
  
  -- Regenerate ID after modification
  let withNewId = newDef { programId = generateEntityHash newDef { programId = undefined } }
  
  pure withNewId

-- | Get resource requirements for a specific function (placeholder)
getFunctionRequirements :: Text -> ProgramDefinition -> Result [ResourceRequirement]
getFunctionRequirements _ _ = pure []

-- Helper function for validation loops
programForM_ :: Monad m => [a] -> (a -> m ()) -> m ()
programForM_ xs f = sequence_ (map f xs) 