{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
This module provides the Effect Executor responsible for executing effects.
It implements the initial effect execution pipeline that replaces implicit
state updates with explicit effect execution.

The Effect Executor ensures that:
- All timeline changes happen via Effect application
- No more direct state modification—everything goes through an effect handler
-}
module TimeBandits.EffectExecutor 
  ( -- * Effect Execution
    applyEffect
  , executeProgram
  , claimResource
  , modifyMemory
  
  -- * Execution Errors
  , ExecutionError(..)
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw, fromEither)

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import TimeBandits.Types
  ( AppError(..)
  , LamportTime(..)
  )
import TimeBandits.Resource (Resource)
import TimeBandits.Program 
  ( ProgramId
  , MemorySlot
  , ProgramState(..)
  , ProgramMemory(..)
  , lookupMemorySlot
  , updateMemorySlot
  , clearMemorySlot
  )
import TimeBandits.ProgramEffect 
  ( Effect(..)
  , FunctionName
  , GuardedEffect(..)
  )

-- | Execution errors that can occur during effect application
data ExecutionError 
  = MemorySlotNotFound MemorySlot
  | ResourceNotInSlot MemorySlot
  | ProgramNotFound ProgramId
  | FunctionNotFound ProgramId FunctionName
  | InvalidEffect Text
  | GuardFailed Text
  | ResourceAlreadyInUse Resource
  | InsufficientResources Text
  deriving (Eq, Show)

-- | Apply an effect to a program state
-- This is the primary interface for executing effects
applyEffect :: 
  (Member (Error ExecutionError) r) => 
  ProgramState -> 
  Effect -> 
  Sem r ProgramState
applyEffect state (EscrowToProgram res pid slot) = modifyMemory pid slot (Just res) state
applyEffect state (InvokeProgram pid fn args) = executeProgram pid fn args state
applyEffect state (ClaimFromProgram pid slot res) = claimResource pid slot res state
applyEffect state (TransferBetweenSlots fromSlot toSlot res) = do
  -- First check if the resource is in the from slot
  let memory = programMemory state
  maybeResource <- fromEither $ checkResourceInSlot memory fromSlot res
  
  -- If it is, remove it from the from slot and add it to the to slot
  case maybeResource of
    Just _ -> do
      -- Clear the from slot
      clearedMemory <- clearMemorySlot memory fromSlot
      -- Update the to slot
      updatedMemory <- updateMemorySlot clearedMemory toSlot res
      -- Return the updated state
      pure $ state { programMemory = updatedMemory }
    Nothing -> throw $ ResourceNotInSlot fromSlot
applyEffect state (CreateToken _ _) = do
  -- In a real implementation, this would create a token on the timeline
  -- For now, just return the state unchanged
  pure state
applyEffect state (DestroyToken _ _) = do
  -- In a real implementation, this would destroy a token on the timeline
  -- For now, just return the state unchanged
  pure state
applyEffect state (TransferToken _ _ _ _) = do
  -- In a real implementation, this would transfer a token on the timeline
  -- For now, just return the state unchanged
  pure state
applyEffect state (CrossTimelineCall _ effect) = do
  -- In a real implementation, this would apply the effect on another timeline
  -- For now, just apply the effect locally
  applyEffect state effect
applyEffect state (LogDiagnostic _) = do
  -- In a real implementation, this would log a diagnostic message
  -- For now, just return the state unchanged
  pure state
applyEffect state (AtomicBatch effects) = do
  -- Apply each effect in sequence
  foldl' (\s e -> s >>= \state' -> applyEffect state' e) (pure state) effects
applyEffect state (DelegateCapability _ _ _) = do
  -- In a real implementation, this would delegate a capability
  -- For now, just return the state unchanged
  pure state
applyEffect state (WatchResource _ _ _) = do
  -- In a real implementation, this would set up a resource watch
  -- For now, just return the state unchanged
  pure state

-- | Execute a program function with arguments
executeProgram :: 
  (Member (Error ExecutionError) r) => 
  ProgramId -> 
  FunctionName -> 
  [Resource] -> 
  ProgramState -> 
  Sem r ProgramState
executeProgram pid fn args state = do
  -- In a real implementation, this would look up the program and execute the function
  -- For now, just return the state unchanged
  pure state

-- | Claim a resource from a program's memory slot
claimResource :: 
  (Member (Error ExecutionError) r) => 
  ProgramId -> 
  MemorySlot -> 
  Resource -> 
  ProgramState -> 
  Sem r ProgramState
claimResource pid slot expectedResource state = do
  -- In a real implementation, this would look up the program and claim the resource
  -- For now, just return the state unchanged
  pure state

-- | Modify a memory slot in a program
modifyMemory :: 
  (Member (Error ExecutionError) r) => 
  ProgramId -> 
  MemorySlot -> 
  Maybe Resource -> 
  ProgramState -> 
  Sem r ProgramState
modifyMemory pid slot maybeResource state = do
  -- In a real implementation, this would look up the program and modify its memory
  -- For now, just update the state's memory directly (assuming it's the current program)
  let memory = programMemory state
  updatedMemory <- case maybeResource of
    Just resource -> updateMemorySlot memory slot resource
    Nothing -> clearMemorySlot memory slot
  
  pure $ state { programMemory = updatedMemory }

-- | Check if a resource is in a memory slot
checkResourceInSlot :: 
  ProgramMemory -> 
  MemorySlot -> 
  Resource -> 
  Either ExecutionError (Maybe Resource)
checkResourceInSlot memory slot expectedResource = do
  case Map.lookup slot (slots memory) of
    Just (Just resource) -> 
      if resource == expectedResource
        then Right $ Just resource
        else Left $ ResourceNotInSlot slot
    Just Nothing -> Left $ ResourceNotInSlot slot
    Nothing -> Left $ MemorySlotNotFound slot

-- | Helper function for foldl with effects
foldl' :: (Monad m) => (a -> b -> m a) -> m a -> [b] -> m a
foldl' f initial [] = initial
foldl' f initial (x:xs) = do
  a <- initial
  a' <- f a x
  foldl' f (pure a') xs 