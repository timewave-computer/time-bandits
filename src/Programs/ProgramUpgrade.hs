{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module: Programs.ProgramUpgrade
Description: Program upgrade and schema evolution functionality

This module handles the process of upgrading programs, including schema evolution
and safe state checking.
-}

module Programs.ProgramUpgrade
  ( -- * Program Upgrade
    upgradeProgram
  , UpgradeError(..)
  , UpgradeResult(..)
  , MigrationFunction
  ) where

import Control.Exception (Exception)
import Control.Monad (void)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Version (Version)

import Core.Common (Hash, EntityHash)
import Core.Types (ProgramId, Effect(..), EffectID, EffectType(..))
import Core.Schema
    ( Schema
    , SchemaField(..)
    , FieldType(..)
    , EvolutionRules(..)
    , EvolutionError(..)
    , EvolutionResult(..)
    , SafeStateStatus(..)
    , Program(..)
    , applySchemaEvolution
    , checkSafeState
    )

-- | Migration function type - modifies program state after schema evolution
type MigrationFunction = Program -> IO Program

-- | Program upgrade errors
data UpgradeError
    = UnsafeState SafeStateStatus
    | SchemaEvolutionError EvolutionError
    | ProtocolVersionMismatch Text
    | ProgramVersionMismatch Text
    | InvalidMigration Text
    | UpgradePermissionDenied Text
    deriving (Show, Eq)

-- | Result of program upgrade
data UpgradeResult
    = UpgradeSuccess Program
    | UpgradeFailure UpgradeError
    deriving (Show, Eq)

-- | Upgrade a program with schema evolution
upgradeProgram :: Program          -- ^ Current program
               -> Schema           -- ^ New schema
               -> Version          -- ^ New program version
               -> Version          -- ^ New protocol version
               -> MigrationFunction -- ^ Migration function (optional)
               -> IO UpgradeResult
upgradeProgram currentProgram newSchema newVersion newProtocolVersion migrationFn = do
    -- Step 1: Check if the program is in a safe state for upgrade
    safeStatus <- checkSafeState currentProgram
    case safeStatus of
        Safe -> do
            -- Step 2: Check schema evolution rules
            let currentSchema = schema currentProgram
            let currentState = programState currentProgram
            
            case applySchemaEvolution currentSchema newSchema (Map.singleton "state" currentState) of
                Left err -> 
                    -- Schema evolution failed
                    pure $ UpgradeFailure $ SchemaEvolutionError err
                    
                Right evolvedState -> do
                    -- Step 3: Create the upgraded program with evolved schema
                    let upgradedProgram = currentProgram
                            { schema = newSchema
                            , version = newVersion
                            , protocolVersion = newProtocolVersion
                            , programState = Map.findWithDefault Map.empty "state" evolvedState
                            }
                    
                    -- Step 4: Apply the migration function if provided
                    migrated <- migrationFn upgradedProgram
                    
                    -- Step 5: Log the schema evolution effect
                    now <- getCurrentTime
                    let evolutionEffect = createEvolutionEffect 
                                            currentSchema 
                                            newSchema 
                                            EvolutionApplied
                                            now
                                      
                    -- Step 6: Append the evolution effect to the effect DAG
                    let finalProgram = appendEffect migrated evolutionEffect
                    
                    -- Return the final upgraded program
                    pure $ UpgradeSuccess finalProgram
        
        unsafeStatus -> 
            -- Program is not in a safe state for upgrade
            pure $ UpgradeFailure $ UnsafeState unsafeStatus

-- | Create an effect for schema evolution
createEvolutionEffect :: Schema -> Schema -> EvolutionResult -> UTCTime -> Effect
createEvolutionEffect oldSchema newSchema result timestamp =
    Effect
        { effectID = "" -- This would be generated based on content in the real implementation
        , parentEffects = [] -- This would link to the latest effect in the real implementation
        , effectType = EvolveSchema oldSchema newSchema result
        , effectTimestamp = timestamp
        , effectMetadata = Map.empty
        }

-- | Append an effect to a program
appendEffect :: Program -> Effect -> Program
appendEffect program effect =
    -- This is a simplified implementation
    -- In a real implementation, we would:
    -- 1. Hash the effect to get its ID
    -- 2. Add it to the effect DAG properly
    -- 3. Update any related state
    program 