{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module: Core.Schema
Description: Schema definition and evolution for program state

This module provides the schema evolution functionality for Time Bandits programs.
It defines the schema type, schema fields, and evolution rules that govern how
program schemas can change across versions while maintaining causal traceability
and preserving replayability.
-}

module Core.Schema
  ( -- * Schema Types
    Schema(..)
  , SchemaField(..)
  , FieldType(..)
  , EvolutionRules(..)
  , EvolutionError(..)
  , EvolutionResult(..)
  , IncompatibilityReason(..)
  , SafeStateStatus(..)
  , ProtocolVersion
  , ProgramState
  , SafeStatePolicy(..)
  , Program(..)
  
  -- * Schema Evolution
  , applySchemaEvolution
  , defaultCoreEvolutionRules
  , checkSchemaCompatibility
  , checkSafeState
  
  -- * Utilities
  , versionToText
  , textToVersion
  ) where

import Control.Exception (Exception)
import Control.Monad (forM, unless, when)
import Data.Aeson (FromJSON, ToJSON, Value(..), Object, toJSON, object)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Data.Version (Version, parseVersion, showVersion, makeVersion)
import Text.ParserCombinators.ReadP (readP_to_S)
import Data.List (find, foldl)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Core.Common (Hash, EntityHash)
import Core.ProgramId (ProgramId)

-- | Protocol version type
type ProtocolVersion = Version

-- | The program state type (simplified for this implementation)
type ProgramState = Map Text Value

-- | Schema definition for program state
data Schema = Schema
    { schemaVersion :: Version
    , fields :: [SchemaField]
    , evolutionRules :: EvolutionRules
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Field definition within a schema
data SchemaField = SchemaField
    { fieldName :: Text
    , fieldType :: FieldType
    , isOptional :: Bool
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Types supported in schema fields
data FieldType 
    = FieldInt 
    | FieldDecimal 
    | FieldText 
    | FieldBool
    | FieldHash
    | FieldEntityHash
    | FieldMap FieldType FieldType 
    | FieldList FieldType
    | FieldCustom Text  -- For custom data types
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Rules governing schema evolution
data EvolutionRules = EvolutionRules
    { allowAddOptionalFields :: Bool
    , allowAddFieldsWithDefault :: Bool
    , allowRemoveUnusedFields :: Bool
    , allowRenameFields :: Bool -- Off by default
    , allowTypeChanges :: Bool -- Off by default
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Default evolution rules for core program state
defaultCoreEvolutionRules :: EvolutionRules
defaultCoreEvolutionRules = EvolutionRules
    { allowAddOptionalFields = True
    , allowAddFieldsWithDefault = True
    , allowRemoveUnusedFields = True
    , allowRenameFields = False
    , allowTypeChanges = False
    }

-- | Error types for schema evolution
data EvolutionError
    = FieldNotInOldSchema Text
    | FieldTypeMismatch Text FieldType FieldType
    | FieldRequiredNoDefault Text
    | RemovalNotAllowed Text
    | RenameNotAllowed Text Text
    | TypeChangeNotAllowed Text FieldType FieldType
    | InvalidDefaultValue Text
    | InvalidSchemaVersion Version Version
    deriving (Show, Eq, Generic, Exception, FromJSON, ToJSON)

-- | Result of schema evolution
data EvolutionResult 
    = EvolutionApplied 
    | EvolutionSkipped Text
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Reasons for schema incompatibility
data IncompatibilityReason
    = ProtocolTooOld Text
    | ProtocolTooNew Text
    | MissingRequiredFields [Text]
    | InvalidSchemaDefinition Text
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Program state safety status for upgrades
data SafeStateStatus 
    = Safe 
    | UnsafePendingCrossProgramCall
    | UnsafeExternalHold
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Safe state policies
data SafeStatePolicy
    = AlwaysSafe
    | NoPendingReturns
    | NoExternalHolds
    | Custom (Program -> IO SafeStateStatus)
    deriving (Generic)

-- | Program type with schema and effect DAG
data Program = Program
    { programID :: ProgramId
    , version :: Version
    , protocolVersion :: ProtocolVersion
    , schema :: Schema
    , safeStatePolicy :: SafeStatePolicy
    , effectDAG :: Map Text Value  -- Simplified for this implementation
    , programState :: ProgramState
    }
    deriving (Generic)

-- | Main schema evolution function
-- | Takes old schema, new schema, and program state, returns evolved state or error
applySchemaEvolution :: Schema -> Schema -> Map Text ProgramState -> Either EvolutionError (Map Text ProgramState)
applySchemaEvolution oldSchema newSchema state = do
    -- Validate schema versions
    validateSchemaVersions oldSchema newSchema
    
    -- Check field compatibility
    checkFieldCompatibility oldSchema newSchema
    
    -- Apply evolution if all checks pass
    Right $ evolveState oldSchema newSchema state
  where
    -- Check that new schema version is greater than old version
    validateSchemaVersions old new =
      if schemaVersion new > schemaVersion old
        then Right ()
        else Left $ InvalidSchemaVersion (schemaVersion old) (schemaVersion new)
    
    -- Check compatibility of each field
    checkFieldCompatibility old new = do
      -- Check existing fields
      forM_ (fields old) $ \oldField -> 
        case findField (fieldName oldField) (fields new) of
          -- Field removed in new schema
          Nothing -> 
            if allowRemoveUnusedFields (evolutionRules old)
              then Right ()
              else Left $ RemovalNotAllowed (fieldName oldField)
          
          -- Field exists in both schemas
          Just newField -> 
            if fieldType oldField == fieldType newField
              then Right ()
              else if allowTypeChanges (evolutionRules old)
                then Right ()
                else Left $ TypeChangeNotAllowed (fieldName oldField) 
                                             (fieldType oldField) 
                                             (fieldType newField)
      
      -- Check new fields
      forM_ (fields new) $ \newField ->
        case findField (fieldName newField) (fields old) of
          -- Field exists in old schema, already checked above
          Just _ -> Right ()
          
          -- New field
          Nothing ->
            if isOptional newField && allowAddOptionalFields (evolutionRules old)
              then Right ()
              else if allowAddFieldsWithDefault (evolutionRules old)
                then Right ()
                else Left $ FieldRequiredNoDefault (fieldName newField)
    
    -- Find a field in a list by name
    findField name = find (\f -> fieldName f == name)
    
    -- Apply the actual state evolution
    evolveState :: Schema -> Schema -> Map Text ProgramState -> Map Text ProgramState
    evolveState old new stateMap = 
      -- For each program state, apply schema evolution
      Map.map (evolveStateFields old new) stateMap
    
    -- Evolve fields in a single program state
    evolveStateFields :: Schema -> Schema -> ProgramState -> ProgramState
    evolveStateFields old new state = 
      -- Remove fields not in new schema if allowed
      let trimmedState = if allowRemoveUnusedFields (evolutionRules old)
                         then removeUnusedFields old new state
                         else state
                         
      -- Add new fields with defaults
      in addNewFields old new trimmedState
    
    -- Remove fields not present in new schema
    removeUnusedFields :: Schema -> Schema -> ProgramState -> ProgramState
    removeUnusedFields old new state =
      let newFieldNames = Set.fromList $ map fieldName (fields new)
          shouldKeep k _ = k `Set.member` newFieldNames
      in Map.filterWithKey shouldKeep state
    
    -- Add new fields with default values
    addNewFields :: Schema -> Schema -> ProgramState -> ProgramState
    addNewFields old new state =
      let oldFieldNames = Set.fromList $ map fieldName (fields old)
          newFields = filter (\f -> fieldName f `Set.notMember` oldFieldNames) (fields new)
          
          -- Add each new field with its default value
          addField :: ProgramState -> SchemaField -> ProgramState
          addField s field = 
            if isOptional field
              then Map.insert (fieldName field) (toJSON Null) s
              else Map.insert (fieldName field) (defaultForType (fieldType field)) s
      in foldl addField state newFields
    
    -- Generate default value for a field type
    defaultForType :: FieldType -> Value
    defaultForType = \case
      FieldInt -> Number 0
      FieldDecimal -> Number 0.0
      FieldText -> String ""
      FieldBool -> Bool False
      FieldHash -> Null
      FieldEntityHash -> Null
      FieldMap _ _ -> object []  -- Use object [] instead of Object Map.empty
      FieldList _ -> Array V.empty
      FieldCustom _ -> Null

-- | Check if a schema is compatible with a protocol version
checkSchemaCompatibility :: Schema -> ProtocolVersion -> Either IncompatibilityReason ()
checkSchemaCompatibility schema protocolVersion =
  if protocolVersion < minimumSupportedProtocol
    then Left $ ProtocolTooOld $ 
      "Protocol version " <> versionToText protocolVersion <> 
      " is too old for schema " <> versionToText (schemaVersion schema) <>
      ". Minimum supported: " <> versionToText minimumSupportedProtocol
    else if protocolVersion > maximumSupportedProtocol
      then Left $ ProtocolTooNew $
        "Protocol version " <> versionToText protocolVersion <>
        " is too new for schema " <> versionToText (schemaVersion schema) <>
        ". Maximum supported: " <> versionToText maximumSupportedProtocol
      else Right ()
  where
    -- These would come from actual configuration in real implementation
    minimumSupportedProtocol = makeVersion [1, 0, 0]
    maximumSupportedProtocol = makeVersion [3, 0, 0]

-- | Check if a program is in a safe state for evolution
checkSafeState :: Program -> IO SafeStateStatus
checkSafeState program = 
  case safeStatePolicy program of
    -- Always safe
    AlwaysSafe -> pure Safe
    
    -- Check for pending cross-program calls
    NoPendingReturns -> do
      let pendingCalls = hasPendingCrossProgramCalls program
      pure $ if pendingCalls then UnsafePendingCrossProgramCall else Safe
      
    -- Check for external holds (e.g., timeline locks)
    NoExternalHolds -> do
      externalHolds <- checkExternalHolds program
      pure $ if externalHolds then UnsafeExternalHold else Safe
    
    -- Custom policy defined by program
    Custom checkFn -> checkFn program

-- Helper function to check if a program has pending cross-program calls
hasPendingCrossProgramCalls :: Program -> Bool
hasPendingCrossProgramCalls program =
  -- This is a simplified implementation
  -- In a real world scenario, we'd check the effect DAG for pending calls
  False

-- Helper function to check if external systems have holds on this program
checkExternalHolds :: Program -> IO Bool
checkExternalHolds program = 
  -- This is a simplified implementation
  -- In a real world scenario, we'd check external systems
  pure False

-- | Convert a Version to Text
versionToText :: Version -> Text
versionToText = T.pack . showVersion

-- | Convert Text to a Version
textToVersion :: Text -> Maybe Version
textToVersion text =
  case readP_to_S parseVersion (T.unpack text) of
    [] -> Nothing
    -- Use a safe way to get the last element
    xs -> safeLast xs >>= \(v, _) -> Just v
  where
    -- Safe last function
    safeLast [] = Nothing
    safeLast [x] = Just x
    safeLast (_:xs) = safeLast xs