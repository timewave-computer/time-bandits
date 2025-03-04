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
This module generates type-safe effect adapters from timeline descriptors.
It reads timeline.toml files and produces Haskell code that adapts
abstract effects to timeline-specific operations.

Each adapter handles:
- Encoding and signing transactions specific to the timeline
- Proof collection and validation
- State queries and verification
-}
module TimeBandits.EffectAdapterGenerator 
  ( -- * Core Types
    TimelineDescriptor(..)
  , EffectAdapter(..)
  , TimelineDescriptorError(..)
  
  -- * Descriptor Operations
  , loadTimelineDescriptor
  , parseTimelineDescriptor
  , generateEffectAdapters
  , writeAdapterModule
  
  -- * Timeline Adapter Interface
  , TimelineAdapterInterface(..)
  , applyTimelineEffect
  , verifyTimelineState
  ) where

import Control.Monad (forM_, when)
import Control.Exception (try, SomeException)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Polysemy (Member, Sem, Embed, embed)
import Polysemy.Error (Error, throw, runError, fromEither)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import TimeBandits.Types
  ( AppError(..)
  , LamportTime(..)
  )
import TimeBandits.Effect
  ( Effect(..)
  )
import TimeBandits.Timeline
  ( Timeline
  , TimelineId
  )
import TimeBandits.TimeMap
  ( TimeMap
  )

-- | A timeline descriptor parsed from a TOML file
data TimelineDescriptor = TimelineDescriptor
  { descriptorId :: TimelineId
  , descriptorName :: Text
  , descriptorType :: Text
  , descriptorDescription :: Text
  , descriptorAdapterModule :: Text
  , descriptorProperties :: Map Text Text
  , descriptorClock :: Map Text Text
  , descriptorEffectAdapters :: Map Text Text
  , descriptorProofAdapters :: Map Text Text
  , descriptorResourceMappings :: Map Text Text
  , descriptorRpcEndpoints :: Map Text Text
  , descriptorValidationRules :: Map Text Bool
  , descriptorStateQueries :: Map Text Text
  }
  deriving (Eq, Show, Generic)

-- | An effect adapter for a specific timeline
data EffectAdapter = EffectAdapter
  { adapterName :: Text
  , adapterFunction :: Text
  , adapterCode :: Text
  , adapterImports :: [Text]
  }
  deriving (Eq, Show, Generic)

-- | Timeline descriptor errors
data TimelineDescriptorError
  = DescriptorParseError Text
  | MissingRequiredField Text
  | InvalidFieldValue Text Text
  | AdapterGenerationError Text
  | IOError Text
  deriving (Eq, Show, Generic)

-- | Timeline adapter interface
class TimelineAdapterInterface a where
  -- | Apply an effect to a timeline
  applyEffect :: Effect -> a -> IO (Either AppError a)
  -- | Query the state of a timeline
  queryState :: Text -> [Text] -> a -> IO (Either AppError Text)
  -- | Generate a proof for a timeline state
  generateProof :: Text -> a -> IO (Either AppError Text)
  -- | Verify a proof against a timeline state
  verifyProof :: Text -> Text -> a -> IO (Either AppError Bool)

-- | Load a timeline descriptor from a TOML file
loadTimelineDescriptor :: 
  (Member (Error TimelineDescriptorError) r, Member (Embed IO) r) => 
  FilePath -> 
  Sem r TimelineDescriptor
loadTimelineDescriptor path = do
  -- Read the TOML file
  tomlResult <- liftIO $ try $ TIO.readFile path
  tomlContent <- case tomlResult of
    Left (e :: SomeException) -> throw $ IOError $ "Failed to read file: " <> T.pack (show e)
    Right content -> return content
  
  -- Parse the TOML content
  parseTimelineDescriptor tomlContent

-- | Parse a timeline descriptor from TOML content
parseTimelineDescriptor :: 
  (Member (Error TimelineDescriptorError) r) => 
  Text -> 
  Sem r TimelineDescriptor
parseTimelineDescriptor content = do
  -- Parse the TOML content using a simple line-by-line approach
  -- In a real implementation, this would use a TOML parser library
  
  let lines = T.lines content
      
      -- Function to find key-value pairs
      findValue section key = T.strip <$> findValueInSection section key lines
      
      -- Extract timeline section
      timelineId <- fromMaybe "" <$> findValue "timeline" "id"
      timelineName <- fromMaybe "" <$> findValue "timeline" "name"
      timelineType <- fromMaybe "" <$> findValue "timeline" "type"
      timelineDesc <- fromMaybe "" <$> findValue "timeline" "description"
      timelineModule <- fromMaybe "" <$> findValue "timeline" "adapter_module"
      
      -- Extract other sections as maps
      properties = extractSectionAsMap "properties" lines
      clock = extractSectionAsMap "clock" lines
      effectAdapters = extractSectionAsMap "effect_adapters" lines
      proofAdapters = extractSectionAsMap "proof_adapters" lines
      resourceMappings = extractSectionAsMap "resource_mappings" lines
      rpcEndpoints = extractSectionAsMap "rpc_endpoints" lines
      validationRules = Map.map parseBoolValue $ extractSectionAsMap "validation_rules" lines
      stateQueries = extractSectionAsMap "state_queries" lines
  
  -- Validate required fields
  when (T.null timelineId) $
    throw $ MissingRequiredField "timeline.id"
  when (T.null timelineName) $
    throw $ MissingRequiredField "timeline.name"
  when (T.null timelineType) $
    throw $ MissingRequiredField "timeline.type"
  when (T.null timelineModule) $
    throw $ MissingRequiredField "timeline.adapter_module"
  
  -- Create the timeline descriptor
  return TimelineDescriptor
    { descriptorId = EntityHash $ Hash $ T.encodeUtf8 timelineId
    , descriptorName = timelineName
    , descriptorType = timelineType
    , descriptorDescription = timelineDesc
    , descriptorAdapterModule = timelineModule
    , descriptorProperties = properties
    , descriptorClock = clock
    , descriptorEffectAdapters = effectAdapters
    , descriptorProofAdapters = proofAdapters
    , descriptorResourceMappings = resourceMappings
    , descriptorRpcEndpoints = rpcEndpoints
    , descriptorValidationRules = validationRules
    , descriptorStateQueries = stateQueries
    }

-- | Generate effect adapters from a timeline descriptor
generateEffectAdapters :: 
  (Member (Error TimelineDescriptorError) r) => 
  TimelineDescriptor -> 
  Sem r [EffectAdapter]
generateEffectAdapters descriptor = do
  -- Generate adapters for each effect
  let effectMappings = Map.toList $ descriptorEffectAdapters descriptor
  
  -- Create adapters for standard effects
  adapters <- forM effectMappings $ \(effectType, functionName) -> do
    adapterCode <- generateAdapterCode descriptor effectType functionName
    return EffectAdapter
      { adapterName = effectType
      , adapterFunction = functionName
      , adapterCode = adapterCode
      , adapterImports = ["TimeBandits.Core", "TimeBandits.Effect", "TimeBandits.Timeline"]
      }
  
  return adapters

-- | Generate code for an effect adapter
generateAdapterCode :: 
  (Member (Error TimelineDescriptorError) r) => 
  TimelineDescriptor -> 
  Text -> 
  Text -> 
  Sem r Text
generateAdapterCode descriptor effectType functionName = do
  -- Generate function signature and implementation based on effect type
  case effectType of
    "create_resource" -> 
      return $ T.unlines
        [ "-- | Create a resource on the " <> descriptorName descriptor <> " timeline"
        , functionName <> " :: ResourceMetadata -> Address -> TimeMap -> IO (Either AppError Resource)"
        , functionName <> " metadata owner timeMap = do"
        , "  -- Implement resource creation for " <> descriptorName descriptor
        , "  -- This would use the RPC endpoints to create a token or other resource"
        , "  -- and return the resulting Resource wrapped in Either"
        , "  undefined -- Placeholder"
        ]
    
    "transfer_resource" ->
      return $ T.unlines
        [ "-- | Transfer a resource on the " <> descriptorName descriptor <> " timeline"
        , functionName <> " :: Resource -> Address -> TimeMap -> IO (Either AppError Resource)"
        , functionName <> " resource recipient timeMap = do"
        , "  -- Implement resource transfer for " <> descriptorName descriptor
        , "  -- This would use the RPC endpoints to transfer ownership"
        , "  -- and return the updated Resource wrapped in Either"
        , "  undefined -- Placeholder"
        ]
    
    "escrow_to_program" ->
      return $ T.unlines
        [ "-- | Escrow a resource to a program on the " <> descriptorName descriptor <> " timeline"
        , functionName <> " :: Resource -> ProgramId -> MemorySlot -> TimeMap -> IO (Either AppError Resource)"
        , functionName <> " resource programId slot timeMap = do"
        , "  -- Implement escrow operation for " <> descriptorName descriptor
        , "  -- This would lock the resource in a contract or escrow account"
        , "  -- and return the updated Resource wrapped in Either"
        , "  undefined -- Placeholder"
        ]
    
    "claim_from_program" ->
      return $ T.unlines
        [ "-- | Claim a resource from a program on the " <> descriptorName descriptor <> " timeline"
        , functionName <> " :: ProgramId -> MemorySlot -> Address -> TimeMap -> IO (Either AppError Resource)"
        , functionName <> " programId slot recipient timeMap = do"
        , "  -- Implement claim operation for " <> descriptorName descriptor
        , "  -- This would release the resource from escrow to the recipient"
        , "  -- and return the claimed Resource wrapped in Either"
        , "  undefined -- Placeholder"
        ]
    
    "verify_resource" ->
      return $ T.unlines
        [ "-- | Verify a resource exists on the " <> descriptorName descriptor <> " timeline"
        , functionName <> " :: Resource -> TimeMap -> IO (Either AppError Bool)"
        , functionName <> " resource timeMap = do"
        , "  -- Implement resource verification for " <> descriptorName descriptor
        , "  -- This would check if the resource exists and has the expected state"
        , "  -- and return a Bool wrapped in Either"
        , "  undefined -- Placeholder"
        ]
    
    "watch_resource" ->
      return $ T.unlines
        [ "-- | Watch a resource for changes on the " <> descriptorName descriptor <> " timeline"
        , functionName <> " :: Resource -> Condition -> Trigger -> TimeMap -> IO (Either AppError TimeMap)"
        , functionName <> " resource condition trigger timeMap = do"
        , "  -- Implement resource watching for " <> descriptorName descriptor
        , "  -- This would set up monitoring for the resource based on the condition"
        , "  -- and update the TimeMap when the condition is met"
        , "  -- It returns the updated TimeMap wrapped in Either"
        , "  undefined -- Placeholder"
        ]
    
    _ ->
      throw $ InvalidFieldValue "effect_type" effectType

-- | Write adapter module to a file
writeAdapterModule :: 
  (Member (Error TimelineDescriptorError) r, Member (Embed IO) r) => 
  TimelineDescriptor -> 
  [EffectAdapter] -> 
  FilePath -> 
  Sem r ()
writeAdapterModule descriptor adapters outputPath = do
  -- Create the module content
  let moduleName = descriptorAdapterModule descriptor
      imports = generateImports $ concatMap adapterImports adapters
      adapterCode = T.unlines $ map adapterCode adapters
      
      moduleContent = T.unlines
        [ "{-# LANGUAGE BlockArguments #-}"
        , "{-# LANGUAGE DataKinds #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "{-# LANGUAGE ScopedTypeVariables #-}"
        , ""
        , "-- | This module was automatically generated from a timeline descriptor"
        , "-- | for " <> descriptorName descriptor <> " (" <> descriptorType descriptor <> ")"
        , "-- |"
        , "-- | It provides adapter functions for applying effects to this specific timeline"
        , "module " <> moduleName <> " where"
        , ""
        , imports
        , ""
        , "-- | " <> descriptorName descriptor <> " (" <> descriptorType descriptor <> ")"
        , "-- | " <> descriptorDescription descriptor
        , ""
        , adapterCode
        ]
  
  -- Ensure the output directory exists
  liftIO $ createDirectoryIfMissing True $ takeDirectory outputPath
  
  -- Write the module to a file
  ioResult <- liftIO $ try $ TIO.writeFile outputPath moduleContent
  case ioResult of
    Left (e :: SomeException) -> throw $ IOError $ "Failed to write file: " <> T.pack (show e)
    Right () -> return ()

-- | Generate import statements
generateImports :: [Text] -> Text
generateImports imports = 
  let uniqueImports = Map.keys $ Map.fromList $ map (\i -> (i, ())) imports
  in T.unlines $ map (\i -> "import " <> i) uniqueImports

-- Helper functions

-- | Find a value for a key in a specific section
findValueInSection :: Text -> Text -> [Text] -> Maybe Text
findValueInSection section key allLines = 
  let sectionStart = "[" <> section <> "]"
      inSection = dropWhile (\l -> not $ T.strip l == sectionStart) allLines
      sectionLines = takeWhile (\l -> not $ T.isPrefixOf "[" (T.strip l) && not (T.strip l == sectionStart)) $ 
                     if null inSection then [] else tail inSection
      keyPrefix = key <> " = "
      matchingLines = filter (\l -> keyPrefix `T.isPrefixOf` T.strip l) sectionLines
  in case matchingLines of
       [] -> Nothing
       (line:_) -> 
         let parts = T.splitOn "=" line
         in if length parts >= 2
            then Just $ removeQuotes $ T.strip $ parts !! 1
            else Nothing

-- | Remove quotes from a text value
removeQuotes :: Text -> Text
removeQuotes t =
  let t' = T.strip t
  in if T.length t' >= 2 && T.head t' == '"' && T.last t' == '"'
     then T.drop 1 $ T.dropEnd 1 t'
     else t'

-- | Extract a section as a Map
extractSectionAsMap :: Text -> [Text] -> Map Text Text
extractSectionAsMap section allLines = 
  let sectionStart = "[" <> section <> "]"
      inSection = dropWhile (\l -> not $ T.strip l == sectionStart) allLines
      sectionLines = takeWhile (\l -> not $ T.isPrefixOf "[" (T.strip l) && not (T.strip l == sectionStart)) $ 
                     if null inSection then [] else tail inSection
      keyValuePairs = mapMaybe extractKeyValue sectionLines
  in Map.fromList keyValuePairs

-- | Extract a key-value pair from a line
extractKeyValue :: Text -> Maybe (Text, Text)
extractKeyValue line =
  let stripped = T.strip line
  in if "#" `T.isPrefixOf` stripped || T.null stripped
     then Nothing
     else case T.splitOn "=" stripped of
            [k, v] -> Just (T.strip k, removeQuotes $ T.strip v)
            _ -> Nothing

-- | Parse a boolean value
parseBoolValue :: Text -> Bool
parseBoolValue t =
  let lower = T.toLower $ T.strip t
  in lower == "true" || lower == "yes" || lower == "1"

-- | Helper function to lift IO actions into Sem
liftIO :: Member (Embed IO) r => IO a -> Sem r a
liftIO = embed

-- | Apply an effect to a timeline using the appropriate adapter
applyTimelineEffect :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  Effect -> 
  Timeline ->
  TimeMap ->
  Sem r (Either AppError (Timeline, TimeMap))
applyTimelineEffect effect timeline timeMap = do
  -- In a real implementation, this would dynamically select the appropriate
  -- adapter based on the timeline type and effect type, then call the adapter.
  -- For now, it's just a placeholder.
  return $ Right (timeline, timeMap)

-- | Verify a timeline state using the appropriate adapter
verifyTimelineState :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  Timeline ->
  TimeMap ->
  Sem r (Either AppError Bool)
verifyTimelineState timeline timeMap = do
  -- In a real implementation, this would verify the timeline state
  -- using the appropriate adapter.
  -- For now, it's just a placeholder.
  return $ Right True 