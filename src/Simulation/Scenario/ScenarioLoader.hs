{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Simulation.Scenario.ScenarioLoader
Description : Loader for scenario-based simulation
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module implements functionality to load simulation scenarios from
TOML configuration files. It handles parsing TOML into Scenario data structures.
-}
module Simulation.Scenario.ScenarioLoader
  ( -- * Scenario Loading
    loadScenarioFromFile
  , loadScenarioFromText
  , parseScenario
  
    -- * Scenario Serialization
  , serializeScenario
  , serializeScenarioToFile
  
    -- * Error Types
  , ScenarioLoadError(..)
  ) where

import Control.Exception (try)
import Control.Monad (forM, when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, catMaybes)
import qualified Toml
import qualified Toml.Type as Toml

import Types.Actor (ActorID(..), ActorType(..))
import Core.TimelineId (TimelineID(..))
import Types.Effect (Fact)
import Simulation.Scenario.Scenario

-- | Errors that can occur during scenario loading
data ScenarioLoadError
  = FileReadError Text       -- ^ Error reading the file
  | ParseError Text          -- ^ Error parsing TOML
  | ValidationError Text     -- ^ Error validating the scenario
  | SerializationError Text  -- ^ Error serializing the scenario
  deriving (Show, Eq)

-- | Load a scenario from a TOML file
loadScenarioFromFile :: FilePath -> IO (Either ScenarioLoadError Scenario)
loadScenarioFromFile filePath = do
  -- Try to read the file
  contentResult <- try $ TIO.readFile filePath
  case contentResult of
    Left err -> return $ Left $ FileReadError $ T.pack $ show (err :: IOError)
    Right content -> loadScenarioFromText content

-- | Load a scenario from TOML text
loadScenarioFromText :: Text -> IO (Either ScenarioLoadError Scenario)
loadScenarioFromText tomlText = do
  case parseScenario tomlText of
    Left err -> return $ Left $ ParseError err
    Right scenario -> do
      -- Validate the scenario
      case validateScenario scenario of
        Left err -> return $ Left $ ValidationError err
        Right validScenario -> return $ Right validScenario

-- | Parse a scenario from TOML text
parseScenario :: Text -> Either Text Scenario
parseScenario tomlText = do
  -- Parse the TOML
  tomlValue <- case Toml.parse tomlText of
    Left err -> Left $ T.pack $ show err
    Right val -> Right val
  
  -- Extract the scenario section
  scenarioTable <- getTable "scenario" tomlValue
  
  -- Extract basic scenario properties
  name <- getText "name" scenarioTable
  modeText <- getText "mode" scenarioTable
  mode <- parseSimulationMode modeText
  
  -- Create the basic scenario
  let scenario = createScenario name mode
  
  -- Parse actors
  actorsArray <- getArrayOrEmpty "actors" tomlValue
  actorSpecs <- forM actorsArray parseActorSpec
  
  -- Parse initial facts
  factsArray <- getArrayOrEmpty "initialFacts" tomlValue
  factSpecs <- forM factsArray parseFactSpec
  
  -- Parse invariants
  invariantsTable <- getTableOrEmpty "invariants" tomlValue
  invariantSpecs <- parseInvariants invariantsTable
  
  -- Build the complete scenario
  let withActors = foldl (\s a -> s { scenarioActors = scenarioActors s ++ [a] }) scenario actorSpecs
      withFacts = foldl (\s f -> s { scenarioFacts = scenarioFacts s ++ [f] }) withActors factSpecs
      withInvariants = foldl (\s i -> s { scenarioInvariants = scenarioInvariants s ++ [i] }) withFacts invariantSpecs
  
  return withInvariants

-- | Serialize a scenario to TOML text
serializeScenario :: Scenario -> Either Text Text
serializeScenario scenario = do
  -- Create the scenario section
  let scenarioSection = T.unlines
        [ "[scenario]"
        , "name = " <> quoted (scenarioName scenario)
        , "mode = " <> serializeSimulationMode (scenarioMode scenario)
        , ""
        ]
  
  -- Serialize actors
  actorsSections <- forM (scenarioActors scenario) $ \actor -> do
    serializeActorSpec actor
  
  -- Serialize facts
  factsSections <- forM (scenarioFacts scenario) $ \fact -> do
    serializeFactSpec fact
  
  -- Serialize invariants
  invariantsSection <- serializeInvariants (scenarioInvariants scenario)
  
  -- Combine all sections
  return $ T.unlines
    [ scenarioSection
    , T.intercalate "\n" actorsSections
    , ""
    , T.intercalate "\n" factsSections
    , ""
    , invariantsSection
    ]

-- | Serialize a scenario to a TOML file
serializeScenarioToFile :: FilePath -> Scenario -> IO (Either ScenarioLoadError ())
serializeScenarioToFile filePath scenario = do
  case serializeScenario scenario of
    Left err -> return $ Left $ SerializationError err
    Right tomlText -> do
      writeResult <- try $ TIO.writeFile filePath tomlText
      case writeResult of
        Left err -> return $ Left $ FileReadError $ T.pack $ show (err :: IOError)
        Right () -> return $ Right ()

-- Helper functions for parsing

-- | Parse a simulation mode from text
parseSimulationMode :: Text -> Either Text SimulationMode
parseSimulationMode "InMemory" = Right InMemory
parseSimulationMode "LocalProcesses" = Right LocalProcesses
parseSimulationMode "GeoDistributed" = Right GeoDistributed
parseSimulationMode other = Left $ "Unknown simulation mode: " <> other

-- | Serialize a simulation mode to text
serializeSimulationMode :: SimulationMode -> Text
serializeSimulationMode InMemory = "\"InMemory\""
serializeSimulationMode LocalProcesses = "\"LocalProcesses\""
serializeSimulationMode GeoDistributed = "\"GeoDistributed\""

-- | Parse an actor specification from a TOML table
parseActorSpec :: Toml.Value -> Either Text ActorSpec
parseActorSpec value = do
  table <- asTable value
  idText <- getText "id" table
  typeText <- getText "type" table
  actorType <- parseActorType typeText
  
  timelineText <- getTextMaybe "timeline" table
  let timelineId = fmap TimelineID timelineText
  
  return ActorSpec
    { actorId = ActorID idText
    , actorType = actorType
    , timeline = timelineId
    }

-- | Serialize an actor specification to TOML
serializeActorSpec :: ActorSpec -> Either Text Text
serializeActorSpec ActorSpec{..} = do
  let timelineSection = case timeline of
        Nothing -> ""
        Just (TimelineID tid) -> "timeline = " <> quoted tid <> "\n"
  
  return $ T.unlines
    [ "[[actors]]"
    , "id = " <> quoted (case actorId of ActorID aid -> aid)
    , "type = " <> quoted (serializeActorType actorType)
    , timelineSection
    ]

-- | Parse an actor type from text
parseActorType :: Text -> Either Text ActorType
parseActorType "Trader" = Right Trader
parseActorType "TimeKeeper" = Right TimeKeeper
parseActorType "TimeBandit" = Right TimeBandit
parseActorType other = Left $ "Unknown actor type: " <> other

-- | Serialize an actor type to text
serializeActorType :: ActorType -> Text
serializeActorType Trader = "Trader"
serializeActorType TimeKeeper = "TimeKeeper"
serializeActorType TimeBandit = "TimeBandit"

-- | Parse a fact specification from a TOML table
parseFactSpec :: Toml.Value -> Either Text FactSpec
parseFactSpec value = do
  table <- asTable value
  timelineText <- getText "timeline" table
  -- Note: In a real implementation, you'd parse the fact based on its type
  -- This is a simplified version that assumes MockFact
  factTable <- getTable "fact" table
  
  -- You would replace this with actual fact parsing logic
  let mockFact = undefined -- MockFact "name" "value"
  
  return $ FactSpec
    { factTimeline = TimelineID timelineText
    , fact = mockFact
    }

-- | Serialize a fact specification to TOML
serializeFactSpec :: FactSpec -> Either Text Text
serializeFactSpec FactSpec{..} = do
  -- In a real implementation, you'd serialize the fact based on its type
  let factSection = "# Simplified fact representation\n"
  
  return $ T.unlines
    [ "[[initialFacts]]"
    , "timeline = " <> quoted (case factTimeline of TimelineID tid -> tid)
    , factSection
    ]

-- | Parse invariants from a TOML table
parseInvariants :: Toml.Value -> Either Text [InvariantSpec]
parseInvariants table = do
  -- Convert the table to a map of key-value pairs
  pairs <- tableItems table
  
  -- Create an invariant for each key-value pair
  forM pairs $ \(name, value) -> do
    -- The value can be a boolean or a table with parameters
    case value of
      Toml.Bool True -> 
        return $ InvariantSpec name "boolean" Map.empty
      Toml.Table params ->
        InvariantSpec name "parameterized" <$> parseParams params
      _ ->
        Left $ "Invalid invariant value for " <> name

-- | Serialize invariants to TOML
serializeInvariants :: [InvariantSpec] -> Either Text Text
serializeInvariants [] = Right ""
serializeInvariants invariants = do
  let header = "[invariants]\n"
  
  invariantLines <- forM invariants $ \InvariantSpec{..} -> do
    if Map.null invariantParams
      then return $ invariantName <> " = true"
      else do
        -- For invariants with parameters, we'd need to serialize the parameters
        return $ "# Parameters not implemented: " <> invariantName
  
  return $ header <> T.unlines invariantLines

-- | Parse parameters from a TOML table
parseParams :: Map Text Toml.Value -> Either Text (Map Text Text)
parseParams params = do
  pairs <- forM (Map.toList params) $ \(key, value) -> do
    textValue <- case value of
      Toml.Text t -> Right t
      Toml.Integer i -> Right $ T.pack $ show i
      Toml.Bool b -> Right $ T.pack $ show b
      _ -> Left $ "Unsupported parameter type for " <> key
    return (key, textValue)
  
  return $ Map.fromList pairs

-- | Validate a scenario
validateScenario :: Scenario -> Either Text Scenario
validateScenario scenario = do
  -- Validate scenario name
  when (T.null (scenarioName scenario)) $
    Left "Scenario name cannot be empty"
  
  -- Validate actors
  forM_ (scenarioActors scenario) $ \actor -> do
    let ActorID aid = actorId actor
    when (T.null aid) $
      Left "Actor ID cannot be empty"
    
    -- TimeKeepers must have a timeline
    when (actorType actor == TimeKeeper && timeline actor == Nothing) $
      Left $ "TimeKeeper actor " <> aid <> " must have a timeline"
  
  -- Validate facts
  forM_ (scenarioFacts scenario) $ \factSpec -> do
    let TimelineID tid = factTimeline factSpec
    when (T.null tid) $
      Left "Fact timeline cannot be empty"
  
  -- Validate invariants
  forM_ (scenarioInvariants scenario) $ \inv -> do
    when (T.null (invariantName inv)) $
      Left "Invariant name cannot be empty"
    
    when (T.null (invariantType inv)) $
      Left "Invariant type cannot be empty"
  
  -- Return the validated scenario
  return scenario

-- Helper functions for working with TOML

-- | Get a table from a TOML value
getTable :: Text -> Toml.Value -> Either Text Toml.Value
getTable key value = case findValue key value of
  Just v@(Toml.Table _) -> Right v
  Just _ -> Left $ "Value at key " <> key <> " is not a table"
  Nothing -> Left $ "Table not found: " <> key

-- | Get a table or an empty table if not found
getTableOrEmpty :: Text -> Toml.Value -> Either Text Toml.Value
getTableOrEmpty key value = case findValue key value of
  Just v@(Toml.Table _) -> Right v
  Just _ -> Left $ "Value at key " <> key <> " is not a table"
  Nothing -> Right $ Toml.Table Map.empty

-- | Get an array from a TOML value
getArray :: Text -> Toml.Value -> Either Text [Toml.Value]
getArray key value = case findValue key value of
  Just (Toml.Array arr) -> Right $ Toml.toList arr
  Just _ -> Left $ "Value at key " <> key <> " is not an array"
  Nothing -> Left $ "Array not found: " <> key

-- | Get an array or an empty array if not found
getArrayOrEmpty :: Text -> Toml.Value -> Either Text [Toml.Value]
getArrayOrEmpty key value = case findValue key value of
  Just (Toml.Array arr) -> Right $ Toml.toList arr
  Just _ -> Left $ "Value at key " <> key <> " is not an array"
  Nothing -> Right []

-- | Get a text value from a TOML table
getText :: Text -> Toml.Value -> Either Text Text
getText key table = case findValue key table of
  Just (Toml.Text text) -> Right text
  Just _ -> Left $ "Value at key " <> key <> " is not a string"
  Nothing -> Left $ "Text value not found: " <> key

-- | Get an optional text value from a TOML table
getTextMaybe :: Text -> Toml.Value -> Either Text (Maybe Text)
getTextMaybe key table = case findValue key table of
  Just (Toml.Text text) -> Right $ Just text
  Just _ -> Left $ "Value at key " <> key <> " is not a string"
  Nothing -> Right Nothing

-- | Convert a TOML value to a table
asTable :: Toml.Value -> Either Text Toml.Value
asTable v@(Toml.Table _) = Right v
asTable _ = Left "Value is not a table"

-- | Find a value in a TOML table by key
findValue :: Text -> Toml.Value -> Maybe Toml.Value
findValue key (Toml.Table m) = Map.lookup key m
findValue _ _ = Nothing

-- | Get items from a TOML table
tableItems :: Toml.Value -> Either Text [(Text, Toml.Value)]
tableItems (Toml.Table m) = Right $ Map.toList m
tableItems _ = Left "Value is not a table"

-- | Quote a text value for TOML
quoted :: Text -> Text
quoted text = "\"" <> text <> "\"" 