{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Simulation.ScenarioLoader
Description: TOML-based scenario loader for Time Bandits

This module provides functionality to load scenarios from TOML files,
implementing the scenario-first approach described in ADR 009.

Scenarios define:
1. Actors (time travelers, time keepers, time bandits)
2. Timelines (Ethereum, Solana, etc.)
3. Initial facts (balances, prices, etc.)
4. Invariants (conditions that must hold)
5. Simulation mode (in-memory, local processes, geo-distributed)

Example TOML scenario:

```toml
[scenario]
name = "CrossChainArbTest"
mode = "LocalProcesses"

[[actors]]
id = "trader1"
type = "Trader"

[[actors]]
id = "keeper_eth"
type = "TimeKeeper"
timeline = "Ethereum"

[[initialFacts]]
timeline = "Ethereum"
fact = { balance = { asset = "USDC", amount = 100 } }

[invariants]
noNegativeBalances = true
```
-}

module Simulation.ScenarioLoader
  ( -- * Loading Functions
    loadScenarioFromTOML
  , parseScenarioTOML
  , validateScenario
  
  -- * Serialization Functions
  , scenarioToTOML
  , writeScenarioToFile
  
  -- * Helper Functions
  , scenarioFromMap
  , actorsFromList
  , factsFromList
  , invariantsFromMap
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (forM, unless, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import System.IO (withFile, IOMode(..))

import qualified Data.ByteString as BS
import qualified Toml

import Core.Common (ActorId(..), TimelineHash)
import Simulation.Scenario (Scenario(..), ScenarioConfig(..), SimulationMode(..))
import Simulation.Messaging (ActorSpec(..), ActorID, ActorRole(..))

-- | TOML parsing options
type Parser = Toml.Parser

-- | TOML codecs for our types
tomlCodecs :: Toml.TomlCodec Scenario
tomlCodecs = Scenario
  <$> Toml.table scenarioConfigCodec "scenario" .= scenarioConfig
  <*> Toml.list actorSpecCodec "actors" .= scenarioActors
  <*> Toml.list factSpecCodec "initialFacts" .= scenarioInitialFacts
  <*> Toml.table invariantsCodec "invariants" .= scenarioInvariants
  <*> Toml.int "scenarioTimeoutSecs" .= scenarioTimeoutSecs

-- | Codec for ScenarioConfig
scenarioConfigCodec :: Toml.TomlCodec ScenarioConfig
scenarioConfigCodec = ScenarioConfig
  <$> Toml.text "name" .= scenarioName
  <*> Toml.text "description" .= scenarioDescription
  <*> Toml.text "mode" .= scenarioMode
  <*> Toml.tableMap Toml.text Toml.text "timeline_config" .= scenarioTimelineConfig

-- | Codec for ActorSpec
actorSpecCodec :: Toml.TomlCodec ActorSpec
actorSpecCodec = ActorSpec
  <$> Toml.text "id" .= actorId
  <*> Toml.text "type" .= actorType
  <*> Toml.dioptional (Toml.text "timeline") .= actorTimeline

-- | Codec for FactSpec
factSpecCodec :: Toml.TomlCodec FactSpec
factSpecCodec = FactSpec
  <$> Toml.text "timeline" .= factTimeline
  <*> Toml.table factValueCodec "fact" .= factValue

-- | Codec for FactValue
factValueCodec :: Toml.TomlCodec FactValue
factValueCodec = Toml.table balanceCodec "balance" .= factValueBalance

-- | Codec for BalanceSpec
balanceCodec :: Toml.TomlCodec BalanceSpec
balanceCodec = BalanceSpec
  <$> Toml.text "asset" .= balanceAsset
  <*> Toml.double "amount" .= balanceAmount

-- | Codec for invariants
invariantsCodec :: Toml.TomlCodec (Map Text Bool)
invariantsCodec = Toml.tableMap Toml.text Toml.bool mempty

-- | Specification of a fact (used in scenarios)
data FactSpec = FactSpec
  { factTimeline :: Text
  , factValue :: FactValue
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Value of a fact (e.g., balance, price)
data FactValue = FactValue
  { factValueBalance :: Maybe BalanceSpec
  -- Add other fact types as needed (prices, events, etc.)
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Specification of a balance fact
data BalanceSpec = BalanceSpec
  { balanceAsset :: Text
  , balanceAmount :: Double
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Load a scenario from a TOML file
loadScenarioFromTOML :: FilePath -> IO Scenario
loadScenarioFromTOML path = do
  content <- BS.readFile path
  case Toml.decode tomlCodecs content of
    Left err -> throwIO $ userError $ "Error parsing TOML scenario: " ++ show err
    Right scenario -> do
      validateScenario scenario
      return scenario

-- | Parse a scenario from TOML text
parseScenarioTOML :: Text -> Either Toml.TomlDecodeError Scenario
parseScenarioTOML = Toml.decode tomlCodecs . encodeUtf8
  where
    encodeUtf8 = BS.pack . map (fromIntegral . fromEnum) . T.unpack

-- | Validate a scenario for correctness
validateScenario :: Scenario -> IO ()
validateScenario scenario = do
  -- Check that all actors are valid
  forM_ (scenarioActors scenario) validateActor
  
  -- Check that all timelines referenced in facts exist
  let timelineActors = [actorId spec | spec <- scenarioActors scenario, isJust (actorTimeline spec)]
  forM_ (scenarioInitialFacts scenario) $ \fact -> do
    unless (factTimeline fact `elem` timelineActors) $
      throwIO $ userError $ "Timeline " ++ T.unpack (factTimeline fact) ++ " referenced in fact does not exist"
  
  -- Additional checks as needed
  where
    validateActor :: ActorSpec -> IO ()
    validateActor ActorSpec{..} = do
      -- Check that actor type is valid
      unless (actorType `elem` ["Trader", "TimeKeeper", "TimeBandit"]) $
        throwIO $ userError $ "Invalid actor type: " ++ T.unpack actorType
      
      -- Check that timeline is specified for TimeKeeper actors
      when (actorType == "TimeKeeper" && isNothing actorTimeline) $
        throwIO $ userError $ "TimeKeeper " ++ T.unpack actorId ++ " must have a timeline"
    
    isJust :: Maybe a -> Bool
    isJust Nothing = False
    isJust _ = True
    
    isNothing :: Maybe a -> Bool
    isNothing = not . isJust

-- | Convert a scenario to TOML
scenarioToTOML :: Scenario -> Text
scenarioToTOML scenario = 
  case Toml.encode tomlCodecs scenario of
    Left err -> error $ "Error encoding scenario to TOML: " ++ show err
    Right bs -> T.pack $ map (toEnum . fromIntegral) $ BS.unpack bs

-- | Write a scenario to a TOML file
writeScenarioToFile :: FilePath -> Scenario -> IO ()
writeScenarioToFile path scenario = do
  let toml = scenarioToTOML scenario
  TIO.writeFile path toml

-- | Create a scenario from a map of values
scenarioFromMap :: Map Text Text -> [ActorSpec] -> [FactSpec] -> Map Text Bool -> Int -> Scenario
scenarioFromMap configMap actors facts invariants timeout =
  Scenario
    { scenarioConfig = ScenarioConfig
        { scenarioName = Map.findWithDefault "Unnamed Scenario" "name" configMap
        , scenarioDescription = Map.findWithDefault "" "description" configMap
        , scenarioMode = Map.findWithDefault "InMemory" "mode" configMap
        , scenarioTimelineConfig = Map.fromList 
            [ (k, v) | (k, v) <- Map.toList configMap, "timeline_" `T.isPrefixOf` k ]
        }
    , scenarioActors = actors
    , scenarioInitialFacts = facts
    , scenarioInvariants = invariants
    , scenarioTimeoutSecs = timeout
    }

-- | Create a list of actors from a list of maps
actorsFromList :: [Map Text Text] -> [ActorSpec]
actorsFromList = map $ \m -> ActorSpec
  { actorId = Map.findWithDefault "unnamed" "id" m
  , actorType = Map.findWithDefault "Trader" "type" m
  , actorTimeline = Map.lookup "timeline" m
  }

-- | Create a list of facts from a list of maps
factsFromList :: [Map Text (Map Text Text)] -> [FactSpec]
factsFromList = map $ \m -> 
  let timeline = maybe "unknown" id $ Map.lookup "timeline" =<< Map.lookup "fact" m
      factMap = Map.findWithDefault Map.empty "fact" m
      asset = Map.findWithDefault "UNKNOWN" "asset" =<< Map.lookup "balance" factMap
      amountText = Map.findWithDefault "0.0" "amount" =<< Map.lookup "balance" factMap
      amount = case reads (T.unpack amountText) of
                 [(n, "")] -> n
                 _ -> 0.0
  in FactSpec
    { factTimeline = timeline
    , factValue = FactValue
        { factValueBalance = Just $ BalanceSpec
            { balanceAsset = asset
            , balanceAmount = amount
            }
        }
    }

-- | Create invariants from a map
invariantsFromMap :: Map Text Text -> Map Text Bool
invariantsFromMap = Map.map $ \v -> v == "true" || v == "yes" || v == "1" 