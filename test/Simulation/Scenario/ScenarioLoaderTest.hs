module Simulation.Scenario.ScenarioLoaderTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad (forM_)
import System.IO.Temp (withSystemTempFile)
import qualified Data.Text.IO as TIO

import Types.Actor (ActorID(..), ActorType(..))
import Core.TimelineId (TimelineID(..))
import Types.Effect (Fact(..))
import Simulation.Scenario.Scenario
import Simulation.Scenario.ScenarioLoader

tests :: TestTree
tests = testGroup "Scenario Loader Tests"
  [ testCase "Parse basic scenario" testParseBasicScenario
  , testCase "Parse scenario with actors" testParseWithActors
  , testCase "Parse scenario with invariants" testParseWithInvariants
  , testCase "Serialize and parse scenario" testSerializeAndParse
  , testCase "Load and save scenario file" testLoadAndSaveScenario
  ]

testParseBasicScenario :: Assertion
testParseBasicScenario = do
  let tomlText = T.unlines
        [ "[scenario]"
        , "name = \"Test Scenario\""
        , "mode = \"InMemory\""
        ]
      
      result = parseScenario tomlText
  
  case result of
    Left err -> assertFailure $ "Failed to parse scenario: " ++ T.unpack err
    Right scenario -> do
      scenarioName scenario @?= "Test Scenario"
      scenarioMode scenario @?= InMemory
      scenarioActors scenario @?= []
      scenarioFacts scenario @?= []
      scenarioInvariants scenario @?= []

testParseWithActors :: Assertion
testParseWithActors = do
  let tomlText = T.unlines
        [ "[scenario]"
        , "name = \"Test Scenario\""
        , "mode = \"LocalProcesses\""
        , ""
        , "[[actors]]"
        , "id = \"trader1\""
        , "type = \"Trader\""
        , ""
        , "[[actors]]"
        , "id = \"keeper1\""
        , "type = \"TimeKeeper\""
        , "timeline = \"ethereum\""
        ]
      
      result = parseScenario tomlText
  
  case result of
    Left err -> assertFailure $ "Failed to parse scenario: " ++ T.unpack err
    Right scenario -> do
      scenarioName scenario @?= "Test Scenario"
      scenarioMode scenario @?= LocalProcesses
      length (scenarioActors scenario) @?= 2
      
      let trader = head (scenarioActors scenario)
          keeper = scenarioActors scenario !! 1
      
      actorId trader @?= ActorID "trader1"
      actorType trader @?= Trader
      timeline trader @?= Nothing
      
      actorId keeper @?= ActorID "keeper1"
      actorType keeper @?= TimeKeeper
      timeline keeper @?= Just (TimelineID "ethereum")

testParseWithInvariants :: Assertion
testParseWithInvariants = do
  let tomlText = T.unlines
        [ "[scenario]"
        , "name = \"Test Scenario\""
        , "mode = \"GeoDistributed\""
        , ""
        , "[invariants]"
        , "no_negative_balances = true"
        , "valid_transactions = true"
        ]
      
      result = parseScenario tomlText
  
  case result of
    Left err -> assertFailure $ "Failed to parse scenario: " ++ T.unpack err
    Right scenario -> do
      scenarioName scenario @?= "Test Scenario"
      scenarioMode scenario @?= GeoDistributed
      length (scenarioInvariants scenario) @?= 2
      
      let inv1 = head (scenarioInvariants scenario)
          inv2 = scenarioInvariants scenario !! 1
      
      invariantName inv1 @?= "no_negative_balances"
      invariantType inv1 @?= "boolean"
      
      invariantName inv2 @?= "valid_transactions"
      invariantType inv2 @?= "boolean"

testSerializeAndParse :: Assertion
testSerializeAndParse = do
  -- Create a test scenario
  let traderId = ActorID "trader1"
      keeperId = ActorID "keeper1"
      timelineId = TimelineID "ethereum"
      
      scenario = createScenario "Roundtrip Test" LocalProcesses
                 `addActor` traderId Trader Nothing
                 `addActor` keeperId TimeKeeper (Just timelineId)
                 `addInvariant` "no_negative" "balance" Map.empty
  
  -- Serialize it
  case serializeScenario scenario of
    Left err -> assertFailure $ "Failed to serialize scenario: " ++ T.unpack err
    Right tomlText -> do
      -- Parse it back
      case parseScenario tomlText of
        Left err -> assertFailure $ "Failed to parse serialized scenario: " ++ T.unpack err
        Right parsedScenario -> do
          -- Check that key properties round-tripped correctly
          scenarioName parsedScenario @?= scenarioName scenario
          scenarioMode parsedScenario @?= scenarioMode scenario
          length (scenarioActors parsedScenario) @?= length (scenarioActors scenario)
          length (scenarioInvariants parsedScenario) @?= length (scenarioInvariants scenario)
          
          -- Check actors
          let actors = scenarioActors parsedScenario
          length actors @?= 2
          actorId (head actors) @?= traderId
          actorType (head actors) @?= Trader
          actorId (actors !! 1) @?= keeperId
          actorType (actors !! 1) @?= TimeKeeper
          timeline (actors !! 1) @?= Just timelineId
          
          -- Check invariants
          let invariants = scenarioInvariants parsedScenario
          length invariants @?= 1
          invariantName (head invariants) @?= "no_negative"

testLoadAndSaveScenario :: Assertion
testLoadAndSaveScenario = do
  withSystemTempFile "scenario.toml" $ \filePath handle -> do
    -- Create a test scenario TOML
    let tomlText = T.unlines
          [ "[scenario]"
          , "name = \"File IO Test\""
          , "mode = \"InMemory\""
          , ""
          , "[[actors]]"
          , "id = \"trader1\""
          , "type = \"Trader\""
          ]
    
    -- Write it to the temp file
    TIO.hPutStr handle tomlText
    
    -- Load the scenario
    loadResult <- loadScenarioFromFile filePath
    
    -- Verify loading worked
    case loadResult of
      Left err -> assertFailure $ "Failed to load scenario: " ++ show err
      Right scenario -> do
        -- Save it back to another temp file
        withSystemTempFile "scenario2.toml" $ \filePath2 handle2 -> do
          saveResult <- serializeScenarioToFile filePath2 scenario
          
          -- Verify saving worked
          case saveResult of
            Left err -> assertFailure $ "Failed to save scenario: " ++ show err
            Right () -> do
              -- Load the saved scenario
              loadResult2 <- loadScenarioFromFile filePath2
              
              -- Verify round-trip worked
              case loadResult2 of
                Left err -> assertFailure $ "Failed to load saved scenario: " ++ show err
                Right scenario2 -> do
                  scenarioName scenario2 @?= scenarioName scenario
                  scenarioMode scenario2 @?= scenarioMode scenario
                  length (scenarioActors scenario2) @?= length (scenarioActors scenario) 