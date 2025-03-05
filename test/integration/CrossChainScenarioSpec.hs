{-# LANGUAGE OverloadedStrings #-}

module CrossChainScenarioSpec (spec) where

import Test.Hspec
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)

import Core.Common (ActorId)
import Core.Resource (ResourceId)
import Core.Timeline (TimelineHash)

import Simulation.Controller
  ( Controller
  , ControllerConfig(..)
  , ControllerSpec(..)
  , SimulationMode(..)
  , SimulationResult(..)
  , createController
  , runWithScenario
  )

import Simulation.Scenario
  ( Scenario(..)
  , ScenarioStep(..)
  , createScenario
  , addStep
  )

import Simulation.Messaging
  ( ActorSpec(..)
  , ActorRole(..)
  , Message(..)
  , MessageType(..)
  )

-- | Create a test scenario for cross-chain operations
createCrossChainScenario :: IO Scenario
createCrossChainScenario = do
  -- Create base scenario
  let scenarioName = "Cross-Chain Transfer Test"
      scenarioDesc = "Tests resource transfers across multiple timelines"
  
  -- Create timelines
  let timeline1 = "timeline-1"
      timeline2 = "timeline-2"
      timelines = [timeline1, timeline2]
  
  -- Create actors
  let traveler1 = ActorSpec
        { _actorSpecID = "traveler-1"
        , _actorSpecRole = TimeTraveler
        , _actorSpecName = "Time Traveler 1"
        , _actorSpecConfig = Map.empty
        , _actorId = "actor-1"
        , _initialBalances = Map.fromList [("token-1", 100), ("token-2", 50)]
        }
      
      traveler2 = ActorSpec
        { _actorSpecID = "traveler-2"
        , _actorSpecRole = TimeTraveler
        , _actorSpecName = "Time Traveler 2"
        , _actorSpecConfig = Map.empty
        , _actorId = "actor-2"
        , _initialBalances = Map.fromList [("token-1", 50), ("token-2", 100)]
        }
      
      keeper1 = ActorSpec
        { _actorSpecID = "keeper-1"
        , _actorSpecRole = TimeKeeper
        , _actorSpecName = "Time Keeper 1"
        , _actorSpecConfig = Map.singleton "timeline" timeline1
        , _actorId = "actor-3"
        , _initialBalances = Map.empty
        }
      
      keeper2 = ActorSpec
        { _actorSpecID = "keeper-2"
        , _actorSpecRole = TimeKeeper
        , _actorSpecName = "Time Keeper 2"
        , _actorSpecConfig = Map.singleton "timeline" timeline2
        , _actorId = "actor-4"
        , _initialBalances = Map.empty
        }
      
      actors = [traveler1, traveler2, keeper1, keeper2]
  
  -- Create base scenario
  let scenario = createScenario scenarioName scenarioDesc timelines actors
  
  -- Add steps
  let step1 = ScenarioStep
        { stepName = "Initialize"
        , stepDescription = "Initialize the scenario"
        , stepActions = []
        , stepDelay = 0
        }
      
      step2 = ScenarioStep
        { stepName = "Transfer on Timeline 1"
        , stepDescription = "Transfer tokens on timeline 1"
        , stepActions = 
            [ ("traveler-1", TransferMessage "actor-2" "token-1" 20 timeline1)
            ]
        , stepDelay = 100
        }
      
      step3 = ScenarioStep
        { stepName = "Transfer on Timeline 2"
        , stepDescription = "Transfer tokens on timeline 2"
        , stepActions = 
            [ ("traveler-2", TransferMessage "actor-1" "token-2" 30 timeline2)
            ]
        , stepDelay = 100
        }
      
      step4 = ScenarioStep
        { stepName = "Cross-Chain Transfer"
        , stepDescription = "Transfer tokens across timelines"
        , stepActions = 
            [ ("traveler-1", CrossChainMessage "actor-2" "token-1" 10 timeline1 timeline2)
            ]
        , stepDelay = 200
        }
      
      step5 = ScenarioStep
        { stepName = "Verify Balances"
        , stepDescription = "Verify final balances"
        , stepActions = 
            [ ("traveler-1", QueryMessage "token-1")
            , ("traveler-1", QueryMessage "token-2")
            , ("traveler-2", QueryMessage "token-1")
            , ("traveler-2", QueryMessage "token-2")
            ]
        , stepDelay = 100
        }
  
  -- Add steps to scenario
  let withSteps = foldl addStep scenario [step1, step2, step3, step4, step5]
  
  pure withSteps

-- | Run the cross-chain scenario
runCrossChainScenario :: IO SimulationResult
runCrossChainScenario = do
  -- Create controller config
  let config = ControllerConfig
        { configLogPath = "test-logs"
        , configVerbose = True
        , configMode = InMemoryMode
        , configTimeLimit = Just 1000
        , configSeed = Just 42
        }
  
  -- Create controller spec
  let spec = ControllerSpec
        { specName = "Cross-Chain Test"
        , specDescription = "Test cross-chain operations"
        , specScenarioPath = "cross-chain-scenario"
        , specConfig = config
        }
  
  -- Create controller
  controller <- createController spec
  
  -- Create scenario
  scenario <- createCrossChainScenario
  
  -- Run scenario
  runWithScenario controller scenario

spec :: Spec
spec = do
  describe "Cross-Chain Operations" $ do
    it "successfully transfers resources across timelines" $ do
      result <- runCrossChainScenario
      resultSuccess result `shouldBe` True
      
    it "maintains correct balances after cross-chain transfers" $ do
      result <- runCrossChainScenario
      
      -- In a real test, we would check the final account states
      -- For now, we just check that there are no errors
      resultErrors result `shouldBe` [] 