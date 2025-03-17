module TimeBandits.Simulation.Controller.ControllerTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Either (isRight, isLeft)

import Types.Actor (ActorID(..), ActorType(..))
import TimeBandits.Core.TimelineId (TimelineID(..))
import Types.Effect (Fact(..))
import Simulation.Actors.Actor
import Simulation.Scenario.Scenario
import Simulation.Controller.Controller
import Simulation.Controller.InMemoryRunner

tests :: TestTree
tests = testGroup "Controller Interface Tests"
  [ testCase "Create and run controller" testCreateAndRunController
  , testCase "Actor management in controller" testActorManagement
  , testCase "Inject fact to actors" testInjectFact
  , testCase "Observer actor logs" testObserveLogs
  ]

-- | Create a test scenario with some actors
createTestScenario :: Scenario
createTestScenario =
  createScenario "Test Scenario" InMemory
    `addActor` ActorID "trader1" Trader Nothing
    `addActor` ActorID "keeper1" TimeKeeper (Just $ TimelineID "ethereum")
    `addActor` ActorID "bandit1" TimeBandit (Just $ TimelineID "ethereum")

testCreateAndRunController :: Assertion
testCreateAndRunController = do
  -- Create a scenario
  let scenario = createTestScenario
  
  -- Run the scenario with an in-memory controller
  controllerResult <- runInMemoryScenario scenario
  
  -- Check that the controller was created successfully
  case controllerResult of
    Left err -> assertFailure $ "Failed to create controller: " ++ show err
    Right controller -> do
      -- Check controller state
      state <- getControllerState controller
      state @?= ControllerRunning
      
      -- Get actor info
      actorInfo <- getActorInfo controller
      Map.size actorInfo @?= 3
      
      -- Clean up
      void $ stopController controller

testActorManagement :: Assertion
testActorManagement = do
  -- Create a scenario
  let scenario = createTestScenario
  
  -- Run the scenario with an in-memory controller
  controllerResult <- runInMemoryScenario scenario
  
  -- Check that the controller was created successfully
  case controllerResult of
    Left err -> assertFailure $ "Failed to create controller: " ++ show err
    Right controller -> do
      -- Check state of trader1
      traderStateResult <- getActorState controller (ActorID "trader1")
      case traderStateResult of
        Left err -> assertFailure $ "Failed to get trader state: " ++ show err
        Right state -> state @?= Running
      
      -- Pause trader1
      pauseResult <- pauseActor controller (ActorID "trader1")
      pauseResult @?= Right ()
      
      -- Check paused state
      pausedStateResult <- getActorState controller (ActorID "trader1")
      case pausedStateResult of
        Left err -> assertFailure $ "Failed to get paused state: " ++ show err
        Right state -> state @?= Paused
      
      -- Resume trader1
      resumeResult <- resumeActor controller (ActorID "trader1")
      resumeResult @?= Right ()
      
      -- Check running state again
      runningAgainResult <- getActorState controller (ActorID "trader1")
      case runningAgainResult of
        Left err -> assertFailure $ "Failed to get running state: " ++ show err
        Right state -> state @?= Running
      
      -- Try to get state of non-existent actor
      nonExistentResult <- getActorState controller (ActorID "nonexistent")
      nonExistentResult `shouldSatisfy` isLeft
      
      -- Clean up
      void $ stopController controller

testInjectFact :: Assertion
testInjectFact = do
  -- Create a scenario
  let scenario = createTestScenario
  
  -- Run the scenario with an in-memory controller
  controllerResult <- runInMemoryScenario scenario
  
  -- Check that the controller was created successfully
  case controllerResult of
    Left err -> assertFailure $ "Failed to create controller: " ++ show err
    Right controller -> do
      -- Create a test fact (using a simple mock implementation here)
      let fact = TestFact "test_fact" "value"
      
      -- Inject the fact
      injectResult <- injectFact controller fact
      injectResult @?= Right ()
      
      -- Get logs to verify the fact was injected
      logs <- getLogs controller
      let factLogs = filter isFact logs
      length factLogs @?= 1
      
      -- Clean up
      void $ stopController controller
  where
    isFact (LogFact _) = True
    isFact _ = False

testObserveLogs :: Assertion
testObserveLogs = do
  -- Create a scenario
  let scenario = createTestScenario
  
  -- Run the scenario with an in-memory controller
  controllerResult <- runInMemoryScenario scenario
  
  -- Check that the controller was created successfully
  case controllerResult of
    Left err -> assertFailure $ "Failed to create controller: " ++ show err
    Right controller -> do
      -- Pause and resume an actor to generate some logs
      void $ pauseActor controller (ActorID "trader1")
      void $ resumeActor controller (ActorID "trader1")
      
      -- Observe logs for trader1
      logsResult <- observeLog controller (ActorID "trader1")
      case logsResult of
        Left err -> assertFailure $ "Failed to observe logs: " ++ show err
        Right actorLogs -> do
          -- Should have at least 2 state change logs (paused and resumed)
          let stateChangeLogs = filter isStateChange actorLogs
          length stateChangeLogs >= 2 @? "Should have at least 2 state change logs"
      
      -- Try to observe logs for non-existent actor
      nonExistentResult <- observeLog controller (ActorID "nonexistent")
      case nonExistentResult of
        Left _ -> return ()  -- Expected
        Right logs -> 
          length logs @?= 0  -- Should have no logs
      
      -- Clean up
      void $ stopController controller
  where
    isStateChange (LogActorState _ _) = True
    isStateChange _ = False

-- | Simple fact implementation for testing
data TestFact = TestFact Text Text
  deriving (Show, Eq)

instance Fact TestFact where
  factType _ = "test_fact"
  factId (TestFact name value) = name <> ":" <> value
  factHash _ = "test_hash"
  factTimestamp _ = undefined

-- | Helper function to check if a result is Left
shouldSatisfy :: (Show a, Show b) => Either a b -> (Either a b -> Bool) -> Assertion
shouldSatisfy result predicate =
  unless (predicate result) $
    assertFailure $ "Result " ++ show result ++ " did not satisfy predicate"
  where
    unless cond action = if cond then return () else action 