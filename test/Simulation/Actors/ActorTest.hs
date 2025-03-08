module Simulation.Actors.ActorTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Types.Actor (ActorID(..), ActorType(..))
import Core.TimelineId (TimelineID(..))
import Simulation.Actors.Actor
import Simulation.Actors.SimpleActor

tests :: TestTree
tests = testGroup "Actor Interface Tests"
  [ testCase "Create and run actor" testCreateAndRunActor
  , testCase "Send commands to actor" testSendCommands
  , testCase "Actor state transitions" testActorStateTransitions
  , testCase "Get actor info" testGetActorInfo
  ]

testCreateAndRunActor :: Assertion
testCreateAndRunActor = do
  -- Create actor configuration
  let config = ActorConfig
        { configActorId = ActorID "test-actor-1"
        , configActorType = Trader
        , configTimeline = Nothing
        , configLogFile = Nothing
        , configParams = Map.empty
        }
  
  -- Create actor
  actor <- createSimpleActor config
  
  -- Check initial state
  initialState <- getActorState actor
  initialState @?= Initializing
  
  -- Run the actor
  threadId <- runActor actor
  
  -- Give it time to start
  threadDelay 100000  -- 100ms
  
  -- Check running state
  runningState <- getActorState actor
  runningState @?= Running
  
  -- Stop the actor
  void $ stopActor actor
  
  -- Give it time to stop
  threadDelay 100000  -- 100ms
  
  -- Check stopped state
  stoppedState <- getActorState actor
  stoppedState @?= Stopped

testSendCommands :: Assertion
testSendCommands = do
  -- Create actor configuration
  let config = ActorConfig
        { configActorId = ActorID "test-actor-2"
        , configActorType = Trader
        , configTimeline = Nothing
        , configLogFile = Nothing
        , configParams = Map.empty
        }
  
  -- Create and run actor
  actor <- createSimpleActor config
  void $ runActor actor
  
  -- Give it time to start
  threadDelay 100000  -- 100ms
  
  -- Send a custom command
  cmdResult1 <- sendCommand actor (CustomCommand "hello")
  cmdResult1 @?= Success
  
  -- Send a pause command
  cmdResult2 <- sendCommand actor Pause
  cmdResult2 @?= Success
  
  -- Check paused state
  pausedState <- getActorState actor
  pausedState @?= Paused
  
  -- Send a resume command
  cmdResult3 <- sendCommand actor Resume
  cmdResult3 @?= Success
  
  -- Check running state
  runningState <- getActorState actor
  runningState @?= Running
  
  -- Clean up
  void $ stopActor actor

testActorStateTransitions :: Assertion
testActorStateTransitions = do
  -- Create actor configuration
  let config = ActorConfig
        { configActorId = ActorID "test-actor-3"
        , configActorType = TimeKeeper
        , configTimeline = Just (TimelineID "test-timeline")
        , configLogFile = Nothing
        , configParams = Map.empty
        }
  
  -- Create actor
  actor <- createSimpleActor config
  
  -- Check initial state
  initialState <- getActorState actor
  initialState @?= Initializing
  
  -- Run the actor
  void $ runActor actor
  
  -- Give it time to start
  threadDelay 100000  -- 100ms
  
  -- Check running state
  runningState <- getActorState actor
  runningState @?= Running
  
  -- Pause
  void $ pauseActor actor
  
  -- Check paused state
  pausedState <- getActorState actor
  pausedState @?= Paused
  
  -- Resume
  void $ resumeActor actor
  
  -- Check running again
  runningAgainState <- getActorState actor
  runningAgainState @?= Running
  
  -- Stop
  void $ stopActor actor
  
  -- Give it time to stop
  threadDelay 100000  -- 100ms
  
  -- Check stopped state
  stoppedState <- getActorState actor
  stoppedState @?= Stopped

testGetActorInfo :: Assertion
testGetActorInfo = do
  -- Create actor configuration
  let config = ActorConfig
        { configActorId = ActorID "test-actor-4"
        , configActorType = TimeBandit
        , configTimeline = Just (TimelineID "test-timeline")
        , configLogFile = Nothing
        , configParams = Map.fromList [("param1", "value1"), ("param2", "value2")]
        }
  
  -- Create and run actor
  actor <- createSimpleActor config
  void $ runActor actor
  
  -- Give it time to start
  threadDelay 100000  -- 100ms
  
  -- Get actor info
  info <- getInfo actor
  
  -- Check info contents
  Map.lookup "id" info @?= Just "test-actor-4"
  Map.lookup "type" info @?= Just "TimeBandit"
  Map.lookup "state" info @?= Just "Running"
  
  -- Check that log_count is present and is a number
  case Map.lookup "log_count" info of
    Nothing -> assertFailure "log_count not found in actor info"
    Just countText -> 
      case reads (T.unpack countText) of
        [(count, "")] -> 
          assertBool "log_count should be positive" (count > 0)
        _ -> 
          assertFailure $ "log_count is not a number: " ++ T.unpack countText
  
  -- Clean up
  void $ stopActor actor 