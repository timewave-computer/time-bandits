module Simulation.Scenario.ScenarioTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Types.Actor (ActorID(..), ActorType(..))
import Core.TimelineId (TimelineID(..))
import Types.Effect (Fact(..))
import Simulation.Scenario.Scenario

tests :: TestTree
tests = testGroup "Scenario Tests"
  [ testCase "Create basic scenario" testCreateScenario
  , testCase "Add actors to scenario" testAddActors
  , testCase "Add facts to scenario" testAddFacts
  , testCase "Add invariants to scenario" testAddInvariants
  , testCase "Create complete scenario" testCompleteScenario
  ]

testCreateScenario :: Assertion
testCreateScenario = do
  let scenario = createScenario "Test Scenario" InMemory
  
  scenarioName scenario @?= "Test Scenario"
  scenarioMode scenario @?= InMemory
  scenarioActors scenario @?= []
  scenarioFacts scenario @?= []
  scenarioInvariants scenario @?= []

testAddActors :: Assertion
testAddActors = do
  let scenario = createScenario "Test Scenario" LocalProcesses
      traderId = ActorID "trader1"
      keeperId = ActorID "keeper1"
      timelineId = TimelineID "ethereum"
      
      scenarioWithTrader = addActor scenario traderId Trader Nothing
      scenarioWithBoth = addActor scenarioWithTrader keeperId TimeKeeper (Just timelineId)
  
  length (scenarioActors scenarioWithTrader) @?= 1
  actorId (head $ scenarioActors scenarioWithTrader) @?= traderId
  actorType (head $ scenarioActors scenarioWithTrader) @?= Trader
  timeline (head $ scenarioActors scenarioWithTrader) @?= Nothing
  
  length (scenarioActors scenarioWithBoth) @?= 2
  actorId (scenarioActors scenarioWithBoth !! 1) @?= keeperId
  actorType (scenarioActors scenarioWithBoth !! 1) @?= TimeKeeper
  timeline (scenarioActors scenarioWithBoth !! 1) @?= Just timelineId

testAddFacts :: Assertion
testAddFacts = do
  let scenario = createScenario "Test Scenario" InMemory
      timelineId = TimelineID "ethereum"
      fact1 = MockFact "balance" "100"
      fact2 = MockFact "price" "1000"
      
      scenarioWithFact1 = addFact scenario timelineId fact1
      scenarioWithBothFacts = addFact scenarioWithFact1 timelineId fact2
  
  length (scenarioFacts scenarioWithFact1) @?= 1
  factTimeline (head $ scenarioFacts scenarioWithFact1) @?= timelineId
  fact (head $ scenarioFacts scenarioWithFact1) @?= fact1
  
  length (scenarioFacts scenarioWithBothFacts) @?= 2
  factTimeline (scenarioFacts scenarioWithBothFacts !! 1) @?= timelineId
  fact (scenarioFacts scenarioWithBothFacts !! 1) @?= fact2

testAddInvariants :: Assertion
testAddInvariants = do
  let scenario = createScenario "Test Scenario" InMemory
      params1 = Map.fromList [("min", "0"), ("max", "100")]
      params2 = Map.fromList [("asset", "ETH")]
      
      scenarioWithInv1 = addInvariant scenario "no_negative" "balance" params1
      scenarioWithBothInvs = addInvariant scenarioWithInv1 "asset_exists" "existence" params2
  
  length (scenarioInvariants scenarioWithInv1) @?= 1
  invariantName (head $ scenarioInvariants scenarioWithInv1) @?= "no_negative"
  invariantType (head $ scenarioInvariants scenarioWithInv1) @?= "balance"
  invariantParams (head $ scenarioInvariants scenarioWithInv1) @?= params1
  
  length (scenarioInvariants scenarioWithBothInvs) @?= 2
  invariantName (scenarioInvariants scenarioWithBothInvs !! 1) @?= "asset_exists"
  invariantType (scenarioInvariants scenarioWithBothInvs !! 1) @?= "existence"
  invariantParams (scenarioInvariants scenarioWithBothInvs !! 1) @?= params2

testCompleteScenario :: Assertion
testCompleteScenario = do
  let traderId = ActorID "trader1"
      keeperId = ActorID "keeper1"
      timelineId = TimelineID "ethereum"
      fact1 = MockFact "balance" "100"
      invariantParams = Map.fromList [("min", "0")]
      
      scenario = createScenario "Complete Scenario" GeoDistributed
                 `addActor` traderId Trader Nothing
                 `addActor` keeperId TimeKeeper (Just timelineId)
                 `addFact` timelineId fact1
                 `addInvariant` "no_negative" "balance" invariantParams
  
  scenarioName scenario @?= "Complete Scenario"
  scenarioMode scenario @?= GeoDistributed
  length (scenarioActors scenario) @?= 2
  length (scenarioFacts scenario) @?= 1
  length (scenarioInvariants scenario) @?= 1

-- Mock fact type for testing
data MockFact = MockFact Text Text
  deriving (Show, Eq)

instance Fact MockFact where
  -- Minimal implementation for testing
  factType _ = "mock_fact"
  factId (MockFact n v) = n <> ":" <> v
  factHash _ = "mock_hash"
  factTimestamp _ = undefined 