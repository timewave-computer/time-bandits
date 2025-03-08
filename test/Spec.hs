{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Hspec.Runner as Hspec
import qualified Test.Hspec as Hspec

import qualified Core.SchemaTest
import qualified Core.TECLTest
import qualified Core.ProgramTest
import qualified Core.EffectDAGTest
import qualified Core.FactTest
import qualified Core.AccountProgramTest
import qualified Core.InvocationTest
import qualified Core.P2PEffectTest
import qualified Core.SchemaEvolutionTest
import qualified Core.LoggingTest
import qualified Simulation.EnvironmentTest
import qualified Visualization.VisualizerTest
import qualified EndToEnd.FullLifecycleTest
import qualified SimpleNetworkConfig
import qualified TimelineScenarioTest
import qualified ControllerTest
import qualified TimelineDescriptorTest
import qualified DistributedLogTest

-- Import Hspec modules (discovered via hspec-discover)
import qualified Core.FactObservation.RulesSpec
import qualified Core.FactObservation.TOMLParserSpec
import qualified Core.FactObservation.EngineSpec
import qualified Core.FactObservation.IntegrationSpec
import qualified Core.FactObservation.CLISpec

import qualified Core.Log.StandardLogTest
import qualified Core.Log.LogIntegrationTest
import qualified Core.HashingTest
import qualified Network.Discovery.PeerDiscoveryTest
import qualified Network.ManagerTest
import qualified Simulation.Scenario.ScenarioTest
import qualified Simulation.Scenario.ScenarioLoaderTest
import qualified Simulation.Actors.ActorTest
import qualified Simulation.Controller.ControllerTest
import qualified Network.Protocol.VersionTest

main :: IO ()
main = do
  -- Run Hspec tests first
  putStrLn "Running Hspec tests for Fact Observation components..."
  Hspec.hspec $ Hspec.describe "Fact Observation Tests" $ do
    Core.FactObservation.RulesSpec.spec
    Core.FactObservation.TOMLParserSpec.spec
    Core.FactObservation.EngineSpec.spec
    Core.FactObservation.IntegrationSpec.spec
    Core.FactObservation.CLISpec.spec
  
  -- Then run Tasty tests
  putStrLn "\nRunning Tasty tests for Time Bandits core components..."
  defaultMain tests

tests :: TestTree
tests = testGroup "Time Bandits Tests"
  [ testGroup "Core" 
    [ Core.Log.StandardLogTest.tests
    , Core.Log.LogIntegrationTest.tests
    , Core.HashingTest.tests
    ]
  , testGroup "Network"
    [ Network.Discovery.PeerDiscoveryTest.tests
    , Network.ManagerTest.tests
    , Network.Protocol.VersionTest.tests
    ]
  , testGroup "Simulation" 
    [ Simulation.Scenario.ScenarioTest.tests
    , Simulation.Scenario.ScenarioLoaderTest.tests
    , Simulation.Actors.ActorTest.tests
    , Simulation.Controller.ControllerTest.tests
    ]
  ] 