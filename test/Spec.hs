module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

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

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Time Bandits Tests"
  [ Core.SchemaTest.tests
  , Core.TECLTest.tests
  , Core.ProgramTest.tests
  , Core.EffectDAGTest.tests
  , Core.FactTest.tests
  , Core.AccountProgramTest.tests
  , Core.InvocationTest.tests
  , Core.P2PEffectTest.tests
  , Core.SchemaEvolutionTest.tests
  , Core.LoggingTest.tests
  , Simulation.EnvironmentTest.tests
  , Visualization.VisualizerTest.tests
  , EndToEnd.FullLifecycleTest.tests
  , SimpleNetworkConfig.tests
  , TimelineScenarioTest.tests
  , ControllerTest.tests
  , TimelineDescriptorTest.timelineDescriptorTests
  , DistributedLogTest.tests
  ] 