module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Core.SchemaTest
import qualified Core.TECLTest
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
  , SimpleNetworkConfig.tests
  , TimelineScenarioTest.tests
  , ControllerTest.tests
  , TimelineDescriptorTest.timelineDescriptorTests
  , DistributedLogTest.tests
  ] 