module Main where

import qualified Test.Hspec as Hspec
import qualified Test.Tasty as Tasty
import qualified SimpleNetworkConfig
import qualified TimelineScenarioTest
import qualified ControllerTest
import qualified TimelineDescriptorTest
import qualified DistributedLogTest

{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

main :: IO ()
main = do
  -- Run hspec tests
  Hspec.hspec $ do
    AccountProgramSpec.spec
    ConcurrentEffectsSpec.spec
  
  -- Run tasty tests if needed
  Tasty.defaultMain $ Tasty.testGroup "Time Bandits Tests"
    [ SimpleNetworkConfig.tests
    , TimelineScenarioTest.tests
    , ControllerTest.tests
    , TimelineDescriptorTest.timelineDescriptorTests
    , DistributedLogTest.tests
    ] 