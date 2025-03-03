module Main where

import qualified Test.Tasty as Tasty
import qualified SimpleNetworkConfig
import qualified TimelineScenarioTest

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "Time Bandits Tests" 
  [ SimpleNetworkConfig.tests
  , TimelineScenarioTest.tests
  ] 