import qualified Test.Tasty as Tasty
import qualified SimpleNetworkConfig
import qualified TimelineScenarioTest
import qualified ControllerTest
import qualified TimelineDescriptorTest
import qualified DistributedLogTest

{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "Time Bandits Tests"
  [ SimpleNetworkConfig.tests
  , TimelineScenarioTest.tests
  , ControllerTest.tests
  , TimelineDescriptorTest.timelineDescriptorTests
  , DistributedLogTest.tests
  ] 