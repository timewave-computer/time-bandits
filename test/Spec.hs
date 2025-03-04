import qualified Test.Tasty as Tasty
import qualified TimelineScenarioTest
import qualified ControllerTest
import qualified TimelineDescriptorTest

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "Time Bandits Tests"
  [ TimelineScenarioTest.tests
  , ControllerTest.tests
  , TimelineDescriptorTest.timelineDescriptorTests
  ] 