module Main (main) where

import qualified Test.Tasty as Tasty

-- Unit tests
import qualified Test.Unit.Core.TimelineDescriptorTest as TimelineDescriptorTest
import qualified Test.Unit.Execution.ControllerTest as ControllerTest
import qualified Test.Unit.Execution.DistributedLogTest as DistributedLogTest
import qualified Test.Unit.Proofs.ZKProofTest as ZKProofTest
import qualified Core.TELTest as TELTest
import qualified Core.TEL.InterpreterTest as TELInterpreterTest
import qualified Core.TEL.ContentAddressableTest as TELContentAddressableTest

-- Integration tests
import qualified Test.Integration.ProgramExecution.TimelineScenarioTest as TimelineScenarioTest
import qualified TestModeScenarioTest as TestModeScenarioTest

-- Fixtures
import qualified Test.Fixtures.SimpleNetworkConfig as SimpleNetworkConfig

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "Time Bandits Tests"
  [ Tasty.testGroup "Unit Tests" 
    [ Tasty.testGroup "Core"
      [ TimelineDescriptorTest.timelineDescriptorTests
      , Tasty.testGroup "TEL" 
        [ Tasty.testGroup "Language" [TELTest.testTEL]
        , Tasty.testGroup "Tasty-Based Tests" [TELTest.tests]  
        , Tasty.testGroup "Interpreter" [TELInterpreterTest.testTELInterpreter]
        , Tasty.testGroup "Content Addressable" [TELContentAddressableTest.testTELContentAddressable]
        ]
      ]
    , Tasty.testGroup "Execution"
      [ ControllerTest.tests
      , DistributedLogTest.tests
      ]
    , Tasty.testGroup "Proofs"
      [ ZKProofTest.tests
      ]
    ]
  , Tasty.testGroup "Integration Tests"
    [ Tasty.testGroup "Program Execution"
      [ TimelineScenarioTest.tests
      , TestModeScenarioTest.tests
      ]
    ]
  , Tasty.testGroup "Fixtures"
    [ SimpleNetworkConfig.tests
    ]
  ] 