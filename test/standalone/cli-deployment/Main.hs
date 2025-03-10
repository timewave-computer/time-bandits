module Main where

import CLI.Deployment

main :: IO ()
main = do
  let config = ScenarioConfig
        { scenarioName = "Test Scenario"
        , scenarioMode = Interactive
        , scenarioActors = ["Actor1", "Actor2"]
        , scenarioPrograms = ["Prog1", "Prog2"]
        , scenarioLogPath = "/tmp/log"
        }
      
      scenario = Scenario
        { scenarioConfig = config
        , scenarioDeployment = Just "Sample Deployment"
        }
  
  putStrLn $ "Scenario Config: " ++ show config
  putStrLn $ "Formatted Scenario: " ++ displayScenario scenario 