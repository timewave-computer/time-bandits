module Main (main) where

import System.Environment (getArgs)
import System.Process (callCommand)
import System.Exit (exitSuccess, exitFailure)
import Control.Monad (when)
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  args <- getArgs
  let testToRun = if null args then "all" else head args
  
  -- Ensure the bin directory exists
  createDirectoryIfMissing True "test/bin"
  
  putStrLn "======================================================"
  putStrLn "Running Time Bandits standalone tests"
  putStrLn "======================================================"
  
  case testToRun of
    "all" -> do
      runSchemaTest
      runTELTest
      runFactObservationTest
      runNetworkTest
      runLogTest
      runTimelineTest
      runConsensusTest
    "schema" -> runSchemaTest
    "tel" -> runTELTest
    "fact" -> runFactObservationTest
    "network" -> runNetworkTest
    "log" -> runLogTest
    "timeline" -> runTimelineTest
    "consensus" -> runConsensusTest
    _ -> do
      putStrLn $ "Unknown test: " ++ testToRun
      putStrLn "Available tests: all, schema, tel, fact, network, log, timeline, consensus"
      exitFailure

-- Run the Schema test
runSchemaTest :: IO ()
runSchemaTest = do
  putStrLn "\n------ Running Schema Evolution Tests ------"
  result <- system "ghc -outputdir test/bin -o test/bin/mini_schema_test test/MiniSchemaTest.hs && test/bin/mini_schema_test"
  when (result /= 0) exitFailure
  
-- Run the TEL test
runTELTest :: IO ()
runTELTest = do
  putStrLn "\n------ Running TEL Parser Tests ------"
  result <- system "ghc -outputdir test/bin -o test/bin/mini_tel_test test/MiniTELTest.hs && test/bin/mini_tel_test"
  when (result /= 0) exitFailure

-- Run the FactObservation test
runFactObservationTest :: IO ()
runFactObservationTest = do
  putStrLn "\n------ Running FactObservation Tests ------"
  result <- system "ghc -outputdir test/bin -o test/bin/mini_fact_test test/MiniFactObservationTest.hs && test/bin/mini_fact_test"
  when (result /= 0) exitFailure

-- Run the Network test
runNetworkTest :: IO ()
runNetworkTest = do
  putStrLn "\n------ Running Network Tests ------"
  result <- system "ghc -outputdir test/bin -o test/bin/mini_network_test test/MiniNetworkTest.hs && test/bin/mini_network_test"
  when (result /= 0) exitFailure

-- Run the Log test
runLogTest :: IO ()
runLogTest = do
  putStrLn "\n------ Running Log Tests ------"
  result <- system "ghc -outputdir test/bin -o test/bin/mini_log_test test/MiniLogTest.hs && test/bin/mini_log_test"
  when (result /= 0) exitFailure

-- Run the Timeline test
runTimelineTest :: IO ()
runTimelineTest = do
  putStrLn "\n------ Running Timeline Tests ------"
  result <- system "ghc -outputdir test/bin -o test/bin/mini_timeline_test test/MiniTimelineTest.hs && test/bin/mini_timeline_test"
  when (result /= 0) exitFailure

-- Run the Consensus test
runConsensusTest :: IO ()
runConsensusTest = do
  putStrLn "\n------ Running Consensus Tests ------"
  result <- system "ghc -outputdir test/bin -o test/bin/mini_consensus_test test/MiniConsensusTest.hs && test/bin/mini_consensus_test"
  when (result /= 0) exitFailure

-- Helper to run shell commands and capture exit code
system :: String -> IO Int
system cmd = do
  putStrLn $ "Executing: " ++ cmd
  callCommand cmd
  -- Since callCommand throws an exception on failure, if we get here, it succeeded
  return 0 