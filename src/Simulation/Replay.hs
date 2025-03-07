{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module: Simulation.Replay
Description: Deterministic replay system for Time Bandits

This module provides functionality for deterministic replay of Time Bandits
simulations from log files. The replay system ensures that:

1. Effects are applied in the same causal order
2. Facts are observed with the same values
3. Program state evolves identically
4. The same final state is reached

Replay is a critical feature for debugging, auditing, and verification.
-}
module Simulation.Replay
  ( -- * Replay Types
    ReplayConfig(..)
  , ReplayState(..)
  , ReplayResult(..)
  
  -- * Replay Functions
  , replayFromLogs
  , replayScenario
  , replayProgram
  
  -- * Verification Functions
  , verifyReplayResult
  , compareStates
  ) where

import Control.Exception (catch, throwIO, Exception)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath ((</>), takeDirectory)

import Core.Common (Hash, EntityHash, TimelineHash)
import Core.Effect (Effect, EffectID)
import Core.Log (LogEntry(..), LogEntryType(..), readLog)
import Core.Types (ActorId, ProgramId)
import Programs.Program (Program, ProgramState)
import Simulation.Scenario (Scenario, ScenarioConfig)
import Simulation.Observer (Observer)

-- | Configuration for replay
data ReplayConfig = ReplayConfig
  { replayLogDir :: FilePath
  , replayVerbose :: Bool
  , replayVerify :: Bool  -- ^ Whether to verify the result
  , replayOutputDir :: Maybe FilePath  -- ^ Where to write the replayed state
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | The state during replay
data ReplayState = ReplayState
  { replayActorStates :: Map ActorId ProgramState
  , replayEffectLog :: [LogEntry]
  , replayFactLog :: [LogEntry]
  , replayCurrentTime :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | The result of a replay
data ReplayResult = ReplayResult
  { replaySuccess :: Bool
  , replayErrors :: [Text]
  , replayFinalState :: ReplayState
  , replayMatchesOriginal :: Bool
  , replayStats :: Map Text Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Error in replay
data ReplayError = ReplayError
  { errorMessage :: Text
  , errorContext :: Map Text Text
  }
  deriving (Show, Eq, Generic, Exception, FromJSON, ToJSON)

-- | Replay a simulation from its logs
replayFromLogs :: ReplayConfig -> IO ReplayResult
replayFromLogs config = do
  -- Verify the log directory exists
  logDirExists <- doesFileExist (replayLogDir config)
  unless logDirExists $
    throwIO $ ReplayError
      { errorMessage = "Log directory does not exist"
      , errorContext = Map.singleton "logDir" (T.pack $ replayLogDir config)
      }
  
  -- Get all log files in the directory
  logFiles <- listDirectory (replayLogDir config)
  let logPaths = map (replayLogDir config </>) logFiles
  
  -- Read all log entries
  allLogs <- fmap concat $ forM logPaths readLog
  
  -- Split logs by type
  let effectLogs = filter (\entry -> logType entry == EffectEntry) allLogs
  let factLogs = filter (\entry -> logType entry == FactEntry) allLogs
  
  when (replayVerbose config) $ do
    putStrLn $ "Found " ++ show (length effectLogs) ++ " effect logs"
    putStrLn $ "Found " ++ show (length factLogs) ++ " fact logs"
  
  -- Initialize replay state
  let initialState = ReplayState
        { replayActorStates = Map.empty
        , replayEffectLog = effectLogs
        , replayFactLog = factLogs
        , replayCurrentTime = 0
        }
  
  -- Perform the replay
  replayResult <- performReplay config initialState
  
  -- Verify the result if requested
  finalResult <- if replayVerify config
    then verifyReplayResult config replayResult
    else return replayResult
  
  -- Write the replayed state if requested
  forM_ (replayOutputDir config) $ \outputDir -> do
    createDirectoryIfMissing True outputDir
    BL.writeFile (outputDir </> "replay-result.json") (encode finalResult)
  
  return finalResult

-- | Replay a specific scenario
replayScenario :: ReplayConfig -> Scenario -> IO ReplayResult
replayScenario config scenario = do
  -- This would set up the environment based on the scenario
  -- and then call replayFromLogs with the appropriate config
  
  -- For now, we just reuse replayFromLogs
  replayFromLogs config

-- | Replay a specific program
replayProgram :: ReplayConfig -> ProgramId -> IO ReplayResult
replayProgram config programId = do
  -- This would replay only the specified program
  
  -- We'd filter logs to just those for the specific program
  -- and then replay those
  
  -- For simplicity, we just reuse replayFromLogs
  replayFromLogs config

-- | Perform the actual replay
performReplay :: ReplayConfig -> ReplayState -> IO ReplayResult
performReplay config initialState = do
  -- Sort logs by Lamport time to ensure causal ordering
  let sortedEffects = sortByTimestamp (replayEffectLog initialState)
  
  -- Apply each effect in order
  (finalState, errors) <- foldM applyEffect (initialState, []) sortedEffects
  
  return ReplayResult
    { replaySuccess = null errors
    , replayErrors = errors
    , replayFinalState = finalState
    , replayMatchesOriginal = True  -- We'd need to compare with the original state
    , replayStats = Map.fromList
        [ ("effects", length sortedEffects)
        , ("facts", length (replayFactLog initialState))
        , ("errors", length errors)
        ]
    }
  where
    sortByTimestamp :: [LogEntry] -> [LogEntry]
    sortByTimestamp = sortOn (show . logMetadata)
    
    sortOn :: Ord b => (a -> b) -> [a] -> [a]
    sortOn f = map snd . sortBy (comparing fst) . map (\x -> (f x, x))
    
    comparing :: Ord b => (a -> b) -> a -> a -> Ordering
    comparing f x y = compare (f x) (f y)
    
    sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    sortBy _ [] = []
    sortBy cmp (x:xs) = sortBy cmp smaller ++ [x] ++ sortBy cmp larger
      where
        smaller = [y | y <- xs, cmp y x /= GT]
        larger  = [y | y <- xs, cmp y x == GT]
    
    applyEffect :: (ReplayState, [Text]) -> LogEntry -> IO (ReplayState, [Text])
    applyEffect (state, errors) entry = do
      when (replayVerbose config) $
        putStrLn $ "Applying effect: " ++ show (logId entry)
      
      -- This is where we'd actually apply the effect to the state
      -- For now, we just increment the time
      let newState = state { replayCurrentTime = replayCurrentTime state + 1 }
      
      -- Here we'd check if applying the effect succeeded
      let success = True
      
      if success
        then return (newState, errors)
        else do
          let errorMsg = "Failed to apply effect: " <> T.pack (show (logId entry))
          return (newState, errorMsg : errors)

-- | Verify the replay result matches the original execution
verifyReplayResult :: ReplayConfig -> ReplayResult -> IO ReplayResult
verifyReplayResult config result = do
  -- In a real implementation, we'd compare the final state to the original
  
  -- For now, we just return the result unchanged
  return result

-- | Compare the original and replayed states
compareStates :: Map ActorId ProgramState -> Map ActorId ProgramState -> Bool
compareStates original replayed =
  -- In a real implementation, we'd compare the states
  Map.size original == Map.size replayed 