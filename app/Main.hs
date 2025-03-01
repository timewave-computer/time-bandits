{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.IORef (IORef, newIORef)
import Main.Utf8 qualified as Utf8
import Polysemy (Member, Members, Sem, runM)
import Polysemy.Error (Error, runError)
import Polysemy.Output (Output, output, runOutputList)
import Polysemy.Trace (Trace)
import Polysemy.Trace qualified as Trace (trace)
import System.Environment qualified as Env
import TimeBandits.Core
import TimeBandits.Effects (
  AppEffects,
  InterpreterConfig (..),
  TraceConfig (..),
  defaultConfig,
  interpretAppEffects,
  interpretWithConfig,
  silentConfig,
  verboseConfig,
 )
import TimeBandits.Types (
  Actor (..),
  ActorType (TimeTraveler),
  AppError,
  LamportTime (..),
  Log (..),
  ResourceLog,
  TimelineLog,
  TransientDatastore (..),
 )
import Prelude hiding (newIORef)

-- | Main entry point
main :: IO ()
main = Utf8.withUtf8 $ do
  -- Parse command line arguments
  args <- Env.getArgs
  let config = parseConfig args

  -- Initialize Lamport clock at 0
  timeRef <- newIORef (LamportTime 0)

  -- Initialize empty logs
  resourceLogRef <- newIORef [] -- Empty resource log
  actorLogRef <- newIORef (Log []) -- Empty actor log
  timelineLogRef <- newIORef (Log []) -- Empty timeline log

  -- Initialize empty transient datastore
  let initialStore =
        TransientDatastore
          { tdReplicationFactor = 3
          , tdTimeBandits = [] -- Will be populated as nodes join
          }
  storeRef <- newIORef initialStore

  -- Initialize empty subscriptions
  subsRef <- newIORef []

  -- Run the main program with configured effects
  result <- interpretWithConfig config timeRef resourceLogRef storeRef subsRef mainProgram
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (logs, _) -> mapM_ putStrLn logs

-- | Parse command line arguments into an interpreter configuration
parseConfig :: [String] -> InterpreterConfig
parseConfig args
  | "--verbose" `elem` args = verboseConfig
  | "--silent" `elem` args = silentConfig
  | otherwise = defaultConfig

-- | Main program logic
mainProgram :: (Members (AppEffects r) r, Member (Output String) r, Member Trace r) => Sem r ()
mainProgram = do
  output "Starting Time Bandits application..."

  -- Add trace statements to demonstrate tracing system
  Trace.trace "Initializing Time Bandits core systems..."
  Trace.trace "Setting up timeline management..."
  Trace.trace "Configuring resource tracking..."
  Trace.trace "Preparing actor registration..."
  Trace.trace "Time Bandits system initialization complete."

  -- Actual program logic would go here
  output "Time Bandits ready."

-- Add your program logic here
