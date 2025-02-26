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
import TimeBandits.Core
import TimeBandits.Effects (AppEffects, interpretAppEffects)
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
  -- Initialize Lamport clock at 0
  timeRef <- newIORef (LamportTime 0)

  -- Initialize empty logs
  resourceLogRef <- newIORef [] -- Empty resource log
  actorLogRef <- newIORef (Log []) -- Empty actor log (MapOfTime)
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

  -- Run the main program with all effects
  result <- interpretAppEffects timeRef resourceLogRef storeRef subsRef mainProgram
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (logs, _) -> mapM_ putStrLn logs

-- | Main program logic
mainProgram :: (Members (AppEffects r) r, Member (Output String) r) => Sem r ()
mainProgram = do
  output "Starting Time Bandits application..."

-- Add your program logic here
