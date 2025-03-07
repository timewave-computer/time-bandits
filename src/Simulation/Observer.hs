{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module: Simulation.Observer
Description: Observer mechanism for Time Bandits simulations

This module provides a way to monitor and influence running simulations.
Observers can:

1. Monitor effects, facts, and events as they occur
2. Inject faults to test resilience
3. Check invariants to validate correctness
4. Generate reports and visualizations

Observers are a key part of the simulation system, allowing for
in-depth analysis and validation of Time Bandits behavior.
-}
module Simulation.Observer
  ( -- * Observer Types
    Observer(..)
  , ObserverConfig(..)
  , InvariantCheck(..)
  , InvariantResult(..)
  , InvariantViolation(..)
  , FaultInjection(..)
  , ObserverEvent(..)
  
  -- * Creating Observers
  , createObserver
  , createLoggingObserver
  , createFaultInjectionObserver
  , createInvariantCheckingObserver
  , composeObservers
  
  -- * Observer Actions
  , notifyEffect
  , notifyFact
  , notifyEvent
  , checkAllInvariants
  , injectFault
  , generateReport
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, modifyTVar', atomically)
import Control.Exception (catch, throwIO, Exception)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)

import Core.Common (Hash, EntityHash, TimelineHash)
import Core.Effect (Effect, EffectID)
import Core.Log (LogEntry, LogEntryType(..))
import Core.Types (ActorId, ProgramId)
import Simulation.Scenario (Scenario)

-- | Observer configuration
data ObserverConfig = ObserverConfig
  { observerName :: Text
  , observerLogPath :: FilePath
  , observerVerbose :: Bool
  , observerInvariants :: [InvariantCheck]
  , observerFaults :: [FaultInjection]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A specification for an invariant that should be checked
data InvariantCheck = InvariantCheck
  { invariantName :: Text
  , invariantDescription :: Text
  , invariantCheckFn :: IO InvariantResult
  }

instance Show InvariantCheck where
  show check = T.unpack $ invariantName check <> ": " <> invariantDescription check

instance Eq InvariantCheck where
  a == b = invariantName a == invariantName b

-- | The result of checking an invariant
data InvariantResult
  = InvariantPassed
  | InvariantFailed InvariantViolation
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Details about an invariant violation
data InvariantViolation = InvariantViolation
  { violationInvariant :: Text
  , violationMessage :: Text
  , violationDetails :: Map Text Text
  , violationTimestamp :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A specification for a fault to inject
data FaultInjection = FaultInjection
  { faultName :: Text
  , faultDescription :: Text
  , faultTrigger :: Text  -- ^ When to inject the fault (e.g., "after:deposit", "random:0.1")
  , faultAction :: IO ()  -- ^ The action to take when injecting the fault
  }

instance Show FaultInjection where
  show fault = T.unpack $ faultName fault <> ": " <> faultDescription fault

instance Eq FaultInjection where
  a == b = faultName a == faultName b

-- | Events that an observer can monitor
data ObserverEvent
  = EffectEvent Effect
  | FactEvent Fact
  | ActorEvent ActorEvent
  | SystemEvent Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A simplified fact type for observer purposes
data Fact = Fact
  { factId :: Hash
  , factType :: Text
  , factContent :: ByteString
  , factTimeline :: Maybe TimelineHash
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Actor events that can be observed
data ActorEvent = ActorEvent
  { actorEventType :: Text
  , actorEventActor :: ActorId
  , actorEventDetails :: Map Text Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | The observer type
data Observer = Observer
  { observerConfig :: ObserverConfig
  , observerOnEffect :: Effect -> IO ()
  , observerOnFact :: Fact -> IO ()
  , observerOnEvent :: ObserverEvent -> IO ()
  , observerCheckInvariants :: IO [InvariantResult]
  , observerInjectFault :: Text -> IO ()
  , observerGenerateReport :: IO Text
  }

-- | Create a new observer with default handlers
createObserver :: ObserverConfig -> IO Observer
createObserver config = do
  -- Create an IORef to store observed events
  eventsRef <- newIORef []
  
  -- Return the observer
  return Observer
    { observerConfig = config
    , observerOnEffect = \effect -> do
        when (observerVerbose config) $
          putStrLn $ "Observer observed effect: " ++ show effect
        modifyIORef eventsRef ((EffectEvent effect):)
    , observerOnFact = \fact -> do
        when (observerVerbose config) $
          putStrLn $ "Observer observed fact: " ++ show fact
        modifyIORef eventsRef ((FactEvent fact):)
    , observerOnEvent = \event -> do
        when (observerVerbose config) $
          putStrLn $ "Observer observed event: " ++ show event
        modifyIORef eventsRef (event:)
    , observerCheckInvariants = do
        -- Run all invariant checks
        forM (observerInvariants config) $ \check -> do
          when (observerVerbose config) $
            putStrLn $ "Checking invariant: " ++ T.unpack (invariantName check)
          result <- invariantCheckFn check
          case result of
            InvariantPassed -> 
              when (observerVerbose config) $
                putStrLn $ "Invariant passed: " ++ T.unpack (invariantName check)
            InvariantFailed violation ->
              putStrLn $ "Invariant failed: " ++ T.unpack (invariantName check) ++ 
                        " - " ++ T.unpack (violationMessage violation)
          return result
    , observerInjectFault = \faultName -> do
        -- Find the fault with the given name
        let matchingFaults = filter (\f -> faultName fault == faultName) (observerFaults config)
        case matchingFaults of
          [] -> putStrLn $ "No fault found with name: " ++ T.unpack faultName
          (fault:_) -> do
            when (observerVerbose config) $
              putStrLn $ "Injecting fault: " ++ T.unpack (faultName fault)
            faultAction fault
    , observerGenerateReport = do
        events <- readIORef eventsRef
        let effectCount = length [e | EffectEvent _ <- events]
        let factCount = length [e | FactEvent _ <- events]
        let actorEventCount = length [e | ActorEvent _ <- events]
        let systemEventCount = length [e | SystemEvent _ <- events]
        
        return $ T.unlines
          [ "Observer Report: " <> observerName config
          , "======================"
          , "Effects observed: " <> T.pack (show effectCount)
          , "Facts observed: " <> T.pack (show factCount)
          , "Actor events: " <> T.pack (show actorEventCount)
          , "System events: " <> T.pack (show systemEventCount)
          ]
    }

-- | Create an observer that just logs events
createLoggingObserver :: FilePath -> Bool -> IO Observer
createLoggingObserver logPath verbose = do
  createObserver ObserverConfig
    { observerName = "LoggingObserver"
    , observerLogPath = logPath
    , observerVerbose = verbose
    , observerInvariants = []
    , observerFaults = []
    }

-- | Create an observer that injects faults
createFaultInjectionObserver :: [FaultInjection] -> Bool -> IO Observer
createFaultInjectionObserver faults verbose = do
  createObserver ObserverConfig
    { observerName = "FaultInjectionObserver"
    , observerLogPath = "/tmp/fault-observer.log"
    , observerVerbose = verbose
    , observerInvariants = []
    , observerFaults = faults
    }

-- | Create an observer that checks invariants
createInvariantCheckingObserver :: [InvariantCheck] -> Bool -> IO Observer
createInvariantCheckingObserver invariants verbose = do
  createObserver ObserverConfig
    { observerName = "InvariantCheckingObserver"
    , observerLogPath = "/tmp/invariant-observer.log"
    , observerVerbose = verbose
    , observerInvariants = invariants
    , observerFaults = []
    }

-- | Compose multiple observers into one
composeObservers :: [Observer] -> IO Observer
composeObservers observers = do
  let config = ObserverConfig
        { observerName = "CompositeObserver"
        , observerLogPath = "/tmp/composite-observer.log"
        , observerVerbose = any (observerVerbose . observerConfig) observers
        , observerInvariants = concatMap (observerInvariants . observerConfig) observers
        , observerFaults = concatMap (observerFaults . observerConfig) observers
        }
  
  return Observer
    { observerConfig = config
    , observerOnEffect = \effect -> 
        forM_ observers $ \obs -> observerOnEffect obs effect
    , observerOnFact = \fact ->
        forM_ observers $ \obs -> observerOnFact obs fact
    , observerOnEvent = \event ->
        forM_ observers $ \obs -> observerOnEvent obs event
    , observerCheckInvariants = do
        results <- forM observers $ \obs -> observerCheckInvariants obs
        return (concat results)
    , observerInjectFault = \faultName ->
        forM_ observers $ \obs -> observerInjectFault obs faultName
    , observerGenerateReport = do
        reports <- forM observers $ \obs -> observerGenerateReport obs
        return $ T.unlines reports
    }

-- | Notify an observer about an effect
notifyEffect :: Observer -> Effect -> IO ()
notifyEffect observer = observerOnEffect observer

-- | Notify an observer about a fact
notifyFact :: Observer -> Fact -> IO ()
notifyFact observer = observerOnFact observer

-- | Notify an observer about an event
notifyEvent :: Observer -> ObserverEvent -> IO ()
notifyEvent observer = observerOnEvent observer

-- | Check all invariants
checkAllInvariants :: Observer -> IO [InvariantResult]
checkAllInvariants observer = observerCheckInvariants observer

-- | Inject a specific fault
injectFault :: Observer -> Text -> IO ()
injectFault observer = observerInjectFault observer

-- | Generate a report from the observer
generateReport :: Observer -> IO Text
generateReport observer = observerGenerateReport observer 