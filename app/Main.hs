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

import Data.IORef (newIORef)
import Main.Utf8 qualified as Utf8
import Polysemy (Member, Sem, runM, Embed, interpret, embed)
import Polysemy.Error (Error, runError)
import Polysemy.Output (Output, output, runOutputList)
import Polysemy.State qualified as PS
import Polysemy.Trace (Trace, ignoreTrace, traceToStdout, Trace(Trace))
import Polysemy.Trace qualified as Trace (trace)
import System.Environment qualified as Env
import TimeBandits.Core (EntityHash(..), Hash(..), TimelineHash)
import TimeBandits.Effects (
  InterpreterConfig (..),
  TraceConfig (..),
  defaultConfig,
  silentConfig,
  verboseConfig,
 )
import TimeBandits.Network qualified as Network
  ( P2PNetwork
  , P2PNode
  , P2PConfig(..)
  , defaultP2PConfig
  , interpretP2PNetwork
  )
import TimeBandits.Types (
  Actor (..),
  ActorType (TimeTraveler),
  AppError,
  LamportTime (..),
  Log (..),
  PubKey(..), 
  ResourceLog,
  TransientDatastore (..)
 )
import Network.Socket (SockAddr(..), tupleToHostAddress)
import Prelude hiding (newIORef)
import Data.Time.Clock (getCurrentTime)

-- | Main entry point
main :: IO ()
main = Utf8.withUtf8 $ do
  -- Parse command line arguments
  args <- Env.getArgs
  let config = parseConfig args

  -- Initialize Lamport clock at 0
  timeRef <- newIORef (LamportTime 0)

  -- Initialize empty logs
  _resourceLogRef <- newIORef [] -- Empty resource log
  _actorLogRef <- newIORef (Log []) -- Empty actor log
  _timelineLogRef <- newIORef (Log []) -- Empty timeline log

  -- Initialize empty transient datastore
  let initialStore =
        TransientDatastore
          { tdReplicationFactor = 3
          , tdTimeBandits = [] -- Will be populated as nodes join
          }
  storeRef <- newIORef initialStore

  -- Initialize empty subscriptions
  subsRef <- newIORef []
  
  -- Initialize empty P2P node list
  p2pNodesRef <- newIORef []

  -- Run the main program with configured effects
  result <- Main.interpretWithConfig config timeRef _resourceLogRef storeRef subsRef p2pNodesRef mainProgram
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
mainProgram :: 
  ( Member Network.P2PNetwork r
  , Member (PS.State [Network.P2PNode]) r
  , Member (PS.State ResourceLog) r
  , Member Trace r
  , Member (Output String) r
  , Member (Error AppError) r
  , Member (Embed IO) r
  ) => 
  Sem r ()
mainProgram = do
  Trace.trace "Initializing Time Bandits core systems..."
  output "Time Bandits application initialized successfully!"

-- | Interpreter configuration
interpretWithConfig ::
    InterpreterConfig ->
    IORef LamportTime ->
    IORef ResourceLog ->
    IORef TransientDatastore ->
    IORef [TimelineHash] ->
    IORef [Network.P2PNode] ->
    Sem
      '[ Network.P2PNetwork
       , PS.State [Network.P2PNode]
       , PS.State ResourceLog
       , Trace
       , Output String
       , Error AppError
       , Embed IO
       ] a
    -> IO (Either AppError ([String], a))
interpretWithConfig config _timeRef _resourceLogRef _storeRef _subsRef p2pNodesRef program = do
    -- Create a dummy local actor for P2P networking
    _now <- getCurrentTime
    let dummyActor = Actor (EntityHash (Hash "local-node")) TimeTraveler
        dummyPubKey = PubKey "local-node-key"
        -- Modify the default P2P configuration with a valid bind address
        p2pConfig = Network.defaultP2PConfig {
          Network.pcBindAddress = SockAddrInet 8888 (tupleToHostAddress (127, 0, 0, 1))
          -- Other defaults are kept from defaultP2PConfig
        }
    
    putStrLn "Starting effect interpreter chain..."
    
    -- Choose the appropriate trace interpreter
    let traceInterpreter = case traceConfig config of
            NoTracing -> ignoreTrace
            SimpleTracing -> traceToStdout
            VerboseTracing -> myTraceVerbose
    
    -- Run the program with the configured interpreters
    result <-
        runM
            . runError
            . runOutputList
            . traceInterpreter
            . PS.runStateIORef _resourceLogRef
            . PS.runStateIORef p2pNodesRef
            . Network.interpretP2PNetwork p2pConfig dummyActor dummyPubKey
            $ program

    -- Process the result
    pure $ case result of
        Left err -> Left err
        Right (logs, value) -> Right (logs, value)

-- | Custom verbose trace interpreter that adds timestamps and context
myTraceVerbose :: (Member (Embed IO) r) => Sem (Trace ': r) a -> Sem r a
myTraceVerbose = interpret \case
    Trace message -> do
        timestamp <- embed @IO getCurrentTime
        embed @IO $ putStrLn $ "[VERBOSE][" ++ show timestamp ++ "] " ++ message
