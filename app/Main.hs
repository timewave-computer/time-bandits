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
import TimeBandits.Core (TimelineHash, ActorHash, computePubKeyHash)
import TimeBandits.Effects (
  InterpreterConfig (..),
  TraceConfig (..),
  defaultConfig,
  silentConfig,
  verboseConfig,
  interpretKeyManagement,
  KeyManagement
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
  TransientDatastore (..),
  PrivKey(..)
 )
import Network.Socket (SockAddr(..), tupleToHostAddress)
import Prelude hiding (newIORef)
import Data.Time.Clock (getCurrentTime)
import qualified Data.Map as Map
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.IO (appendFile)
import TimeBandits.Utils (generateSecureEd25519KeyPair)
import Relude (atomicModifyIORef')
import Data.ByteString.Char8 qualified as BS

-- | Main entry point
-- Initializes the Time Bandits application with UTF-8 encoding support,
-- configures the system based on command line arguments, and runs the
-- main program with the appropriate effect interpreters.
main :: IO ()
main = Utf8.withUtf8 $ do
  -- Parse command line arguments to determine configuration
  args <- Env.getArgs
  let config = parseConfig args

  -- Initialize Lamport clock at 0 for logical time tracking
  timeRef <- newIORef (LamportTime 0)

  -- Initialize empty logs for storing event histories
  _resourceLogRef <- newIORef [] -- Empty resource log
  _actorLogRef <- newIORef (Log []) -- Empty actor log
  _timelineLogRef <- newIORef (Log []) -- Empty timeline log

  -- Initialize empty transient datastore for distributed storage
  let initialStore =
        TransientDatastore
          { tdReplicationFactor = 3
          , tdTimeBandits = [] -- Will be populated as nodes join
          }
  storeRef <- newIORef initialStore

  -- Initialize empty subscriptions for timeline watching
  subsRef <- newIORef []
  
  -- Initialize empty P2P node list for network communication
  p2pNodesRef <- newIORef []
  
  -- Initialize empty actor type registry
  actorTypeRegistryRef <- newIORef Map.empty
  
  -- Initialize empty key store for public keys
  keyStoreRef <- newIORef Map.empty

  -- Run the main program with configured effects
  -- Handle any errors that might occur during execution
  result <- Main.interpretWithConfig config timeRef _resourceLogRef storeRef subsRef p2pNodesRef keyStoreRef actorTypeRegistryRef mainProgram
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (logs, _) -> mapM_ putStrLn logs

-- | Parse command line arguments into an interpreter configuration
-- Converts user command line options into the appropriate configuration
-- for tracing and other system behaviors.
parseConfig :: [String] -> InterpreterConfig
parseConfig args
  | "--verbose" `elem` args = verboseConfig
  | "--silent" `elem` args = silentConfig
  | otherwise = defaultConfig

-- | Main program logic
-- Core application logic that runs with the full effect stack.
-- This initializes and coordinates the various subsystems.
mainProgram :: 
  ( Member (PS.State [Network.P2PNode]) r
  , Member (PS.State ResourceLog) r
  , Member Trace r
  , Member (Output String) r
  , Member (Error AppError) r
  , Member TimeBandits.Effects.KeyManagement r
  , Member Network.P2PNetwork r
  , Member (Embed IO) r
  ) => 
  Sem r ()
mainProgram = do
  Trace.trace "Initializing Time Bandits core systems..."
  output "Time Bandits application initialized successfully!"

-- | Interpreter configuration
-- Configures and runs the effect interpreters for the main program.
-- This function wires together all the various effect handlers into
-- a cohesive system that can process the application logic.
interpretWithConfig ::
    InterpreterConfig ->
    IORef LamportTime ->
    IORef ResourceLog ->
    IORef TransientDatastore ->
    IORef [TimelineHash] ->
    IORef [Network.P2PNode] ->
    IORef (Map.Map ActorHash PubKey) ->
    IORef (Map.Map ActorHash ActorType) ->
    Sem
      '[ Network.P2PNetwork
       , KeyManagement
       , PS.State [Network.P2PNode]
       , PS.State ResourceLog
       , Trace
       , Output String
       , Error AppError
       , Embed IO
       ] a
    -> IO (Either AppError ([String], a))
interpretWithConfig config _timeRef _resourceLogRef _storeRef _subsRef p2pNodesRef _keyStoreRef _actorTypeRegistryRef program = do
    -- Create a local actor with a proper identity
    -- In a real application, we would use generateSecureEd25519KeyPair
    -- but for simplicity in this context, we'll create deterministic keys
    _now <- getCurrentTime
    
    -- Create deterministic keys for the local node
    let privKeyData = BS.pack "local-node-private-key-secure"
        pubKeyData = BS.pack "local-node-public-key-secure" 
        privKey = PrivKey privKeyData
        pubKey = PubKey pubKeyData
        actorId = computePubKeyHash pubKey
        localActor = Actor actorId TimeTraveler
        
        -- Modify the default P2P configuration with a valid bind address
        p2pConfig = Network.defaultP2PConfig {
          Network.pcBindAddress = SockAddrInet 8888 (tupleToHostAddress (127, 0, 0, 1))
          -- Other defaults are kept from defaultP2PConfig
        }
    
    -- Register the actor and its public key in our stores
    atomicModifyIORef' _keyStoreRef $ \keyStore -> 
        (Map.insert actorId pubKey keyStore, ())
    atomicModifyIORef' _actorTypeRegistryRef $ \registry -> 
        (Map.insert actorId TimeTraveler registry, ())
    
    putStrLn "Starting effect interpreter chain..."
    
    -- Choose the appropriate trace interpreter based on configuration
    let traceInterpreter = case traceConfig config of
            NoTracing -> ignoreTrace
            SimpleTracing -> traceToStdout
            VerboseTracing -> myTraceVerbose
    
    -- Run the program with the configured interpreters
    -- Each interpreter in this chain handles a specific effect,
    -- transforming the effect operations into concrete implementations
    result <-
        runM                                    -- Run the final IO
            . runError                          -- Handle errors
            . runOutputList                     -- Collect output messages
            . traceInterpreter                  -- Log trace messages
            . PS.runStateIORef _resourceLogRef  -- Manage resource state
            . PS.runStateIORef p2pNodesRef      -- Manage P2P network state
            . interpretKeyManagement _keyStoreRef _actorTypeRegistryRef      -- Handle key management and actor registry
            . Network.interpretP2PNetwork p2pConfig localActor pubKey   -- Handle P2P networking
            $ program

    -- Process the result and return it with any collected logs
    pure $ case result of
        Left err -> Left err
        Right (logs, value) -> Right (logs, value)

-- | Custom verbose trace interpreter that adds timestamps and context
-- Enhances the standard trace implementation with timestamps and additional
-- context, useful for debugging and monitoring complex distributed operations.
myTraceVerbose :: (Member (Embed IO) r) => Sem (Trace ': r) a -> Sem r a
myTraceVerbose = interpret \case
    Trace message -> do
        timestamp <- embed @IO getCurrentTime
        embed @IO $ putStrLn $ "[VERBOSE][" ++ show timestamp ++ "] " ++ message
