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
import TimeBandits.Controller (
  Controller,
  ControllerConfig(..),
  SimulationMode(..),
  initController
 )
import TimeBandits.Actor (
  ActorSpec(..),
  ActorRole(..),
  ActorCapability(..),
  deployActor
 )
import TimeBandits.Scenario (
  Scenario,
  ScenarioConfig(..),
  loadScenario,
  runScenario
 )
import TimeBandits.Deployment (
  Deployment,
  DeploymentConfig(..),
  createDeployment,
  startDeployment
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
import Data.Text (Text)
import qualified Data.Text as T
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when)
import System.Directory (createDirectoryIfMissing)
import TimeBandits.NetworkQUIC qualified as NetworkQUIC
import TimeBandits.Crypto (PrivKey(..), PubKey(..))
import qualified Polysemy.State as PS
import qualified TimeBandits.Network as Network
import TimeBandits.Core (Actor(..), ActorRole(..), computePubKeyHash)
import TimeBandits.Controller (ControllerConfig(..), SimulationMode(..))
import TimeBandits.Effects.KeyManagement
import TimeBandits.Effects.Trace as Trace
import TimeBandits.Effects.Output (Output, output)
import TimeBandits.Error (AppError)
import Polysemy.Embed (Embed)

-- | Resource log type for tracking resource operations
type ResourceLog = [(Text, Text)]

-- | Command line options
data CommandLineOptions = CommandLineOptions
  { optVerbose :: Bool
  , optSilent :: Bool
  , optScenarioFile :: Maybe FilePath
  , optSimulationMode :: SimulationMode
  }

-- | Default command line options
defaultOptions :: CommandLineOptions
defaultOptions = CommandLineOptions
  { optVerbose = False
  , optSilent = False
  , optScenarioFile = Nothing
  , optSimulationMode = InMemory
  }

-- | Main entry point
-- Initializes the Time Bandits application with UTF-8 encoding support,
-- configures the system based on command line arguments, and runs the
-- main program with the appropriate effect interpreters.
main :: IO ()
main = Utf8.withUtf8 $ do
  -- Parse command line arguments to determine configuration
  args <- Env.getArgs
  let options = parseOptions args
      config = parseConfig options

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

  -- Check if a scenario file was provided
  case optScenarioFile options of
    Just scenarioFile -> do
      putStrLn $ "Running scenario from file: " ++ scenarioFile
      runScenarioFromFile config scenarioFile
    Nothing -> do
      -- Run the main program with configured effects
      -- Handle any errors that might occur during execution
      result <- Main.interpretWithConfig config timeRef _resourceLogRef storeRef subsRef p2pNodesRef keyStoreRef actorTypeRegistryRef mainProgram
      case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (logs, _) -> mapM_ putStrLn logs

-- | Parse command line arguments into options
parseOptions :: [String] -> CommandLineOptions
parseOptions args = foldr parseArg defaultOptions args
  where
    parseArg "--verbose" opts = opts { optVerbose = True }
    parseArg "--silent" opts = opts { optSilent = True }
    parseArg "--scenario" opts = opts -- Next arg will be the file
    parseArg "--in-memory" opts = opts { optSimulationMode = InMemory }
    parseArg "--local-processes" opts = opts { optSimulationMode = LocalProcesses }
    parseArg "--geo-distributed" opts = opts { optSimulationMode = GeoDistributed }
    parseArg arg opts
      | "--scenario=" `isPrefixOf` arg = opts { optScenarioFile = Just (drop 11 arg) }
      | otherwise = case optScenarioFile opts of
          Nothing -> if last parsed == "--scenario" then opts { optScenarioFile = Just arg } else opts
          Just _ -> opts
      where parsed = take (length args - length (dropWhile (/= arg) args)) args

-- | Parse command line options into an interpreter configuration
parseConfig :: CommandLineOptions -> InterpreterConfig
parseConfig opts
  | optVerbose opts = verboseConfig
  | optSilent opts = silentConfig
  | otherwise = defaultConfig

-- | Run a scenario from a file
runScenarioFromFile :: InterpreterConfig -> FilePath -> IO ()
runScenarioFromFile config scenarioFile = do
  putStrLn $ "Loading scenario from: " ++ scenarioFile
  
  -- Load and run the scenario
  result <- runM . runError $ do
    scenario <- loadScenario scenarioFile
    runScenario scenario
  
  -- Handle the result
  case result of
    Left err -> putStrLn $ "Error running scenario: " ++ show err
    Right scenario -> do
      putStrLn "Scenario completed successfully!"
      case scenarioResults scenario of
        Nothing -> putStrLn "No results available."
        Just results -> do
          putStrLn $ "Success: " ++ show (resultSuccess results)
          putStrLn $ "Steps executed: " ++ show (resultSteps results)
          unless (null (resultErrors results)) $ do
            putStrLn "Errors:"
            mapM_ (putStrLn . ("  - " ++)) (map T.unpack (resultErrors results))

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
  
  -- Initialize the controller
  let controllerConfig = ControllerConfig
        { configMode = InMemory
        , configLogPath = "logs"
        , configVerbose = True
        }
  
  controllerResult <- runError $ initController controllerConfig
  case controllerResult of
    Left err -> do
      Trace.trace $ "Failed to initialize controller: " ++ show err
      output $ "Error: " ++ show err
    Right controller -> do
      Trace.trace "Controller initialized successfully."
      output "Time Bandits application initialized successfully!"
      
      -- Check if we should use QUIC for geo-distributed mode
      when (configMode controllerConfig == GeoDistributed) $ do
        Trace.trace "Setting up QUIC-based networking for Geo-Distributed mode..."
        setupQuicNetworking
      
      -- In a real implementation, we would use the controller here
      pure ()

-- | Set up QUIC-based networking
setupQuicNetworking :: 
  ( Member (PS.State [Network.P2PNode]) r
  , Member Trace r
  , Member (Error AppError) r
  , Member (Embed IO) r
  ) => 
  Sem r ()
setupQuicNetworking = do
  Trace.trace "Initializing QUIC-based networking..."
  
  -- Create a local actor with a proper identity
  now <- embed getCurrentTime
  
  -- Create deterministic keys for the local node
  let privKeyData = BS.pack "local-node-private-key-secure"
      pubKeyData = BS.pack "local-node-public-key-secure" 
      privKey = PrivKey privKeyData
      pubKey = PubKey pubKeyData
      actorId = computePubKeyHash pubKey
      localActor = Actor actorId TimeTraveler
      
      -- Create a QUIC configuration
      quicConfig = NetworkQUIC.defaultQuicConfig {
        NetworkQUIC.qcBindAddress = SockAddrInet 8443 (tupleToHostAddress (127, 0, 0, 1)),
        NetworkQUIC.qcBindPort = 8443,
        NetworkQUIC.qcNetworkMode = NetworkQUIC.HybridMode
      }
  
  -- Start the QUIC server
  embed $ do
    -- Create the certificate directory if it doesn't exist
    createDirectoryIfMissing True "certs"
    
    -- In a real implementation, we would properly manage this thread
    _ <- forkIO $ runM $ runError @AppError $ traceToStdout $ do
      -- Start the QUIC server
      NetworkQUIC.startQuicServer quicConfig localActor pubKey
      
      -- Log that the server started
      Trace.trace "Started QUIC server for local node"
      
      -- Keep the server running
      embed $ forever $ threadDelay 1000000  -- 1 second
    
    -- Give the server time to start
    threadDelay 100000  -- 100ms
  
  Trace.trace "QUIC-based networking initialized successfully."

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

-- Helper functions
isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
