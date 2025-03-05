{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module: Adapters.MockAdapter
Description: Mock implementation of the TimelineAdapter for testing

This module provides a mock implementation of the TimelineAdapter interface
for testing purposes. It simulates blockchain behavior without requiring
actual external connections.

The MockAdapter maintains an in-memory state that mimics a blockchain:
- It has blocks with headers
- It records transactions and their results
- It simulates balance updates
- It provides proofs that can be verified

This allows tests to run without external dependencies while still
exercising the full adapter interface.
-}
module Adapters.MockAdapter
  ( -- * Mock Adapter Creation
    createMockAdapter
  , mockTimelineDescriptor
  
  -- * Mock State Management
  , MockState(..)
  , initialMockState
  , advanceBlock
  , recordTransaction
  
  -- * Testing Utilities
  , mockTransfer
  , mockDeposit
  , mockWithdraw
  , mockQuery
  , mockProof
  , MockProof(..)
  ) where

import Control.Monad (void, replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize (Serialize, encode, decode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import GHC.Generics (Generic)
import Numeric (showHex)
import Polysemy (Member, Sem, embed)
import Polysemy.Error (Error, throw)
import Polysemy.Embed (Embed)
import System.Random (randomRIO)

-- Import from Core modules
import Core.Common (Hash(..), EntityHash(..), Address, Asset)
import Core.TimelineId (TimelineId(..))
import Core.ProgramId (ProgramId)
import Core.ResourceId (ResourceId)
import Core.Types (AppError(..), ProgramErrorType(..))
import Core.Timeline (TimelineHash, BlockHeader(..))
import Core.Effect (Effect(..))
import Core.Resource (Resource(..))
import Core.TimeMap (TimeMap(..), LamportClock(..))
import Core.TimelineDescriptor (TimelineDescriptor(..), VMType(..), EffectType(..))
import Adapters.TimelineAdapter (TimelineAdapter(..), AdapterConfig(..), AdapterState(..), AdapterError(..))

-- | Mock state for simulating a blockchain
data MockState = MockState
  { mockBlocks :: [BlockHeader]                     -- ^ Chain of block headers
  , mockBalances :: Map Address (Map Asset Integer) -- ^ Account balances
  , mockTransactions :: [(ByteString, ByteString)]  -- ^ Recorded transactions and results
  , mockNonce :: Map Address Integer                -- ^ Account nonces for transaction ordering
  }
  deriving stock (Show, Generic)
  deriving anyclass (Serialize)

-- | Create initial mock state
initialMockState :: IO MockState
initialMockState = do
  now <- getCurrentTime
  let genesisHeader = BlockHeader
        { blockHeight = 1
        , blockHash = EntityHash $ Hash "0x0000000000000000000000000000000000000000000000000000000000000001"
        , blockTimestamp = now
        , blockParentHash = EntityHash $ Hash "0x0000000000000000000000000000000000000000000000000000000000000000"
        }
  
  return MockState
    { mockBlocks = [genesisHeader]
    , mockBalances = Map.empty
    , mockTransactions = []
    , mockNonce = Map.empty
    }

-- | Create a mock timeline descriptor
mockTimelineDescriptor :: TimelineId -> TimelineDescriptor
mockTimelineDescriptor timelineId = TimelineDescriptor
  { tdId = timelineId
  , tdName = "Mock Blockchain"
  , tdVmType = MockVM
  , tdApiEndpoint = "http://localhost:8545"
  , tdBlockTime = 15  -- 15 seconds per block
  , tdEffectHandlers = Map.empty  -- Could be populated with mock handlers
  , tdNetworkId = "mock-1"
  }

-- | Create a mock adapter
createMockAdapter :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  AdapterConfig -> 
  AdapterState -> 
  Sem r TimelineAdapter
createMockAdapter config initialState = do
  mockState <- embed initialMockState
  
  -- Create a reference to hold the mutable mock state
  stateRef <- embed $ newIORef mockState
  
  -- Return a TimelineAdapter with mock implementations
  return TimelineAdapter
    { taConfig = config
    , taState = initialState
    , taExecuteEffect = mockExecuteEffect stateRef
    , taQueryState = mockQueryState stateRef
    , taGetLatestHead = mockGetLatestHead stateRef
    }

-- | Mock implementation of executing effects
mockExecuteEffect :: IORef MockState -> Effect -> IO (Either AdapterError ByteString)
mockExecuteEffect stateRef effect = do
  -- Implement based on effect type
  case effect of
    -- Handle timeline effects with encoded commands
    TimelineEffect timelineId encodedEffect -> do
      case decode encodedEffect of
        Left err -> 
          return $ Left $ DecodingError $ "Failed to decode timeline effect: " <> T.pack err
        
        Right ("deposit", resourceId, amount, address) -> do
          result <- mockDeposit stateRef resourceId amount address
          return $ Right $ encode result
          
        Right ("withdraw", resourceId, amount, address) -> do
          result <- mockWithdraw stateRef resourceId amount address
          return $ Right $ encode result
          
        Right ("transfer", resourceId, amount, fromAddress, toAddress) -> do
          result <- mockTransfer stateRef resourceId amount fromAddress toAddress
          return $ Right $ encode result
          
        Right (effectName, _) ->
          return $ Left $ ExecutionError $ "Unknown timeline effect: " <> T.pack (show effectName)
    
    -- Handle resource effects
    ResourceEffect resourceId encodedEffect -> do
      case decode encodedEffect of
        Left err -> 
          return $ Left $ DecodingError $ "Failed to decode resource effect: " <> T.pack err
        
        Right ("update", newValue) -> do
          -- Mock updating a resource's state
          return $ Right $ encode ("resource-updated", resourceId, newValue)
          
        Right ("create", initialValue) -> do
          -- Mock creating a new resource
          return $ Right $ encode ("resource-created", resourceId, initialValue)
          
        Right ("delete", _) -> do
          -- Mock deleting a resource
          return $ Right $ encode ("resource-deleted", resourceId)
          
        Right (effectName, _) ->
          return $ Left $ ExecutionError $ "Unknown resource effect: " <> T.pack (show effectName)
      
    -- Handle program effects  
    ProgramEffect programId encodedEffect -> do
      case decode encodedEffect of
        Left err -> 
          return $ Left $ DecodingError $ "Failed to decode program effect: " <> T.pack err
        
        Right ("invoke", functionName, args) -> do
          -- Mock invoking a program function
          return $ Right $ encode ("function-invoked", programId, functionName, args)
          
        Right ("update", newCode) -> do
          -- Mock updating a program's code
          return $ Right $ encode ("program-updated", programId, newCode)
          
        Right (effectName, _) ->
          return $ Left $ ExecutionError $ "Unknown program effect: " <> T.pack (show effectName)
      
    -- Handle composite effects by recursively processing each sub-effect
    CompositeEffect subEffects -> do
      results <- mapM (mockExecuteEffect stateRef) subEffects
      
      -- Check if any sub-effects failed
      let failures = [err | Left err <- results]
      if not (null failures)
        then return $ Left $ head failures
        else do
          -- Combine all successful results
          let successResults = [res | Right res <- results]
          return $ Right $ encode successResults
      
    -- Return error for unsupported effects
    _ -> return $ Left $ ExecutionError $ "Unsupported effect type: " <> T.pack (show effect)

-- | Mock implementation of querying state
mockQueryState :: IORef MockState -> ByteString -> IO (Either AdapterError ByteString)
mockQueryState stateRef query = do
  -- Parse the query
  case decode query of
    Left err -> return $ Left $ DecodingError $ T.pack err
    
    Right ("balance", address, asset) -> do
      balance <- mockQuery stateRef address asset
      return $ Right $ encode balance
      
    Right ("nonce", address) -> do
      state <- readIORef stateRef
      let nonce = Map.findWithDefault 0 address (mockNonce state)
      return $ Right $ encode nonce
      
    Right ("block", height) -> do
      state <- readIORef stateRef
      let block = find (\b -> blockHeight b == height) (mockBlocks state)
      case block of
        Nothing -> return $ Left $ ExecutionError $ "Block not found: " <> T.pack (show height)
        Just b -> return $ Right $ encode b
        
    Right ("transaction", txHash) -> do
      state <- readIORef stateRef
      let tx = find (\(_, result) -> txHash `BS.isInfixOf` result) (mockTransactions state)
      case tx of
        Nothing -> return $ Left $ ExecutionError $ "Transaction not found: " <> T.pack (show txHash)
        Just t -> return $ Right $ encode t
        
    Right (queryType, _) ->
      return $ Left $ ExecutionError $ "Unknown query type: " <> T.pack (show queryType)

-- Helper function for list search
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) = if p x then Just x else find p xs

-- | Mock implementation of getting the latest block header
mockGetLatestHead :: IORef MockState -> IO (Either AdapterError BlockHeader)
mockGetLatestHead stateRef = do
  -- Get the current mock state
  state <- readIORef stateRef
  
  -- Return the latest block header
  case mockBlocks state of
    [] -> return $ Left $ ExecutionError "No blocks in mock chain"
    (header:_) -> return $ Right header

-- | Advance the mock blockchain by creating a new block
advanceBlock :: IORef MockState -> IO BlockHeader
advanceBlock stateRef = do
  -- Get the current state
  state <- readIORef stateRef
  
  -- Get the current time
  now <- getCurrentTime
  
  -- Get the latest block
  let latestBlock = head $ mockBlocks state
  
  -- Create a new block header
  let newHeader = BlockHeader
        { blockHeight = blockHeight latestBlock + 1
        , blockHash = EntityHash $ Hash $ "0x" <> T.pack (show $ blockHeight latestBlock + 1)
        , blockTimestamp = now
        , blockParentHash = blockHash latestBlock
        }
  
  -- Update the state with the new block
  let newState = state { mockBlocks = newHeader : mockBlocks state }
  writeIORef stateRef newState
  
  return newHeader

-- | Record a transaction in the mock state
recordTransaction :: IORef MockState -> ByteString -> ByteString -> IO ()
recordTransaction stateRef txData txResult = do
  -- Get the current state
  state <- readIORef stateRef
  
  -- Update the state with the new transaction
  let newState = state { mockTransactions = (txData, txResult) : mockTransactions state }
  writeIORef stateRef newState

-- | Mock implementation of a deposit
mockDeposit :: IORef MockState -> ResourceId -> Integer -> Address -> IO ByteString
mockDeposit stateRef resourceId amount address = do
  -- Get the current state
  state <- readIORef stateRef
  
  -- Update the balances
  let asset = resourceIdToAsset resourceId
      currentBalance = Map.findWithDefault 0 asset $ 
                        Map.findWithDefault Map.empty address $ 
                        mockBalances state
      newAccountBalances = Map.insert asset (currentBalance + amount) $ 
                            Map.findWithDefault Map.empty address $ 
                            mockBalances state
      newBalances = Map.insert address newAccountBalances $ mockBalances state
  
  -- Create the updated state
  let newState = state { mockBalances = newBalances }
  
  -- Update the state
  writeIORef stateRef newState
  
  -- Generate a mock transaction hash
  txHash <- generateMockTxHash
  
  -- Record the transaction
  let txData = encode ("deposit", resourceId, amount, address)
      txResult = encode ("success", txHash)
  recordTransaction stateRef txData txResult
  
  -- Advance the block to simulate blockchain progress
  _ <- advanceBlock stateRef
  
  -- Return the transaction result
  return txResult

-- | Mock implementation of a withdrawal
mockWithdraw :: IORef MockState -> ResourceId -> Integer -> Address -> IO ByteString
mockWithdraw stateRef resourceId amount address = do
  -- Get the current state
  state <- readIORef stateRef
  
  -- Check if the account has enough balance
  let asset = resourceIdToAsset resourceId
      currentBalance = Map.findWithDefault 0 asset $ 
                        Map.findWithDefault Map.empty address $ 
                        mockBalances state
  
  if currentBalance < amount
    then do
      -- Not enough balance
      let txData = encode ("withdraw", resourceId, amount, address)
          txResult = encode ("failure", "insufficient balance")
      recordTransaction stateRef txData txResult
      return txResult
    else do
      -- Update the balances
      let newAccountBalances = Map.insert asset (currentBalance - amount) $ 
                                Map.findWithDefault Map.empty address $ 
                                mockBalances state
          newBalances = Map.insert address newAccountBalances $ mockBalances state
      
      -- Create the updated state
      let newState = state { mockBalances = newBalances }
      
      -- Update the state
      writeIORef stateRef newState
      
      -- Generate a mock transaction hash
      txHash <- generateMockTxHash
      
      -- Record the transaction
      let txData = encode ("withdraw", resourceId, amount, address)
          txResult = encode ("success", txHash)
      recordTransaction stateRef txData txResult
      
      -- Advance the block to simulate blockchain progress
      _ <- advanceBlock stateRef
      
      -- Return the transaction result
      return txResult

-- | Mock implementation of a transfer
mockTransfer :: IORef MockState -> ResourceId -> Integer -> Address -> Address -> IO ByteString
mockTransfer stateRef resourceId amount fromAddress toAddress = do
  -- Get the current state
  state <- readIORef stateRef
  
  -- Check if the account has enough balance
  let asset = resourceIdToAsset resourceId
      fromBalance = Map.findWithDefault 0 asset $ 
                     Map.findWithDefault Map.empty fromAddress $ 
                     mockBalances state
  
  if fromBalance < amount
    then do
      -- Not enough balance
      let txData = encode ("transfer", resourceId, amount, fromAddress, toAddress)
          txResult = encode ("failure", "insufficient balance")
      recordTransaction stateRef txData txResult
      return txResult
    else do
      -- Update the from balance
      let newFromAccountBalances = Map.insert asset (fromBalance - amount) $ 
                                   Map.findWithDefault Map.empty fromAddress $ 
                                   mockBalances state
          
          -- Update the to balance
          toBalance = Map.findWithDefault 0 asset $ 
                       Map.findWithDefault Map.empty toAddress $ 
                       mockBalances state
          newToAccountBalances = Map.insert asset (toBalance + amount) $ 
                                 Map.findWithDefault Map.empty toAddress $ 
                                 mockBalances state
          
          -- Update both balances
          newBalances = Map.insert toAddress newToAccountBalances $ 
                        Map.insert fromAddress newFromAccountBalances $ 
                        mockBalances state
      
      -- Create the updated state
      let newState = state { mockBalances = newBalances }
      
      -- Update the state
      writeIORef stateRef newState
      
      -- Generate a mock transaction hash
      txHash <- generateMockTxHash
      
      -- Record the transaction
      let txData = encode ("transfer", resourceId, amount, fromAddress, toAddress)
          txResult = encode ("success", txHash)
      recordTransaction stateRef txData txResult
      
      -- Advance the block to simulate blockchain progress
      _ <- advanceBlock stateRef
      
      -- Return the transaction result
      return txResult

-- | Mock implementation of a balance query
mockQuery :: IORef MockState -> Address -> Asset -> IO Integer
mockQuery stateRef address asset = do
  -- Get the current state
  state <- readIORef stateRef
  
  -- Get the balance
  let balance = Map.findWithDefault 0 asset $ 
                 Map.findWithDefault Map.empty address $ 
                 mockBalances state
  
  return balance

-- | Data structure for mock proofs
data MockProof = MockProof
  { proofBlock :: BlockHeader
  , proofTxIndex :: Int
  , proofData :: ByteString
  , proofSignature :: ByteString
  }
  deriving stock (Show, Generic)
  deriving anyclass (Serialize)

-- | Generate a mock proof for a transaction
mockProof :: IORef MockState -> ByteString -> IO MockProof
mockProof stateRef txData = do
  -- Get the current state
  state <- readIORef stateRef
  
  -- Get the latest block
  let latestBlock = head $ mockBlocks state
  
  -- Generate a mock transaction index
  txIndex <- randomRIO (0, 100)
  
  -- Generate a mock signature
  signature <- generateMockSignature
  
  -- Create the proof
  return MockProof
    { proofBlock = latestBlock
    , proofTxIndex = txIndex
    , proofData = txData
    , proofSignature = signature
    }

-- | Helper function to convert a ResourceId to an Asset
resourceIdToAsset :: ResourceId -> Asset
resourceIdToAsset = T.pack . show  -- Simplified for mock purposes

-- | Generate a mock transaction hash
generateMockTxHash :: IO Text
generateMockTxHash = do
  -- Generate a random number and format it as a hex string
  n <- randomRIO (0, maxBound :: Int)
  return $ "0x" <> T.pack (showHex n "")

-- | Generate a mock signature
generateMockSignature :: IO ByteString
generateMockSignature = do
  -- Generate a random 64-byte signature (simplified)
  bytes <- replicateM 64 (randomRIO (0, 255 :: Int))
  return $ BS.pack $ map (toEnum . fromIntegral) bytes
