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
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import Numeric (showHex)
import Polysemy (Member, Sem, embed)
import Polysemy.Error (Error, throw)
import Polysemy.Embed (Embed)
import System.Random (randomRIO)
import Prelude hiding (find, readIORef, writeIORef, newIORef)
import qualified Data.IORef as IORef

-- Import from Core modules
import Core.Common (Hash(..), EntityHash(..), Address, Asset(..), LamportTime(..), Actor(..), PubKey(..))
import Core.TimelineId (TimelineId(..))
import Core.ProgramId (ProgramId)
import Core.ResourceId (ResourceId)
import Core.Types (AppError(..))
import Types.Core (ProgramErrorType(..))
import Core.Timeline (TimelineHash, BlockHeader(..))
import Core.Effect (Effect(..))
import Core.Resource (Resource(..))
import Core.TimeMap (TimeMap(..), LamportClock(..))
import Core.TimelineDescriptor (TimelineDescriptor(..), VMType(..))
import Types.EffectTypes (EffectType(..))
import Adapters.TimelineAdapter (TimelineAdapter(..), AdapterConfig(..), AdapterState(..), AdapterError(..))
import Core.Error (ExecutionError(..))

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
  -- Convert UTCTime to LamportTime (using timestamp as Word64)
  let nowLamport = LamportTime $ floor $ utcTimeToPOSIXSeconds now
  let genesisHeader = BlockHeader
        { bhHeight = 1
        , bhMerkleRoot = Hash "0x0000000000000000000000000000000000000000000000000000000000000001"
        , bhTimestamp = nowLamport
        , bhPrevBlockHash = Just $ Hash "0x0000000000000000000000000000000000000000000000000000000000000000"
        , bhTimeline = EntityHash $ Hash "mock-timeline"  -- TimelineId is an EntityHash "Timeline" type alias
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
  { timelineId = timelineId
  , timelineName = "Mock Blockchain"
  , timelineVMType = CustomVM "MockVM"
  , timelineEndpoints = ["http://localhost:8545"]
  , timelineHandlers = []  -- Could be populated with mock handlers
  , timelineExtraConfig = Map.fromList [("blockTime", "15"), ("networkId", "mock-1")]
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
        
        -- Standardize all patterns to use (String, ByteString) format for consistency
        Right (command :: String, effectData :: ByteString) -> case command of
          "deposit" -> do
            -- Decode the effect data
            case decode effectData of
              Left err -> return $ Left $ DecodingError $ "Failed to decode deposit data: " <> T.pack err
              Right (resourceId :: ResourceId, amount :: Integer, address :: Address) -> do
                result <- mockDeposit stateRef resourceId amount address
                return $ Right $ encode result
            
          "withdraw" -> do
            -- Decode the effect data
            case decode effectData of
              Left err -> return $ Left $ DecodingError $ "Failed to decode withdraw data: " <> T.pack err
              Right (resourceId :: ResourceId, amount :: Integer, address :: Address) -> do
                result <- mockWithdraw stateRef resourceId amount address
                return $ Right $ encode result
            
          "transfer" -> do
            -- Decode the effect data
            case decode effectData of
              Left err -> return $ Left $ DecodingError $ "Failed to decode transfer data: " <> T.pack err
              Right (resourceId :: ResourceId, amount :: Integer, fromAddress :: Address, toAddress :: Address) -> do
                result <- mockTransfer stateRef resourceId amount fromAddress toAddress
                return $ Right $ encode result
                
          "query_balance" -> do
            -- Decode the effect data
            case decode effectData of
              Left err -> return $ Left $ DecodingError $ "Failed to decode query data: " <> T.pack err
              Right (address :: Address, asset :: Asset) -> do
                balance <- mockQuery stateRef address asset
                return $ Right $ encode balance
                
          "proof" -> do
            -- Decode the effect data
            case decode effectData of
              Left err -> return $ Left $ DecodingError $ "Failed to decode proof data: " <> T.pack err
              Right (txHash :: ByteString) -> do
                result <- mockProof stateRef txHash
                return $ Right $ encode result
                
          _ -> return $ Left $ ExecutionError $ "Unknown timeline effect: " <> T.pack command
    
    -- Handle resource effects
    ResourceEffect resourceId encodedEffect -> do
      case decode (encodedEffect :: ByteString) of
        Left err ->
          return $ Left $ DecodingError $ "Failed to decode resource effect: " <> T.pack err
        
        Right (effectName :: String, effectData :: ByteString) -> case effectName of
          "update" -> do
            -- Decode the effect data for the update
            case decode (effectData :: ByteString) of
              Left err -> return $ Left $ DecodingError $ "Failed to decode update data: " <> T.pack err
              Right (newValue :: ByteString) -> do
                return $ Right $ encode (("resource-updated" :: String), resourceId, newValue)
                
          "create" -> do
            -- Decode the effect data for the create
            case decode (effectData :: ByteString) of
              Left err -> return $ Left $ DecodingError $ "Failed to decode create data: " <> T.pack err
              Right (initialValue :: ByteString) -> do
                return $ Right $ encode (("resource-created" :: String), resourceId, initialValue)
                
          "delete" -> do
            return $ Right $ encode (("resource-deleted" :: String), resourceId)
            
          _ -> return $ Left $ ExecutionError $ "Unknown resource effect: " <> T.pack (show effectName)
      
    -- Handle program effects  
    ProgramEffect programId encodedEffect -> do
      case decode (encodedEffect :: ByteString) of
        Left err ->
          return $ Left $ DecodingError $ "Failed to decode program effect: " <> T.pack err
        
        Right (effectName :: String, effectData :: ByteString) -> case effectName of
          "invoke" -> do
            -- Decode the effect data
            case decode (effectData :: ByteString) of
              Left err -> return $ Left $ DecodingError $ "Failed to decode invoke data: " <> T.pack err
              Right (functionName :: String, args :: ByteString) -> do
                return $ Right $ encode (("function-invoked" :: String), programId, functionName, args)
            
          "update" -> do
            -- Decode the effect data
            case decode (effectData :: ByteString) of
              Left err -> return $ Left $ DecodingError $ "Failed to decode update data: " <> T.pack err
              Right (newCode :: ByteString) -> do
                return $ Right $ encode (("program-updated" :: String), programId, newCode)
                
          _ -> return $ Left $ ExecutionError $ "Unknown program effect: " <> T.pack (show effectName)
      
    -- Handle composite effects by recursively processing each sub-effect
    CompositeEffect subEffects -> do
      results <- mapM (mockExecuteEffect stateRef) subEffects
      
      -- Check if any sub-effects failed
      let failures = [err | Left err <- results]
      if not (null failures)
        then case safeHead failures of
              Just firstError -> return $ Left firstError
              Nothing -> error "Unexpected empty failures list"  -- This should never happen due to the null check
        else do
          -- Combine all successful results
          let successResults = [res | Right res <- results]
          return $ Right $ encode successResults
      
    -- Return error for unsupported effects
    _ -> return $ Left $ ExecutionError $ "Unsupported effect type: " <> T.pack (show effect)

-- | Mock implementation of querying state
mockQueryState :: IORef MockState -> ByteString -> IO (Either AdapterError ByteString)
mockQueryState stateRef query = do
  case decode (query :: ByteString) of
    Left err -> return $ Left $ DecodingError $ T.pack err
    
    Right (queryType :: String, queryData :: ByteString) -> case queryType of
      "balance" -> do
        -- Decode the query data
        case decode queryData of
          Left err -> return $ Left $ DecodingError $ "Failed to decode balance query data: " <> T.pack err
          Right (address :: Address, asset :: Asset) -> do
            state <- readIORef stateRef
            -- Get the balance for the address and asset
            let balance = Map.findWithDefault 0 asset $ 
                          Map.findWithDefault Map.empty address $ 
                          mockBalances state
            return $ Right $ encode balance
      
      "nonce" -> do
        -- Extract address from queryData
        case decode queryData of
          Left err -> return $ Left $ DecodingError $ "Failed to decode nonce query: " <> T.pack err
          Right address -> do
            state <- readIORef stateRef
            let nonce = Map.findWithDefault 0 address (mockNonce state)
            return $ Right $ encode nonce
      
      "block" -> do
        -- Extract height from queryData
        case decode queryData of
          Left err -> return $ Left $ DecodingError $ "Failed to decode block query: " <> T.pack err
          Right height -> do
            state <- readIORef stateRef
            let block = find (\b -> bhHeight b == height) (mockBlocks state)
            case block of
              Nothing -> return $ Left $ ExecutionError $ "Block not found: " <> T.pack (show height)
              Just b -> return $ Right $ encode b
        
      "transaction" -> do
        -- Use txHash directly from queryData
        let txHash = queryData
        state <- readIORef stateRef
        let tx = find (\(_, result) -> txHash `BS.isInfixOf` result) (mockTransactions state)
        case tx of
          Nothing -> return $ Left $ ExecutionError $ "Transaction not found: " <> T.pack (show txHash)
          Just t -> return $ Right $ encode t
        
      _ ->
        return $ Left $ ExecutionError $ "Unknown query type: " <> T.pack (show queryType)

-- Helper function for list search
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) = if p x then Just x else find p xs

-- | Safe version of head that returns Maybe
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- | Mock implementation of getting the latest block header
mockGetLatestHead :: IORef MockState -> IO (Either AdapterError BlockHeader)
mockGetLatestHead stateRef = do
  -- Get the current mock state
  state <- readIORef stateRef
  
  -- Return the latest block header
  case safeHead (mockBlocks state) of
    Nothing -> return $ Left $ ExecutionError "No blocks in mock chain"
    Just header -> return $ Right header

-- | Advance the blockchain with a new block
advanceBlock :: IORef MockState -> IO BlockHeader
advanceBlock stateRef = do
  state <- readIORef stateRef
  now <- getCurrentTime
  let nowLamport = LamportTime $ floor $ utcTimeToPOSIXSeconds now
  
  -- Get the latest block (using head safely)
  let latestBlock = case mockBlocks state of
        (block:_) -> block
        [] -> error "No blocks available in state"  -- This should never happen in practice
  
  let newHeader = BlockHeader
        { bhHeight = bhHeight latestBlock + 1
        , bhMerkleRoot = Hash $ "0x" <> TE.encodeUtf8 (T.pack (show $ bhHeight latestBlock + 1))
        , bhTimestamp = nowLamport
        , bhPrevBlockHash = Just $ bhMerkleRoot latestBlock
        , bhTimeline = bhTimeline latestBlock
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
  let asset = resourceIdToAsset resourceId
      currentBalance = Map.findWithDefault 0 asset $ 
                      Map.findWithDefault Map.empty address $ 
                      mockBalances state
      
      -- Update the balances
      newAccountBalances = Map.insert asset (currentBalance + amount) $ 
                          Map.findWithDefault Map.empty address $ 
                          mockBalances state
      newBalances = Map.insert address newAccountBalances $ mockBalances state
      
      -- Update the state
      newState = state { mockBalances = newBalances }
      
  -- Write the updated state
  writeIORef stateRef newState
  
  -- Record the transaction
  txHash <- generateMockTxHash
  let txData = encode (("deposit" :: String), resourceId, amount, address)
      txResult = encode (("success" :: String), txHash)
  recordTransaction stateRef txData txResult
  
  -- Return the result
  return txResult

-- | Mock implementation of a withdraw operation
mockWithdraw :: IORef MockState -> ResourceId -> Integer -> Address -> IO ByteString
mockWithdraw stateRef resourceId amount address = do
  -- Check if the account has enough balance
  state <- readIORef stateRef
  let asset = resourceIdToAsset resourceId
      currentBalance = Map.findWithDefault 0 asset $ 
                      Map.findWithDefault Map.empty address $ 
                      mockBalances state
  
  -- If insufficient balance, return an error
  if currentBalance < amount
    then do
      -- Record the failed transaction
      let txData = encode (("withdraw" :: String), resourceId, amount, address)
          txResult = encode (("failure" :: String), ("insufficient balance" :: String))
      recordTransaction stateRef txData txResult
      return txResult
    else do
      -- Update the balances
      let newAccountBalances = Map.insert asset (currentBalance - amount) $ 
                              Map.findWithDefault Map.empty address $ 
                              mockBalances state
          newBalances = Map.insert address newAccountBalances $ mockBalances state
          
          -- Update the state
          newState = state { mockBalances = newBalances }
      
      -- Write the updated state
      writeIORef stateRef newState
      
      -- Record the transaction
      txHash <- generateMockTxHash
      let txData = encode (("withdraw" :: String), resourceId, amount, address)
          txResult = encode (("success" :: String), txHash)
      recordTransaction stateRef txData txResult
      
      -- Return the result
      return txResult

-- | Mock implementation of a transfer
mockTransfer :: IORef MockState -> ResourceId -> Integer -> Address -> Address -> IO ByteString
mockTransfer stateRef resourceId amount fromAddress toAddress = do
  -- Check if the source account has enough balance
  state <- readIORef stateRef
  let asset = resourceIdToAsset resourceId
      fromBalance = Map.findWithDefault 0 asset $ 
                   Map.findWithDefault Map.empty fromAddress $ 
                   mockBalances state
  
  -- If insufficient balance, return an error
  if fromBalance < amount
    then do
      -- Record the failed transaction
      let txData = encode (("transfer" :: String), resourceId, amount, fromAddress, toAddress)
          txResult = encode (("failure" :: String), ("insufficient balance" :: String))
      recordTransaction stateRef txData txResult
      return txResult
    else do
      -- Update the balances
      let newFromAccountBalances = Map.insert asset (fromBalance - amount) $ 
                                  Map.findWithDefault Map.empty fromAddress $ 
                                  mockBalances state
          toBalance = Map.findWithDefault 0 asset $ 
                     Map.findWithDefault Map.empty toAddress $ 
                     mockBalances state
          newToAccountBalances = Map.insert asset (toBalance + amount) $ 
                                Map.findWithDefault Map.empty toAddress $ 
                                mockBalances state
          
          -- Update the balances in the state
          newBalances = Map.insert fromAddress newFromAccountBalances $ 
                       Map.insert toAddress newToAccountBalances $ 
                       mockBalances state
          
          -- Update the state
          newState = state { mockBalances = newBalances }
      
      -- Write the updated state
      writeIORef stateRef newState
      
      -- Record the transaction
      txHash <- generateMockTxHash
      let txData = encode (("transfer" :: String), resourceId, amount, fromAddress, toAddress)
          txResult = encode (("success" :: String), txHash)
      recordTransaction stateRef txData txResult
      
      -- Return the result
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
  let latestBlock = case safeHead (mockBlocks state) of
                      Just block -> block
                      Nothing -> error "No blocks available"  -- Handle empty blocks case appropriately
  
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
resourceIdToAsset resourceId = Asset
  { assetId = EntityHash $ Hash $ TE.encodeUtf8 $ T.pack (show resourceId)  -- Convert ResourceId to Hash
  , assetType = "MOCK_ASSET"                                               -- Mock asset type
  , assetQuantity = 0                                                      -- Default quantity
  }

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
