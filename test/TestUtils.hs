-- | Testing utilities for Time Bandits tests
module TestUtils 
  ( -- * Test Types
    MockProgram(..)
  , MockLogger
  , MockEffect(..)
  , MockTimeMapEntry(..)
    -- * Mock Values
  , mockActorId
  , mockProgramId
  , mockResourceId
    -- * Resource Registry Functions
  , mockResourceRegistry
  , mockRegisterResource
    -- * Account Program Functions
  , mockCreateAccountProgram
  , mockGetBalance
  , mockDepositResource
  , mockWithdrawResource
  , mockTransferResource
  , mockCrossChainTransfer
  , mockLockResource
  , mockUnlockResource
    -- * Effect Logging Functions
  , mockCreateLogger
  , mockLogAppliedEffect
  , mockGetResourceLog
  , mockGetEffectHistory
  , mockGetEffectTimeMapHash
    -- * Time Map Functions
  , mockTimeMapEntry
  , mockGenerateTimeMapHash
  , mockCreateTimeMap
  , mockUpdateTimeMap
  , mockObserveTimeMap
  , mockAdvanceTimeMap
    -- * Helper Functions
  , isRight
  , isLeft
    -- * Re-exports for convenience
  , module Test.Hspec
  , module Test.QuickCheck
  ) where

import Test.Hspec
import Test.QuickCheck
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')

-- | Mock types to represent programs and resources
data MockProgram = MockProgram
  { programId :: Text
  , programResources :: Map Text Int
  , programLocks :: Map Text Bool
  }
  deriving (Show, Eq)

-- | Mock logger type with an IORef for mutability
data MockLogger = MockLogger (IORef (Map Text [MockEffect]))

data MockEffect = MockEffect
  { effectType :: Text
  , effectResource :: Text
  , effectTimeMapHash :: Text
  }
  deriving (Show, Eq)

data MockTimeMapEntry = MockTimeMapEntry
  { timelineId :: Text
  , blockHeight :: Int
  , blockHash :: Text
  }
  deriving (Show, Eq)

-- | Static mock values
mockActorId :: Text
mockActorId = T.pack "mock-actor-1"

mockProgramId :: Text
mockProgramId = T.pack "mock-program-1"

mockResourceId :: Text
mockResourceId = T.pack "mock-resource-1"

-- | Create a mock time map entry
mockTimeMapEntry :: Text -> Int -> Text -> MockTimeMapEntry
mockTimeMapEntry tId height hash = MockTimeMapEntry
  { timelineId = tId
  , blockHeight = height
  , blockHash = hash
  }

-- | Create a mock resource registry
mockResourceRegistry :: IO (Map Text Text)
mockResourceRegistry = pure $ Map.fromList
  [ (T.pack "token-1", T.pack "program-1")
  , (T.pack "token-2", T.pack "program-2")
  ]

-- | Register a resource in the mock registry
mockRegisterResource :: Map Text Text -> Text -> Text -> IO (Either Text (Map Text Text))
mockRegisterResource registry resourceId ownerId =
  if Map.member resourceId registry
    then pure $ Left (T.pack "Resource already registered")
    else pure $ Right $ Map.insert resourceId ownerId registry

-- | Create a mock account program with initial balances
mockCreateAccountProgram :: Text -> Map Text Int -> IO MockProgram
mockCreateAccountProgram actorId balances = pure MockProgram
  { programId = T.append (T.pack "program-") actorId
  , programResources = balances
  , programLocks = Map.empty
  }

-- | Get the balance of a resource in a mock account program
mockGetBalance :: MockProgram -> Text -> Int
mockGetBalance program resourceId =
  Map.findWithDefault 0 resourceId (programResources program)

-- | Deposit resources to a mock account program
mockDepositResource :: MockProgram -> Text -> Int -> IO (Either Text MockProgram)
mockDepositResource program resourceId amount =
  if amount <= 0
    then pure $ Left (T.pack "Amount must be positive")
    else pure $ Right $ program
      { programResources = Map.insertWith (+) resourceId amount (programResources program)
      }

-- | Withdraw resources from a mock account program
mockWithdrawResource :: MockProgram -> Text -> Int -> IO (Either Text MockProgram)
mockWithdrawResource program resourceId amount =
  let currentBalance = mockGetBalance program resourceId
      isLocked = Map.findWithDefault False resourceId (programLocks program)
  in if amount <= 0
       then pure $ Left (T.pack "Amount must be positive")
       else if isLocked
         then pure $ Left (T.pack "Resource is locked")
       else if amount > currentBalance
         then pure $ Left (T.pack "Insufficient balance")
         else pure $ Right $ program
           { programResources = Map.insert resourceId (currentBalance - amount) (programResources program)
           }

-- | Transfer resources between mock account programs
mockTransferResource :: MockProgram -> MockProgram -> Text -> Int -> IO (Either Text (MockProgram, MockProgram))
mockTransferResource fromProgram toProgram resourceId amount = do
  withdrawResult <- mockWithdrawResource fromProgram resourceId amount
  case withdrawResult of
    Left err -> pure $ Left err
    Right updatedFromProgram -> do
      depositResult <- mockDepositResource toProgram resourceId amount
      case depositResult of
        Left err -> pure $ Left err
        Right updatedToProgram -> pure $ Right (updatedFromProgram, updatedToProgram)

-- | Transfer resources between chains
mockCrossChainTransfer :: MockProgram -> MockProgram -> Text -> Int -> IO (Either Text (MockProgram, MockProgram))
mockCrossChainTransfer = mockTransferResource

-- | Lock a resource in a mock account program
mockLockResource :: MockProgram -> Text -> Int -> IO (Either Text MockProgram)
mockLockResource program resourceId amount =
  let currentBalance = mockGetBalance program resourceId
  in if amount <= 0
       then pure $ Left (T.pack "Amount must be positive")
       else if amount > currentBalance
         then pure $ Left (T.pack "Insufficient balance")
         else pure $ Right $ program
           { programLocks = Map.insert resourceId True (programLocks program)
           }

-- | Unlock a resource in a mock account program
mockUnlockResource :: MockProgram -> Text -> IO (Either Text MockProgram)
mockUnlockResource program resourceId =
  pure $ Right $ program
    { programLocks = Map.delete resourceId (programLocks program)
    }

-- | Create a mock effect logger
mockCreateLogger :: IO MockLogger
mockCreateLogger = do
  ref <- newIORef Map.empty
  pure $ MockLogger ref

-- | Log an applied effect in the mock logger
mockLogAppliedEffect :: MockLogger -> Text -> Text -> Text -> IO (Either Text ())
mockLogAppliedEffect (MockLogger logRef) resourceId effectType timeMapHash = do
  let effect = MockEffect
        { effectType = effectType
        , effectResource = resourceId
        , effectTimeMapHash = timeMapHash
        }
  modifyIORef' logRef $ Map.insertWith (++) resourceId [effect]
  pure $ Right ()

-- | Get the resource log from the mock logger
mockGetResourceLog :: MockLogger -> Text -> IO [MockEffect]
mockGetResourceLog (MockLogger logRef) resourceId = do
  logMap <- readIORef logRef
  pure $ Map.findWithDefault [] resourceId logMap

-- | Get the effect history from the mock logger
mockGetEffectHistory :: MockLogger -> Text -> IO (Either Text [MockEffect])
mockGetEffectHistory (MockLogger logRef) resourceId = do
  logMap <- readIORef logRef
  pure $ Right $ Map.findWithDefault [] resourceId logMap

-- | Get the time map hash from a mock effect
mockGetEffectTimeMapHash :: MockEffect -> Text
mockGetEffectTimeMapHash = effectTimeMapHash

-- | Generate a mock time map hash
mockGenerateTimeMapHash :: Map Text MockTimeMapEntry -> Text
mockGenerateTimeMapHash timeMap =
  let hashComponents = Map.foldrWithKey (\k v acc -> 
        acc ++ [T.unpack k, T.unpack (timelineId v), show (blockHeight v), T.unpack (blockHash v)]) [] timeMap
      hashString = concat hashComponents
  in T.pack $ "hash-" ++ show (length hashString) ++ "-" ++ take 8 hashString

-- | Create a mock time map
mockCreateTimeMap :: IO (Map Text MockTimeMapEntry)
mockCreateTimeMap = pure $ Map.fromList
  [ (T.pack "timeline-1", mockTimeMapEntry (T.pack "timeline-1") 1 (T.pack "hash-1"))
  , (T.pack "timeline-2", mockTimeMapEntry (T.pack "timeline-2") 1 (T.pack "hash-2"))
  ]

-- | Update a mock time map
mockUpdateTimeMap :: Map Text MockTimeMapEntry -> Text -> Int -> Text -> Map Text MockTimeMapEntry
mockUpdateTimeMap timeMap timelineId newHeight newHash =
  Map.insert timelineId (mockTimeMapEntry timelineId newHeight newHash) timeMap

-- | Observe a time map and return its hash
mockObserveTimeMap :: Map Text MockTimeMapEntry -> IO Text
mockObserveTimeMap timeMap = pure $ mockGenerateTimeMapHash timeMap

-- | Advance a time map (increment all heights by 1)
mockAdvanceTimeMap :: Map Text MockTimeMapEntry -> Map Text MockTimeMapEntry
mockAdvanceTimeMap timeMap =
  let advanceEntry entry = mockTimeMapEntry 
                            (timelineId entry) 
                            (blockHeight entry + 1) 
                            (T.append (blockHash entry) (T.pack "-advanced"))
  in Map.map advanceEntry timeMap

-- | Check if a result is Right
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- | Check if a result is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False 