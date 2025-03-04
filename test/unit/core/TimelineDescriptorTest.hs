module TimelineDescriptorTest where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Control.Monad.IO.Class (liftIO)
import Polysemy (runM, runError)
import Polysemy.Error (Error)

import TimeBandits.Core (Hash(..), EntityHash(..))
import TimeBandits.Types (AppError(..))
import TimeBandits.Timeline (TimelineHash, BlockHeader(..))
import TimeBandits.ProgramEffect (EffectType(..))
import TimeBandits.TimelineDescriptor
  ( TimelineDescriptor(..)
  , VMType(..)
  , EffectHandlerSpec(..)
  , EndpointConfig(..)
  , ClockType(..)
  , createEthereumDescriptor
  , createSolanaDescriptor
  , validateDescriptor
  , resolveEffectHandler
  )
import TimeBandits.TimelineAdapter
  ( TimelineAdapter(..)
  , AdapterError(..)
  , createAdapter
  , executeEffect
  , queryState
  , getLatestHead
  )

-- | Test for TimelineDescriptor and TimelineAdapter modules
timelineDescriptorTests :: TestTree
timelineDescriptorTests = testGroup "Timeline Descriptor Tests"
  [ testCase "Ethereum descriptor has required effect handlers" $ do
      let descriptor = createEthereumDescriptor
      result <- runTest $ validateDescriptor descriptor
      assertBool "Ethereum descriptor validation failed" result
      
  , testCase "Solana descriptor has required effect handlers" $ do
      let descriptor = createSolanaDescriptor
      result <- runTest $ validateDescriptor descriptor
      assertBool "Solana descriptor validation failed" result
      
  , testCase "Can resolve Transfer effect handler from Ethereum descriptor" $ do
      let descriptor = createEthereumDescriptor
      handlerSpec <- runTest $ resolveEffectHandler descriptor TransferAsset
      assertEqual "Wrong handler name" "ERC20Transfer" (ehsName handlerSpec)
      
  , testCase "Can resolve Query effect handler from Solana descriptor" $ do
      let descriptor = createSolanaDescriptor
      handlerSpec <- runTest $ resolveEffectHandler descriptor QueryState
      assertEqual "Wrong handler name" "AccountInfo" (ehsName handlerSpec)
      
  , testCase "Can create EVM adapter from descriptor" $ do
      let descriptor = createEthereumDescriptor
      adapter <- runTest $ createAdapter descriptor Nothing
      assertBool "Adapter creation failed" (isAdapter adapter)
      
  , testCase "Can create Solana adapter from descriptor" $ do
      let descriptor = createSolanaDescriptor
      adapter <- runTest $ createAdapter descriptor Nothing
      assertBool "Adapter creation failed" (isAdapter adapter)
      
  , testCase "Can get latest head from adapter" $ do
      let descriptor = createEthereumDescriptor
      adapter <- runTest $ createAdapter descriptor Nothing
      header <- runTest $ getLatestHead adapter
      assertBool "Block height should be positive" (blockHeight header > 0)
  ]
  where
    runTest m = do
      result <- runM $ runError m
      case result of
        Left err -> error $ "Test failed with error: " ++ show err
        Right value -> return value
    
    isAdapter adapter = case adapter of
      TimelineAdapter {} -> True
      _ -> False 