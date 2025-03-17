{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TimeBandits.TimeBandits.ZKProofTest where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Serialize (encode, decode)
import Polysemy (runM, runError)
import Polysemy.Error (Error)

import TimeBandits.Core (Hash(..))
import TimeBandits.Types (AppError(..))
import TimeBandits.Resource (Resource(..), Address(..))
import TimeBandits.Timeline (TimelineId(..))
import TimeBandits.TimeMap (TimeMap(..))
import TimeBandits.Program (ProgramId(..), ProgramState(..))
import TimeBandits.ProgramEffect (Effect(..), Guard(..))
import TimeBandits.ZKProof
import TimeBandits.TimelineProof
import TimeBandits.EffectAdapterGenerator (TimelineDescriptor(..), loadTimelineDescriptor)

-- | Main test spec
spec :: Spec
spec = do
  describe "ZKProof" $ do
    it "can generate and verify a mock ZK proof" $ do
      let input = ProofInput "test-input" (encode (42 :: Int))
          proofType = MockProof
      
      result <- runM $ runError $ generateZKProof proofType [input]
      case result of
        Left err -> fail $ "Failed to generate proof: " ++ show err
        Right proof -> do
          proof `shouldSatisfy` isValidProof
          
          -- Verify the proof
          verifyResult <- runM $ runError $ verifyZKProof proof [input]
          case verifyResult of
            Left err -> fail $ "Failed to verify proof: " ++ show err
            Right verified -> verified `shouldBe` True
    
    it "can combine multiple proofs" $ do
      let input1 = ProofInput "test-input-1" (encode (42 :: Int))
          input2 = ProofInput "test-input-2" (encode ("test" :: String))
      
      result1 <- runM $ runError $ generateZKProof MockProof [input1]
      result2 <- runM $ runError $ generateZKProof MockProof [input2]
      
      case (result1, result2) of
        (Right proof1, Right proof2) -> do
          let combinedProof = combineProofs [proof1, proof2]
          combinedProof `shouldSatisfy` isValidProof
          
          -- The combined proof should have a different hash than either original
          proofHash combinedProof `shouldNotBe` proofHash proof1
          proofHash combinedProof `shouldNotBe` proofHash proof2
          
        _ -> fail "Failed to generate test proofs"
  
  describe "TimelineProof" $ do
    it "can create and verify a timeline-specific proof" $ do
      -- Create a mock timeline ID and proof request
      let timelineId = TimelineId "test-timeline"
          effect = NoOpEffect
          guard = TrueGuard
          state = ProgramState "test-program" BS.empty
          request = GuardProofRequest timelineId guard effect state
      
      -- Create a mock proof adapter
      let adapter = TimelineProofAdapter
            { adapterTimeline = timelineId
            , adapterGenerateProof = mockGenerateProof timelineId
            , adapterVerifyProof = mockVerifyProof timelineId
            , adapterName = "Test Adapter"
            }
      
      -- Register the adapter
      liftIO $ registerProofAdapter adapter
      
      -- Generate a proof
      proofResult <- runM $ runError $ generateTimelineProof request
      case proofResult of
        Left err -> fail $ "Failed to generate timeline proof: " ++ show err
        Right proof -> do
          -- Verify the proof
          verifyResult <- runM $ runError $ verifyTimelineProof proof request
          case verifyResult of
            Left err -> fail $ "Failed to verify timeline proof: " ++ show err
            Right (ProofVerified tid _) -> tid `shouldBe` timelineId
            Right other -> fail $ "Unexpected verification result: " ++ show other
    
    it "can load a timeline descriptor and create a proof adapter" $ do
      -- This test would normally load a real descriptor file
      -- For testing, we'll create a mock descriptor
      let descriptor = TimelineDescriptor
            { descriptorId = TimelineId "ethereum-mainnet"
            , descriptorName = "Ethereum Mainnet"
            , descriptorType = "blockchain"
            , descriptorVersion = "1.0.0"
            , descriptorProperties = []
            , descriptorEffectAdapters = []
            , descriptorProofAdapters = []
            }
      
      -- Create an adapter from the descriptor
      adapterResult <- runM $ runError $ getProofAdapterForTimeline descriptor
      case adapterResult of
        Left err -> fail $ "Failed to create proof adapter: " ++ show err
        Right adapter -> do
          adapterTimeline adapter `shouldBe` descriptorId descriptor
          adapterName adapter `shouldBe` descriptorName descriptor

-- | Helper function to check if a proof is valid
isValidProof :: ZKProof -> Bool
isValidProof proof = 
  not (BS.null (proofData proof)) && 
  not (BS.null (proofMetadata proof))

-- | Main test runner
main :: IO ()
main = hspec spec 