{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TimeBandits.Core.Effect.TemplateHaskellTest where

import Test.Hspec
import Polysemy (runM, interpret, Sem, Member, Embed, embed)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)

import TimeBandits.Core.Common.Types (LamportTime(..), Hash, PubKey(..), PrivKey(..), Signature(..))
import TimeBandits.Core.Types (AppError(..), ActorType(..), ResourceInfo(..), ActorHash)
import TimeBandits.Core.ResourceId (ResourceId)
import TimeBandits.Core.TimelineId (TimelineId)
import TimeBandits.Core.Effect

-- | Test handler for ResourceOps
handleResourceOps :: Member (Embed IO) r => Sem (ResourceOps ': r) a -> Sem r a
handleResourceOps = interpret \case
  CreateResource info -> embed $ pure (Right info)
  TransferResource info _ _ -> embed $ pure (Right info)
  ConsumeResource info -> embed $ pure (Right info)
  VerifyResource _ -> embed $ pure (Right True)
  GetResource _ -> embed $ do
    let mockInfo = ResourceInfo "test-resource" "resource-hash" Observer []
    pure (Right mockInfo)
  GetResourcesByOwner _ -> embed $ pure (Right [])
  GetResourcesByTimeline _ -> embed $ pure (Right [])

-- | Test handler for LogicalClock
handleLogicalClock :: Member (Embed IO) r => Sem (LogicalClock ': r) a -> Sem r a
handleLogicalClock = interpret \case
  GetLamportTime -> embed $ pure (LamportTime 0)
  IncrementTime -> embed $ pure (LamportTime 1)
  UpdateTime t -> embed $ pure t

-- | Test handler for KeyManagement
handleKeyManagement :: Member (Embed IO) r => Sem (KeyManagement ': r) a -> Sem r a
handleKeyManagement = interpret \case
  GenerateKeyPair -> embed $ pure (PubKey "pub", PrivKey "priv")
  RegisterPublicKey _ _ -> embed $ pure ()
  LookupPublicKey _ -> embed $ pure Nothing
  SignData _ _ -> embed $ pure (Right (Signature "sig"))
  VerifyWithPublicKey _ _ _ -> embed $ pure True
  RegisterActorType _ _ -> embed $ pure ()
  LookupActorType _ -> embed $ pure (Just Observer)

-- | Tests for the Template Haskell generated functions
templateHaskellTests :: Spec
templateHaskellTests = describe "Template Haskell Generated Functions" do
  it "getLamportTime returns a LamportTime" do
    result <- runM $ handleLogicalClock getLamportTime
    result `shouldBe` LamportTime 0
    
  it "incrementTime increments LamportTime" do
    result <- runM $ handleLogicalClock incrementTime
    result `shouldBe` LamportTime 1
    
  it "generateKeyPair returns a key pair" do
    (pub, priv) <- runM $ handleKeyManagement generateKeyPair
    pub `shouldBe` PubKey "pub"
    priv `shouldBe` PrivKey "priv"
    
  it "lookupActorType returns the actor type" do
    result <- runM $ handleKeyManagement $ lookupActorType "test-actor"
    result `shouldBe` Just Observer 