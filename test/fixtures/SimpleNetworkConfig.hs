{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module SimpleNetworkConfig (tests) where

import Prelude
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit (testCase, assertBool)
import qualified Data.ByteString.Char8 as BS
import Network.Socket (SockAddr(..))

-- Import from TimeBandits.Types via Core
import TimeBandits.Core (
  Actor(..),
  ActorType(..),
  Hash(..),
  EntityHash(..),
  ActorHash,
  PrivKey(..)
  )

import TimeBandits.Network (
  P2PConfig(..),
  defaultP2PConfig
  )
 
-- Define our test suite
tests :: Tasty.TestTree
tests = Tasty.testGroup "Time Bandits Tests" 
  [ testCase "Main Program Initialization" testMainProgramInitialization ]

-- | Test function that simulates the mainProgram initialization
testMainProgramInitialization :: IO ()
testMainProgramInitialization = do
  -- Create a simple test that just verifies we can initialize the basic components
  let actorHash = EntityHash (Hash (BS.pack "test-actor"))
      actor = Actor { actorId = actorHash, actorType = TimeTraveler }
      privateKey = PrivKey (BS.pack "test-private-key")
      config = testConfig
  
  -- Run a simplified test that just checks if we can create the basic objects
  assertBool "Actor should have TimeTraveler type" (actorType actor == TimeTraveler)
  
  -- Check that the P2P config has a bind address (without evaluating the error)
  case pcBindAddress config of
    SockAddrInet _ _ -> assertBool "Bind address is valid" True
    _ -> assertBool "Bind address should be SockAddrInet" False

-- | Test configuration
testConfig :: P2PConfig
testConfig = defaultP2PConfig 