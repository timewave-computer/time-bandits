{-# LANGUAGE OverloadedStrings #-}

module Network.Protocol.VersionTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bits (shift, (.|.))
import Data.Word (Word32, Word64)
import Data.Either (isLeft, isRight)

import Network.Protocol.Version

-- | Main test group
tests :: TestTree
tests = testGroup "Protocol Version Tests"
  [ testCase "Create protocol version" testCreateVersion
  , testCase "Version compatibility checks" testVersionCompatibility
  , testCase "Feature flag checks" testFeatureFlags
  , testCase "Version validation" testVersionValidation
  , testCase "Version serialization" testVersionSerialization
  ]

-- | Test creating protocol versions
testCreateVersion :: Assertion
testCreateVersion = do
  -- Create a protocol version
  let version = ProtocolVersion 1 2 3 0
      
  -- Check fields
  majorVersion version @?= 1
  minorVersion version @?= 2
  patchVersion version @?= 3
  featureFlags version @?= 0
  
  -- Check string representation
  show version @?= "1.2.3"
  
  -- Check current version
  majorVersion currentVersion @?= 0
  minorVersion currentVersion @?= 1
  
  -- Check that current version has required features
  all (hasFeature currentVersion) requiredFeatures @? "Current version should have all required features"

-- | Test version compatibility checks
testVersionCompatibility :: Assertion
testVersionCompatibility = do
  -- Create test versions
  let compatibleVersion = ProtocolVersion 
          (majorVersion currentVersion)
          (minorVersion currentVersion)
          (patchVersion currentVersion + 1)
          (featureFlags currentVersion)
      
      incompatibleMajor = ProtocolVersion 
          (majorVersion currentVersion + 1)
          (minorVersion currentVersion)
          (patchVersion currentVersion)
          (featureFlags currentVersion)
      
      incompatibleMinor = ProtocolVersion 
          (majorVersion currentVersion)
          (minorVersion minimumCompatibleVersion - 1)
          (patchVersion currentVersion)
          (featureFlags currentVersion)
      
      missingFeatures = ProtocolVersion
          (majorVersion currentVersion)
          (minorVersion currentVersion)
          (patchVersion currentVersion)
          0  -- No features
  
  -- Test compatibility
  isCompatible currentVersion compatibleVersion @? "Should be compatible with same major/minor version"
  not (isCompatible currentVersion incompatibleMajor) @? "Should not be compatible with different major version"
  not (isCompatible currentVersion incompatibleMinor) @? "Should not be compatible with too old minor version"
  not (isCompatible currentVersion missingFeatures) @? "Should not be compatible with missing features"

-- | Test feature flag operations
testFeatureFlags :: Assertion
testFeatureFlags = do
  -- Create a version with specific features
  let features = [BasicMessaging, RendezvousDiscovery, SecureTransport]
      flags = featuresToFlags features
      version = ProtocolVersion 1 0 0 flags
  
  -- Test features
  hasFeature version BasicMessaging @? "Should have BasicMessaging"
  hasFeature version RendezvousDiscovery @? "Should have RendezvousDiscovery"
  hasFeature version SecureTransport @? "Should have SecureTransport"
  
  not (hasFeature version PubSubSupport) @? "Should not have PubSubSupport"
  not (hasFeature version ContentAddressing) @? "Should not have ContentAddressing"
  
  -- Check if version has required features
  hasRequiredFeatures version [BasicMessaging, SecureTransport] @? "Should have required features"
  not (hasRequiredFeatures version [BasicMessaging, JSONEncoding]) @? "Should not have all required features"
  
  -- Check supported features list
  let supported = supportedFeatures version
  length supported @?= length features
  all (`elem` features) supported @? "Should return correct list of supported features"

-- | Test version validation
testVersionValidation :: Assertion
testVersionValidation = do
  -- Create test versions
  let compatibleVersion = ProtocolVersion 
          (majorVersion currentVersion)
          (minorVersion currentVersion)
          (patchVersion currentVersion + 1)
          (featureFlags currentVersion)
      
      incompatibleMajor = ProtocolVersion 
          (majorVersion currentVersion + 1)
          (minorVersion currentVersion)
          (patchVersion currentVersion)
          (featureFlags currentVersion)
      
      missingFeatures = ProtocolVersion
          (majorVersion currentVersion)
          (minorVersion currentVersion)
          (patchVersion currentVersion)
          0  -- No features
  
  -- Test validation
  let resultCompat = validateVersion currentVersion compatibleVersion
      resultMajor = validateVersion currentVersion incompatibleMajor
      resultFeatures = validateVersion currentVersion missingFeatures
  
  isRight resultCompat @? "Compatible version should validate successfully"
  isLeft resultMajor @? "Incompatible major version should fail validation"
  isLeft resultFeatures @? "Missing features should fail validation"
  
  -- Check error messages
  case resultMajor of
    Left err -> "Incompatible major version" `T.isInfixOf` err @? "Should mention major version in error"
    Right _ -> assertFailure "Should not be compatible"
  
  case resultFeatures of
    Left err -> "Missing required features" `T.isInfixOf` err @? "Should mention missing features in error"
    Right _ -> assertFailure "Should not be compatible"

-- | Test serialization and deserialization
testVersionSerialization :: Assertion
testVersionSerialization = do
  -- Create a test version
  let version = ProtocolVersion 1 2 3 123456
  
  -- Serialize to bytes
  let bytes = versionToBytes version
      reconstructed = bytesToVersion bytes
  
  -- Check length
  BS.length bytes @?= 20  -- 4 bytes each for major/minor/patch + 8 for flags
  
  -- Check reconstruction
  case reconstructed of
    Nothing -> assertFailure "Failed to deserialize bytes to version"
    Just v -> do
      majorVersion v @?= majorVersion version
      minorVersion v @?= minorVersion version
      patchVersion v @?= patchVersion version
      featureFlags v @?= featureFlags version
  
  -- Test with invalid data
  let tooShort = BS.take 10 bytes
      badDeserialization = bytesToVersion tooShort
  
  badDeserialization @?= Nothing 