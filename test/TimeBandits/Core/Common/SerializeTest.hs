{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TimeBandits.Core.Common.SerializeTest (spec) where

import Test.Hspec
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import Data.Serialize (encode, decode)
import qualified Data.ByteString as BS
import Control.Monad (forM_)

-- Import *only* the standardized serialization implementation to avoid conflicts
import TimeBandits.Core.Common.Serialize ()

-- | Epoch time reference (start of 1970)
epochUTCTime :: UTCTime
epochUTCTime = UTCTime (fromGregorian 1970 1 1) 0

-- Test serialization for UTCTime
spec :: Spec
spec = describe "TimeBandits.Core.Common.Serialize" $ do
  describe "UTCTime serialization" $ do
    let testTimes = 
          [ epochUTCTime                                     -- Epoch time
          , UTCTime (fromGregorian 2023 10 15) 36000         -- Recent time
          , UTCTime (fromGregorian 2050 12 31) 86399         -- Future time
          , UTCTime (fromGregorian 1900 1 1) 0               -- Past time
          ]
    
    -- Test round-trip serialization
    it "should round-trip correctly for various times" $ do
      forM_ testTimes $ \time -> do
        let encoded = encode time
            decoded = decode encoded :: Either String UTCTime
        decoded `shouldBe` Right time
    
    -- Verify stability of encoding
    it "should encode epoch time as expected" $ do
      let encoded = encode epochUTCTime
      -- Epoch time should encode as 0.0
      -- Standard serialization for a Double.
      BS.length encoded `shouldBe` 8  -- Double takes 8 bytes
      
    -- Ensure future times are encoded efficiently
    it "should encode future times efficiently" $ do
      let futureTime = UTCTime (fromGregorian 2050 12 31) 86399
          encoded = encode futureTime
      BS.length encoded `shouldBe` 8  -- Should still be just 8 bytes (Double) 