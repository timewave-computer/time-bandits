{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
-- This file was temporarily disabled for compatibility
module TimeBandits.TestReport (
  TestReport,
  TestResult(..),
  ResultStatus(..),
  emptyReport,
  addTestResult,
  writeReport
) where

import Data.Text (Text)

data TestReport = TestReport deriving stock Show
data TestResult = TestResult {
  testName :: Text,
  testModule :: Text,
  testDescription :: Text,
  testStatus :: ResultStatus,
  testDuration :: Double,
  testMessage :: Maybe Text
} deriving stock Show
data ResultStatus = Pass | Fail | Skip deriving stock Show

emptyReport :: IO TestReport
emptyReport = return TestReport

addTestResult :: TestReport -> TestResult -> TestReport
addTestResult report _ = report

writeReport :: FilePath -> TestReport -> IO ()
writeReport _ _ = return ()
