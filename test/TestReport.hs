{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
-- This file was temporarily disabled for compatibility
module TestReport (
  TestReport,
  TestResult(..),
  ResultStatus(..),
  emptyReport,
  addTestResult,
  writeReport
) where

-- Import Text properly to avoid ambiguity
import qualified Data.Text as T

-- Text type is from the TextShow instance in scope, no need to import Data.Text
-- import Data.Text (Text)
-- FilePath is imported from Prelude, so we don't need this explicit import
-- import System.FilePath (FilePath)

data TestReport = TestReport deriving stock Show
data TestResult = TestResult {
  testName :: T.Text,
  testModule :: T.Text,
  testDescription :: T.Text,
  testStatus :: ResultStatus,
  testDuration :: Double,
  testMessage :: Maybe T.Text
} deriving stock Show
data ResultStatus = Pass | Fail | Skip deriving stock Show

emptyReport :: IO TestReport
emptyReport = return TestReport

addTestResult :: TestReport -> TestResult -> TestReport
addTestResult report _ = report

writeReport :: FilePath -> TestReport -> IO ()
writeReport _ _ = return ()
