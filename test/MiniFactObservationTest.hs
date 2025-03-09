{-# LANGUAGE OverloadedStrings #-}

-- A minimal standalone test for FactObservation rules, with minimal dependencies

import Control.Monad (unless)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort)

-- Simplified FactObservation rule types
data FactType = Transaction | Block | Event | Receipt
  deriving (Show, Eq)

data ProofType = None | Merkle | ZKSnark | Signature
  deriving (Show, Eq)

data PathExpression
  = Root
  | Key Text PathExpression
  | Index Int PathExpression
  | End
  deriving (Show, Eq)

data Condition
  = Equals PathExpression Text
  | Contains PathExpression Text
  | GreaterThan PathExpression Int
  | LessThan PathExpression Int
  | And Condition Condition
  | Or Condition Condition
  deriving (Show, Eq)

data FactObservationRule = FactObservationRule
  { ruleName :: Text
  , ruleDescription :: Text
  , factType :: FactType
  , proofType :: ProofType
  , targetPath :: PathExpression
  , conditions :: [Condition]
  } deriving (Show, Eq)

data ValidationError
  = InvalidPathExpression Text
  | EmptyRuleName
  | InvalidCondition Text
  | UnsupportedProofType
  deriving (Show, Eq)

-- Simplified path expression validator
validatePathExpression :: PathExpression -> Either ValidationError ()
validatePathExpression Root = Right ()
validatePathExpression End = Right ()
validatePathExpression (Key k rest) =
  if T.null k
    then Left $ InvalidPathExpression "Empty key not allowed"
    else validatePathExpression rest
validatePathExpression (Index i rest) =
  if i < 0
    then Left $ InvalidPathExpression "Negative index not allowed"
    else validatePathExpression rest

-- Simplified rule validation
validateRule :: FactObservationRule -> Either ValidationError ()
validateRule rule = do
  if T.null (ruleName rule)
    then Left EmptyRuleName
    else do
      validatePathExpression (targetPath rule)
      -- In a real implementation, we would validate each condition too
      Right ()

-- Test FactObservation rules
testRuleValidation :: IO ()
testRuleValidation = do
  putStrLn "Testing FactObservation rule validation..."
  
  let validRule = FactObservationRule
        { ruleName = "eth_transfer"
        , ruleDescription = "Ethereum transfer"
        , factType = Transaction
        , proofType = Merkle
        , targetPath = Key "value" (Key "transfer" End)
        , conditions = [GreaterThan (Key "amount" End) 0]
        }
        
  let invalidRule = FactObservationRule
        { ruleName = ""  -- Empty name
        , ruleDescription = "Invalid rule"
        , factType = Transaction
        , proofType = Merkle
        , targetPath = Key "value" End
        , conditions = []
        }
  
  case validateRule validRule of
    Left err -> putStrLn $ "Valid rule validation failed: " ++ show err
    Right () -> putStrLn "Valid rule validated successfully!"
    
  case validateRule invalidRule of
    Left EmptyRuleName -> putStrLn "Correctly rejected rule with empty name"
    Left err -> putStrLn $ "Rejected with unexpected error: " ++ show err
    Right () -> putStrLn "FAILED: Accepted invalid rule!"

main :: IO ()
main = do
  putStrLn "Running simplified FactObservation tests"
  testRuleValidation 