{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hPutStrLn, stderr)

import FactObservation

-- | Create a sample rule
sampleRule :: FactObservationRule
sampleRule = FactObservationRule
  { ruleId = "price-observation-001"
  , factType = PriceObservation
  , proofType = InclusionProof
  , enabled = True
  , description = Just "Ethereum ETH/USD price observation"
  , path = PathExpression
      { source = "ethereum"
      , selector = "data.price.eth_usd"
      , parameters = Map.fromList [("network", "mainnet"), ("provider", "infura")]
      }
  , conditions = 
      [ Condition 
          { field = "timestamp"
          , operator = Just ">"
          , value = Just $ Aeson.toJSON (1609459200 :: Integer) -- 2021-01-01
          }
      , Condition
          { field = "confidence"
          , operator = Just ">="
          , value = Just $ Aeson.toJSON (0.95 :: Double)
          }
      ]
  , metadata = Map.fromList 
      [ ("version", "1.0.0")
      , ("author", "Time Bandits")
      ]
  }

-- | Create another sample rule
sampleRule2 :: FactObservationRule
sampleRule2 = FactObservationRule
  { ruleId = "balance-observation-001"
  , factType = BalanceObservation
  , proofType = StateProof
  , enabled = True
  , description = Just "Ethereum account balance observation"
  , path = PathExpression
      { source = "ethereum"
      , selector = "account.balance"
      , parameters = Map.fromList 
          [ ("address", "0x1234567890abcdef1234567890abcdef12345678")
          , ("network", "mainnet")
          ]
      }
  , conditions = 
      [ Condition 
          { field = "balance"
          , operator = Just ">"
          , value = Just $ Aeson.toJSON (1000000000000000000 :: Integer) -- 1 ETH
          }
      ]
  , metadata = Map.fromList 
      [ ("version", "1.0.0")
      , ("author", "Time Bandits")
      ]
  }

-- | Print a rule in a readable format
printRule :: FactObservationRule -> IO ()
printRule rule = do
  putStrLn $ replicate 40 '-'
  putStrLn $ "Rule ID: " ++ T.unpack (ruleId rule)
  putStrLn $ "Fact Type: " ++ show (factType rule)
  putStrLn $ "Proof Type: " ++ show (proofType rule)
  putStrLn $ "Enabled: " ++ show (enabled rule)
  case description rule of
    Nothing -> return ()
    Just desc -> putStrLn $ "Description: " ++ T.unpack desc
  
  putStrLn "Path Expression:"
  putStrLn $ "  Source: " ++ T.unpack (source $ path rule)
  putStrLn $ "  Selector: " ++ T.unpack (selector $ path rule)
  putStrLn "  Parameters:"
  mapM_ (\(k, v) -> putStrLn $ "    " ++ T.unpack k ++ ": " ++ T.unpack v) 
        (Map.toList $ parameters $ path rule)
  
  putStrLn "Conditions:"
  mapM_ printCondition (conditions rule)
  
  putStrLn "Metadata:"
  mapM_ (\(k, v) -> putStrLn $ "  " ++ T.unpack k ++ ": " ++ T.unpack v) 
        (Map.toList $ metadata rule)

-- | Print a condition in a readable format
printCondition :: Condition -> IO ()
printCondition cond = do
  putStr $ "  " ++ T.unpack (field cond)
  case operator cond of
    Nothing -> return ()
    Just op -> putStr $ " " ++ T.unpack op
  case value cond of
    Nothing -> return ()
    Just val -> putStr $ " " ++ show val
  putStrLn ""

-- | Print validation results
printValidationResult :: Text -> Either ValidationError () -> IO ()
printValidationResult ruleId result = do
  putStr $ "Validation for rule " ++ T.unpack ruleId ++ ": "
  case result of
    Right () -> putStrLn "Valid"
    Left err -> putStrLn $ "Invalid: " ++ show err

-- | Main function
main :: IO ()
main = do
  putStrLn "Fact Observation Test"
  putStrLn "===================="
  
  -- Create a rule set
  let ruleSet = createRuleSet
  
  -- Add the first rule
  putStrLn "\nAdding rule 1:"
  let result1 = addRule sampleRule ruleSet
  case result1 of
    Left err -> hPutStrLn stderr $ "Error adding rule 1: " ++ show err
    Right rs1 -> do
      printRule sampleRule
      
      -- Add the second rule
      putStrLn "\nAdding rule 2:"
      let result2 = addRule sampleRule2 rs1
      case result2 of
        Left err -> hPutStrLn stderr $ "Error adding rule 2: " ++ show err
        Right rs2 -> do
          printRule sampleRule2
          
          -- Find rules by fact type
          putStrLn "\nRules for PriceObservation:"
          let priceRules = findRulesByFactType PriceObservation rs2
          mapM_ (\r -> putStrLn $ "- " ++ T.unpack (ruleId r)) priceRules
          
          putStrLn "\nRules for BalanceObservation:"
          let balanceRules = findRulesByFactType BalanceObservation rs2
          mapM_ (\r -> putStrLn $ "- " ++ T.unpack (ruleId r)) balanceRules
          
          -- Validate the rules
          putStrLn "\nValidation Results:"
          printValidationResult (ruleId sampleRule) (validateRule sampleRule)
          printValidationResult (ruleId sampleRule2) (validateRule sampleRule2)
          
          -- Create an invalid rule
          let invalidRule = sampleRule { ruleId = "" }
          printValidationResult "invalid-rule" (validateRule invalidRule)
          
          -- Print rules as JSON
          putStrLn "\nRule 1 as JSON:"
          LBS.putStr $ AesonPretty.encodePretty sampleRule
          putStrLn "\n\nRule 2 as JSON:"
          LBS.putStr $ AesonPretty.encodePretty sampleRule2 