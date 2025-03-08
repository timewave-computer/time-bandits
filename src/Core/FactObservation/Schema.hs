{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : Core.FactObservation.Schema
Description : Schema definitions for fact observation rules
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module defines JSON Schema for fact observation rules,
enabling validation of rule definitions.
-}
module Core.FactObservation.Schema
  ( -- * Schema Definitions
    factObservationRuleSchema
  , factTypeSchema
  , proofTypeSchema
  , pathExpressionSchema
  , conditionSchema
  , ruleSetSchema
  
    -- * Schema Validation
  , validateRuleAgainstSchema
  , validateRuleSetAgainstSchema
  ) where

import Data.Aeson (Value, FromJSON, ToJSON, encode, decode, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Schema as Schema
import qualified Data.Aeson.Schema.Validator as Validator
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.String (IsString(..))
import Text.RawString.QQ (r)
import qualified Data.ByteString.Lazy as LBS

-- | JSON Schema for a fact observation rule
factObservationRuleSchema :: Value
factObservationRuleSchema = Aeson.object
  [ "type" .= ("object" :: Text)
  , "required" .= ["ruleId", "factType", "path", "proof", "enabled"]
  , "properties" .= Aeson.object
      [ "ruleId" .= Aeson.object
          [ "type" .= ("string" :: Text)
          , "minLength" .= (1 :: Int)
          , "maxLength" .= (64 :: Int)
          , "pattern" .= ("^[a-zA-Z0-9_.-]+$" :: Text)
          ]
      , "factType" .= factTypeSchema
      , "path" .= pathExpressionSchema
      , "proof" .= proofTypeSchema
      , "conditions" .= Aeson.object
          [ "type" .= ("array" :: Text)
          , "items" .= conditionSchema
          ]
      , "description" .= Aeson.object
          [ "type" .= ("string" :: Text)
          ]
      , "enabled" .= Aeson.object
          [ "type" .= ("boolean" :: Text)
          ]
      ]
  ]

-- | JSON Schema for fact types
factTypeSchema :: Value
factTypeSchema = Aeson.object
  [ "type" .= ("string" :: Text)
  , "enum" .=
      [ "PriceObservation"
      , "BalanceObservation"
      , "DepositObservation"
      , "WithdrawalObservation"
      , "TransactionObservation"
      , "BlockObservation"
      , "EventObservation"
      , "StateObservation"
      ]
  ]

-- | JSON Schema for proof types
proofTypeSchema :: Value
proofTypeSchema = Aeson.object
  [ "type" .= ("string" :: Text)
  , "enum" .=
      [ "InclusionProof"
      , "HeaderProof"
      , "StateProof"
      , "SignatureProof"
      , "ReceiptProof"
      , "NoProof"
      ]
  ]

-- | JSON Schema for path expressions
pathExpressionSchema :: Value
pathExpressionSchema = Aeson.object
  [ "type" .= ("object" :: Text)
  , "required" .= ["source", "selector"]
  , "properties" .= Aeson.object
      [ "source" .= Aeson.object
          [ "type" .= ("string" :: Text)
          , "minLength" .= (1 :: Int)
          ]
      , "selector" .= Aeson.object
          [ "type" .= ("string" :: Text)
          , "minLength" .= (1 :: Int)
          ]
      , "parameters" .= Aeson.object
          [ "type" .= ("object" :: Text)
          , "additionalProperties" .= Aeson.object
              [ "type" .= ("string" :: Text)
              ]
          ]
      ]
  ]

-- | JSON Schema for conditions
conditionSchema :: Value
conditionSchema = Aeson.object
  [ "oneOf" .=
      [ Aeson.object  -- Comparison condition
          [ "type" .= ("object" :: Text)
          , "required" .= ["field", "operator", "value"]
          , "properties" .= Aeson.object
              [ "field" .= Aeson.object
                  [ "type" .= ("string" :: Text)
                  , "minLength" .= (1 :: Int)
                  ]
              , "operator" .= Aeson.object
                  [ "type" .= ("string" :: Text)
                  , "enum" .=
                      [ "=="
                      , "!="
                      , ">"
                      , "<"
                      , ">="
                      , "<="
                      , "contains"
                      , "startsWith"
                      , "endsWith"
                      ]
                  ]
              , "value" .= Aeson.object []  -- Any value is allowed
              ]
          ]
      , Aeson.object  -- Logical condition
          [ "type" .= ("object" :: Text)
          , "required" .= ["logicalOp", "subConditions"]
          , "properties" .= Aeson.object
              [ "logicalOp" .= Aeson.object
                  [ "type" .= ("string" :: Text)
                  , "enum" .= ["and", "or", "not"]
                  ]
              , "subConditions" .= Aeson.object
                  [ "type" .= ("array" :: Text)
                  , "items" .= conditionSchema  -- Recursive reference
                  , "minItems" .= (1 :: Int)
                  ]
              ]
          ]
      , Aeson.object  -- Exists condition
          [ "type" .= ("object" :: Text)
          , "required" .= ["checkField"]
          , "properties" .= Aeson.object
              [ "checkField" .= Aeson.object
                  [ "type" .= ("string" :: Text)
                  , "minLength" .= (1 :: Int)
                  ]
              ]
          ]
      ]
  ]

-- | JSON Schema for a rule set
ruleSetSchema :: Value
ruleSetSchema = Aeson.object
  [ "type" .= ("object" :: Text)
  , "required" .= ["rules"]
  , "properties" .= Aeson.object
      [ "rules" .= Aeson.object
          [ "type" .= ("array" :: Text)
          , "items" .= factObservationRuleSchema
          ]
      , "metadata" .= Aeson.object
          [ "type" .= ("object" :: Text)
          , "additionalProperties" .= Aeson.object
              [ "type" .= ("string" :: Text)
              ]
          ]
      ]
  ]

-- | Validate a rule against the schema
validateRuleAgainstSchema :: Value -> Either Text [Text]
validateRuleAgainstSchema ruleJson = do
  case Validator.validate factObservationRuleSchema ruleJson of
    [] -> Right []  -- No errors
    errors -> Left $ T.unlines $ map formatValidationError errors

-- | Validate a rule set against the schema
validateRuleSetAgainstSchema :: Value -> Either Text [Text]
validateRuleSetAgainstSchema ruleSetJson = do
  case Validator.validate ruleSetSchema ruleSetJson of
    [] -> Right []  -- No errors
    errors -> Left $ T.unlines $ map formatValidationError errors

-- | Format a validation error
formatValidationError :: Validator.ValidationError -> Text
formatValidationError error =
  let path = T.intercalate "." (Validator.path error)
      message = Validator.message error
  in "Error at " <> path <> ": " <> message

-- | Full TOML schema as a string
tomlSchema :: Text
tomlSchema = [r|
# Schema for fact observation rules (TOML)
[[rules]]
# Rule identifier (required)
rule_id = "string"

# Fact type (required)
# One of: "PriceObservation", "BalanceObservation", "DepositObservation", 
#         "WithdrawalObservation", "TransactionObservation", "BlockObservation",
#         "EventObservation", "StateObservation"
fact_type = "string"

# Path expression (required)
[rules.path]
source = "string"     # Data source (e.g., "ethereum", "uniswapV3Pool")
selector = "string"   # Path selector (e.g., "block.number", "event.Transfer")

# Optional parameters for the selector
[rules.path.parameters]
# Any key-value pairs are allowed here
key1 = "value1"
key2 = "value2"

# Proof type (required)
# One of: "InclusionProof", "HeaderProof", "StateProof", "SignatureProof", 
#         "ReceiptProof", "NoProof"
proof = "string"

# Optional conditions
[[rules.conditions]]
# Comparison condition
field = "string"      # Field to compare
operator = "string"   # Operator (==, !=, >, <, >=, <=, contains, startsWith, endsWith)
value = value         # Value to compare against (any TOML value)

[[rules.conditions]]
# Logical condition
logical_op = "string"  # Logical operator (and, or, not)
# Sub-conditions
[[rules.conditions.sub_conditions]]
# Nested conditions go here

# Optional description
description = "string"

# Whether the rule is enabled (required)
enabled = true

# Metadata (optional)
[metadata]
# Any key-value pairs are allowed here
creator = "string"
version = "string"
description = "string"
|] 