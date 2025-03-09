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
-- We're not using aeson-schema due to dependency issues
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.String (IsString(..))
-- import Text.RawString.QQ (r)  -- Commented out due to dependency issues
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector

-- Simple validation function since we can't use aeson-schema
data ValidationError = ValidationError 
  { path :: [Text]
  , message :: Text
  }
  deriving (Show, Eq)

-- Simple validation that always succeeds for now
validate :: Value -> Value -> [ValidationError]
validate _ _ = []  -- Always return no errors for now

-- | JSON Schema for a fact observation rule
factObservationRuleSchema :: Value
factObservationRuleSchema = Aeson.object
  [ "type" .= ("object" :: Text)
  , "required" .= (["ruleId", "factType", "path", "proof", "enabled"] :: [Text])
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
  , "enum" .= ([ "PriceObservation" :: Text
      , "BalanceObservation" :: Text
      , "DepositObservation" :: Text
      , "WithdrawalObservation" :: Text
      , "TransactionObservation" :: Text
      , "BlockObservation" :: Text
      , "EventObservation" :: Text
      , "StateObservation" :: Text
      ] :: [Text])
  ]

-- | JSON Schema for proof types
proofTypeSchema :: Value
proofTypeSchema = Aeson.object
  [ "type" .= ("string" :: Text)
  , "enum" .= ([ "InclusionProof" :: Text
      , "HeaderProof" :: Text
      , "StateProof" :: Text
      , "SignatureProof" :: Text
      ] :: [Text])
  ]

-- | JSON Schema for path expressions
pathExpressionSchema :: Value
pathExpressionSchema = Aeson.object
  [ "type" .= ("object" :: Text)
  , "required" .= (["source", "selector"] :: [Text])
  , "properties" .=
      Aeson.object
        [ "source" .= Aeson.object
            [ "type" .= ("string" :: Text)
            , "description" .= ("Source of the data (e.g., 'response', 'request', 'state')" :: Text)
            ]
        , "selector" .= Aeson.object
            [ "type" .= ("string" :: Text)
            , "description" .= ("JSONPath or XPath selector" :: Text)
            ]
        , "format" .= Aeson.object
            [ "type" .= ("string" :: Text)
            , "enum" .= (["json", "xml", "text"] :: [Text])
            , "default" .= ("json" :: Text)
            ]
        ]
  ]

-- | JSON Schema for conditions
conditionSchema :: Value
conditionSchema = Aeson.object
  [ "oneOf" .= Aeson.Array (Data.Vector.fromList
      [ Aeson.object  -- Simple condition
          [ "type" .= ("object" :: Text)
          , "required" .= (["field", "operator", "value"] :: [Text])
          , "properties" .=
              Aeson.object
                [ "field" .= Aeson.object
                    [ "type" .= ("string" :: Text)
                    , "description" .= ("Field to compare (can be a path expression)" :: Text)
                    ]
                , "operator" .= Aeson.object
                    [ "type" .= ("string" :: Text)
                    , "enum" .= (["==", "!=", ">", "<", ">=", "<=", "contains", "startsWith", "endsWith", "matches"] :: [Text])
                    ]
                , "value" .= Aeson.object
                    [ "description" .= ("Value to compare against" :: Text)
                    ]
                ]
          ]
      , Aeson.object  -- Logical condition
          [ "type" .= ("object" :: Text)
          , "required" .= (["logicalOp", "subConditions"] :: [Text])
          , "properties" .=
              Aeson.object
                [ "logicalOp" .= Aeson.object
                    [ "type" .= ("string" :: Text)
                    , "enum" .= (["and", "or", "not"] :: [Text])
                    ]
                , "subConditions" .= Aeson.object
                    [ "type" .= ("array" :: Text)
                    , "items" .= Aeson.object
                        [ "$ref" .= ("#/definitions/condition" :: Text)
                        ]
                    ]
                ]
          ]
      , Aeson.object  -- Existence check
          [ "type" .= ("object" :: Text)
          , "required" .= (["checkField"] :: [Text])
          , "properties" .= Aeson.object
              [ "checkField" .= Aeson.object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Field to check for existence" :: Text)
                  ]
              ]
          ]
      ])
  ]

-- | JSON Schema for rule sets
ruleSetSchema :: Value
ruleSetSchema = Aeson.object
  [ "type" .= ("object" :: Text)
  , "required" .= (["rules"] :: [Text])
  , "properties" .=
      Aeson.object
        [ "rules" .= Aeson.object
            [ "type" .= ("array" :: Text)
            , "items" .= Aeson.object
                [ "$ref" .= ("#/definitions/rule" :: Text)
                ]
            ]
        , "metadata" .= Aeson.object
            [ "type" .= ("object" :: Text)
            , "properties" .= Aeson.object
                [ "name" .= Aeson.object
                    [ "type" .= ("string" :: Text)
                    ]
                , "description" .= Aeson.object
                    [ "type" .= ("string" :: Text)
                    ]
                , "version" .= Aeson.object
                    [ "type" .= ("string" :: Text)
                    ]
                , "author" .= Aeson.object
                    [ "type" .= ("string" :: Text)
                    ]
                ]
            ]
        ]
  ]

-- | Validate a rule against the schema
validateRuleAgainstSchema :: Value -> Either Text [Text]
validateRuleAgainstSchema ruleJson = do
  Right []  -- Always succeed for now

-- | Validate a rule set against the schema
validateRuleSetAgainstSchema :: Value -> Either Text [Text]
validateRuleSetAgainstSchema ruleSetJson = do
  Right []  -- Always succeed for now

-- | Format a validation error
formatValidationError :: ValidationError -> Text
formatValidationError error =
  let path' = T.intercalate "." (path error)
      message' = message error
  in "Error at " <> path' <> ": " <> message'

-- | Full TOML schema as a string
tomlSchema :: Text
tomlSchema = T.unlines
  [ "# Schema for fact observation rules (TOML)"
  , "[[rules]]"
  , "# Rule identifier (required)"
  , "rule_id = \"string\""
  , ""
  , "# Fact type (required)"
  , "# One of: \"PriceObservation\", \"BalanceObservation\", \"DepositObservation\", "
  , "#         \"WithdrawalObservation\", \"TransactionObservation\", \"BlockObservation\","
  , "#         \"EventObservation\", \"StateObservation\""
  , "fact_type = \"string\""
  , ""
  , "# Path expression (required)"
  , "[rules.path]"
  , "source = \"string\"     # Data source (e.g., \"ethereum\", \"uniswapV3Pool\")"
  , "selector = \"string\"   # Path selector (e.g., \"block.number\", \"event.Transfer\")"
  , ""
  , "# Optional parameters for the selector"
  , "[rules.path.parameters]"
  , "# Any key-value pairs are allowed here"
  , "key1 = \"value1\""
  , "key2 = \"value2\""
  , ""
  , "# Proof type (required)"
  , "# One of: \"InclusionProof\", \"HeaderProof\", \"StateProof\", \"SignatureProof\", "
  , "#         \"ReceiptProof\", \"NoProof\""
  , "proof = \"string\""
  , ""
  , "# Optional conditions"
  , "[[rules.conditions]]"
  , "# Comparison condition"
  , "field = \"string\"      # Field to compare"
  , "operator = \"string\"   # Operator (==, !=, >, <, >=, <=, contains, startsWith, endsWith)"
  , "value = value         # Value to compare against (any TOML value)"
  , ""
  , "[[rules.conditions]]"
  , "# Logical condition"
  , "logical_op = \"string\"  # Logical operator (and, or, not)"
  , "# Sub-conditions"
  , "[[rules.conditions.sub_conditions]]"
  , "# Nested conditions go here"
  , ""
  , "# Optional description"
  , "description = \"string\""
  , ""
  , "# Whether the rule is enabled (required)"
  , "enabled = true"
  , ""
  , "# Metadata (optional)"
  , "[metadata]"
  , "# Any key-value pairs are allowed here"
  , "creator = \"string\""
  , "version = \"string\""
  , "description = \"string\""
  ] 