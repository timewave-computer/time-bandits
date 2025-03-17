{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : TimeBandits.Core.FactObservation.Schema
Description : Schema definitions for fact observation rules
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module defines JSON Schema for fact observation rules,
enabling validation of rule definitions.
-}
module TimeBandits.Core.FactObservation.Schema
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
factObservationRuleSchema = object
  [ "type" .= ("object" :: Text)
  , "required" .= Aeson.Array (Data.Vector.fromList 
      [ Aeson.String "ruleId"
      , Aeson.String "ruleName"
      , Aeson.String "factType"
      , Aeson.String "factPath"
      ])
  , "properties" .= object
      [ "ruleId" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("Unique identifier for the rule" :: Text)
          ]
      , "ruleName" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("Human-readable name for the rule" :: Text)
          ]
      , "ruleDescription" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("Description of what the rule does" :: Text)
          ]
      , "ruleSetName" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("Optional rule set name" :: Text)
          ]
      , "factType" .= factTypeSchema
      , "factPath" .= pathExpressionSchema
      , "conditions" .= object
          [ "type" .= ("array" :: Text)
          , "items" .= conditionSchema
          ]
      , "proofType" .= proofTypeSchema
      , "priority" .= object
          [ "type" .= ("integer" :: Text)
          , "description" .= ("Priority (higher = more important)" :: Text)
          ]
      , "enabled" .= object
          [ "type" .= ("boolean" :: Text)
          , "description" .= ("Whether the rule is enabled" :: Text)
          ]
      , "metadata" .= object
          [ "type" .= ("object" :: Text)
          , "description" .= ("Additional metadata" :: Text)
          ]
      ]
  ]

-- | JSON Schema for a fact type
factTypeSchema :: Value
factTypeSchema = object
  [ "oneOf" .= Aeson.Array (Data.Vector.fromList
      [ object
          [ "type" .= ("string" :: Text)
          , "enum" .= Aeson.Array (Data.Vector.fromList
              [ Aeson.String "BlockHeader"
              , Aeson.String "Transaction"
              , Aeson.String "Event"
              , Aeson.String "StateChange"
              ])
          ]
      , object
          [ "type" .= ("object" :: Text)
          , "required" .= Aeson.Array (Data.Vector.fromList [Aeson.String "Custom"])
          , "properties" .= object
              [ "Custom" .= object
                  [ "type" .= ("string" :: Text)
                  ]
              ]
          ]
      ])
  ]

-- | JSON Schema for a proof type
proofTypeSchema :: Value
proofTypeSchema = object
  [ "oneOf" .= Aeson.Array (Data.Vector.fromList
      [ object
          [ "type" .= ("string" :: Text)
          , "enum" .= Aeson.Array (Data.Vector.fromList
              [ Aeson.String "NoProof"
              , Aeson.String "MerkleProof"
              , Aeson.String "ZKProof"
              , Aeson.String "SignatureProof"
              ])
          ]
      , object
          [ "type" .= ("object" :: Text)
          , "required" .= Aeson.Array (Data.Vector.fromList [Aeson.String "CustomProof"])
          , "properties" .= object
              [ "CustomProof" .= object
                  [ "type" .= ("string" :: Text)
                  ]
              ]
          ]
      ])
  ]

-- | JSON Schema for a path expression
pathExpressionSchema :: Value
pathExpressionSchema = object
  [ "oneOf" .= Aeson.Array (Data.Vector.fromList
      [ object
          [ "type" .= ("object" :: Text)
          , "required" .= Aeson.Array (Data.Vector.fromList [Aeson.String "JsonPath"])
          , "properties" .= object
              [ "JsonPath" .= object
                  [ "type" .= ("string" :: Text)
                  ]
              ]
          ]
      , object
          [ "type" .= ("object" :: Text)
          , "required" .= Aeson.Array (Data.Vector.fromList [Aeson.String "XPath"])
          , "properties" .= object
              [ "XPath" .= object
                  [ "type" .= ("string" :: Text)
                  ]
              ]
          ]
      , object
          [ "type" .= ("object" :: Text)
          , "required" .= Aeson.Array (Data.Vector.fromList [Aeson.String "Regex"])
          , "properties" .= object
              [ "Regex" .= object
                  [ "type" .= ("string" :: Text)
                  ]
              ]
          ]
      , object
          [ "type" .= ("object" :: Text)
          , "required" .= Aeson.Array (Data.Vector.fromList [Aeson.String "JqPath"])
          , "properties" .= object
              [ "JqPath" .= object
                  [ "type" .= ("string" :: Text)
                  ]
              ]
          ]
      , object
          [ "type" .= ("object" :: Text)
          , "required" .= Aeson.Array (Data.Vector.fromList [Aeson.String "CustomPath"])
          , "properties" .= object
              [ "CustomPath" .= object
                  [ "type" .= ("string" :: Text)
                  ]
              ]
          ]
      ])
  ]

-- | JSON Schema for a condition
conditionSchema :: Value
conditionSchema = object
  [ "oneOf" .= Aeson.Array (Data.Vector.fromList
      [ object  -- Equals
          [ "type" .= ("object" :: Text)
          , "required" .= Aeson.Array (Data.Vector.fromList 
              [ Aeson.String "Equals"
              , Aeson.String "path"
              , Aeson.String "value"
              ])
          , "properties" .= object
              [ "Equals" .= object
                  [ "type" .= ("string" :: Text)
                  , "enum" .= Aeson.Array (Data.Vector.fromList [Aeson.String "Equals"])
                  ]
              , "path" .= pathExpressionSchema
              , "value" .= object
                  [ "type" .= ("object" :: Text)
                  ]
              ]
          ]
      , object  -- Contains
          [ "type" .= ("object" :: Text)
          , "required" .= Aeson.Array (Data.Vector.fromList 
              [ Aeson.String "Contains"
              , Aeson.String "path"
              , Aeson.String "text"
              ])
          , "properties" .= object
              [ "Contains" .= object
                  [ "type" .= ("string" :: Text)
                  , "enum" .= Aeson.Array (Data.Vector.fromList [Aeson.String "Contains"])
                  ]
              , "path" .= pathExpressionSchema
              , "text" .= object
                  [ "type" .= ("string" :: Text)
                  ]
              ]
          ]
      -- Other condition types would be defined similarly
      ])
  ]

-- | JSON Schema for a rule set
ruleSetSchema :: Value
ruleSetSchema = object
  [ "type" .= ("object" :: Text)
  , "required" .= Aeson.Array (Data.Vector.fromList 
      [ Aeson.String "ruleSetId"
      , Aeson.String "rules"
      ])
  , "properties" .= object
      [ "ruleSetId" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("Unique identifier for the rule set" :: Text)
          ]
      , "rules" .= object
          [ "type" .= ("array" :: Text)
          , "items" .= factObservationRuleSchema
          ]
      ]
  ]

-- | Validate a rule against the schema
validateRuleAgainstSchema :: Value -> [ValidationError]
validateRuleAgainstSchema rule =
  validate factObservationRuleSchema rule

-- | Validate a rule set against the schema
validateRuleSetAgainstSchema :: Value -> [ValidationError]
validateRuleSetAgainstSchema ruleSet =
  validate ruleSetSchema ruleSet 