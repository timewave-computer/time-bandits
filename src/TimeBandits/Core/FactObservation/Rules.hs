{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : TimeBandits.Core.FactObservation.Rules
Description : Fact observation rules for blockchain data extraction
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module defines fact observation rules for extracting relevant facts
from blockchain data for Time Keepers.
-}
module TimeBandits.Core.FactObservation.Rules
  ( -- * Rule Types
    FactObservationRule(..)
  , FactType(..)
  , ProofType(..)
  , PathExpression(..)
  , Condition(..)
  
    -- * Common Fact Types (Pattern Synonyms)
  , pattern BalanceObservation
  , pattern PriceObservation  
  , pattern EventObservation
  , pattern InclusionProof
  
    -- * Validation
  , validateRule
  , ValidationError(..)
  , validatePathExpression
  
    -- * Rule Management
  , RuleSet(..)
  , createRuleSet
  , addRule
  , removeRule
  , findRulesByFactType
  , ruleSetRules
  , rules
  
    -- * Helper Functions
  , forceAddRule
  , addRuleToSet
  , deduplicateRules
  
    -- * Record Field Names
  , ruleId
  , ruleName
  , ruleDescription
  , ruleSetName
  , factType
  , factPath
  , conditions
  , proofType
  , priority
  , enabled
  , metadata
  
    -- * Compatibility Fields (used in tests)
  , description
  , path
  , proof
  
    -- * Path Expression Fields
  , source
  , selector
  , parameters
  
    -- * Condition Fields
  , field
  , operator
  , value
  
    -- * Helper Functions for Tests
  , makeComparisonCondition
  , makePathExpression
  ) where

import Control.Monad (when)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe, isJust)
import Data.List (nub)

-- | Type of fact being observed
data FactType
  = BlockHeader       -- ^ Block header information
  | Transaction       -- ^ Transaction details
  | Event             -- ^ Smart contract event
  | StateChange       -- ^ State change in the blockchain
  | Custom Text       -- ^ Custom fact type with name
  deriving (Show, Eq, Generic)

instance FromJSON FactType
instance ToJSON FactType

-- | Type of proof to generate
data ProofType
  = NoProof           -- ^ No proof required
  | MerkleProof       -- ^ Merkle proof
  | ZKProof           -- ^ Zero-knowledge proof
  | SignatureProof    -- ^ Signature-based proof
  | CustomProof Text  -- ^ Custom proof type with name
  deriving (Show, Eq, Generic)

instance FromJSON ProofType
instance ToJSON ProofType

-- | Path expression for extracting data
data PathExpression
  = JsonPath Text     -- ^ JSONPath expression
  | XPath Text        -- ^ XPath expression
  | Regex Text        -- ^ Regular expression
  | JqPath Text       -- ^ jq expression
  | CustomPath Text   -- ^ Custom path expression
  deriving (Show, Eq, Generic)

instance FromJSON PathExpression
instance ToJSON PathExpression

-- | Condition for rule matching
data Condition
  = Equals PathExpression Value       -- ^ Path equals value
  | Contains PathExpression Text      -- ^ Path contains text
  | GreaterThan PathExpression Value  -- ^ Path > value
  | LessThan PathExpression Value     -- ^ Path < value
  | Exists PathExpression             -- ^ Path exists
  | NotExists PathExpression          -- ^ Path does not exist
  | And [Condition]                   -- ^ All conditions must be true
  | Or [Condition]                    -- ^ Any condition must be true
  | Not Condition                     -- ^ Condition must be false
  deriving (Show, Eq, Generic)

instance FromJSON Condition
instance ToJSON Condition

-- | Rule for fact observation
data FactObservationRule = FactObservationRule
  { ruleId :: Text                    -- ^ Unique identifier for the rule
  , ruleName :: Text                  -- ^ Human-readable name
  , ruleDescription :: Text           -- ^ Description of what the rule does
  , ruleSetName :: Maybe Text         -- ^ Optional rule set name
  , factType :: FactType              -- ^ Type of fact to extract
  , factPath :: PathExpression        -- ^ Path to extract the fact
  , conditions :: [Condition]         -- ^ Conditions for rule to apply
  , proofType :: ProofType            -- ^ Type of proof to generate
  , priority :: Int                   -- ^ Priority (higher = more important)
  , enabled :: Bool                   -- ^ Whether the rule is enabled
  , metadata :: Map Text Value        -- ^ Additional metadata
  } deriving (Show, Eq, Generic)

instance FromJSON FactObservationRule
instance ToJSON FactObservationRule

-- | Errors that can occur during rule validation
data ValidationError
  = MissingRequiredField Text         -- ^ Required field is missing
  | InvalidPathExpression Text        -- ^ Path expression is invalid
  | InvalidCondition Text             -- ^ Condition is invalid
  | DuplicateRuleId Text              -- ^ Rule ID already exists
  | InvalidFactType Text              -- ^ Fact type is invalid
  | InvalidProofType Text             -- ^ Proof type is invalid
  deriving (Show, Eq)

-- | Validate a rule
validateRule :: FactObservationRule -> Either ValidationError ()
validateRule rule = do
  -- Check required fields
  when (T.null (ruleId rule)) $
    Left $ MissingRequiredField "ruleId"
  when (T.null (ruleName rule)) $
    Left $ MissingRequiredField "ruleName"
  
  -- Validate path expression
  validatePathExpression (factPath rule)
  
  -- Validate conditions
  mapM_ validateCondition (conditions rule)
  
  -- All checks passed
  return ()

-- | Validate a path expression
validatePathExpression :: PathExpression -> Either ValidationError ()
validatePathExpression pathExpr =
  case pathExpr of
    JsonPath path ->
      when (T.null path) $
        Left $ InvalidPathExpression "JSONPath cannot be empty"
    XPath path ->
      when (T.null path) $
        Left $ InvalidPathExpression "XPath cannot be empty"
    Regex regexPattern ->
      when (T.null regexPattern) $
        Left $ InvalidPathExpression "Regex pattern cannot be empty"
    JqPath path ->
      when (T.null path) $
        Left $ InvalidPathExpression "jq path cannot be empty"
    CustomPath path ->
      when (T.null path) $
        Left $ InvalidPathExpression "Custom path cannot be empty"

-- | Validate a condition
validateCondition :: Condition -> Either ValidationError ()
validateCondition condition =
  case condition of
    Equals path _ -> validatePathExpression path
    Contains path _ -> validatePathExpression path
    GreaterThan path _ -> validatePathExpression path
    LessThan path _ -> validatePathExpression path
    Exists path -> validatePathExpression path
    NotExists path -> validatePathExpression path
    And conditions -> mapM_ validateCondition conditions
    Or conditions -> mapM_ validateCondition conditions
    Not cond -> validateCondition cond

-- | A set of rules
data RuleSet = RuleSet
  { ruleSetId :: Text                 -- ^ Unique identifier for the rule set
  , ruleSetRules :: Map Text FactObservationRule -- ^ Rules by ID
  } deriving (Show, Eq)

-- | Create a new rule set
createRuleSet :: Text -> RuleSet
createRuleSet name = RuleSet
  { ruleSetId = name
  , ruleSetRules = Map.empty
  }

-- | Add a rule to a rule set
addRule :: FactObservationRule -> RuleSet -> RuleSet
addRule rule ruleSet =
  -- Only add the rule if it's not already in the set
  if Map.member (ruleId rule) (ruleSetRules ruleSet)
    then ruleSet
    else ruleSet { ruleSetRules = Map.insert (ruleId rule) rule (ruleSetRules ruleSet) }

-- | Force add a rule to a rule set, replacing any existing rule with the same ID
forceAddRule :: FactObservationRule -> RuleSet -> RuleSet
forceAddRule rule ruleSet =
  ruleSet { ruleSetRules = Map.insert (ruleId rule) rule (ruleSetRules ruleSet) }

-- | Add a rule to a rule set, with validation
addRuleToSet :: FactObservationRule -> RuleSet -> Either ValidationError RuleSet
addRuleToSet rule ruleSet = do
  -- Validate the rule
  validateRule rule
  
  -- Check for duplicate rule ID
  when (Map.member (ruleId rule) (ruleSetRules ruleSet)) $
    Left $ DuplicateRuleId (ruleId rule)
  
  -- Add the rule to the set
  return $ ruleSet { ruleSetRules = Map.insert (ruleId rule) rule (ruleSetRules ruleSet) }

-- | Remove a rule from a rule set
removeRule :: Text -> RuleSet -> RuleSet
removeRule ruleId ruleSet =
  ruleSet { ruleSetRules = Map.delete ruleId (ruleSetRules ruleSet) }

-- | Find rules by fact type
findRulesByFactType :: FactType -> RuleSet -> [FactObservationRule]
findRulesByFactType factType ruleSet =
  -- Filter rules by fact type and enabled status
  filter (\rule -> factType == TimeBandits.Core.FactObservation.Rules.factType rule && enabled rule) $
    Map.elems (ruleSetRules ruleSet)

-- | Deduplicate rules by ID
deduplicateRules :: [FactObservationRule] -> [FactObservationRule]
deduplicateRules rules =
  -- Use a Map to deduplicate by ID
  Map.elems $ Map.fromList $ map (\rule -> (ruleId rule, rule)) rules

-- | Get all rules from a rule set
rules :: RuleSet -> [FactObservationRule]
rules = Map.elems . ruleSetRules

-- Pattern synonyms for common fact types
pattern BalanceObservation :: FactType
pattern BalanceObservation = Custom "BalanceObservation"

pattern PriceObservation :: FactType
pattern PriceObservation = Custom "PriceObservation"

pattern EventObservation :: FactType
pattern EventObservation = Custom "EventObservation"

-- Pattern synonyms for common proof types
pattern InclusionProof :: ProofType
pattern InclusionProof = CustomProof "InclusionProof"

-- Compatibility functions for old field names (used in tests)
-- These provide backward compatibility with the older field names
description :: FactObservationRule -> Maybe Text
description rule = Just (ruleDescription rule)

path :: FactObservationRule -> PathExpression
path = factPath

proof :: FactObservationRule -> ProofType
proof = proofType

-- Accessor functions for PathExpression fields
source :: PathExpression -> Text
source (JsonPath t) = t
source (XPath t) = t
source (Regex t) = t
source (JqPath t) = t
source (CustomPath t) = t

selector :: PathExpression -> Text
selector _ = ""  -- For backward compatibility

parameters :: PathExpression -> Map.Map Text Text
parameters _ = Map.empty  -- For backward compatibility

-- Condition helpers
field :: Condition -> Text
field (Equals _ _) = ""
field (Contains _ t) = t
field (GreaterThan _ _) = ""
field (LessThan _ _) = ""
field (Exists _) = ""
field (NotExists _) = ""
field (And _) = ""
field (Or _) = ""
field (Not _) = ""

operator :: Condition -> Text
operator _ = ">"  -- For backward compatibility

value :: Condition -> Value
value _ = Aeson.String ""  -- For backward compatibility

-- Create a comparison condition from field, operator, value  
makeComparisonCondition :: Text -> Text -> Value -> Condition
makeComparisonCondition fieldName op val = 
  case op of
    ">" -> GreaterThan (JsonPath fieldName) val
    "<" -> LessThan (JsonPath fieldName) val
    "=" -> Equals (JsonPath fieldName) val
    "contains" -> Contains (JsonPath fieldName) (T.pack $ show val)
    _ -> Equals (JsonPath fieldName) val  -- Default to equals

-- Create a path expression from source, selector, parameters
makePathExpression :: Text -> Text -> Map.Map Text Text -> PathExpression
makePathExpression src sel params = JsonPath src  -- For backward compatibility

-- For backward compatibility in tests
