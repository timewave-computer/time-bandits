{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Core.FactObservation.Rules
Description : Fact observation rules for blockchain data extraction
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module defines fact observation rules for extracting relevant facts
from blockchain data for Time Keepers.
-}
module Core.FactObservation.Rules
  ( -- * Rule Types
    FactObservationRule(..)
  , FactType(..)
  , ProofType(..)
  , PathExpression(..)
  , Condition(..)
  
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
import Data.Monoid ((<>))

-- | Fact observation rule for extracting facts from blockchain data
data FactObservationRule = FactObservationRule
  { ruleId :: Text                -- ^ Unique identifier for the rule
  , factType :: FactType          -- ^ Type of fact to observe
  , path :: PathExpression        -- ^ Path to the data to extract
  , proof :: ProofType            -- ^ Type of proof to generate
  , conditions :: [Condition]     -- ^ Optional conditions for when to apply the rule
  , description :: Maybe Text     -- ^ Optional human-readable description
  , enabled :: Bool               -- ^ Whether the rule is enabled
  } deriving (Eq, Show, Generic)

instance ToJSON FactObservationRule
instance FromJSON FactObservationRule

-- | Type of fact to observe
data FactType
  = PriceObservation                -- ^ Price or exchange rate observation
  | BalanceObservation              -- ^ Account balance observation
  | DepositObservation              -- ^ Deposit event observation
  | WithdrawalObservation           -- ^ Withdrawal event observation
  | TransactionObservation          -- ^ General transaction observation
  | BlockObservation                -- ^ Block metadata observation
  | EventObservation                -- ^ Smart contract event observation
  | StateObservation                -- ^ Contract or account state observation
  | CustomFact Text                 -- ^ Custom fact type with name
  deriving (Eq, Ord, Show, Generic)

instance ToJSON FactType
instance FromJSON FactType

-- | Type of proof to generate for a fact
data ProofType
  = InclusionProof                  -- ^ Merkle inclusion proof
  | HeaderProof                     -- ^ Block header proof
  | StateProof                      -- ^ State transition proof
  | SignatureProof                  -- ^ Signature-based proof
  | ReceiptProof                    -- ^ Transaction receipt proof
  | NoProof                         -- ^ No proof required
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ProofType
instance FromJSON ProofType

-- | Path expression for locating data in blockchain structures
data PathExpression = PathExpression
  { source :: Text                  -- ^ Data source (e.g., "ethereum", "uniswapV3Pool")
  , selector :: Text                -- ^ Path selector (e.g., "block.number", "event.Transfer")
  , parameters :: Map Text Text     -- ^ Optional parameters for the selector
  } deriving (Eq, Show, Generic)

instance ToJSON PathExpression
instance FromJSON PathExpression

-- | Condition for when to apply a rule
data Condition
  = ComparisonCondition
      { field :: Text               -- ^ Field to compare
      , operator :: Text            -- ^ Comparison operator (e.g., "==", ">")
      , value :: Value              -- ^ Value to compare against
      }
  | LogicalCondition
      { logicalOp :: Text           -- ^ Logical operator ("and", "or", "not")
      , subConditions :: [Condition] -- ^ Sub-conditions
      }
  | ExistsCondition
      { checkField :: Text          -- ^ Field to check for existence
      }
  deriving (Eq, Show, Generic)

instance ToJSON Condition
instance FromJSON Condition

-- | Validation error for rules
data ValidationError
  = InvalidRuleId Text              -- ^ Invalid rule ID
  | InvalidFactType Text            -- ^ Invalid fact type
  | InvalidPathExpression Text      -- ^ Invalid path expression
  | InvalidProofType Text           -- ^ Invalid proof type
  | InvalidCondition Text           -- ^ Invalid condition
  | MissingRequiredField Text       -- ^ Missing required field
  | IncompatibleProofType Text      -- ^ Proof type incompatible with fact type
  deriving (Eq, Show)

-- | Validate a fact observation rule
validateRule :: FactObservationRule -> Either ValidationError ()
validateRule rule = do
  -- Validate rule ID
  when (T.null (ruleId rule)) $
    Left $ InvalidRuleId "Rule ID cannot be empty"
  
  when (T.length (ruleId rule) > 64) $
    Left $ InvalidRuleId "Rule ID cannot exceed 64 characters"
  
  -- Validate path expression
  validatePathExpression (path rule)
  
  -- Validate compatibility between fact type and proof type
  case (factType rule, proof rule) of
    (BlockObservation, HeaderProof) -> Right ()
    (BlockObservation, NoProof) -> Right ()
    (BlockObservation, _) -> 
      Left $ IncompatibleProofType "Block observations should use HeaderProof or NoProof"
    
    (TransactionObservation, InclusionProof) -> Right ()
    (TransactionObservation, ReceiptProof) -> Right ()
    (TransactionObservation, NoProof) -> Right ()
    (TransactionObservation, _) ->
      Left $ IncompatibleProofType "Transaction observations should use InclusionProof, ReceiptProof, or NoProof"
    
    (EventObservation, InclusionProof) -> Right ()
    (EventObservation, ReceiptProof) -> Right ()
    (EventObservation, NoProof) -> Right ()
    (EventObservation, _) ->
      Left $ IncompatibleProofType "Event observations should use InclusionProof, ReceiptProof, or NoProof"
    
    (StateObservation, StateProof) -> Right ()
    (StateObservation, NoProof) -> Right ()
    (StateObservation, _) ->
      Left $ IncompatibleProofType "State observations should use StateProof or NoProof"
    
    (_, NoProof) -> Right ()  -- NoProof is always valid
    (_, _) -> Right ()  -- Allow any other combinations for flexibility
  
  -- Validate conditions
  mapM_ validateCondition (conditions rule)
  
  -- All validations passed
  return ()

-- | Validate a path expression
validatePathExpression :: PathExpression -> Either ValidationError ()
validatePathExpression PathExpression{..} = do
  -- Source cannot be empty
  when (T.null source) $
    Left $ InvalidPathExpression "Source cannot be empty"
  
  -- Selector cannot be empty
  when (T.null selector) $
    Left $ InvalidPathExpression "Selector cannot be empty"
  
  -- All validations passed
  return ()

-- | Validate a condition
validateCondition :: Condition -> Either ValidationError ()
validateCondition (ComparisonCondition field op val) = do
  -- Field cannot be empty
  when (T.null field) $
    Left $ InvalidCondition "Field cannot be empty"
  
  -- Operator must be valid
  when (op `notElem` ["==", "!=", ">", "<", ">=", "<=", "contains", "startsWith", "endsWith"]) $
    Left $ InvalidCondition ("Invalid operator: " <> op)
  
  -- All validations passed
  return ()
  
validateCondition (LogicalCondition op conds) = do
  -- Operator must be valid
  when (op `notElem` ["and", "or", "not"]) $
    Left $ InvalidCondition ("Invalid logical operator: " <> op)
  
  -- For "not", there should be exactly one sub-condition
  when (op == "not" && length conds /= 1) $
    Left $ InvalidCondition "'not' operator requires exactly one sub-condition"
  
  -- For "and" and "or", there should be at least two sub-conditions
  when (op `elem` ["and", "or"] && length conds < 2) $
    Left $ InvalidCondition (op <> " operator requires at least two sub-conditions")
  
  -- Validate sub-conditions
  mapM_ validateCondition conds
  
  -- All validations passed
  return ()
  
validateCondition (ExistsCondition field) = do
  -- Field cannot be empty
  when (T.null field) $
    Left $ InvalidCondition "Field cannot be empty"
  
  -- All validations passed
  return ()

-- | A set of fact observation rules
data RuleSet = RuleSet
  { rules :: [FactObservationRule]   -- ^ The rules in this set
  , metadata :: Map Text Text        -- ^ Metadata about the rule set
  } deriving (Eq, Show, Generic)

instance ToJSON RuleSet
instance FromJSON RuleSet

-- | Create a new rule set
createRuleSet :: [FactObservationRule] -> Map Text Text -> RuleSet
createRuleSet rs meta = RuleSet
  { rules = rs
  , metadata = meta
  }

-- | Add a rule to a rule set
addRule :: RuleSet -> FactObservationRule -> RuleSet
addRule RuleSet{..} rule =
  let existingIds = Set.fromList $ map ruleId rules
      newRule = if ruleId rule `Set.member` existingIds
                  then rule { ruleId = generateUniqueId (ruleId rule) existingIds }
                  else rule
  in RuleSet
       { rules = newRule : rules
       , metadata = metadata
       }

-- | Remove a rule from a rule set
removeRule :: RuleSet -> Text -> RuleSet
removeRule RuleSet{..} ruleIdToRemove =
  RuleSet
    { rules = filter (\r -> ruleId r /= ruleIdToRemove) rules
    , metadata = metadata
    }

-- | Find rules by fact type
findRulesByFactType :: RuleSet -> FactType -> [FactObservationRule]
findRulesByFactType RuleSet{..} factTypeToFind =
  filter (\r -> factType r == factTypeToFind) rules

-- | Generate a unique ID based on an existing ID
generateUniqueId :: Text -> Set Text -> Text
generateUniqueId baseId existingIds =
  let tryWithSuffix suffix =
        let candidateId = baseId <> "_" <> T.pack (show suffix)
        in if candidateId `Set.member` existingIds
             then tryWithSuffix (suffix + 1)
             else candidateId
  in tryWithSuffix (1 :: Int) 