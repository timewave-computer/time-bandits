{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module FactObservation where

import Control.Monad (when)
import Data.Aeson (FromJSON(..), ToJSON(..), Value)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Fact types that can be observed
data FactType = 
    PriceObservation
  | BalanceObservation
  | TransactionObservation
  | BlockheightObservation
  | ContractStateObservation
  | CustomObservation Text
  deriving (Show, Eq, Generic, Ord)

instance FromJSON FactType
instance ToJSON FactType

-- | Proof types that can be generated
data ProofType = 
    InclusionProof
  | StateProof
  | SignatureProof
  | ZKProof
  | NoProof
  deriving (Show, Eq, Generic)

instance FromJSON ProofType
instance ToJSON ProofType

-- | Path expression for data extraction
data PathExpression = PathExpression
  { source :: Text         -- ^ Data source (e.g., blockchain name)
  , selector :: Text       -- ^ Path selector (e.g., JSON path)
  , parameters :: Map Text Text  -- ^ Additional parameters
  } deriving (Show, Eq, Generic)

instance FromJSON PathExpression
instance ToJSON PathExpression

-- | Condition for data validation
data Condition = Condition
  { field :: Text          -- ^ Field to check
  , operator :: Maybe Text -- ^ Comparison operator
  , value :: Maybe Value   -- ^ Value to compare against
  } deriving (Show, Eq, Generic)

instance FromJSON Condition
instance ToJSON Condition

-- | Fact observation rule for extracting facts from blockchain data
data FactObservationRule = FactObservationRule
  { ruleId :: Text                -- ^ Unique identifier for the rule
  , factType :: FactType          -- ^ Type of fact to observe
  , proofType :: ProofType        -- ^ Type of proof to generate
  , enabled :: Bool               -- ^ Whether the rule is enabled
  , description :: Maybe Text     -- ^ Optional description
  , path :: PathExpression        -- ^ Path expression for data extraction
  , conditions :: [Condition]     -- ^ Conditions for validation
  , metadata :: Map Text Text     -- ^ Additional metadata
  } deriving (Show, Eq, Generic)

instance FromJSON FactObservationRule
instance ToJSON FactObservationRule

-- | Validation error types
data ValidationError = 
    MissingRuleId
  | InvalidFactType
  | InvalidProofType
  | InvalidPathExpression Text
  | InvalidCondition Text
  | DuplicateRuleId Text
  deriving (Show, Eq)

-- | Validate a rule
validateRule :: FactObservationRule -> Either ValidationError ()
validateRule rule = do
  -- Check for required fields
  when (T.null (ruleId rule)) (Left MissingRuleId)
  validatePathExpression (path rule)
  pure ()

-- | Validate a path expression
validatePathExpression :: PathExpression -> Either ValidationError ()
validatePathExpression path = do
  when (T.null (source path)) (Left $ InvalidPathExpression "Missing source")
  when (T.null (selector path)) (Left $ InvalidPathExpression "Missing selector")
  pure ()

-- | A set of fact observation rules
data RuleSet = RuleSet
  { rules :: Map Text FactObservationRule  -- ^ Rules indexed by ID
  , factTypeIndex :: Map FactType [Text]   -- ^ Rules indexed by fact type
  } deriving (Show, Eq)

-- | Create an empty rule set
createRuleSet :: RuleSet
createRuleSet = RuleSet Map.empty Map.empty

-- | Add a rule to a rule set
addRule :: FactObservationRule -> RuleSet -> Either ValidationError RuleSet
addRule rule rs = do
  -- Validate the rule
  validateRule rule
  
  -- Check for duplicate rule ID
  when (Map.member (ruleId rule) (rules rs)) 
       (Left $ DuplicateRuleId (ruleId rule))
  
  -- Add the rule to the maps
  let updatedRules = Map.insert (ruleId rule) rule (rules rs)
      
      updatedFactTypeIndex = 
        Map.insertWith (++) (factType rule) [ruleId rule] (factTypeIndex rs)
  
  Right $ RuleSet updatedRules updatedFactTypeIndex

-- | Remove a rule from a rule set
removeRule :: Text -> RuleSet -> RuleSet
removeRule rId rs = 
  case Map.lookup rId (rules rs) of
    Nothing -> rs
    Just rule -> 
      let updatedRules = Map.delete rId (rules rs)
          
          updatedFactTypeIndex = 
            Map.adjust (filter (/= rId)) (factType rule) (factTypeIndex rs)
      
      in RuleSet updatedRules updatedFactTypeIndex

-- | Find rules by fact type
findRulesByFactType :: FactType -> RuleSet -> [FactObservationRule]
findRulesByFactType ft rs =
  case Map.lookup ft (factTypeIndex rs) of
    Nothing -> []
    Just ruleIds -> 
      mapMaybe (\rId -> Map.lookup rId (rules rs)) ruleIds

-- Helper function
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr go []
  where
    go x acc = case f x of
                 Nothing -> acc
                 Just y  -> y : acc 