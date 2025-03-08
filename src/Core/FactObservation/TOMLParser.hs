{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Core.FactObservation.TOMLParser
Description : TOML parser for fact observation rules
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module implements a TOML parser for fact observation rules,
enabling the definition of rules in a human-readable format.
-}
module Core.FactObservation.TOMLParser
  ( -- * Parsing
    parseRuleSet
  , parseRule
  , parseRuleFromFile
  , parseRuleSetFromFile
  
    -- * Serialization
  , serializeRuleSet
  , serializeRule
  , serializeRuleToFile
  , serializeRuleSetToFile
  
    -- * Error Types
  , ParseError(..)
  ) where

import Control.Exception (try)
import Control.Monad (forM, when)
import Data.Aeson (Value, (.=), object)
import qualified Data.Aeson as Aeson
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Toml
import qualified Toml.FromValue as Toml
import qualified Toml.FromValueSpec as Toml
import qualified Toml.Type as Toml
import qualified Toml.Codec as Toml

-- Import from TimeBandits modules
import Core.FactObservation.Rules

-- | Error type for TOML parsing
data ParseError
  = TomlParseError Text         -- ^ Error from the TOML parser
  | ValidationError Text        -- ^ Validation error for the rule
  | FileError Text              -- ^ Error reading or writing a file
  | SerializationError Text     -- ^ Error during serialization
  deriving (Eq, Show)

-- | Parse a rule set from TOML text
parseRuleSet :: Text -> Either ParseError RuleSet
parseRuleSet tomlText = do
  -- Parse the TOML
  tomlValue <- case Toml.parse tomlText of
    Left err -> Left $ TomlParseError $ T.pack $ show err
    Right val -> Right val
  
  -- Extract the rules
  rules <- parseRules tomlValue
  
  -- Extract the metadata
  metadata <- parseMetadata tomlValue
  
  -- Return the rule set
  return $ RuleSet rules metadata

-- | Parse a single rule from TOML text
parseRule :: Text -> Either ParseError FactObservationRule
parseRule tomlText = do
  -- Parse the TOML
  tomlValue <- case Toml.parse tomlText of
    Left err -> Left $ TomlParseError $ T.pack $ show err
    Right val -> Right val
  
  -- Extract the rule
  case parseRuleFromTable tomlValue of
    Left err -> Left err
    Right [] -> Left $ ValidationError "No rules found in TOML"
    Right (rule:_) -> Right rule

-- | Parse a rule set from a file
parseRuleSetFromFile :: FilePath -> IO (Either ParseError RuleSet)
parseRuleSetFromFile filePath = do
  -- Try to read the file
  contentResult <- try $ TIO.readFile filePath
  case contentResult of
    Left err -> return $ Left $ FileError $ T.pack $ show (err :: IOError)
    Right content -> return $ parseRuleSet content

-- | Parse a rule from a file
parseRuleFromFile :: FilePath -> IO (Either ParseError FactObservationRule)
parseRuleFromFile filePath = do
  -- Try to read the file
  contentResult <- try $ TIO.readFile filePath
  case contentResult of
    Left err -> return $ Left $ FileError $ T.pack $ show (err :: IOError)
    Right content -> return $ parseRule content

-- | Serialize a rule set to TOML text
serializeRuleSet :: RuleSet -> Either ParseError Text
serializeRuleSet RuleSet{..} = do
  -- Convert each rule to a TOML table
  ruleTablesResult <- mapM serializeRule rules
  
  -- Handle errors
  case sequence ruleTablesResult of
    Left err -> Left err
    Right ruleTables -> do
      -- Combine the rule tables
      let rulesSection = T.intercalate "\n\n" ruleTables
      
      -- Convert metadata to TOML
      let metadataSection = serializeMetadata metadata
      
      -- Combine sections
      let tomlText = rulesSection <> "\n\n" <> metadataSection
      
      return tomlText

-- | Serialize a rule to TOML text
serializeRule :: FactObservationRule -> Either ParseError Text
serializeRule rule = do
  -- Convert to TOML manually (simplified version)
  let ruleId' = "rule_id = " <> quoted (ruleId rule)
      factType' = "fact_type = " <> quoted (serializeFactType $ factType rule)
      proof' = "proof = " <> quoted (serializeProofType $ proof rule)
      enabled' = "enabled = " <> (if enabled rule then "true" else "false")
      description' = maybe "" (\d -> "description = " <> quoted d) (description rule)
      
      -- Path section
      pathSection = 
        "path.source = " <> quoted (source $ path rule) <> "\n" <>
        "path.selector = " <> quoted (selector $ path rule) <> "\n" <>
        serializeParameters (parameters $ path rule)
      
      -- Conditions section
      conditionsSection = serializeConditions (conditions rule)
  
  -- Combine sections
  return $ "[[rules]]\n" <>
           ruleId' <> "\n" <>
           factType' <> "\n" <>
           proof' <> "\n" <>
           enabled' <> "\n" <>
           (if T.null description' then "" else description' <> "\n") <>
           "\n" <>
           pathSection <> "\n" <>
           conditionsSection

-- | Serialize a rule set to a file
serializeRuleSetToFile :: FilePath -> RuleSet -> IO (Either ParseError ())
serializeRuleSetToFile filePath ruleSet = do
  -- Serialize the rule set
  case serializeRuleSet ruleSet of
    Left err -> return $ Left err
    Right tomlText -> do
      -- Try to write to the file
      writeResult <- try $ TIO.writeFile filePath tomlText
      case writeResult of
        Left err -> return $ Left $ FileError $ T.pack $ show (err :: IOError)
        Right () -> return $ Right ()

-- | Serialize a rule to a file
serializeRuleToFile :: FilePath -> FactObservationRule -> IO (Either ParseError ())
serializeRuleToFile filePath rule = do
  -- Serialize the rule
  case serializeRule rule of
    Left err -> return $ Left err
    Right tomlText -> do
      -- Try to write to the file
      writeResult <- try $ TIO.writeFile filePath tomlText
      case writeResult of
        Left err -> return $ Left $ FileError $ T.pack $ show (err :: IOError)
        Right () -> return $ Right ()

-- Helper functions

-- | Parse rules from a TOML value
parseRules :: Toml.Value -> Either ParseError [FactObservationRule]
parseRules tomlValue = 
  case findArray "rules" tomlValue of
    Nothing -> return []  -- No rules section, return empty list
    Just rulesArray -> parseRuleFromTable tomlValue

-- | Parse rules from a TOML table
parseRuleFromTable :: Toml.Value -> Either ParseError [FactObservationRule]
parseRuleFromTable tomlValue = do
  -- Find the rules array
  rulesArray <- case findArray "rules" tomlValue of
    Nothing -> Left $ ValidationError "Rules section not found in TOML"
    Just arr -> Right arr
  
  -- Parse each rule
  rulesResults <- forM rulesArray $ \ruleTable -> do
    -- Extract fields
    ruleId' <- extractText "rule_id" ruleTable
    factTypeText <- extractText "fact_type" ruleTable
    proofTypeText <- extractText "proof" ruleTable
    enabled' <- extractBool "enabled" ruleTable
    description' <- extractMaybeText "description" ruleTable
    
    -- Parse fact type
    factType' <- parseFactType factTypeText
    
    -- Parse proof type
    proofType' <- parseProofType proofTypeText
    
    -- Parse path expression
    pathExpr <- parsePathExpression ruleTable
    
    -- Parse conditions
    conditions' <- parseConditions ruleTable
    
    -- Create the rule
    let rule = FactObservationRule
          { ruleId = ruleId'
          , factType = factType'
          , path = pathExpr
          , proof = proofType'
          , conditions = conditions'
          , description = description'
          , enabled = enabled'
          }
    
    -- Validate the rule
    case validateRule rule of
      Left err -> Left $ ValidationError $ T.pack $ show err
      Right () -> Right rule
  
  -- Return only the successful results
  return $ catMaybes $ map (either (const Nothing) Just) rulesResults

-- | Parse metadata from a TOML value
parseMetadata :: Toml.Value -> Either ParseError (Map Text Text)
parseMetadata tomlValue =
  case findTable "metadata" tomlValue of
    Nothing -> Right Map.empty  -- No metadata section, return empty map
    Just metadataTable -> do
      -- Convert each key-value pair
      let kvPairs = Toml.toHashMap metadataTable
      let textPairs = Map.fromList $ catMaybes $ map convertToTextPair $ Map.toList kvPairs
      return textPairs
  where
    convertToTextPair :: (Text, Toml.Value) -> Maybe (Text, Text)
    convertToTextPair (key, value) = case value of
      Toml.Text t -> Just (key, t)
      _ -> Nothing  -- Skip non-text values

-- | Parse a fact type from text
parseFactType :: Text -> Either ParseError FactType
parseFactType text = case text of
  "PriceObservation" -> Right PriceObservation
  "BalanceObservation" -> Right BalanceObservation
  "DepositObservation" -> Right DepositObservation
  "WithdrawalObservation" -> Right WithdrawalObservation
  "TransactionObservation" -> Right TransactionObservation
  "BlockObservation" -> Right BlockObservation
  "EventObservation" -> Right EventObservation
  "StateObservation" -> Right StateObservation
  _ -> if T.isPrefixOf "Custom_" text
        then Right $ CustomFact $ T.drop 7 text
        else Left $ ValidationError $ "Invalid fact type: " <> text

-- | Parse a proof type from text
parseProofType :: Text -> Either ParseError ProofType
parseProofType text = case text of
  "InclusionProof" -> Right InclusionProof
  "HeaderProof" -> Right HeaderProof
  "StateProof" -> Right StateProof
  "SignatureProof" -> Right SignatureProof
  "ReceiptProof" -> Right ReceiptProof
  "NoProof" -> Right NoProof
  _ -> Left $ ValidationError $ "Invalid proof type: " <> text

-- | Parse a path expression from a TOML table
parsePathExpression :: Toml.Value -> Either ParseError PathExpression
parsePathExpression table = do
  -- Find the path section
  pathTable <- case findTable "path" table of
    Nothing -> Left $ ValidationError "Path section not found in rule"
    Just t -> Right t
  
  -- Extract fields
  source' <- extractText "source" pathTable
  selector' <- extractText "selector" pathTable
  
  -- Extract parameters
  params <- case findTable "parameters" pathTable of
    Nothing -> Right Map.empty
    Just paramsTable -> do
      let kvPairs = Toml.toHashMap paramsTable
      let textPairs = Map.fromList $ catMaybes $ map convertToTextPair $ Map.toList kvPairs
      return textPairs
  
  -- Create the path expression
  return PathExpression
    { source = source'
    , selector = selector'
    , parameters = params
    }
  where
    convertToTextPair :: (Text, Toml.Value) -> Maybe (Text, Text)
    convertToTextPair (key, value) = case value of
      Toml.Text t -> Just (key, t)
      _ -> Nothing  -- Skip non-text values

-- | Parse conditions from a TOML table
parseConditions :: Toml.Value -> Either ParseError [Condition]
parseConditions table = do
  -- Find the conditions array
  case findArray "conditions" table of
    Nothing -> Right []  -- No conditions section, return empty list
    Just conditionsArray -> do
      -- Parse each condition
      conditionsResults <- forM conditionsArray parseCondition
      
      -- Return only the successful results
      return $ catMaybes $ map (either (const Nothing) Just) conditionsResults

-- | Parse a condition from a TOML value
parseCondition :: Toml.Value -> Either ParseError Condition
parseCondition condTable = do
  -- Check condition type
  if hasField "field" condTable && hasField "operator" condTable && hasField "value" condTable then
    -- Comparison condition
    parseComparisonCondition condTable
  else if hasField "logical_op" condTable && hasField "sub_conditions" condTable then
    -- Logical condition
    parseLogicalCondition condTable
  else if hasField "check_field" condTable then
    -- Exists condition
    parseExistsCondition condTable
  else
    Left $ ValidationError "Invalid condition format"

-- | Parse a comparison condition
parseComparisonCondition :: Toml.Value -> Either ParseError Condition
parseComparisonCondition table = do
  field' <- extractText "field" table
  operator' <- extractText "operator" table
  value' <- extractValue "value" table
  
  return $ ComparisonCondition field' operator' (tomlToAeson value')

-- | Parse a logical condition
parseLogicalCondition :: Toml.Value -> Either ParseError Condition
parseLogicalCondition table = do
  logicalOp' <- extractText "logical_op" table
  
  -- Find the sub_conditions array
  subCondArray <- case findArray "sub_conditions" table of
    Nothing -> Left $ ValidationError "Sub-conditions not found in logical condition"
    Just arr -> Right arr
  
  -- Parse each sub-condition
  subCondResults <- forM subCondArray parseCondition
  
  -- Check for errors
  case sequence subCondResults of
    Left err -> Left err
    Right subConds -> return $ LogicalCondition logicalOp' subConds

-- | Parse an exists condition
parseExistsCondition :: Toml.Value -> Either ParseError Condition
parseExistsCondition table = do
  checkField' <- extractText "check_field" table
  return $ ExistsCondition checkField'

-- | Serialize a fact type to text
serializeFactType :: FactType -> Text
serializeFactType = \case
  PriceObservation -> "PriceObservation"
  BalanceObservation -> "BalanceObservation"
  DepositObservation -> "DepositObservation"
  WithdrawalObservation -> "WithdrawalObservation"
  TransactionObservation -> "TransactionObservation"
  BlockObservation -> "BlockObservation"
  EventObservation -> "EventObservation"
  StateObservation -> "StateObservation"
  CustomFact name -> "Custom_" <> name

-- | Serialize a proof type to text
serializeProofType :: ProofType -> Text
serializeProofType = \case
  InclusionProof -> "InclusionProof"
  HeaderProof -> "HeaderProof"
  StateProof -> "StateProof"
  SignatureProof -> "SignatureProof"
  ReceiptProof -> "ReceiptProof"
  NoProof -> "NoProof"

-- | Serialize parameters to TOML
serializeParameters :: Map Text Text -> Text
serializeParameters params =
  if Map.null params
    then ""
    else "[path.parameters]\n" <>
         T.intercalate "\n" (map serializeParameter $ Map.toList params)
  where
    serializeParameter :: (Text, Text) -> Text
    serializeParameter (key, val) = key <> " = " <> quoted val

-- | Serialize conditions to TOML
serializeConditions :: [Condition] -> Text
serializeConditions [] = ""
serializeConditions conds =
  T.intercalate "\n\n" $ map serializeCondition conds

-- | Serialize a condition to TOML
serializeCondition :: Condition -> Text
serializeCondition (ComparisonCondition field' op val) =
  "[[conditions]]\n" <>
  "field = " <> quoted field' <> "\n" <>
  "operator = " <> quoted op <> "\n" <>
  "value = " <> "true"  -- Placeholder, in a real implementation this would serialize the value

serializeCondition (LogicalCondition op subs) =
  "[[conditions]]\n" <>
  "logical_op = " <> quoted op <> "\n\n" <>
  T.intercalate "\n\n" (map serializeSubCondition subs)
  where
    serializeSubCondition :: Condition -> Text
    serializeSubCondition sub =
      "[[conditions.sub_conditions]]\n" <>
      case sub of
        ComparisonCondition f o v ->
          "field = " <> quoted f <> "\n" <>
          "operator = " <> quoted o <> "\n" <>
          "value = " <> "true"  -- Placeholder
        
        LogicalCondition op' _ ->
          "logical_op = " <> quoted op' <> "\n" <>
          "# Nested conditions omitted"
        
        ExistsCondition f ->
          "check_field = " <> quoted f

serializeCondition (ExistsCondition field') =
  "[[conditions]]\n" <>
  "check_field = " <> quoted field'

-- | Serialize metadata to TOML
serializeMetadata :: Map Text Text -> Text
serializeMetadata meta =
  if Map.null meta
    then ""
    else "[metadata]\n" <>
         T.intercalate "\n" (map serializeMetaItem $ Map.toList meta)
  where
    serializeMetaItem :: (Text, Text) -> Text
    serializeMetaItem (key, val) = key <> " = " <> quoted val

-- | Quote a text value for TOML
quoted :: Text -> Text
quoted text = "\"" <> escapeText text <> "\""

-- | Escape special characters in text
escapeText :: Text -> Text
escapeText = T.replace "\"" "\\\"" . T.replace "\\" "\\\\"

-- | Find a value in a table by key
findValue :: Text -> Toml.Value -> Maybe Toml.Value
findValue key (Toml.Table kvs) = Map.lookup key kvs
findValue _ _ = Nothing

-- | Find a table in a value by key
findTable :: Text -> Toml.Value -> Maybe Toml.Value
findTable key value = do
  foundValue <- findValue key value
  case foundValue of
    t@(Toml.Table _) -> Just t
    _ -> Nothing

-- | Find an array in a value by key
findArray :: Text -> Toml.Value -> Maybe [Toml.Value]
findArray key value = do
  foundValue <- findValue key value
  case foundValue of
    Toml.Array arr -> Just $ Toml.toList arr
    _ -> Nothing

-- | Check if a table has a field
hasField :: Text -> Toml.Value -> Bool
hasField key (Toml.Table kvs) = Map.member key kvs
hasField _ _ = False

-- | Extract a text value
extractText :: Text -> Toml.Value -> Either ParseError Text
extractText key table = do
  case findValue key table of
    Just (Toml.Text text) -> Right text
    Just _ -> Left $ ValidationError $ "Field " <> key <> " is not a string"
    Nothing -> Left $ ValidationError $ "Required field " <> key <> " is missing"

-- | Extract an optional text value
extractMaybeText :: Text -> Toml.Value -> Either ParseError (Maybe Text)
extractMaybeText key table = do
  case findValue key table of
    Just (Toml.Text text) -> Right $ Just text
    Just _ -> Left $ ValidationError $ "Field " <> key <> " is not a string"
    Nothing -> Right Nothing

-- | Extract a boolean value
extractBool :: Text -> Toml.Value -> Either ParseError Bool
extractBool key table = do
  case findValue key table of
    Just (Toml.Bool bool) -> Right bool
    Just _ -> Left $ ValidationError $ "Field " <> key <> " is not a boolean"
    Nothing -> Left $ ValidationError $ "Required field " <> key <> " is missing"

-- | Extract any value
extractValue :: Text -> Toml.Value -> Either ParseError Toml.Value
extractValue key table = do
  case findValue key table of
    Just value -> Right value
    Nothing -> Left $ ValidationError $ "Required field " <> key <> " is missing"

-- | Convert a TOML value to an Aeson value
tomlToAeson :: Toml.Value -> Value
tomlToAeson = \case
  Toml.Table kvs -> Aeson.object $ map (\(k, v) -> k .= tomlToAeson v) $ Map.toList kvs
  Toml.Array arr -> Aeson.toJSON $ map tomlToAeson $ Toml.toList arr
  Toml.Text t -> Aeson.toJSON t
  Toml.Bool b -> Aeson.toJSON b
  Toml.Integer i -> Aeson.toJSON i
  Toml.Float f -> Aeson.toJSON f
  Toml.OffsetDateTime d -> Aeson.toJSON $ show d
  Toml.LocalDateTime d -> Aeson.toJSON $ show d
  Toml.LocalDate d -> Aeson.toJSON $ show d
  Toml.LocalTime t -> Aeson.toJSON $ show t 