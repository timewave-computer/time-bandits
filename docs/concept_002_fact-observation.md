# Fact Observation Rules

## Overview

The Fact Observation component of Time Bandits provides a flexible rule-based system for extracting and validating facts from blockchain data. This component allows users to define rules in TOML format that specify what data to extract, how to validate it, and what proofs to generate.

## Rule Structure

Fact observation rules are defined in TOML format with the following structure:

```toml
[[rules]]
rule_id = "unique-rule-id"
fact_type = "FactType"
proof = "ProofType"
enabled = true
description = "Optional description of the rule"

path.source = "blockchain-source"
path.selector = "data.selector.path"

[path.parameters]
param1 = "value1"
param2 = "value2"

[[conditions]]
field = "field_name"
operator = ">"
value = 1000

[[conditions]]
check_field = "another_field"

[metadata]
version = "1.0.0"
author = "Author Name"
```

### Rule Fields

- `rule_id`: A unique identifier for the rule
- `fact_type`: The type of fact to generate (e.g., `PriceObservation`, `BalanceObservation`)
- `proof`: The type of proof to generate (e.g., `InclusionProof`, `StateProof`)
- `enabled`: Whether the rule is enabled
- `description`: Optional description of the rule

### Path Expression

The path expression specifies where to find the data in the blockchain:

- `path.source`: The blockchain source (e.g., `ethereum`, `bitcoin`)
- `path.selector`: A dot-separated path to the data (e.g., `account.balance`)
- `path.parameters`: Additional parameters for the path (e.g., `address`, `network`)

### Conditions

Conditions specify when the rule should match:

1. Comparison conditions:
   ```toml
   [[conditions]]
   field = "balance"
   operator = ">"
   value = 1000000000000000000  # 1 ETH in wei
   ```

2. Existence conditions:
   ```toml
   [[conditions]]
   check_field = "last_updated"
   ```

3. Logical conditions:
   ```toml
   [[conditions]]
   logical_op = "AND"
   
   [[conditions.sub_conditions]]
   field = "timestamp"
   operator = ">"
   value = 1672531200  # Jan 1, 2023
   
   [[conditions.sub_conditions]]
   field = "amount"
   operator = ">"
   value = 100000000  # 1 BTC in satoshis
   ```

### Metadata

Optional metadata for the rule set:

```toml
[metadata]
version = "1.0.0"
author = "Time Bandits Team"
created_at = "2024-06-01"
```

## Fact Types

The following fact types are supported:

- `PriceObservation`: Observes price data from oracles or exchanges
- `BalanceObservation`: Observes account balance changes
- `DepositObservation`: Observes deposits to an account
- `WithdrawalObservation`: Observes withdrawals from an account
- `TransactionObservation`: Observes transactions
- `BlockObservation`: Observes block data
- `EventObservation`: Observes contract events
- `StateObservation`: Observes state changes
- `CustomFact`: Custom fact types (prefixed with `Custom_`)

## Proof Types

The following proof types are supported:

- `InclusionProof`: Merkle proof of inclusion in a block
- `HeaderProof`: Proof based on block headers
- `StateProof`: Proof of state (e.g., Merkle Patricia Trie proof)
- `SignatureProof`: Proof based on signatures
- `ReceiptProof`: Proof based on transaction receipts
- `NoProof`: No proof generated

## CLI Usage

The Fact Observation CLI provides a command-line interface for working with fact observation rules:

```bash
# Load rules from a file or directory
fact-observation-cli --command load --input rules/ethereum_balance.toml

# Validate rules without loading them
fact-observation-cli --command validate --input rules/ethereum_balance.toml

# Evaluate data against loaded rules
fact-observation-cli --command evaluate --input data/ethereum_data.json --output facts/results.json

# List all loaded rules
fact-observation-cli --command list-rules

# List all generated facts
fact-observation-cli --command list-facts

# Show help
fact-observation-cli --help

# Show version
fact-observation-cli --version
```

## Example Rules

### Ethereum Balance Observation

```toml
[[rules]]
rule_id = "eth-balance-observation-1"
fact_type = "BalanceObservation"
proof = "StateProof"
enabled = true
description = "Observes ETH balance changes for a specific address"

path.source = "ethereum"
path.selector = "account.balance"

[path.parameters]
address = "0x742d35Cc6634C0532925a3b844Bc454e4438f44e"
network = "mainnet"

[[conditions]]
field = "balance"
operator = ">"
value = 1000000000000000000  # 1 ETH in wei

[[conditions]]
check_field = "last_updated"
```

### Bitcoin Transaction Observation

```toml
[[rules]]
rule_id = "btc-transaction-observation-1"
fact_type = "TransactionObservation"
proof = "InclusionProof"
enabled = true
description = "Observes Bitcoin transactions to a specific address with minimum value"

path.source = "bitcoin"
path.selector = "transaction"

[path.parameters]
address = "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
network = "mainnet"
min_confirmations = "6"

[[conditions]]
field = "amount"
operator = ">="
value = 100000000  # 1 BTC in satoshis

[[conditions]]
logical_op = "AND"

[[conditions.sub_conditions]]
field = "timestamp"
operator = ">"
value = 1672531200  # Jan 1, 2023 in Unix timestamp

[[conditions.sub_conditions]]
check_field = "fee"
```

## API Reference

### Rule Engine

```haskell
-- Create a new rule engine
createEngine :: EngineConfig -> IO RuleEngine

-- Load rules from a file
loadRules :: RuleEngine -> FilePath -> IO (Either EngineError RuleSet)

-- Load rules from a directory
loadRulesFromDirectory :: RuleEngine -> FilePath -> IO (Either EngineError RuleSet)

-- Evaluate data against all enabled rules
evaluateData :: RuleEngine -> Value -> IO [Either EngineError FactResult]

-- Evaluate data against a specific rule
evaluateDataWithRule :: RuleEngine -> FactObservationRule -> Value -> IO (Either EngineError FactResult)
```

### TOML Parser

```haskell
-- Parse a rule set from TOML text
parseRuleSet :: Text -> Either ParseError RuleSet

-- Parse a single rule from TOML text
parseRule :: Text -> Either ParseError FactObservationRule

-- Parse a rule set from a file
parseRuleSetFromFile :: FilePath -> IO (Either ParseError RuleSet)

-- Serialize a rule set to TOML text
serializeRuleSet :: RuleSet -> Either ParseError Text

-- Serialize a rule to TOML text
serializeRule :: FactObservationRule -> Either ParseError Text
```

## Integration with Time Bandits

The Fact Observation component integrates with other Time Bandits components:

1. Uses the `ResourceLock` from the Concurrency module for thread-safe access to rules
2. Uses the `EffectLog` for logging engine events
3. Generates facts that can be used by other components for decision-making and consensus

## Future Enhancements

Planned enhancements for the Fact Observation component:

1. Support for more blockchain sources
2. Enhanced proof generation with cryptographic verification
3. Rule templates for common observation patterns
4. Real-time monitoring and alerting
5. Integration with external data sources and oracles 