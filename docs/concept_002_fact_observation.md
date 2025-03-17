# Fact Observation Rules

## Overview

The Fact Observation component of Time Bandits provides a flexible rule-based system for extracting and validating facts from blockchain data. This component allows users to define rules in TOML format that specify what data to extract, how to validate it, and what proofs to generate.

With the resource formalization model introduced in ADR_018, fact observation becomes even more important as it provides the foundation for tracking resources across chains, implementing resource conservation laws, and supporting dual validation through temporal and ancestral consistency checks.

The ZK-based register system (ADR_022) further extends fact observation by formalizing the interface between observed facts and on-chain state through registers. Facts now include register observations, verifiable through ZK proofs, which serve as the bridge between external blockchain state and the Time Bandits internal system.

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
- `fact_type`: The type of fact to generate (e.g., `PriceObservation`, `BalanceObservation`, `RegisterObservation`)
- `proof`: The type of proof to generate (e.g., `InclusionProof`, `StateProof`, `ZKProof`)
- `enabled`: Whether the rule is enabled
- `description`: Optional description of the rule

### Path Expression

The `path` field specifies where to extract data from:

```toml
path.source = "ethereum"
path.selector = "contracts.0xabcdef.events.Deposit"
```

For register-specific observations:

```toml
path.source = "ethereum"
path.selector = "registers.0xabcdef.state"
```

### Parameters

Parameters provide additional context for the observation:

```toml
[path.parameters]
contract_address = "0xabcdef1234567890"
event_signature = "Deposit(address,uint256)"
start_block = 14000000
```

For register observations:

```toml
[path.parameters]
register_id = "123"
verification_key = "0xabcdef1234567890"
zk_circuit = "RegisterStateCircuit"
```

### Conditions

Conditions filter observations based on field values:

```toml
[[conditions]]
field = "amount"
operator = ">"
value = 1000

[[conditions]]
check_field = "token_address"
equals = "0xabcdef1234567890"
```

## Fact Types

The system supports various fact types, including:

### Basic Fact Types
- `PriceObservation`: Observes asset price data
- `BalanceObservation`: Observes account balances
- `TransactionObservation`: Observes transaction data
- `EventObservation`: Observes smart contract events
- `BlockHeaderObservation`: Observes block header data

### Resource-Related Fact Types (ADR_018)
- `ResourceCreationObservation`: Observes the creation of a new resource
- `ResourceTransferObservation`: Observes the transfer of a resource
- `ResourceConsumptionObservation`: Observes the consumption of a resource
- `ResourceNullifierObservation`: Observes nullifiers for consumed resources
- `ControllerLabelObservation`: Observes controller label updates

### Register-Related Fact Types (ADR_022)
- `RegisterCreationObservation`: Observes the creation of a new register
- `RegisterUpdateObservation`: Observes updates to register state
- `RegisterTransferObservation`: Observes transfers between registers
- `RegisterNullifierObservation`: Observes nullifier registers
- `ZKProofVerificationObservation`: Observes verification of ZK proofs
- `TimeMapRegisterObservation`: Observes Time Map updates in registers
- `ExecutionSequenceObservation`: Observes execution sequence progress

## Proof Types

Different proof types provide varying levels of security:

### Basic Proof Types
- `InclusionProof`: Proves that data was included in a block
- `StateProof`: Proves the state of an account or contract
- `ReceiptProof`: Proves transaction execution and events
- `MerkleProof`: Proves inclusion in a Merkle tree

### Advanced Proof Types (ADR_018)
- `ResourceProof`: Proves the existence and state of a resource
- `NullifierProof`: Proves a resource has been consumed
- `ControllerProof`: Proves the validity of a controller label
- `ConservationProof`: Proves resource conservation (ΔTX = 0)

### Register-Related Proof Types (ADR_022)
- `ZKProof`: Zero-knowledge proof for register operations
- `RegisterStateProof`: Proves the state of a register
- `RegisterSequenceProof`: Proves a series of register operations
- `DAProof`: Proves data was posted to a data availability layer
- `TimeMapCommitmentProof`: Proves time map state at a point in time
- `DualValidationProof`: Combines temporal and ancestral validation

## Example Rules

### Basic Token Transfer Observation

```toml
[[rules]]
rule_id = "erc20-transfer"
fact_type = "EventObservation"
proof = "InclusionProof"
enabled = true
description = "Observes ERC-20 token transfers"

path.source = "ethereum"
path.selector = "contracts.events.Transfer"

[path.parameters]
contract_address = "0xabcdef1234567890"
event_signature = "Transfer(address,address,uint256)"
min_confirmations = 12

[[conditions]]
field = "value"
operator = ">"
value = 1000
```

### Resource Transfer Observation (ADR_018)

```toml
[[rules]]
rule_id = "resource-transfer"
fact_type = "ResourceTransferObservation"
proof = "ResourceProof"
enabled = true
description = "Observes formalized resource transfers"

path.source = "ethereum"
path.selector = "contracts.events.ResourceTransfer"

[path.parameters]
contract_address = "0xabcdef1234567890"
event_signature = "ResourceTransfer(bytes32,bytes32,uint256)"
resource_type = "ERC20"
min_confirmations = 12

[[conditions]]
field = "resource.quantity"
operator = ">"
value = 1000

[metadata]
resource_logic = "TokenLogic"
fungibility_domain = "DAI"
```

### Register Observation (ADR_022)

```toml
[[rules]]
rule_id = "register-update"
fact_type = "RegisterUpdateObservation"
proof = "ZKProof"
enabled = true
description = "Observes updates to a token register"

path.source = "ethereum"
path.selector = "registers.state"

[path.parameters]
register_id = "123"
register_type = "TokenRegister"
verification_key = "0xabcdef1234567890"
zk_circuit = "TokenRegisterUpdateCircuit"
min_confirmations = 12

[[conditions]]
field = "contents.token_balance"
operator = "changed"

[metadata]
resource_tracking = true
conservation_check = true
verification_key_hash = "0xabcdef1234567890"
```

### Cross-Chain Resource Observation with Registers (ADR_022)

```toml
[[rules]]
rule_id = "cross-chain-resource"
fact_type = "RegisterTransferObservation"
proof = "DualValidationProof"
enabled = true
description = "Observes cross-chain resource transfers via registers"

path.source = "ethereum"
path.selector = "contracts.events.RegisterTransfer"

[path.parameters]
contract_address = "0xabcdef1234567890"
event_signature = "RegisterTransfer(uint256,bytes32,uint256)"
source_register_id = "123"
target_chain = "solana"
target_register_id = "456"
min_confirmations = 12

[[conditions]]
field = "resource.fungibility_domain"
equals = "USDC"

[metadata]
dual_validation = true
temporal_validation_key = "0x1234567890abcdef"
ancestral_validation_key = "0xfedcba0987654321"
controller_label_check = true
```

## Implementation

### CLI Usage

The Fact Observation Rules system can be used via the CLI:

```bash
# Deploy a new rule
time-bandits fact-rules deploy --file rules/erc20-transfer.toml

# List active rules
time-bandits fact-rules list

# Observe facts using a rule
time-bandits observe --rule erc20-transfer --block-range 14000000:14001000

# Validate a fact
time-bandits validate-fact --fact-id "fact:1234" --proof-type "InclusionProof"

# Validate resource conservation
time-bandits validate-conservation --resource-id "res:5678" --operations "create,transfer,consume"

# Perform dual validation for a cross-chain resource
time-bandits dual-validate --resource-id "res:5678" --source-chain "ethereum" --target-chain "solana"
```

### Register-Specific Commands (ADR_022)

```bash
# Observe register creation
time-bandits observe-register --register-id "123" --chain "ethereum"

# Verify register state with ZK proof
time-bandits verify-register --register-id "123" --proof "proof:5678"

# Check register sequence execution
time-bandits check-execution --sequence-id "seq:1234" --register-ids "123,456,789"

# Verify data availability proof
time-bandits verify-da-proof --proof "da:1234" --data-hash "0xabcdef"
```

## Integration with Time Maps and the Map of Time

Fact observations are incorporated into Time Maps and ultimately contribute to the Map of Time:

1. Facts are observed according to rules
2. Proofs are generated for each observation
3. Observations are recorded in the observer's Unified Log
4. The Time Map is updated to reflect the new observations
5. The Map of Time incorporates these observations as nodes
6. Edges in the Map of Time connect these facts to effects that depend on them

## Resource Formalization and Fact Observation (ADR_018)

The resource formalization model enhances fact observation through:

1. **Resource-specific facts**: Facts that explicitly track resources with formalized properties
2. **Conservation validation**: Facts that help validate resource conservation laws
3. **Controller label extraction**: Facts that track controller labels for cross-chain resources
4. **Dual validation support**: Facts that enable both temporal and ancestral validation
5. **Nullifier tracking**: Facts that track resource consumption through nullifiers

## Register System and Fact Observation (ADR_022)

The ZK-based register system further enhances fact observation through:

1. **Register state observations**: Facts about on-chain register state
2. **ZK proof verification**: Facts that include verification of ZK proofs
3. **Data availability proofs**: Facts that verify data was posted to a chain's DA layer
4. **Execution sequence tracking**: Facts that track progress of complex execution sequences
5. **Time Map register updates**: Facts about time map state stored in registers
6. **Register-based resource tracking**: Facts that connect register state to resource model

### Register Observation Process

The process for observing register state involves:

1. **Register State Snapshot**: Capture the current state of a register
2. **ZK Proof Generation**: Generate a proof that the register state is valid
3. **Verification**: Verify the proof against the register's verification key
4. **Fact Creation**: Create a fact recording the register state and proof
5. **Log Recording**: Record the fact in the observer's Unified Log
6. **Map of Time Integration**: Incorporate the fact into the Map of Time

## Future Enhancements

Planned enhancements to the Fact Observation system include:

1. **Advanced conservation validation**: More sophisticated rules for validating complex resource transformations
2. **ZK-based fact verification**: Zero-knowledge proofs for verifying facts without revealing sensitive data
3. **Aggregated fact proofs**: Batching multiple fact proofs into a single verification
4. **Streaming observation rules**: Rules that continuously observe facts as they occur
5. **Cross-chain fact correlation**: Correlating related facts across different chains
6. **Resource commitment verification**: Verifying resource commitments without revealing the underlying resources
7. **Register-based fact indexing**: Using registers to efficiently index and query facts
8. **ZK-based verification of execution sequences**: Verifying complex sequences of register operations 