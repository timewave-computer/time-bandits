# ADR 015: Blockchain Adapter Architecture

## Status

Accepted, with register-based extensions

## Context

Time Bandits needs to interact with multiple blockchains, each with different:

- APIs and RPC endpoints
- State models and data structures
- Transaction formats and signatures
- Smart contract interfaces
- Register implementations

The system needs a standardized approach to:

1. Deposit to and withdraw from blockchains
2. Observe facts from blockchains
3. Interact with smart contracts
4. Manage register operations and ZK proofs
5. Handle cross-chain register transfers

## Decision

We will implement a standardized **Blockchain Adapter** architecture with the following components:

1. **Core Adapter Interface**: Common interface for all blockchain integrations
2. **Per-Blockchain Adapter**: Implementation for each supported blockchain
3. **Register System Integration**: Functions for register operations and ZK proofs
4. **Cross-Chain Coordination**: Methods for register transfers across chains

### Core Adapter Interface

```haskell
data BlockchainAdapter = BlockchainAdapter
  { adapterID :: Text                     -- Unique identifier
  , supportedTimeline :: TimelineID       -- Timeline supported
  , connect :: ConnectionConfig -> IO Connection       -- Connect to blockchain
  , observeFact :: FactType -> IO Fact    -- Observe external fact
  , deposit :: Account -> Asset -> Amount -> IO Effect  -- Deposit to blockchain
  , withdraw :: Account -> Asset -> Amount -> Address -> IO Effect  -- Withdraw from blockchain
  , submitTransaction :: SerializedTx -> IO TxHash  -- Submit raw transaction
  
  -- Register system extensions
  , createRegister :: RegisterContents -> IO RegisterID  -- Create register
  , updateRegister :: RegisterID -> RegisterContents -> Authorization -> IO Effect  -- Update register
  , observeRegister :: RegisterID -> IO RegisterFact  -- Observe register state
  , verifyProof :: VerificationKey -> Proof -> IO ZKProofFact  -- Verify ZK proof
  , transferRegister :: RegisterID -> TimelineID -> ControllerLabel -> IO Effect  -- Cross-chain transfer
  , registerExists :: RegisterID -> IO Bool  -- Check if register exists
  , generateProof :: CircuitType -> Inputs -> IO Proof  -- Generate ZK proof
  }
```

### Register System Integration

Blockchain adapters must handle register operations:

1. **Register Creation**: Creating on-chain representations of registers
2. **Register Updates**: Updating register contents with appropriate authorization
3. **Register Observation**: Observing register state from the blockchain
4. **Proof Verification**: Verifying ZK proofs for register operations
5. **Cross-Chain Transfers**: Transferring registers between blockchains

### Register Operation Flow

For register operations, the flow is:

1. **Input Validation**: Validate register operation request
2. **Authorization Check**: Verify operation is authorized
3. **ZK Proof Generation**: Generate proof if required
4. **Transaction Creation**: Create blockchain-specific transaction
5. **Transaction Submission**: Submit transaction to blockchain
6. **Observation**: Observe register state after operation
7. **Fact Generation**: Generate fact for register operation
8. **Propagation**: Propagate fact to other components

### Cross-Chain Register Transfer

Cross-chain register transfers require special handling:

1. **Source Chain**: Lock or burn register on source chain
2. **Controller Label**: Generate controller label for ancestral validation
3. **Proof Generation**: Generate proof of source chain operation
4. **Target Chain**: Create register on target chain with proof
5. **Time Map**: Update time map for temporal validation
6. **Observation**: Observe register transfer as a fact

## Consequences

### Positive

- Standardized interface for all blockchain interactions
- Simplified integration of new blockchains
- Consistent handling of register operations
- Improved cross-chain coordination
- Enhanced security through ZK proof verification
- Clear separation of concerns between business logic and blockchain interaction

### Negative

- Complexity in implementing adapters for diverse blockchains
- Performance overhead from abstraction
- Challenges in handling blockchain-specific features
- Development effort required for ZK circuit implementation

### Neutral

- Requires ongoing maintenance as blockchains evolve
- May need extensions for blockchain-specific features
- Adapters may vary in feature support based on blockchain capabilities 