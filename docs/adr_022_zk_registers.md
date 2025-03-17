# ADR 022: ZK-Based Register System for Timeline Adapters

## Status

Accepted

## Revision

1.0

## Author

@luca-rand

## Last updated

2023-09-14

## Context

The Time Bandits system enables provable execution across multiple chains (called timelines) to build secure cross-chain applications. The time-oriented model ensures no causality violation by ordering events in a coherent manner using the distributed map of time approach. However, as the system grows with more timelines and participants, we need a more structured way to handle the state interaction between timelines. This structure should:

1. Make the safety and correctness of operations provable on-chain
2. Enable native privacy-preserving design
3. Maintain blockchain-level security guarantees across timelines
4. Support atomic and conditional transfers across timelines

The current approach relies on each adapter being independently designed, creating inconsistencies and increasing the complexity of verifying correct operation across timelines.

## Decision

We will implement a register-based architecture for timeline adapters that uses Zero-Knowledge (ZK) proofs to verify operations. This approach standardizes how resources move across timelines and enables on-chain verification of the correctness of these operations.

### Core Components

1. **Registers**: Discrete units of storage that can hold resources, commitments, or other data
2. **Register Operations**: Well-defined operations that can be performed on registers (create, update, delete)
3. **Register Authorization**: Mechanisms to authorize register operations, including ZK proofs
4. **Resource Conservation Laws**: Mathematical invariants that ensure resources are neither created nor destroyed in an operation
5. **Proof Generation**: Generation of ZK proofs to verify the correctness of register operations
6. **On-Chain Verification**: Mechanisms to verify ZK proofs on-chain to ensure operation correctness

### Register Data Structure

```haskell
data Register = Register 
    { registerId :: RegisterID
    , owner :: Address
    , contents :: RegisterContents
    , lastUpdated :: BlockHeight
    , metadata :: Map Text Value
    }

data RegisterContents 
    = FormalizedResource Resource
    | TokenBalance TokenType Address Amount
    | NFTContent CollectionAddress TokenId
    | StateCommitment CommitmentType ByteString
    | TimeMapCommitment BlockHeight ByteString
    | DataObject DataFormat ByteString
    | EffectDAG EffectID ByteString
    | ResourceNullifier NullifierKey ByteString
    | ResourceCommitment CommitmentKey ByteString
    | CompositeContents [RegisterContents]
```

### Register Operations

```haskell
data RegisterOperation = RegisterOperation
    { opType :: OperationType
    , registers :: [RegisterID]
    , newContents :: Maybe RegisterContents
    , authorization :: Authorization
    , proof :: Proof
    , resourceDelta :: Delta
    }

data OperationType
    = CreateRegister
    | UpdateRegister
    | DeleteRegister
    | TransferOwnership Address
    | CompositeOperation [OperationType]
```

### Operation Authorization Methods

```haskell
data AuthorizationMethod
    = ZKProofAuthorization VerificationKey Proof
    | TokenOwnershipAuthorization TokenAddress Amount
    | NFTOwnershipAuthorization CollectionAddress TokenId
    | MultiSigAuthorization [Address] Int [Signature]
    | DAOAuthorization DAOAddress ProposalId
    | TimelockAuthorization Address Timestamp
    | CompositeAuthorization [AuthorizationMethod] AuthCombinator
```

### Resource Conservation

For any operation that manipulates resources, the system enforces:

```
Σ resources_before_operation = Σ resources_after_operation
```

This is verified through ZK proofs, ensuring that resources are neither created nor destroyed inappropriately, maintaining mathematical invariants even across multiple timelines.

### ZK Proof System Integration

Timeline adapters will generate ZK proofs to verify:

1. Valid register ownership and authorization
2. Correct execution of register operations
3. Maintenance of resource conservation laws
4. Temporal validation (ensuring operation follows causal rules)
5. Correct construction of register contents

These proofs are verified on-chain to provide blockchain-level security guarantees for cross-timeline operations.

### Cross-Timeline Operations

For operations involving multiple timelines:

1. The source timeline records the operation and generates a proof
2. The destination timeline verifies the proof
3. Both timelines maintain register records that are provably consistent
4. Resource conservation is verified across the entire operation

### Temporal Validation Integration

The register system integrates with temporal validation by:

1. Storing time map commitments in registers for on-chain verification
2. Generating ZK proofs that operations respect temporal ordering
3. Using the distributed map of time to order register operations causally

### Register ID Generation

To ensure uniqueness, each blockchain will have a dedicated function for register creation that sequentially increments an integer register identifier.

### Data Availability Requirements

To ensure all necessary data is available for on-chain verification, all transactions that reference data posted to a timeline's data availability layer must include the data itself, not just a reference.

## Advantages Over Traditional Adapter Approach

1. **Unified Model**: All adapters follow the same register-based architecture
2. **Provable Correctness**: Operations can be verified on-chain through ZK proofs
3. **Enhanced Security**: Resource conservation laws provide mathematical guarantees
4. **Privacy Preservation**: ZK proofs enable privacy-preserving design
5. **Cross-Timeline Atomicity**: Operations can be composed across timelines with atomic guarantees
6. **Standardized Development**: Adapter development follows consistent patterns
7. **Reduced Attack Surface**: The attack surface is minimized through formal verification

## Implementation Considerations

1. **ZK Circuits**: Develop standardized circuits for common register operations
2. **Gas Optimization**: Optimize ZK verification for gas cost on different chains
3. **Circuit Upgradability**: Design for upgradable ZK circuits as requirements evolve
4. **Migration Path**: Create a smooth migration path from existing adapter implementations
5. **Developer Experience**: Build tools to simplify working with the register system
6. **Testing Framework**: Implement comprehensive testing of ZK proof generation and verification
7. **Security Auditing**: Conduct thorough security audits of the ZK circuits and verification mechanisms

## Related Implementation Work

The detailed implementation plan for the ZK-based register system is documented in [Work Plan: Resource Formalization and ZK Register Implementation](../work/014.md). This comprehensive plan outlines:

1. A phased approach to implementation across six phases, from core data structures to production deployment
2. Detailed technical specifications for registers, operations, and ZK proof integration
3. Integration with the resource formalization model from ADR_018
4. Development of ZK circuits for register operations and resource conservation
5. Migration strategy from the previous adapter approach
6. Testing, security, and deployment considerations

The implementation work combines the register system with the resource formalization model to create a powerful framework for secure, verifiable cross-chain operations with strong mathematical guarantees.

## Impact on Other ADRs

The register-based architecture has significant implications for other ADRs in the Time Bandits system:

### ADR_002: Time Model
- Time map commitments will be stored in registers for on-chain verification
- ZK circuits will verify time map updates and merges
- Cross-timeline temporal validation will leverage register-based time commitments

### ADR_003: Effect Adapter Generation
- Adapters must handle register operations and ZK proofs
- Adapter schemas need to include register fields and ZK verification keys
- Generated code must support encoding register operations into blockchain-specific transactions

### ADR_006: Resource Ownership
- Complete rework to implement register-based resource ownership
- Resources will be held in registers with clear ownership boundaries
- Resource transfers will require register operations with ZK proofs

### ADR_007: Fact Management
- Facts will include register operations and ZK proof verification results
- Specialized handling for register-related facts
- Register state observations will be recorded as facts

### ADR_013: Temporal Effect Language
- Language extensions to express register operations and ZK proofs
- Compiler validation for register operations
- Register operation optimization during compilation

### ADR_015: Blockchain Adapter
- Adapters must implement register creation and management
- Support for ZK proof generation and verification
- Cross-chain register transfers with conservation validation

### ADR_018: Resource Formalization
- Formalized resources will be stored in registers
- Resource operations will be verified using ZK proofs
- Resource conservation laws will be enforced through register operations

## Conclusion

The ZK-based register system provides a robust, secure, and standardized approach to handling resources and state across multiple timelines. By using ZK proofs and enforcing resource conservation laws, we can build cross-chain applications with blockchain-level security guarantees. This architecture is a significant step toward making the Time Bandits system production-ready for critical financial applications across multiple blockchains.