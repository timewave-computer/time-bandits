# ADR 006: Resource Ownership

## Status

Accepted, with register-based extensions

## Context

Time Bandits programs need to interact with resources on external blockchains, including:

- Fungible tokens (e.g., ERC-20)
- Non-fungible tokens (e.g., ERC-721)
- Native chain assets (e.g., ETH, BTC)
- Data items (e.g., price feeds, oracle responses)
- State commitments (e.g., Merkle roots)

In previous iterations, resources were directly associated with programs, leading to:

1. Unclear ownership boundaries
2. Difficulties in tracking causal dependencies
3. Complex access control patterns
4. Security vulnerabilities from direct resource manipulation

## Decision

We will transition to a **register-based resource ownership model** where:

1. **Account Programs Own Registers**: Each account program manages a set of registers.
2. **Registers Hold Resources**: External resources are held in registers with clear ownership.
3. **Register Operations Control Access**: Resources are accessed through formally verified register operations.
4. **ZK Proofs Ensure Correctness**: Register operations are verified with zero-knowledge proofs.

### Resource Register Structure

Resources will be held in registers with the following structure:

```haskell
data Register = Register
    { registerId :: RegisterID
    , owner :: Address
    , contents :: RegisterContents
    , lastUpdated :: BlockHeight
    , metadata :: Map Text Value
    , controllerLabel :: Maybe ControllerLabel
    }

data RegisterContents 
    = FormalizedResource Resource
    | TokenBalance TokenType Address Amount
    | NFTContent CollectionAddress TokenId
    | StateCommitment CommitmentType ByteString
    | DataObject DataFormat ByteString
    | CompositeContents [RegisterContents]
```

### Resource Access Patterns

Access to resources follows these patterns:

1. **Creation**: Resources are created as registers by account programs.
2. **Authorization**: Register operations require explicit authorization.
3. **Operations**: Resources can be manipulated through authorized register operations.
4. **Transfer**: Resources can be transferred between registers with appropriate authorization.
5. **Cross-Chain Movement**: Resources can move across chains through register transfers with controller labels.

### Authorization Methods

Register operations can be authorized through various methods:

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

### Conservation Laws

All register operations must satisfy resource conservation laws:

1. **Creation Conservation**: Resources can only be created with valid external proofs.
2. **Transfer Conservation**: Transfers must preserve total resource amounts.
3. **Destruction Conservation**: Resources can only be destroyed with valid external proofs.
4. **Cross-Chain Conservation**: Cross-chain transfers must preserve controller labels for ancestral validation.

## Consequences

### Positive

- Clear resource ownership boundaries
- Improved security through formalized access control
- Better tracking of resource provenance
- Simplified resource management for program developers
- Enhanced auditability through register operations log
- ZK proofs provide strong correctness guarantees

### Negative

- Additional complexity in resource access patterns
- Performance overhead from ZK proof generation and verification
- Learning curve for developers accustomed to direct resource access

### Neutral

- Requires standardized register interfaces across blockchains
- May need periodic updates to authorization methods as security practices evolve
