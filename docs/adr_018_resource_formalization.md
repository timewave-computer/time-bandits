# ADR 018: Resource Formalization

## Status

Accepted, with register-based extensions

## Context

Time Bandits programs manipulate resources across timelines. Resources include:

- Tokens
- NFTs
- Access rights
- State commitments
- Data objects

Currently, resources are managed through ad-hoc implementations with:
- Inconsistent conservation rules
- Limited composability
- Unclear ownership boundaries
- Insufficient verification mechanisms

We need a formal model that enables:
- Resource conservation across timelines
- Verifiable resource ownership
- Resource composition and transformation
- Zero-knowledge verification of resource operations

## Decision

We will formalize resources as structured tuples with the following components:

```haskell
data Resource = Resource
    { resourceLogic :: Logic               -- Rules governing usage
    , fungibilityDomain :: Label           -- Defines equivalence class
    , quantity :: Quantity                 -- Numerical representation
    , metadata :: Value                    -- Associated data
    , ephemeral :: Bool                    -- Whether existence must be verified
    , nonce :: Nonce                       -- Uniqueness identifier
    , nullifierPubKey :: NullifierPK       -- For verifying consumption
    , randomnessSeed :: Seed               -- For deriving randomness
    }
```

With derived properties:
- `commitment` - Commitment to resource existence
- `nullifier` - Proof of resource consumption
- `kind` - Classification of resource type
- `delta` - Resource balance change

### Register-Based Resource Management

Resources will be managed through registers:

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

### Resource Conservation Laws

All register operations must satisfy resource conservation laws:

1. **Creation Conservation**: Resources can only be created with valid external proofs
2. **Transfer Conservation**: Transfers must preserve total resource amounts
3. **Destruction Conservation**: Resources can only be destroyed with valid external proofs
4. **Cross-Chain Conservation**: Cross-chain transfers preserve controller labels

### ZK Verification of Resource Operations

Resource operations will be verified using zero-knowledge proofs:

```haskell
data ResourceOperation = ResourceOperation
    { opType :: OperationType
    , inputs :: [Resource]
    , outputs :: [Resource]
    , delta :: Delta
    , authorization :: Authorization
    , proof :: Proof
    }

data Proof = Proof
    { verificationKey :: VerificationKey
    , proofData :: ByteString
    , publicInputs :: [ByteString]
    }
```

ZK circuits will verify:
1. Resource conservation (sum of inputs = sum of outputs)
2. Valid transformations according to resource logic
3. Proper authorization of operations
4. Correct nullifier generation
5. Valid commitment opening

### Cross-Timeline Resource Flow

Resources that cross timelines will maintain controller labels:

```haskell
data ControllerLabel = ControllerLabel
    { creatingController :: ControllerID
    , terminalController :: ControllerID
    , affectingControllers :: [ControllerID]
    , backupControllers :: [ControllerID]
    }
```

Cross-timeline transfers will undergo dual validation:
1. **Temporal Validation** - Using time maps to verify causal consistency
2. **Ancestral Validation** - Using controller labels to verify provenance

## Consequences

### Positive

- Formal guarantee of resource conservation across operations
- ZK proofs provide strong verification with privacy
- Clear resource ownership through register system
- Composable resource operations
- Cross-timeline resource tracking
- Unified resource model for all timelines

### Negative

- Additional complexity in implementing resource operations
- Computational overhead for ZK proof generation and verification
- Learning curve for developers
- ZK circuit complexity for complex resource transformations

### Neutral

- Requires standardization of resource representations
- May need extensions for specialized resource types
- Evolution of ZK technology may require updates