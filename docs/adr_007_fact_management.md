# ADR 007: Fact Management

## Status

Accepted, with register-based extensions

## Context

Time Bandits programs need to observe and react to **facts** from external timelines. Facts could be:

1. Account balances
2. Price data
3. Transaction confirmations
4. State transitions
5. Register state updates
6. ZK proof verifications

Previously, facts were treated as secondary to effects - programs would apply effects and then check facts. This led to:

- Unclear causality between facts and effects
- Difficulties in replay and simulation
- Inconsistent fact verification across timelines
- Challenges in tracking fact dependencies

## Decision

We will transition to treating **facts as first-class causal entities** with the following approach:

1. Facts are **observed by keepers**
2. Facts are **signed and timestamped**
3. Facts are **content-addressed**
4. Programs **depend on facts explicitly**
5. Facts include **register observations and proofs**

### Core Data Structures

```haskell
data Fact = Fact
  { factID :: FactID                 -- Content hash of the fact
  , timeline :: TimelineID           -- Which timeline the fact comes from
  , factType :: FactType             -- Categorization of fact
  , factValue :: Value               -- The actual data
  , observedAt :: LamportTime        -- When the fact was observed
  , observationProof :: Proof        -- Proof of observation
  }

data FactType
  = BalanceFact                      -- Token or native currency balance
  | TransactionFact                  -- Transaction completion on external chain
  | OracleFact                       -- Data from external oracle
  | BlockFact                        -- Block information
  | TimeFact                         -- Time observation
  | RegisterFact                     -- Register state or operation
  | ZKProofFact                      -- ZK proof verification result
  | Custom Text                      -- Custom fact type

data RegisterFact
  = RegisterCreation RegisterID RegisterContents
  | RegisterUpdate RegisterID RegisterContents
  | RegisterTransfer RegisterID TimelineID ControllerLabel
  | RegisterMerge [RegisterID] RegisterID
  | RegisterSplit RegisterID [RegisterID]

data ZKProofFact
  = ProofVerification VerificationKey Proof
  | BatchVerification [VerificationKey] [Proof]
  | CircuitExecution CircuitType Inputs Outputs
  | ProofComposition ProofID [ProofID]
```

### Fact Observation Workflow

1. **External Observation**: Time Keepers observe external events
2. **Proof Generation**: Keepers generate observation proofs
3. **Fact Creation**: Keepers create facts with proofs
4. **Fact Propagation**: Facts are gossiped to Bandits
5. **Fact Verification**: Bandits verify fact proofs
6. **Fact Storage**: Facts are stored in fact logs
7. **Register Observation**: Register states are observed and recorded as facts
8. **ZK Proof Verification**: ZK proofs are verified and recorded as facts

### FactSnapshot and Effect Dependencies

Programs explicitly depend on facts through fact snapshots:

```haskell
data FactSnapshot = FactSnapshot
  { observedFacts :: [FactID]        -- Facts observed before effect
  , observer :: KeeperID             -- Who observed the facts
  , registerObservations :: Map RegisterID ByteString  -- Register state observations
  }

data Effect a = Effect
  { effectType :: EffectType a       -- Type of effect
  , factSnapshot :: FactSnapshot     -- Facts depended on
  , effectValue :: a                 -- Effect payload
  , appliedAt :: LamportTime         -- When effect was applied
  }
```

### Register-Related Facts

Register facts have special handling:

1. **Register Creation**: When a register is created, a RegisterCreation fact is observed
2. **Register Updates**: When a register is updated, a RegisterUpdate fact is observed
3. **Register Transfers**: When a register moves across chains, a RegisterTransfer fact is observed
4. **ZK Proofs**: When a ZK proof is verified, a ZKProofFact is observed

These facts enable programs to track register state and verify operations without direct blockchain queries.

### Fact Replay and Simulation

During replay and simulation:

1. Facts are replayed in observed order
2. Register facts are used to reconstruct register state
3. ZK proof facts are used to verify operation correctness
4. Programs only see facts they explicitly depended on
5. Fact snapshots establish clear causal relationships

## Consequences

### Positive

- Clear causality between facts and effects
- Improved replay and simulation fidelity
- Consistent fact verification across timelines
- Better tracking of fact dependencies
- Register operations can be tracked with explicit facts
- ZK proofs can be verified and recorded as facts

### Negative

- Additional complexity in handling fact dependencies
- Potential overhead from fact storage and propagation
- Learning curve for developers used to checking facts after effects

### Neutral

- Requires standardized fact formats across timelines
- May need periodic updates as new fact types emerge
- Register and ZK proof facts require specialized handling
