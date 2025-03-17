# ADR-002: Time Model


## Status

**Accepted - Updated 2025-03-07**


## Context

Time Bandits programs operate across **multiple independent timelines** — each corresponding to a blockchain, rollup, or distributed ledger. Each timeline advances **independently and asynchronously**, with its own:

- Consensus process.
- Block height and timestamps.
- Inclusion and proof mechanisms.
- Finality guarantees.

This makes it essential for Time Bandits to maintain **a unified and causally consistent view of time** across all timelines participating in a program's execution. This view must:
- **Capture external observations from each timeline.**
- **Provide replayable proofs of external facts.**
- **Preserve internal causal ordering between program effects.**

Programs need to reason about time across timelines to ensure:

1. Causal consistency
2. Temporal ordering
3. Finality tracking
4. Cross-timeline coordination


## Decision

We will use a unified time model that combines:

1. **Timeline-local Time**: Each timeline maintains its own Lamport clock
2. **Cross-Timeline Time Maps**: Programs track relative time positions across timelines
3. **Register-Based Time Commitments**: Time maps stored in registers and verified with ZK proofs

### Time Model Components

```haskell
-- Timeline-local Lamport clock
data LamportClock = LamportClock
  { timelineID :: TimelineID
  , counter :: Counter
  }

-- Map of timeline positions
data TimeMap = TimeMap
  { positions :: Map TimelineID Height
  , observedAt :: LamportTime
  , commitments :: Map TimelineID Commitment
  }

-- Register-based time commitment
data TimeMapCommitment = TimeMapCommitment
  { registerId :: RegisterID
  , timeMap :: TimeMap
  , proof :: Proof
  , lastUpdated :: BlockHeight
  }
```

### Time Operations

```haskell
-- Update local Lamport clock
tickClock :: LamportClock -> LamportClock
tickClock clock = clock { counter = clock.counter + 1 }

-- Merge two time maps, taking the later position for each timeline
mergeTimeMaps :: TimeMap -> TimeMap -> TimeMap
mergeTimeMaps tm1 tm2 = TimeMap
  { positions = Map.unionWith max tm1.positions tm2.positions
  , observedAt = max tm1.observedAt tm2.observedAt
  , commitments = Map.union tm1.commitments tm2.commitments
  }

-- Check if a time map is ahead of another for all timelines
isAheadOf :: TimeMap -> TimeMap -> Bool
isAheadOf tm1 tm2 = 
  all (\(tid, h) -> Map.findWithDefault 0 tid tm1.positions >= h) 
      (Map.toList tm2.positions)

-- Create a time map commitment in a register
commitTimeMap :: TimeMap -> IO TimeMapCommitment
commitTimeMap tm = do
  proof <- generateTimeMapProof tm
  regId <- createRegister (TimeMapContents tm.positions tm.commitments)
  return TimeMapCommitment
    { registerId = regId
    , timeMap = tm
    , proof = proof
    , lastUpdated = getCurrentHeight()
    }

-- Verify a time map commitment
verifyTimeMapCommitment :: TimeMapCommitment -> IO Bool
verifyTimeMapCommitment tmc = do
  registerExists <- checkRegisterExists tmc.registerId
  proofValid <- verifyProof tmc.proof tmc.timeMap
  return (registerExists && proofValid)
```

### Register-Based Time Maps

Time maps will be stored in registers to enable:

1. **On-chain verification**: Timelines can verify time maps in smart contracts
2. **ZK proof generation**: Generate ZK proofs of time map correctness
3. **Cross-timeline coordination**: Share time maps between timelines securely
4. **Temporal validation**: Verify temporal ordering of operations
5. **Auditability**: Track when timelines have been observed

### ZK Circuit for Time Map Verification

```haskell
-- ZK circuit for verifying time map updates
verifyTimeMapUpdate :: Circuit
verifyTimeMapUpdate = Circuit
  { name = "TimeMapUpdate"
  , inputs = 
      [ "oldTimeMap" # commitment
      , "newTimeMap" # commitment
      , "timelineUpdates" # list (pair timelineId height)
      ]
  , outputs = 
      [ "valid" # boolean
      ]
  , constraints =
      [ "valid" === allUpdatesValid "oldTimeMap" "newTimeMap" "timelineUpdates"
      ]
  }

-- Generate a proof for a time map update
generateTimeMapProof :: TimeMap -> TimeMap -> [(TimelineID, Height)] -> IO Proof
generateTimeMapProof oldTm newTm updates = do
  -- Generate ZK proof that newTm is a valid update to oldTm
  circuit <- compileCircuit verifyTimeMapUpdate
  witness <- generateWitness circuit
    [ "oldTimeMap" := oldTm
    , "newTimeMap" := newTm
    , "timelineUpdates" := updates
    ]
  generateProof circuit witness
```

### Cross-Timeline Temporal Validation

```haskell
-- Validate that an operation respects temporal ordering
validateTemporalOrdering :: TimeMap -> Operation -> TimeMap -> IO Bool
validateTemporalOrdering requiredTm op actualTm = do
  -- Check if actual time map is ahead of required time map
  let temporallyValid = isAheadOf actualTm requiredTm
  
  -- For register operations, verify time map commitment
  case op of
    RegisterOp regId _ _ -> do
      commitment <- getRegisterTimeMapCommitment regId
      commitmentValid <- verifyTimeMapCommitment commitment
      return (temporallyValid && commitmentValid)
    
    _ -> return temporallyValid
```

## Time Map Components

| Field | Description |
|---|---|
| Timeline ID | Ethereum, Solana, Celestia, etc. |
| Height | Current block height or slot number. |
| Hash | Block hash or equivalent commitment. |
| Timestamp | Block timestamp (if provided by the chain). |


## Example Time Map

```toml
[time_map.Ethereum]
height = 123456
hash = "0xabc123"
timestamp = 1710768000

[time_map.Celestia]
height = 98765
hash = "0xdef456"
timestamp = 1710768005
```


## Observed Time Map (Per Effect)

Every proposed effect — whether originating from a time traveler, account program, or program-to-program invocation — records the **time map snapshot** that was observed when the effect was proposed.

This **Observed Time Map** is **part of the effect proof**, ensuring that:

- Each effect is tied to **a specific set of external facts**.
- Each precondition check (e.g., balance proofs) is tied to the **exact external state** at the time of proposal.
- Replay and audit can reconstruct **the same snapshot** to check for validity.


## Time Map in the Effect Pipeline

| Stage | Role of Time Map |
|---|---|
| Proposal | Proposing actor queries latest Time Map and embeds it in effect proposal. |
| Application | Effect is re-validated against current Time Map before application. |
| Replay | Replay reconstructs each observed Time Map to re-run all precondition checks. |


## Internal Logical Time: Lamport Clock

Each program maintains an **internal Lamport clock**, which tracks:

- Total causal ordering of all effects applied within the program.
- Monotonic sequence number for each applied effect.
- Links to the **per-resource effect log**.

This ensures internal time is totally ordered **within each program** — even if external timelines advance asynchronously.


## Precondition Horizon

Every effect records:
- The **observed time map** (snapshot at proposal time).
- The external facts it depended on (balance proofs, inclusion proofs).

At the time of application, the **current time map** is compared to the observed one:

- If the time map advanced (new blocks observed), external preconditions are **revalidated**.
- If preconditions still hold, the effect applies.
- If preconditions fail under the new time map, the effect is rejected.

This protects programs against:

- Reorgs.  
- Double-spends (withdrawals already processed).  
- External state drift (balance changes, price changes).  


## Time Map Hashing

Each Time Map is **content-addressed**:

```haskell
timeMapHash = hash(all timelines' heights, hashes, timestamps)
```

This hash is:

- Stored directly in every applied effect's log entry.
- Included in every effect proof.
- Passed into proof-of-correct-execution for zk generation.

This guarantees:

- Effects are cryptographically linked to external state.
- Time consistency is independently verifiable.
- Effects cannot retroactively depend on altered facts.


## Time Map and the Unified Log

Every applied effect in the **unified log** includes:

- Observed Time Map.
- Time Map hash.
- Parent effect hash (causal link).
- Logical timestamp (Lamport clock tick).

This ensures the unified log records:

- **Causal history of effects.**
- **External timeline observations at each step.**
- **Causal consistency with both internal and external time.**


## Replay and Time Reconstruction

Replay must reconstruct:

- Full sequence of applied effects from the unified log.  
- Exact time map snapshots that were observed at proposal time.  
- Precondition checks against reconstructed time maps.  

This makes Time Bandits **fully replayable and auditable from first principles**, even if no live blockchain connection exists during replay.


## Watches and Observations

The **watch primitive** (e.g., "wait for a deposit") works by:

1. Querying the current time map.
2. Proposing an effect that **observes** the desired event at a known block height.
3. Validating that the event still exists at effect application time.

This provides:

- Causal consistency between observation and program state.  
- Replayable proof that the observation was valid.  
- Defense against reorg-based ambiguity.  


## Summary - What Each Effect Carries

| Field | Purpose |
|---|---|
| Observed Time Map | Declares external state known at proposal time. |
| Time Map Hash | Commit to specific external snapshot. |
| Parent Effect Hash | Causal predecessor. |
| Lamport Clock | Internal ordering. |
| Proof | Proves valid state transition given observed facts. |


## Cross-Timeline Consistency

The Time Map serves as the **global clock boundary** across all timelines:

- Internal causal order (Lamport clock) applies within programs.
- External timeline order (Time Map snapshots) applies across programs and timelines.
- Cross-timeline consistency is ensured by:
    - Observing facts via Time Keepers.
    - Embedding observed facts into effects.
    - Linking effects to the observed Time Map.


## Time in Simulation and Production

This model applies equally to:

| Mode | Time Source |
|---|---|
| In-Memory Simulation | Synthetic Time Map generated by controller. |
| Multi-Process Local | Each process queries local Time Keeper for Time Map. |
| Geo-Distributed | Each actor queries remote Time Keeper for Time Map. |

Everywhere, the **Time Map API** is:

```haskell
getLatestTimeMap :: TimelineID -> IO TimeMap
observeFact :: FactQuery -> IO (ObservedFact, TimeMap)
```


## Example Time Map Evolution Flow

1. Traveler proposes effect at block 100.
    - Observed Time Map includes block 100 hash.
    - Balance proof at block 100.
2. By the time the effect applies, block 102 is observed.
    - Precondition check:
        - Re-fetch balance at block 102.
        - Check if balance still meets preconditions.
        - Check inclusion proof is still valid in canonical chain.
    - If valid, apply.
    - If invalid, reject.


## Time Map Consistency Invariants

- Every effect carries exactly **one observed time map**.  
- Every applied effect records the **time map hash**.  
- No effect can apply unless preconditions hold against the **current time map**.  
- Every fact and observation passes through Time Keepers — no direct timeline RPC in programs.  
- Time Maps are **content-addressed and signed**.

## Mock ZK Proof Implementation

For development and testing purposes, the Time Map system integrates with a mock ZK proof and verification system that provides the same logical interfaces as actual ZK proofs while simplifying the cryptographic aspects.

### Mock ZK Store

Each timeline maintains a key-value store for verification keys and proofs:

```haskell
-- Mock ZK system key-value store
data MockZKStore = MockZKStore
  { verificationKeys :: Map VerificationKey CircuitType
  , proofPairs :: Map VerificationKey ProofData
  , validationResults :: Map (ProofData, ByteString) Bool
  }

type VerificationKey = ByteString  -- Random string mimicking a real verification key
type ProofData = ByteString  -- Random string mimicking a real ZK proof
```

### Verification Key Generation

Prior to timeline instantiation, the system generates random strings to serve as verification keys:
- Each verification key is associated with a specific circuit type
- These keys are stored in the timeline's key-value store
- The keys mimic real ZK verification keys without requiring actual cryptographic operations

### Proof Generation and Validation

For each verification key, the system generates a corresponding proof string:

```haskell
-- Generate mock proof
generateMockProof :: TimeMap -> VerificationKey -> IO ProofData
generateMockProof timeMap verificationKey = do
  -- Generate random string as mock proof
  proofData <- generateRandomBytes 32
  -- Store association between verification key and proof
  storeProofPair verificationKey proofData
  -- Return the mock proof
  return proofData

-- Mock prove function
mockProve :: TimeMap -> ByteString -> VerificationKey -> ProofData -> IO Bool
mockProve timeMap computationOutput verificationKey proofData = do
  -- Look up verification key
  foundKey <- lookupVerificationKey verificationKey
  
  -- Get expected proof for this key
  expectedProof <- lookupProofForKey foundKey
  
  -- Validate computation against Time Map
  timeMapValid <- validateAgainstTimeMap timeMap computationOutput
  
  -- Check if proof matches and computation is valid
  let result = proofData == expectedProof && timeMapValid
  
  -- Record validation result
  storeValidationResult (proofData, computationOutput) result
  
  return result
```

### Time Map Integration

The mock ZK system integrates with the Time Map in several key ways:

1. **Time Map Inclusion**: Each mock proof contains a reference to the Time Map hash, ensuring that proofs are associated with a specific observed state.

2. **Temporal Validation**: The mock proof system validates that the computation output is consistent with the observed Time Map.

3. **Cross-Timeline Operations**: For operations spanning multiple timelines, the system verifies that Time Maps across timelines are consistent, as part of the validation process.

### Resource Conservation Validation

For resource operations that must maintain conservation (ΔTX = 0):

```haskell
validateResourceOperation :: TimeMap -> [ResourceOp] -> IO (Either ValidationError ProofData)
validateResourceOperation timeMap operations = do
  -- Calculate resource delta
  let delta = calculateDelta operations
  
  -- Choose appropriate verification key
  vk <- getVerificationKeyForResourceOps operations
  
  if delta == 0
    then do
      -- Generate valid proof for conservative operations
      proof <- generateMockProof timeMap vk
      return (Right proof)
    else
      -- Return error for non-conservative operations
      return (Left $ ConservationViolation delta)
```

This integration ensures that all temporal proofs (based on the Time Map) and resource conservation proofs work together, maintaining the same logical guarantees that real ZK proofs would provide, while simplifying development and testing.

## Benefits

- Works across any blockchain (timeline-agnostic).  
- Replayable even with no live blockchain access.  
- Handles reorgs gracefully.  
- No program ever queries timelines directly.  
- Fully auditable causal link between program state and external reality.  
- Compatible with zk proof generation.


This time model ensures:

- **Internal causal consistency.**
- **External factual consistency.**
- **Replayable proofs of all observations.**
- **Auditability across program boundaries.**

It is **foundational** to the Time Bandits architecture and is required for secure, auditable, cross-timeline programs.
