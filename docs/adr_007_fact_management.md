# ADR 007: Fact Management and Observation Pipeline


## Status

Proposed

## Context

Facts are **observations of external state** collected from timelines, oracles, and other programs. In previous iterations of the system, facts were handled informally — they were retrieved ad hoc via queries and were **not part of the causal history** of programs.

This created several problems:

- **Replays were incomplete:** Replaying a program required querying live timelines or re-deriving facts from external sources.
- **Causal Traceability was Broken:** Effects often depended on facts (e.g., asset prices) without explicitly recording the facts themselves.
- **Fact Provenance was Unclear:** Facts could not be individually proven — there was no consistent way to verify who observed a fact and how it was authenticated.
- **Cross-Timeline Coordination was Compromised:** Facts that came from other timelines were treated differently from local facts, introducing inconsistency.

### New Approach: Facts as First-Class Causal Entities

In the new model, **facts are causal objects** — every effect that depends on external state must explicitly reference the facts it observed. Facts are:
- Stored in a dedicated **FactLog** (append-only, content-addressed).
- Included in every effect that depends on them as a **FactSnapshot**.
- Fully replayable, like effects themselves.
- Proven by the observer who collected them (usually a Time Keeper).
- Verifiable via cryptographic proofs.


# Decision

## Core Data Structures

### Fact

```haskell
data Fact = Fact
    { factID :: FactID
    , timeline :: TimelineID
    , factType :: FactType
    , factValue :: FactValue
    , observedAt :: LamportTime
    , observationProof :: ObservationProof
    }
```

- `factID`: Content hash of the fact (unique identifier).
- `timeline`: Source timeline (e.g., Ethereum).
- `factType`: What kind of fact (e.g., Balance, Price, Transaction Inclusion).
- `factValue`: Actual value (e.g., `{"ETH/USDC": 2900}`).
- `observedAt`: Lamport timestamp of observation.
- `observationProof`: Cryptographic proof of correctness (signed by Time Keeper).


### FactSnapshot (included in every effect)

```haskell
data FactSnapshot = FactSnapshot
    { observedFacts :: [FactID]
    , observer :: KeeperID
    }
```

- `observedFacts`: The set of facts the effect depended on.
- `observer`: Who observed them (trusted source).


### FactLog

```haskell
data FactLog = FactLog
    { timeline :: TimelineID
    , entries :: [FactLogEntry]
    }
data FactLogEntry = FactObserved Fact
                  | FactExpired FactID
```

- Timeline-specific log of observed facts.
- Append-only.
- Content-addressed (the entire log has a root hash).


## Fact Sources

Facts can originate from:
- Timeline Keepers (on-chain balances, prices, transaction confirmations).
- Oracle Programs (external feeds).
- Derived Facts (computed from existing facts by other programs).


## Fact Lifecycle

| Phase | Action |
|---|---|
| Observation | Keeper observes fact, writes to FactLog. |
| Proposal | Bandits gossip observed fact to peers. |
| Consumption | Programs read facts from log during effect application. |
| Snapshotting | Every effect that reads facts records their IDs in FactSnapshot. |
| Replay | Replaying an effect verifies fact proofs directly from FactLog. |
| Expiry (optional) | Certain facts may have TTLs, after which they are archived. |


## Interface with Other Parts of the System

| Component | Role |
|---|---|
| Time Keeper | Observes facts, signs proofs, appends to FactLog. |
| Program Execution | Queries facts before applying effects. |
| Effect DAG | Every effect references observed facts causally. |
| Bandits (P2P) | Gossip facts alongside proposed effects to ensure consensus on external observations. |
| Replay Engine | Rehydrates facts during replay, verifies proofs. |
| Observer System | Can inject synthetic facts for testing. |


# Example Fact Observation Flow

### Initial Observation (Ethereum Price)

1. Ethereum Time Keeper observes:
    - `ETH/USDC = 2900`
2. Keeper signs fact with proof of inclusion (block header + Merkle proof).
3. Fact recorded in Ethereum FactLog.

### Bandit Gossip

1. Bandits gossip the observed fact.
2. Each Bandit appends to its local FactLog.
3. Facts are content-addressed (same fact = same hash for all Bandits).

### Program Consumption

1. Program calls `observeFact("ETH/USDC")`.
2. Keeper retrieves fact from FactLog.
3. Program embeds fact hash into next effect’s FactSnapshot.


# Example Fact Format (JSON)

```json
{
    "factID": "bafy...",
    "timeline": "Ethereum",
    "factType": "Price",
    "factValue": { "ETH/USDC": 2900 },
    "observedAt": "LamportTime(12345)",
    "observationProof": {
        "inclusionProof": "0xabc123...",
        "signedBy": "keeper.eth"
    }
}
```


# Benefits

- Programs have explicit, immutable causal references to all external state they depend on.  
- All fact observations are provable — each fact has a signed proof.  
- Replays become fully deterministic — facts do not change between runs.  
- Observers can simulate new facts (fault injection for testing).  
- Programs become **timeline-agnostic** — they consume facts, not raw RPC responses.  
- Fact history is **independently auditable** — Bandits can prove they acted on authentic observations.


# Changes to Existing Systems

| Component | Change |
|---|---|
| Program State | Add fact snapshot to every effect. |
| Effect DAG | Add fact snapshot to effect payload. |
| P2P Messaging | Add facts to message format, sync facts before syncing effects. |
| Time Keeper | Writes fact log and generates proofs. |
| Fact Store | New module responsible for persisting FactLog to disk. |
| Replay Engine | Rehydrate facts before effect replay. |
| Observer System | Can inject synthetic facts into FactLog. |


# Safe State and Fact Consistency

Safe states now depend partially on fact freshness. For example:
- Programs that rely on a **price feed** should only be in safe states if:
    - They have an observed price within the last N blocks.
    - This price was signed by the correct keeper.
- This creates a direct dependency between **timeline liveness** and program upgradability.


# Testing Considerations

- Unit tests for Fact, FactSnapshot, FactLog types.  
- Tests for FactLog append, read, expiry.  
- Test full observation lifecycle (observe → gossip → consume → replay).  
- Test cross-timeline observation (Ethereum keeper sends fact to Solana program).  
- Test replay mismatch (fact missing or proof invalid).

# Example FactLog Storage on Disk

Fact logs are per-timeline, append-only files:

- `/var/time-bandits/facts/ethereum/ 0001.log 0002.log`
- `/var/time-bandits/facts/solana/ 0001.log`


Each entry is a content-addressed fact (CID):

```json
{
    "cid": "bafy...",
    "fact": { ... }
}
```

---

# Summary

This ADR formalizes facts as **first-class causal entities**, tightly coupled to the effect pipeline and replay engine. This ensures:

- Programs never depend on mutable external state directly.  
- All external state changes are provable and replayable.  
- Cross-timeline facts flow through the same causal pipeline as local effects.  
- Facts have consistent semantics across all timelines.
