# ADR-002: Time Model (Updated)


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


## Decision

### Core Concept: The **Time Map**

The **Time Map** is the canonical record of the **latest observed state across all timelines**. It serves as the **bridge between internal program time and external timeline time**.


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
timeMapHash = hash(all timelines’ heights, hashes, timestamps)
```

This hash is:

- Stored directly in every applied effect’s log entry.
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
