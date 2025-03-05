# ADR-002: Time Model

## Status

Accepted

## Context

Time Bandits programs operate across multiple independent timelines, each of which corresponds to a blockchain, rollup, or distributed ledger. These timelines advance asynchronously and independently, which introduces the core challenge: how to define a single, causally consistent view of time across all timelines involved in a program's execution.

## Decision

We will implement a time model consisting of:
- The Time Map, a shared, observable snapshot of external time.
- The Observed Time Map, recorded by every effect when proposed.
- The Causal Clock, which tracks not only external timeline heights, but also the internal logical order of applied effects.
- The Precondition Horizon, which defines the safe window for effect preconditions based on timeline advancement.

### Key Concept: The Time Map

The Time Map is the authoritative record of the latest known state of all observed timelines. It is maintained collectively by the Time Keepers, who are responsible for:
- Observing each supported timeline (Ethereum, Celestia, etc.).
- Recording:
    - The latest block height.
    - The latest block hash.
    - The block's timestamp.
- Combining these into a content-addressed Time Map.

### Example Time Map

```yaml
time_map:
    Ethereum:
        height: 123456
        hash: "0xabc123"
        timestamp: 1710768000
    Celestia:
        height: 98765
        hash: "0xdef456"
        timestamp: 1710768005
```

### Observed Time Map (Per Effect)

Every effect proposed by a Time Traveler or by a program invoking another program records the exact time map snapshot it observed when it was proposed.

This snapshot becomes part of the effect's proof, ensuring:

- The effect depended on specific external facts (balances, proofs, etc.).
- The preconditions were originally validated against that snapshot.

### Time Map and Causal Ordering

The Time Map acts as the causal boundary between:

- Internal program memory, which follows strict causal order between effects applied to the same program.
- External facts from timelines, which advance independently, and whose latest state must be observed before proposing a valid effect.

Rule - Every applied effect must be provably linked to:

- Its immediate causal parent in the resource log.
- The time map snapshot it observed at proposal time.

This dual causality ensures that:

- Effects obey program-level causality (internal sequencing of state updates).
- Effects obey timeline-level causality (external facts were correct at the time of proposal).

### Precondition Horizon

When applying an effect, the system compares:

- The time map the effect observed.
- The current time map maintained by Time Keepers.

If the time map advanced (new blocks were observed), the system must:

- Revalidate all preconditions that depended on external facts.
- If the preconditions still hold, the effect can apply.
- If the preconditions fail under the new time map, the effect is rejected.

This ensures effects do not rely on stale external facts, protecting against:

- Reorgs.
- Withdrawals that already happened.
- Changes in external balances.

### Lamport Clock and Internal Logical Time

The Time Map tracks external (real-world) time, but programs also have an internal Lamport clock that tracks:

- The causal order of all effects applied within each program.

The Lamport clock advances every time an effect applies, forming the internal logical time for the program. This internal clock:

- Orders internal state updates.
- Provides a monotonic sequence number for each applied effect.
- Links directly into the per-resource logs.

### Time Map Hashing and Immutability

Each Time Map is content-addressed, meaning:

```haskell
timeMapHash = hash(latest height, hash, timestamp of all observed timelines)
```

This hash is:

- Stored in each applied effect's log entry.
- Included in each effect's proof.
- Passed into any proof-of-correct-execution.

This ensures that any effect can be independently verified as:

- Based on a correct, observable external state.
- Consistent with all other effects applied to the same program and resource.

## Consequences

### Time Map Consistency Rule

Whenever an effect is proposed, the proposing actor (time traveler or program) must observe the current time map first.

This means: 
- No effect can be proposed without knowing the latest external state.
- Effects that rely on old time maps are invalid, unless explicitly allowed by the program logic (e.g., archival queries).

### Program Replay and Time Reconstruction

When a program's state is reconstructed (e.g., for auditing or proving execution), the replay logic must:

- Reconstruct the full sequence of applied effects by following per-resource logs.
- Reconstruct the time map snapshot observed at each step.
- Re-run precondition checks against the observed time map (if required for audit).

This ensures:

- Programs can be proven correct post hoc, even if the program state is deleted and only the logs remain.
- Time Bandits programs are fully replayable from first principles.

### External Observations and Watches

The watch effect (e.g., observing a cross-chain deposit) works by:

- Querying the current time map for the target timeline.
- Submitting a proposed effect referencing that time map.
- When the effect applies, re-checking the current time map.

Generating a proof that the deposit existed at both proposal and application time.

## Implementation

### Summary: What Each Effect Carries

| Field             | Purpose                                                       |
|-------------------|---------------------------------------------------------------|
| Observed Time Map | Declares external facts known at proposal time.               |
| Time Map Hash     | Commit to specific external snapshot.                         |
| Parent Log Hash   | Commit to previous effect (causal parent).                    |
| Lamport Clock     | Monotonic internal clock within the program.                  |
| Proof             | Demonstrates valid state transition based on the above.       |

### Time and Simulation Consistency

This entire time model applies consistently across:

- In-memory simulation.
- Multi-process local simulation.
- Geo-distributed deployment.

In each case, the Time Keepers enforce a single consistent Time Map API:

- Fetch latest time map.
- Propose effect referencing current time map.
- Apply effect after validating against current time map.

### Example Time Map Handling in Effect Application

1. Traveler submits a deposit effect via their account program.
2. Effect references time map hash T123.
3. Time Bandit sees time map has advanced to T125.
4. Before applying, Bandit:
  - Fetches new balances using T125.
  - Re-validates deposit preconditions (correct balances, correct proofs).
  - If preconditions hold under T125, apply the effect.
  - If preconditions fail under T125, reject the effect.

### Summary Invariants

| Invariant | Description |
|-----------|-------------|
| Time Map Observation | Every proposed effect references a specific time map. |
| Time Map Causality | Every applied effect records its time map hash. |
| Time Map Replay | Replaying a program must reconstruct every time map observed. |
| Time Map Hashing | Time maps are content-addressed. |
| Precondition Horizon | Effects are re-validated if the time map advances before they apply. |

### Summary

Time Bandits time handling ensures: 
- Programs operate against a single causal snapshot of external reality.
- All external dependencies are fully recorded and proven.
- Programs are replayable with time map reconstruction.
- Time is handled uniformly across all simulation and deployment modes.
- Precondition checks are always anchored to observed reality.
