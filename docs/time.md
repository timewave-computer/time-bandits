# Time in Time Bandits: Multiple Layers

## Time Types

| Time Type | What it Tracks | Where it Lives | What it Guarantees |
| --- | --- | --- | --- |
| Lamport Time | Causal Order | Within each timeline and program | Total order of observed events per timeline |
| Timeline Time | External time (block height, slot number) | Native to each timeline | Timelines decide own consistency rules |
| Wall Clock Time | Local machine wall clock | Each peer | Untrusted, used only for diagnostics |
| Program Logical Time | Effect counter (per program) | Inside each program state | Sequential step order per program |
| Composite Time Map | Cross-timeline view of timeline heads | Observed by programs | Snapshot of causal state across time map |

## Lamport Clock Role

### Purpose
- To establish a total order of events within each timeline as observed by Time Bandits peers.
- To establish a total order of effects within a program execution (internal program step counter).
- To create a globally consistent ordering for all cross-timeline events within a given time map.

### Why Lamport Clocks?
Lamport clocks are the minimal viable causal clock — they only track:
- "Happens-before" relationships.
- Logical step counting.

This is all you need to enforce the causal ordering guarantees in the system contract, like:
- "Programs must observe each timeline as monotonically advancing."
- "Effects must apply in declared order."

### What Lamport Clocks Do NOT Do
- They do not track real time.
- They do not detect external clock drift.
- They do not relate to timestamps written into blocks.

### Lamport in Time Bandits — Where They Live

#### Per Timeline Observation
- Each peer assigns a Lamport timestamp to every event received from a timeline.
- Each peer increments its Lamport clock every time it relays or applies an event.
- This defines a local causal view of each timeline.

#### Per Program Execution
- Each program has a simple Program Counter (effectively a Lamport clock for its own internal sequence).
- Each time a program step is applied, the program counter increments.

#### Cross-Timeline Time Map
- Each time map combines the Lamport heads of each timeline.
- The time map's state hash is a Merkle root of per-timeline Lamport heads.

### Lamport Example (Per Timeline)
```
Timeline A:
Event 1: L = 1
Event 2: L = 2
Event 3: L = 3
```

This is a purely logical view that guarantees:
- Event 1 happened before Event 2.
- Event 2 happened before Event 3.
- If two peers disagree on the Lamport number of an event, one of them is provably out of sync.

### Lamport Example (Within Program)
```
Program P:
Step 1 (Deposit Escrow): L = 1
Step 2 (Release Escrow): L = 2
```

This guarantees:
- Step 1 must execute before Step 2.
- If Step 2 is observed without Step 1, the system is provably corrupt.

## Real-Time Clocks Role (Wall Clocks)

### Purpose
- Detect anomalies caused by faulty time sources.
- Provide diagnostic metadata, especially for cross-peer time reconciliation.
- Establish coarse bounds for finality windows (e.g., timeline A and timeline B need to be "close enough" for a program to advance).

### Wall Clock Example (Diagnostic)
```
Peer A received Block 500 at 12:00:01 UTC
Peer B received Block 500 at 12:00:05 UTC
```

If peers disagree significantly on arrival time, this indicates:
- A network partition.
- Or a drifting clock.

This has no direct causal effect on program correctness (which is governed by Lamport), but it does trigger anomaly detection.

## Timeline Clocks (Native Timeline Time)

### Purpose
- Respect each timeline's own notion of time.
- May be:
  - Block height.
  - Slot number.
  - Chain timestamp.
- Time Bandits must trust these as long as the timeline's own consensus holds.

### Timeline Clock Example
```
Ethereum Block 12,345 with timestamp 1709450000
Celestia Block 4567 with timestamp 1709450200
```

Time Bandits does not modify or re-interpret these timestamps — it imports them into the time map as facts, not opinions.

## Time Map (Composite Cross-Timeline Snapshot)

### Purpose
- Represents a coherent multi-timeline view.
- Combines:
  - Timeline Heads (block heights).
  - Timeline Logical Times (Lamport clocks).
  - Timeline Native Times (timestamps).
- Programs execute against a fixed time map.

### Example Time Map
```
TimeMap {
  ethereum = (head = block 12345, lamport = 100, timestamp = 1709450000)
  celestia = (head = block 4567, lamport = 67, timestamp = 1709450200)
}
```

## How This Fulfills the System Contract

| System Contract Clause | Fulfilled By |
| --- | --- |
| Programs inherit timeline consistency | Timeline Clocks (imported directly) |
| Programs observe each timeline monotonically | Lamport Clocks (per-timeline) |
| Programs execute steps in defined order | Program Counter (internal Lamport) |
| Programs see only declared time maps | Time Map (explicit boundary) |
| Cross-timeline events inherit causal order | Time Map's Lamport + Head ordering |
| Timeline drift must be detectable | Wall Clock vs Observed Timestamp |
| Every transition must link to causal parents | Event Trace (links via Lamport) |
| Causal order is provable in ZK proof | Lamport Clocks form proof basis |

## Illustrated Example

Imagine a cross-timeline escrow program:
1. Step 1: Alice deposits 100 GoldCoin on Ethereum.
2. Step 2: Bob deposits 200 SilverCoin on Celestia.
3. Step 3: If both deposits confirmed, release both.

Here's how time works:

| Event | Timeline | Lamport | Native Time | Wall Time |
| --- | --- | --- | --- | --- |
| Deposit Gold | Ethereum | 100 | 1709450000 | 12:00:00 |
| Deposit Silver | Celestia | 67 | 1709450200 | 12:00:02 |
| Program Step 3 | Time Bandits | 3 | N/A | 12:00:03 |

### Invariant Checks
- Step 3's Lamport must be > Step 2 and Step 1.
- Step 3 must only observe time maps that include Step 1 + 2.
- Step 3's proof must attest that:
  - GoldCoin exists on Ethereum (block 12345, L = 100).
  - SilverCoin exists on Celestia (block 4567, L = 67).

## Summary Table

| Time Layer | Purpose | Enforced By |
| --- | --- | --- |
| Native Timeline Time | Trusting chain clocks | External consensus |
| Lamport (per timeline) | Local causal order | Time Bandits peer |
| Lamport (per program) | Internal step order | Program execution engine |
| Time Map | Cross-timeline snapshot | Time Bandits system |
| Wall Clock | Drift detection only | Diagnostic only |
