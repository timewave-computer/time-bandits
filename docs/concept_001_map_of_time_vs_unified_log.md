# Concept Reference: Relationship Between the Map of Time and the Unified Log


## Overview

This document explains the relationship between **the Map of Time** and **the Unified Log** in the Time Bandits system. While both relate to the **ordering of events**, they exist at different **levels of abstraction** and serve distinct but complementary purposes.

With the introduction of the **resource formalization model** in ADR_018, this relationship has been extended to include **controller labels** and **formalized resources**, creating a comprehensive framework for tracking both causal dependencies and resource provenance across multiple chains.

The **ZK-based register system** (ADR_022) further extends this architecture, providing a clear boundary between on-chain state (registers) and off-chain logic, while ensuring cryptographic verification of all operations through zero-knowledge proofs.


## The Unified Log: Local, Concrete History

### What It Is

The **Unified Log** is the **local, append-only record** maintained by each actor (program, account program, time keeper, bandit) that records **what that actor did or observed**. Every actor has its own **separate log**.

### What It Contains

Each entry records one of:

- An **effect** applied by the actor.
- A **fact** observed by the actor (in the case of keepers).
- A **lifecycle event** (such as schema evolution or safe state transition).
- A **resource operation** that creates, transforms, or consumes resources (from ADR_018).
- A **controller label update** that tracks resource provenance across chains (from ADR_018).
- A **register operation** that modifies register state, including associated ZK proof (from ADR_022).
- A **register verification** that validates register state through ZK proofs (from ADR_022).

### Key Attributes

- **Actor-scoped:** Each actor has its own log.
- **Append-only:** New entries are appended, never overwritten.
- **Content-addressed:** Each entry has a unique hash (CID) derived from its content.
- **Immutable:** Once written, entries cannot be modified.
- **Replayable:** Replaying the log fully reconstructs actor state.
- **Resource-aware:** Tracks resource creation, transformation, and consumption with conservation laws (ΔTX = 0).
- **Proof-verifiable:** Register operations include ZK proofs that can be cryptographically verified.


## The Map of Time: Global, Abstract Causal Graph

### What It Is

The **Map of Time** is a **causal graph** that spans across all actors and timelines. It shows the **causal relationships** between:

- Observed facts (external timeline events observed by keepers).
- Applied effects (program state transitions triggered by facts or prior effects).
- Cross-program invocations (causal links between programs).
- Cross-timeline operations (causal links between facts on different chains).
- Register operations (cryptographically verified state transitions in registers).
- Register-based cross-timeline transfers (movement of resources between registers on different chains).

The Map of Time is **derived** from the logs — it is **not stored directly**, but can always be recomputed from the collection of all Unified Logs.

### What It Contains

The Map of Time contains **nodes** representing:

- **Facts:** Observations about external timelines.
- **Effects:** State transitions in programs.
- **Lifecycle events:** Program evolution events.
- **Resource Operations:** Creation, transformation, and consumption of resources.
- **Register Operations:** State transitions in on-chain registers with cryptographic proof.
- **ZK Proof Verifications:** Verification of register operations through zero-knowledge proofs.

And **edges** representing:

- **Causal dependencies:** When an effect depends on a fact or another effect.
- **Time dependencies:** When an observation happened after another.
- **Cross-timeline links:** When an effect on one chain caused an effect on another.
- **Resource flow:** Tracing resources as they move through the system.
- **Register Dependencies:** When a register operation depends on previous register state.
- **Cross-Register Flow:** When resources move between registers, particularly across timelines.

### Key Attributes

- **Global:** Spans all actors and chains.
- **Causal:** Shows what caused what.
- **Abstract:** A higher-level view of system state.
- **Derived:** Built from individual logs.
- **Non-linear:** A graph, not a simple sequence.
- **Verifiable:** All causal links can be verified through proofs.
- **Resource-coherent:** Ensures resource conservation across all paths.
- **ZK-verified:** Register operations are verified through zero-knowledge proofs.


## Register Operations in the Map of Time

The ZK-based register system introduces several key elements to the Map of Time:

### Register Nodes

Register operations appear as specific nodes in the Map of Time, including:

- **Register Creation:** The initial creation of a register on a specific chain.
- **Register Update:** State transitions in a register, accompanied by ZK proofs.
- **Register Resource Transfer:** Movement of resources between registers.
- **Register Nullifier Creation:** Consumption of resources with nullifier creation.
- **Register Commitment Verification:** Verification of resource commitments.

### ZK Proof Edges

Zero-knowledge proofs create new types of causal edges in the Map of Time:

- **Proof Verification Edges:** Connecting a register operation to its verification.
- **Time Map Verification Edges:** Connecting a register operation to Time Map verification.
- **Conservation Verification Edges:** Connecting multiple register operations involved in a resource conservation verification.
- **Execution Sequence Edges:** Connecting nodes in a complex execution sequence.

### Cross-Timeline Register Transfer

When resources move between registers across different chains, the Map of Time captures:

- **Source Register Operation:** The register update on the source chain.
- **Cross-Chain Message:** The message sent between chains.
- **Target Register Operation:** The register update on the target chain.
- **Dual Validation:** Both temporal and ancestral validation of the transfer.


## Register Operations in the Unified Log

Each actor's Unified Log now includes register-related entries:

### Program Account Logs

Program accounts log:
- Register creation requests
- Register operation authorizations
- Register state transitions
- ZK proof generation for register operations
- Register-based resource mappings

### Time Keeper Logs

Time Keepers log:
- Register observation facts
- Time Map updates in registers
- ZK proof verification for register operations
- Cross-timeline register transfer observations

### Time Bandit Logs

Time Bandits log:
- Register verification operations
- Execution sequence orchestration for registers
- ZK proof batching and verification
- Conservation law verification across registers


## The Inside/Outside Boundary

The ZK register system establishes a clear boundary between what lives "inside" the Time Bandits system and what lives "outside":

**Outside the System**:
- Tokens on native chains (ERC-20s, NFTs, native assets)
- Raw data on DA layers
- External chain state (block headers, transaction receipts)

**Inside the System**:
- Resources (internal accounting of what belongs to whom)
- Effect DAGs (causal history of operations)
- Time Maps (system view of external timeline state)

Registers sit precisely at this boundary, functioning as dimensional portals that connect the internal resource model with external token reality. Each register is simultaneously:
1. An on-chain entity that can hold real tokens
2. An internal accounting entry in the resource model

This boundary clarification has profound implications for the Map of Time and Unified Log:
1. The Unified Log tracks operations on both sides of the boundary, providing a complete history
2. The Map of Time represents the causal relationships that cross this boundary
3. ZK proofs provide cryptographic assurance that the boundary crossing was valid


## Controller Labels and the Register System

Controller labels in the register system serve as the ancestral validation component of dual validation:

```haskell
-- Controller label for cross-chain resources
data ControllerLabel = ControllerLabel
  { creatingController  :: ControllerID
  , terminalController  :: ControllerID
  , affectingControllers :: [ControllerID]  -- DAG of controllers that affected the resource
  , backupControllers   :: [ControllerID]   -- In case terminal fails
  }
```

When a resource moves between registers across different chains:
1. The controller label is updated to reflect the new path
2. This update is recorded in the Unified Log
3. The Map of Time captures the causal relationship between the source and target registers
4. Both temporal validation (via the Time Map) and ancestral validation (via the controller label) must succeed


## Analogies

To clarify these concepts:

- **Unified Log** is like a **personal diary** that records everything an individual actor did or saw.
- **Map of Time** is like a **history textbook** that connects events across many individuals into a coherent narrative.
- **Controller Labels** are like a **passport** that shows where a resource has traveled.
- **Registers** are like **safe deposit boxes** that physically hold external assets.

Together, they provide a complete system for tracking both what happened (Unified Log), why it happened (Map of Time), where resources came from (Controller Labels), and where assets are stored (Registers).


## Example Flow: Cross-Timeline Swap with Register Tracking

Consider a cross-timeline swap between Ethereum and Solana, using the register system:

1. **Ethereum Keeper Observes Deposit**
   - Unified Log Entry: "Observed deposit of 10 ETH to register R1 at block X"
   - Map of Time Node: Fact(Deposit, 10 ETH, R1, Ethereum:X)

2. **Ethereum Program Creates Register Resource**
   - Unified Log Entry: "Created resource in register R1 with ZK proof P1"
   - Map of Time Node: Effect(CreateResource, R1, 10 ETH, P1)
   - Controller Label: {creating: Ethereum, terminal: Ethereum, affecting: [Ethereum]}

3. **Cross-Timeline Transfer Initiated**
   - Unified Log Entry: "Transferred resource from register R1 to Solana register R2 with ZK proof P2"
   - Map of Time Node: Effect(TransferResource, R1->R2, P2)
   - Controller Label: {creating: Ethereum, terminal: Solana, affecting: [Ethereum, Solana]}

4. **Solana Keeper Observes Receipt**
   - Unified Log Entry: "Observed creation of register R2 on Solana at block Y"
   - Map of Time Node: Fact(RegisterCreation, R2, Solana:Y)

5. **Solana Program Updates Register**
   - Unified Log Entry: "Updated register R2 with resource and ZK proof P3"
   - Map of Time Node: Effect(UpdateRegister, R2, P3)

6. **ZK Verification**
   - Unified Log Entry: "Verified register operations with conservation proof P4"
   - Map of Time Node: Verification(R1, R2, P4)


## Dual Validation in the Register System

The register system implements dual validation:

### Temporal Validation
- Uses the Time Map to verify causal consistency
- Ensures operations happen in a valid temporal order
- Captured in the Map of Time as causal dependencies
- Implemented through ZK proofs referencing Time Map commitments

### Ancestral Validation
- Uses Controller Labels to verify resource provenance
- Ensures resources have valid ancestral history
- Captured in the Map of Time as resource flow
- Implemented through ZK proofs verifying controller label updates

Both validation types must succeed for a cross-timeline register operation to be valid.


## Important Clarifications

1. **Reconstructibility**: The Map of Time can always be reconstructed from the collection of all Unified Logs. This means we don't need to store it explicitly.

2. **Timekeeper Role in Production**: In the production system, Timekeepers serve different roles:
   - **Observers**: actively watching timelines and recording observations in their logs.
   - **Verifiers**: validating register operations through ZK proof verification.
   - **Coordinators**: ensuring register operations are properly sequenced and validated.

3. **Register State vs. Resource State**: Registers represent the on-chain physical state, while resources represent the logical accounting state. The Map of Time captures both perspectives.


## Key Invariants

The Time Bandits system maintains several invariants across the Unified Log, Map of Time, and Register System:

1. **Effect Invariant**: Every effect in every Unified Log has a ZK proof verifying its correctness.

2. **Fact Snapshot Invariant**: Every effect that depends on external facts includes those facts in its snapshot, with corresponding register observations.

3. **Resource Operation Invariant**: All resource operations maintain conservation laws (ΔTX = 0) with ZK proof verification.

4. **Cross-Chain Resource Invariant**: Resources that cross chains have controller labels tracking their complete provenance, maintained in registers on both chains.

5. **Register Verification Invariant**: Every register operation is verified through ZK proofs that are recorded in the Unified Log and reflected in the Map of Time.


## Visualization Example

```
┌───────────────────┐     ┌───────────────────┐     ┌───────────────────┐
│  Ethereum Log     │     │  Map of Time      │     │  Solana Log       │
├───────────────────┤     ├───────────────────┤     ├───────────────────┤
│ Fact: Deposit     │────>│ Temporal Order:   │<────│ Fact: Price Event │
│ Effect: Reg Create│────>│                   │<────│ Effect: Reg Create│
│ Effect: Transfer  │────>│ Ethereum:100      │<────│ Effect: Swap      │
│ Proof: Verify P1  │     │    │              │     │ Proof: Verify P3  │
└───────────────────┘     │    ▼              │     └───────────────────┘
                          │ Solana:120        │
                          │                   │
                          │ Resource Flow:    │
                          │                   │
                          │ ETH Register R1   │
                          │    │  (P1)        │
                          │    ▼              │
                          │ SOL Register R2   │
                          │    │  (P3)        │
                          │    ▼              │
                          │ Final Verify (P4) │
                          └───────────────────┘
```

The Map of Time integrates events from different logs, ensuring comprehensive validation through both temporal consistency and resource integrity, now with register operations and ZK proofs providing cryptographic verification.