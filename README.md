# Time Bandits

![](map_of_time.png)

Time Bandits is a distributed system for deploying and executing programs that coordinate logic and assets across multiple independent timelines (blockchains).

Programs are not executed directly on any single chain. Instead, they live as a set of zero knowledge verification keys committed to the involved chains, and executed by a P2P network. All program actions are captured by content-addressed, hash-linked logs. These append-only logs, combined with zk proofs of execution, ensure that programs are fully replayable and auditable, even across conflicting or adversarial timelines.

## Getting Started

### Enter the environment and build

```
# Set up development environment with Nix
nix develop

# Build the project
cabal build
```

### Running a Simple Scenario

```bash
# Run an in-memory simulation with the basic scenario
./scripts/run_in_memory_simulation.sh --scenario basic

# Or run directly with cabal
cabal run time-bandits -- sim in-memory --scenario basic
```

### Documentation

For more detailed information about the Time-Bandits system, please refer to:

- [Codebase Overview](docs/codebase_overview.md) - Overview of the main components and architecture
- [Onboarding Guide](docs/onboarding_guide.md) - Detailed guide for new developers
- [Developer Workflow](docs/dev_workflow.md) - Common development workflows
- [Glossary](docs/glossary.md) - Definitions of key terms and concepts

## Actors

**Time Travelers**

Time Travelers are the entities who deploy programs and submit state transition messages to the timelines. They are responsible for initiating program execution by creating signed transition messages that include necessary proofs and resources.

**Time Keepers**

Time Keepers are per-timeline actors responsible for:
- Maintaining the integrity of individual timelines (e.g., blockchains, event logs).
- Validating and recording new messages (deposits, claims, calls).
- Serving timeline state queries to authorized actors.
- Ensuring that all applied transitions follow timeline-specific rules.

**Time Bandits**

Time Bandits operate the P2P network that forms the backbone of the system. They:
- Execute program steps and generate cryptographic proofs.
- Disseminate messages through the network.
- Maintain the execution log and validate proofs.
- Enforce security properties across the system.

## System overview

This project was created to prototype a number of ideas. A few key features:

- Cross-Timeline Program Execution: Programs operate across multiple independent blockchains, rollups, and distributed logs, coordinating assets and logic across these systems.
- Effect Composition System: Programs are defined entirely through composable effects — each effect is a declared, verifiable, atomic operation, ensuring explicit control over all state transitions.
- Causal Time Map: Time Bandits maintains a sharded map of time, tracking the latest state and causal order across all participating timelines.
- Content-Addressed Execution Logs: Every applied effect appends to a hash-linked, append-only log, providing full replayability and a tamper-proof audit trail.
- Zero-Knowledge Proofs of Execution (mocked): Each effect application produces a proof that all preconditions were satisfied and the effect was applied correctly.
- Single-Owner Resource Model: Every resource (token, capability, escrow) has exactly one owner at all times, ensuring clear and provable custody across program interactions.
- Timeline-Aware Effect Adapters: External interactions (e.g., deposits, claims) are handled through generated effect adapters, ensuring programs remain timeline-agnostic while adapting to different VMs (EVM, WASM, etc.).
- P2P Transient Storage Network: Programs, logs, and proofs are stored and replicated across a peer-to-peer network, ensuring durability, auditability, and resistance to censorship.

One of the key features is a cross-timeline distributed state machine with a pluggable event/effect system where:
- Resources represent both program state (internal conditions, requirements, capacities) and control authority (who can advance state, under what conditions).
- Effects represent state advances—i.e., token transfers, escrows, swaps—triggered by actors (time travelers or other programs).
- Timelines act as independent shards or zones of causality, with the option to have programs that span multiple timelines.
- Effects compose dynamically: They can be sequenced, combined, and even interact (e.g., dependencies between effects or higher-order effects like "retry on failure").
This system forms a temporal process algebra, where programs are compositions of token-based effects guarded by resource predicates. This ensures causality and security across distributed timelines.

