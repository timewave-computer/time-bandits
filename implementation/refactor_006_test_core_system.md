# Refactor 006: Comprehensive Test Plan


## Objective

This document defines a **comprehensive set of tests** that must be implemented to validate all core functionality, simulation functionality, APIs, actor interactions, and the foundational infrastructure required for the upcoming **Temporal Effect Language (TECL)** implementation.

This test plan is designed to stand alone, so it includes detailed context for each test case, including what specific code is being tested, what behaviors or properties are under test, and guidance for the test's structure.


# Test Categories

## 1. Core Program State Tests ✅

### Context
Every program maintains a `ProgramState` which contains its effect history (DAG), current balances, and observed facts. These tests ensure program state behaves correctly across creation, mutation, serialization, deserialization, and replay.

### Tests
- **Program Initialization**: Create a new program with initial schema, version, and safe state policy. Assert correct defaults.
- **Program Serialization**: Serialize to JSON, then deserialize and check equality.
- **Content Addressing**: Compute hash of program state and check that serialization does not alter hash.
- **Effect Application**: Apply a sequence of effects to the program. Check that balances and derived facts update correctly.
- **Replayability**: Serialize a fully populated program, clear memory, reload, and replay all effects. Final state must match original.

### Code Under Test
- `Core.Program`
- `Core.ProgramState`
- `Core.Effect`


## 2. Effect DAG Tests ✅

### Context
Every program's history is represented as a **DAG of effects**. These tests ensure the DAG behaves correctly for adding, retrieving, and replaying effects.

### Tests
- **Effect Linking**: Apply effects with explicit parent-child relationships and check causal graph.
- **Content Addressing**: Ensure effect hashes match across serialization/deserialization.
- **Multiple Parent Effects**: Apply effects with multiple parents (e.g., fork resolution) and check ordering.
- **DAG Traversal**: Walk the DAG from root to tip and ensure the full causal chain is visited.

### Code Under Test
- `Core.EffectDAG`


## 3. Fact Observation and Handling Tests ✅

### Context
Programs depend on observed facts (prices, balances, external state). These tests ensure fact handling works correctly across observation, serialization, and replay.

### Tests
- **Fact Observation**: Observe a fact (e.g., price from OracleProgram) and store it in a fact snapshot.
- **Fact Logging**: Check observed facts are durably logged.
- **Fact Replay**: Replay program and check each observed fact reappears with the correct proof.
- **Invalid Fact Proof**: Inject tampered proof and check replay rejects it.

### Code Under Test
- `Core.Fact`
- `Core.FactSnapshot`
- `Timeline API Fact Queries`


## 4. Resource Handling and Account Program Tests ✅

### Context
All resource flows (deposits, withdrawals, transfers) go through account programs. These tests validate the account program's correctness and interactions.

### Tests
- **Deposit Handling**: Deposit asset into account, check balance updates.
- **Withdrawal Handling**: Withdraw asset and check balance updates.
- **Cross-Program Transfer**: Transfer asset from account to a program. Check balances and effects.
- **Replay Consistency**: Replay all deposits and withdrawals and check final balance.
- **Cross-Timeline Deposit**: Simulate asset arriving from a different timeline, check account updates.

### Code Under Test
- `AccountProgram`
- `Resource Types`
- `Effect Types: Deposit, Withdraw, Transfer`


## 5. Invocation Handling (Direct and Cross-Program) ✅

### Context
Programs communicate by **sending and receiving messages**, encapsulated in invocations. These tests validate invocation processing across direct calls and cross-program calls.

### Tests
- **Direct Invocation**: Invoke function directly inside one program, check correct effect applied.
- **Cross-Program Invocation**: Program A invokes Program B, program B observes fact, applies effect, and returns result to A.
- **Error Handling**: Program B fails, check A handles failure correctly.
- **Replay Consistency**: Replay full cross-program interaction and check state matches.

### Code Under Test
- `Invocation Handling`
- `Program Execution Interface`


## 6. P2P Effect Propagation and Consensus ✅

### Context
Effects are proposed and agreed via P2P consensus before they are applied. These tests validate the effect proposal, gossip, and confirmation pipeline.

### Tests
- **Single Bandit Propose**: Bandit proposes effect, applies locally.
- **Multiple Bandit Sync**: Bandit A proposes, Bandit B syncs, both apply.
- **Conflict Resolution**: Bandit A and Bandit B propose non-conflicting effects, both merge into DAG.
- **Invalid Proposal Rejection**: Bandit proposes effect with invalid parent hash, check rejection.
- **Replay After Gossip**: Bandits gossip effects, then both replay and compare final state.

### Code Under Test
- `P2P Messaging`
- `Effect Proposal`
- `Effect DAG`


## 7. Safe State Enforcement and Schema Evolution ✅

### Context
Programs upgrade safely only if they are in a **declared safe state**, and only if the schema evolution rules allow the upgrade. These tests validate safe state checks and schema handling.

### Tests
- **Safe State Check**: Apply non-blocking effects, check safe state reports `True`. Apply cross-program call, check safe state reports `False`.
- **Schema Evolution**: Add optional field, check evolution applies. Remove unused field, check removal applies. Rename field (disallowed), check evolution fails.
- **Schema Mismatch**: Program pinned to v1.0 tries to run on Bandit requiring v2.0, check rejection.
- **Upgrade and Replay**: Apply schema evolution and replay, check evolved schema still allows full replay.

### Code Under Test
- `SafeStatePolicy`
- `Schema`
- `SchemaEvolution`


## 8. Simulation Environment Tests ✅

### Context
The system supports 3 simulation modes (in-memory, local multi-process, geo-distributed). These tests ensure all 3 modes work identically from a functional point of view.

### Tests
- **Single Program Run (In-Memory)**: Deploy program, apply effects, query state.
- **Cross-Program Call (Multi-Process)**: Trader deposits into AccountProgram running in separate process.
- **Geo-Distributed Gossip**: Deposit proposed on one machine, confirmed and applied on another.
- **Effect Replay Consistency**: Run same program across all 3 modes, replay each, check identical final state.

### Code Under Test
- `Simulation Controller`
- `Actor Spawning`
- `P2P Networking`


## 9. Logging and Traceability ✅

### Context
Every effect, fact, and program transition must be durably logged for audit and replay. These tests validate logging and causal reconstruction.

### Tests
- **Effect Log Persistence**: Apply effects, write log, restart, replay from log.
- **Fact Log Persistence**: Observe facts, write log, restart, replay from log.
- **Causal Trace Reconstruction**: Walk effect log and build visual DAG.
- **Tamper Detection**: Modify log, check replay detects invalid chain.

### Code Under Test
- `EffectLog`
- `FactLog`
- `Replay Engine`


## 10. Visualization and Developer Tools ✅

### Context
Developers should be able to **see program history and state** using visualization tools. These tests ensure visual tools work across all cases.

### Tests
- **Single Program Visualization**: Apply 5 effects, generate DAG visualization.
- **Cross-Program Visualization**: Show full causality across multiple programs.
- **Fact Observation Visualization**: Show facts entering program state over time.
- **Schema Evolution Visualization**: Show schema versions alongside effects.

### Code Under Test
- `Program Visualizer`
- `Effect DAG Visualization`
- `Schema Visualizer`


## 11. Full Lifecycle Test (End-to-End) ✅

### Context
This test exercises the **full lifecycle** from program deployment, through cross-timeline interactions, program upgrades, and replay.

### Tests
- Traveler deploys v1 program.
- Deposits assets from Ethereum and Celestia.
- Executes cross-program calls.
- Observes price and derives spread.
- Upgrades to v2 with schema change.
- Completes cross-program settlement.
- Replays full program history.
- Compares final balances and facts to expected.

### Code Under Test
- Full System


# Completion Criteria ✅

- All tests implemented in `time-bandits-core/test` and `time-bandits-simulation/test`.  
- Tests run and pass in CI pipeline.  
- Tests work across all simulation modes.  
- No test relies on fragile manual setup — all use clear fixture data and mock timelines.

