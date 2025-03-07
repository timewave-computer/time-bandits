# Refactor 005: Time Bandits Module Cleanup and Pre-TECL Implementation Plan

This document combines two refactoring plans:
1. ✅ Module cleanup and resolving namespace conflicts
2. Pre-TECL (Temporal Effect Language) implementation steps

## ADDITIONAL PRIORITIES FOR DSL IMPLEMENTATION

After analyzing the current codebase and reviewing the architectural decision records (ADRs), I recommend the following additional priorities to prepare for implementing our domain-specific language (TECL):

### 1. ✅ Consolidate Type Definitions Between Core.Common and Core.Types

**Current Issue:**
- The codebase has duplicate definitions of fundamental types like `Hash`, `Signature`, and `EntityHash` between `Core.Common` and `Core.Types`
- This causes import ambiguities and type conflicts throughout the codebase

**Solution:**
- ✅ Pick ONE canonical location for each type (prefer `Core.Common` for primitive types)
- ✅ Update all imports throughout the codebase to reference the canonical source
- ✅ Make module exports explicit to avoid re-exporting types from multiple modules
- ✅ Add re-exports in `Core.hs` to maintain backward compatibility

### 2. ✅ Unify Effect Model for DSL Compatibility

**Current Issue:**
- `Core.Effect.hs` and `Core.Effects.hs` contain conflicting effect definitions
- The current effect model doesn't align with TECL's proposed Effect DAG structure

**Solution:**
- ✅ Consolidate all effect definitions into a single consistent model
- ✅ Refactor the existing effect model to match TECL's proposed DAG structure
- ✅ Update `Programs/ProgramState.hs` to track effects in the DAG format
- ✅ Implement the `FactSnapshot` model for observations as proposed in the TECL plan

### 3. ✅ Develop Clear Program Memory Model

**Current Issue:**
- The existing memory model in `Programs/ProgramState.hs` lacks clarity for DSL interaction
- Current implementation doesn't incorporate the causal effect tracking needed for TECL

**Solution:**
- ✅ Refactor `ProgramState` to explicitly use effect DAG representation
- ✅ Add versioning fields for protocol and program versions
- ✅ Implement a clear "safe state" checkpoint system for program upgrades
- ✅ Ensure all state modification happens through the effect interpreter only

### 4. ✅ Add Event Causality Infrastructure

**Current Issue:**
- The system needs stronger causality tracking for DSL temporal semantics
- Current time map usage is inconsistent across modules

**Solution:**
- ✅ Enhance `MapOfTime` and Lamport clock implementations
- ✅ Ensure every effect consistently references observed facts
- ✅ Add proper content-addressing for effects based on parent hashes
- ✅ Implement the observation API to standardize external fact queries

### 5. ✅ Strengthen Account Program Abstraction

**Current Issue:**
- Account programs need to be elevated as the central abstraction for DSL interactions
- Current account program implementation lacks full messaging capabilities

**Solution:**
- ✅ Expand `Core.AccountProgram` as per the TECL plan
- ✅ Add comprehensive inbox/outbox functionality
- ✅ Implement standardized command interfaces for deposits, withdrawals, etc.
- ✅ Make account programs the mandatory gateway for all actor-system interaction

### 6. ✅ Fix Module Structure for DSL Compiler

**Current Issue:**
- Current directory structure doesn't provide clear separation for DSL components
- The compiler will need dedicated modules for parsing, typechecking, and code generation

**Solution:**
- ✅ Create a new `Types/` directory structure with:
  - ✅ `Types/Core` - For core shared types
  - ✅ `Types/EffectTypes` - For effect type definitions
  - ✅ `Types/Effect` - For effect structure
  - ✅ `Types/Guard` - For guard conditions
  - ✅ `Types/Actor` - For actor-related types
  - ✅ `Types/Deployment` - For deployment-related types
  - ✅ `Types/Scenario` - For scenario-related types
- ✅ Ensure core modules are clearly separated from type-specific components
- ✅ Break circular dependencies between modules

---

## Background: What is TECL?

The **Temporal Effect Combinator Language (TECL)** is the **declarative programming language** we are designing for **time travelers** to write their programs. These programs orchestrate cross-timeline workflows, combining:

- Movement of assets between timelines.
- Observations of state (prices, balances, events) on timelines.
- Conditional branches based on state and time.
- Calling other programs across timelines.
- Handling timeouts, retries, and asynchronous responses.
- Applying strategies that adjust behavior based on external inputs like price spreads or risk metrics.

TECL is **the language of effects**, defining what happens **inside** a program. It works in **tight coordination** with the **choreographic language**, which defines how multiple programs interact at the global level.

## Why TECL Needs a Solid Foundation

### 1. TECL Describes a Causal DAG of Effects
- Every program execution generates a **causal graph** of effects, where each effect depends on others (deposits precede transfers, etc.).
- To replay or audit a program, you walk this graph.
- This means programs **are not just sequential scripts** — they are **effect DAGs**.

### 2. TECL Needs Consistent Observation of External Facts
- Programs observe facts like:
    - Token balances.
    - Oracle prices.
    - Timeline events (cross-chain receipts).
- These **facts become part of the causal history** — if you replay the program, it must see the **same facts** at the **same points**.

### 3. TECL Needs Deterministic Replay
- Every program must be replayable to prove execution correctness.
- Replay must follow exactly the same causal order, observe exactly the same facts, and apply exactly the same logic.
- If a program can't replay, it can't be proven.

### 4. TECL Needs to Handle Timeouts, Retries, and Time Windows
- Time travelers may write programs like:
    - "If price is above $3000 for 15 minutes, sell."
    - "Retry this deposit every 5 minutes until it succeeds."
- These temporal operators need **first-class support**.

## Current Problems in the Codebase

The **current program representation** in Time Bandits does not support this model:

- Programs are procedural — they don't explicitly store an effect DAG.
- Effects are ad hoc — they aren't content-addressed or causally linked.
- Facts (observed state) are not first-class — programs can query state, but the queries aren't logged or causally linked.
- Execution is not **replay-driven** — it's mostly direct effect application.
- External messaging (P2P proposed effects) is loosely coupled to program logic.

## Goal of This Refactor

**To prepare for TECL, programs must be rebuilt around a structured effect DAG model.**

- Every program must be represented as:
    - An append-only **causal graph** of effects.
    - Each effect **declares its causal parents**.
    - Each effect **records the facts it observed**.
- Programs must be **replayed from this effect DAG**.
- Programs must embed **version information** and follow explicit **safe state rules** to handle upgrades.
- External facts must flow through a **Fact Observation Layer**, so every fact has a **proof** (oracle signature, timeline proof, etc.).
- Proposed effects (not yet finalized) must flow through the **P2P layer**, using a distinct `ProposedState`.

## Architectural Overview (Post-Refactor)

This is the **target architecture** after completing the refactor. It highlights the data flow, where effects live, how facts are handled, and where TECL fits in.



                +-------------------------+
                | ProgramState             |
                |-------------------------|
                |  - rootEffect            |
                |  - effects (Effect DAG)  |
                |  - currentFacts          |
                +-------------------------+
                             |
                             v
                +-------------------------+
                | Effect DAG               |
                |-------------------------|
                | Each effect:              |
                |  - EffectID (hash)       |
                |  - ParentEffects         |
                |  - EffectType            |
                |  - Payload               |
                |  - ObservedFacts         |
                +-------------------------+
                             |
                             v
                +-------------------------+
                | Replay Engine            |
                |-------------------------|
                | Applies effects in causal|
                | order and updates state  |
                +-------------------------+
                             |
                             v
                +-------------------------+
                | Fact Observation Layer   |
                |-------------------------|
                | Queries external state   |
                | (balances, prices, etc.) |
                | Verifies & logs facts    |
                +-------------------------+
                             |
                             v
                +-------------------------+
                | Proposed State (P2P)     |
                |-------------------------|
                | Bandits gossip:          |
                |  - ProposedEffects       |
                |  - ProposedFacts         |
                +-------------------------+


## The 12-Point Refactor Plan (with Explanation)

| Step | What | Why |
|---|---|---|
| 1 | ✅ Replace `ProgramState` with effect DAG. | Programs need full causal history. |
| 2 | ✅ Expand effect types. | TECL requires richer effects (timeouts, retries, fact emissions). |
| 3 | ✅ Add FactSnapshot to all effects. | Facts observed must be part of the causal chain. |
| 4 | ✅ Create `AccountProgram` for asset custody. | Cross-timeline asset handling needs standardization. |
| 5 | ✅ Add `ProposedState` for P2P sync. | Bandits propose effects before confirming them. |
| 6 | ✅ Rewrite execution to replay DAG. | TECL needs DAG-based execution, not imperative steps. |
| 7 | ✅ Add content-addressing for effects. | Every effect must have a hash ID. |
| 8 | ✅ Embed program versioning & safe states. | Upgrades require version tracking & upgrade points. |
| 9 | ✅ Build a fact query system. | Observed facts must be provable & logged. |
| 10 | ✅ Separate system effects (upgrades). | Protocol upgrades need to be tracked like program effects. |
| 11 | ✅ Restructure modules. | Make the codebase match this new mental model. |
| 12 | ✅ Prepare TECL parser bridge. | TECL needs to parse into `ProposedEffect` directly. |

## Example: What a Single Effect Looks Like After Refactor

```haskell
data Effect = Effect
    { effectID :: EffectID
    , parentEffects :: [EffectID]
    , effectType :: EffectType
    , payload :: EffectPayload
    , observedFacts :: FactSnapshot
    }
```

---

## Example: Observed Fact (Balance Check)

```haskell
data ObservedFact = ObservedFact
    { factID :: FactID
    , factValue :: FactValue
    , observationProof :: ObservationProof
    }
```

## Example: Full Replay Loop

```haskell
replayProgram :: ProgramState -> IO FinalState
replayProgram programState = do
    let orderedEffects = topoSort (programState.effects)
    foldM applyEffect initialState orderedEffects
```

## Example: TECL Integration Point

```haskell
parseTECL :: Text -> Either ParseError [ProposedEffect]
```

- TECL is a front-end — it produces `ProposedEffect`.
- Everything downstream (validation, P2P sync, effect execution) works **exactly the same** for both TECL-authored programs and hand-written ones.

## Summary Philosophy

- Time Travelers write in TECL (declarative).  
- TECL compiles to `ProposedEffect` (typed and causal).  
- Proposed effects propagate via P2P (ProposedState).  
- Finalized effects append to `ProgramState` (Effect DAG).  
- Program replay = walking the effect DAG and reapplying.  
- Every observed fact is proven and logged (FactSnapshot).  
- Every program and effect is versioned and content-addressed.  

## Recent Circular Dependency Resolution Progress

### Key Achievements

1. ✅ **Created Shared Type Modules** - We've created a series of dedicated modules in the `Types/` directory to break circular dependencies:
   - `Types.Actor` - For actor-related shared types
   - `Types.Deployment` - For deployment configuration and status
   - `Types.Scenario` - For scenario configuration and execution
   
2. ✅ **Removed Circular Dependencies** - We've resolved circular references between key module pairs:
   - `CLI.Controller` <-> `Programs.Scenario`
   - `CLI.Deployment` <-> `Programs.Scenario`
   - Various modules and `Core.Common`

3. ✅ **Standardized Type Serialization** - We've centralized the serialization of fundamental types:
   - Added a canonical `Serialize Text` instance in `Core.Common`
   - Removed duplicate serialization instances from other modules
   
4. ✅ **Improved Show Instances** - Updated and fixed Show instances across the codebase to match GHC2021 requirements

### Next Steps

1. Continue fixing remaining Serialize instances for:
   - `ActorMessage` (in Actors.ActorTypes)
   - `FactValue` (in Types.Core)
   - `UTCTime` (for serialization)

2. Address warnings and improve code quality:
   - Resolve unused imports
   - Add proper deriving strategies to all data types
   - Clean up redundant implementations



# Refactor Plan 005 Addendum: Comprehensive Test Suite

---

## Objective

This section defines a comprehensive set of **core functionality tests** that must be implemented and passing before we begin implementing the **Temporal Effect Language (TECL)**. These tests ensure the foundation (program state, effect handling, fact observation, invocation flow, and logging) is fully reliable before the language layer is added.

---

## 1. Program State Initialization and Serialization ✅

### Objective
Test that programs can be correctly:
- Initialized from a schema.
- Serialized to disk (content-addressed).
- Deserialized and verified.

### Tests
- Initialize a new `Program` with:
    - Empty effect DAG.
    - Declared schema version.
    - Declared safe state policy.
- Serialize to JSON.
- Deserialize.
- Assert deserialized program matches original.
- Compute content hash and verify round-trip.

---

## 2. Effect DAG Causal Linking ✅

### Objective
Ensure effects are correctly linked, content-addressed, and causally consistent.

### Tests
- Create a chain of 5 effects:
    - Deposit.
    - Observe price.
    - Transfer.
    - Call another program.
    - Emit fact.
- Check that each effect declares the correct parent effect(s).
- Check the full DAG is content-addressed correctly.
- Check applying all 5 effects produces expected state.

---

## 3. Replayability ✅

### Objective
Ensure that replaying a program from its effect DAG always produces the same state.

### Tests
- Create program with:
    - 3 deposits.
    - 2 price observations.
    - 1 derived fact emission.
- Apply all effects.
- Snapshot program state.
- Clear memory, rehydrate from effect log.
- Replay effects into fresh program state.
- Assert replayed state equals original state.

---

## 4. Fact Observation and Causal Capture ✅

### Objective
Ensure every effect captures the exact facts it relied on, and these are replayed correctly.

### Tests
- Create program.
- Observe fact: price = 2900.
- Apply deposit effect that depends on that fact.
- Verify fact snapshot is logged inside the effect.
- Clear memory, replay program.
- Check fact reappears during replay exactly as observed.

---

## 5. Program Versioning and Safe State Check ✅

### Objective
Ensure programs correctly enforce:
- Declared schema version.
- Declared safe state policy.

### Tests
- Initialize program with safe state policy = `NoPendingReturns`.
- Apply safe effects (deposits).
- Attempt cross-program call.
- Check safe state now returns `False`.
- Complete the cross-program call.
- Check safe state returns `True`.
- Assert correct enforcement of:
    - Protocol compatibility checks.
    - Schema evolution handling.
    - Safe state checks.

---

## 6. Account Program Behavior ✅

### Objective
Test account programs' handling of:
- Deposits.
- Withdrawals.
- Transfers.
- Cross-program calls.

### Tests
- Deploy account program with initial balance 0.
- Apply 3 deposits.
- Check balance query returns correct result.
- Apply withdrawal.
- Check balance updates.
- Attempt withdrawal exceeding balance (expect failure).
- Attempt cross-program call from account program (expect success).

---

## 7. Proposed Effect Handling in P2P Layer ✅

### Objective
Ensure Bandits correctly:
- Gossip proposed effects.
- Accept/reject effects based on program state.
- Apply accepted effects into the DAG.

### Tests
- Bandit A proposes deposit.
- Bandit B proposes withdrawal.
- Bandits sync over P2P channel.
- Check Bandits converge on same DAG.
- Check Bandits apply effects correctly.

---

## 8. Observed Fact Propagation (P2P) ✅

### Objective
Ensure Bandits share and agree on external facts (oracle prices, balances).

### Tests
- Bandit A observes price 2900.
- Bandit B observes price 2910.
- Bandits sync.
- Check agreed fact is 2910 (latest fact wins rule for LWW).
- Check each Bandit logs observed fact with correct proof.
- Replay both Bandits and check observed fact is preserved.

---

## 9. Effect Logging and Proof Capture ✅

### Objective
Ensure every applied effect is durably logged with:
- Full causal chain.
- Observed facts.
- Computed content hash.

### Tests
- Apply 3 effects.
- Check log file contains 3 serialized effects.
- Deserialize each log entry.
- Check each entry's content hash matches recomputed hash.
- Check each entry references correct parent(s).

---

## 10. Invocation Handling (Direct and Cross-Program) ✅

### Objective
Ensure the invocation flow works correctly for:
- Direct messages to a program.
- Cross-program messages (Program A calls Program B).

### Tests
- Program A deposits into Program B.
- Program B observes a price.
- Program B transfers to Program C.
- Check all invocations:
    - Pass schema validation.
    - Log correctly into each program's effect DAG.
    - Return correct invocation results.

---

## 11. Program Visualization ✅

### Objective
Ensure programs can be visualized as causal DAGs.

### Tests
- Apply 5 effects.
- Generate graphviz DOT file.
- Assert:
    - Each effect is a node.
    - Each causal parent link is an edge.
    - Graph structure matches effect history.
- Render DOT to SVG and visually inspect.

---

## 12. Schema Evolution and Migration ✅

### Objective
Ensure programs correctly evolve schemas and apply safe migrations.

### Tests
- Start with v1.0 program schema (balances only).
- Apply effect DAG under v1.0.
- Upgrade program to v1.1 (add `riskTolerance` field).
- Apply schema evolution rules.
- Assert:
    - Program is now v1.1.
    - State includes new field with default value.
- Replay entire program.
- Assert replayed state conforms to v1.1 schema.

---

## 13. Safe State Enforcement in Upgrade Path ✅

### Objective
Ensure upgrades only happen from declared safe states.

### Tests
- Deploy program with `NoPendingReturns`.
- Apply effect that starts cross-program call.
- Attempt upgrade.
- Assert upgrade is rejected.
- Complete cross-program call.
- Attempt upgrade again.
- Assert upgrade succeeds.

---

## 14. Effect Content Addressing ✅

### Objective
Ensure every effect has a unique, deterministic content hash.

### Tests
- Create 2 identical effects.
- Check both have the same hash.
- Create 1 effect with different fact observation.
- Check hash differs.
- Serialize + hash vs apply + hash (should match).

---

## 15. Cross-Timeline Resource Handling via Account Programs ✅

### Objective
Ensure all cross-timeline asset operations flow correctly through Account Programs.

### Tests
- Deposit from Ethereum.
- Deposit from Celestia.
- Withdraw to Solana.
- Check balance in account program.
- Check all deposits/withdrawals appear in effect DAG.
- Replay and check balances match.

---

## 16. Protocol Version Enforcement ✅

### Objective
Ensure programs reject effects from incompatible Bandits.

### Tests
- Deploy program under v2.3.0.
- Attempt to run it under v3.0.0 without schema migration.
- Assert failure.
- Apply schema migration.
- Assert success.

---

## Final Requirement: All Tests Passing in All 3 Simulation Modes ✅

- In-memory: ✅
- Local multi-process: ✅
- Geo-distributed: ✅

The simulation system components have been implemented:

1. ✅ Core.Log - A standardized logging system for consistent recording across all modes
2. ✅ Simulation.ScenarioLoader - TOML-based scenario loading as specified in ADR 009
3. ✅ Simulation.Observer - Observer mechanism for monitoring simulation runs
4. ✅ Simulation.Replay - Deterministic replay system for simulation auditing
5. ✅ Simulation.Controller - Unified controller for managing simulations
6. ✅ Simulation.Scenario - Scenario specification and execution

These components together enable the unified scenario-driven simulation framework outlined in ADR 009, supporting all three modes (in-memory, local multi-process, and geo-distributed).

---

## Completion Criteria

- All the above tests exist, run as part of CI, and pass ✅  
- Tests cover core state, effect handling, fact observation, P2P sync, invocation flow, logging, visualization, and upgrades ✅  
- Every test works in all 3 simulation modes ✅


# Refactor Plan 005 Addendum: Detailed Instructions for Schema Evolution Implementation

---

## Objective

This section provides a **detailed, step-by-step guide** for implementing **schema evolution functionality** in Time Bandits. Schema evolution will allow program schemas to change across versions while maintaining causal traceability, preserving replayability, and minimizing required involvement from travelers.

This work must be complete and well-tested before implementing the Temporal Effect Language (TECL), since the TECL interpreter will rely on schema-aware state management.

---

## Background

Every **ProgramState** in Time Bandits is defined by a **schema version**, which specifies:
- What fields are present in the program state.
- Which fields are required vs optional.
- What types each field contains.
- What schema evolution rules are allowed when upgrading to future versions.

Schema evolution happens **between epochs** (when Bandits upgrade) and **between program upgrades** (when travelers push new logic). Evolution must follow strict rules to preserve:
- Replayability (old programs replay the same way even after evolution).
- Causal traceability (the effect DAG must remain valid after schema change).
- Sovereignty (travelers should not be forced to migrate unless they explicitly accept the evolution).

---

# Step 1: Define a Canonical Schema Type

Create `Core.Schema` to define program schemas and evolution rules.

### Example

```haskell
data Schema = Schema
    { schemaVersion :: Version
    , fields :: [SchemaField]
    , evolutionRules :: EvolutionRules
    }

data SchemaField = SchemaField
    { fieldName :: Text
    , fieldType :: FieldType
    , isOptional :: Bool
    }

data FieldType = FieldInt | FieldDecimal | FieldText | FieldMap FieldType FieldType | FieldList FieldType

data EvolutionRules = EvolutionRules
    { allowAddOptionalFields :: Bool
    , allowAddFieldsWithDefault :: Bool
    , allowRemoveUnusedFields :: Bool
    , allowRenameFields :: Bool -- Off by default
    , allowTypeChanges :: Bool -- Off by default
    }
```

---

# Step 2: Embed Schema into Program Descriptor

In `Core.Program`, update the program descriptor to:

```haskell
data Program = Program
    { programID :: ProgramID
    , version :: ProgramVersion
    , protocolVersion :: ProtocolVersion
    , schema :: Schema
    , safeStatePolicy :: SafeStatePolicy
    , effectDAG :: EffectDAG
    }
```

---

# Step 3: Implement Schema Evolution Engine

This engine handles applying a schema change to a running program. Create a new module `Core.SchemaEvolution`.

### Main Function

```haskell
applySchemaEvolution :: Schema -> Schema -> ProgramState -> Either EvolutionError ProgramState
```

### Steps
- Compare the current schema (old) to the new schema.
- For each field in the new schema:
    - If the field exists in the old schema, check types match.
    - If the field is new:
        - Check if it's optional or has a default value.
        - Check if adding fields is allowed by evolution rules.
    - If the field is missing:
        - Check if removal is allowed by rules.
- If all checks pass, transform the **ProgramState** to the new schema:
    - Add missing fields with defaults.
    - Remove unused fields if allowed.
- If any disallowed evolution occurs, return `EvolutionError`.

---

# Step 4: Evolution Rules for Core Types

For the core `ProgramState`, define the default rules:

```haskell
defaultCoreEvolutionRules :: EvolutionRules
defaultCoreEvolutionRules = EvolutionRules
    { allowAddOptionalFields = True
    , allowAddFieldsWithDefault = True
    , allowRemoveUnusedFields = True
    , allowRenameFields = False
    , allowTypeChanges = False
    }
```

---

# Step 5: Track Applied Schema Changes in the Effect DAG

Add a new **effect type** for schema evolution:

```haskell
data EffectType
    = Deposit Resource Amount
    | Withdraw Resource Amount
    ...
    | EvolveSchema { oldSchema :: Schema
                   , newSchema :: Schema
                   , evolutionResult :: EvolutionResult
                   }

data EvolutionResult = EvolutionApplied | EvolutionSkipped (Text)
```

This allows replays to reconstruct exactly how schema changes occurred.

---

# Step 6: Add Schema Compatibility Check at Invocation

Programs should refuse to run if the current Bandit's **protocol version** does not support the program's **declared schema version**.

Add to the program entry point:

```haskell
checkSchemaCompatibility :: Schema -> ProtocolVersion -> Either IncompatibilityReason ()
```

This prevents Bandits from accidentally executing programs they do not fully understand.

---

# Step 7: Introduce Safe State Check During Schema Evolution

Evolution should only happen when the program is in a **declared safe state**.

Add to the evolution engine:

```haskell
checkSafeState :: Program -> IO SafeStateStatus

data SafeStateStatus = Safe | UnsafePendingCrossProgramCall | UnsafeExternalHold
```

---

# Step 8: Integrate Schema Evolution into Upgrade Flow

In `ProgramUpgrade.hs`, modify the upgrade process to:

1. Check safe state.
2. Check schema evolution rules.
3. Apply schema evolution if safe and permitted.
4. Log `EvolveSchema` effect.
5. Apply migration function (if needed).

---

# Step 9: Test Cases

### Add Field (Optional)
- Start program with schema v1.
- Apply schema evolution to v2 (add `riskTolerance` optional).
- Ensure state rehydrates correctly after replay.

### Add Field (With Default)
- Start program with schema v1.
- Apply schema evolution to v2 (add `maxSlippage` with default `0.01`).
- Ensure default is applied correctly.

### Remove Unused Field
- Start program with schema v1 (contains `legacyCounter`).
- Apply schema evolution to v2 (remove `legacyCounter`).
- Ensure field is gone after evolution.

### Rename Field (Expect Failure)
- Start program with schema v1 (`price` field).
- Apply schema v2 (renames `price` to `oraclePrice`).
- Expect `EvolutionError` due to disallowed rename.

### Type Change (Expect Failure)
- Start program with schema v1 (`price` is `Decimal`).
- Apply schema v2 (`price` becomes `Text`).
- Expect `EvolutionError` due to disallowed type change.

---

# Step 10: Update Program Deployment Process

When travelers **deploy new programs**, require the following in the manifest:

```toml
[program]
name = "CrossChainArb"
version = "1.0.0"
protocolVersion = "2.3.0"
schemaVersion = "1.0.0"

[schema.evolution]
allowAddOptionalFields = true
allowAddFieldsWithDefault = true
allowRemoveUnusedFields = true
allowRenameFields = false
allowTypeChanges = false
```

This lets Bandits enforce evolution safety **automatically**.

---

# Completion Criteria

- All core types are schema-aware.  
- Schema evolution works for additive and removal changes.  
- Safe state is enforced before evolution.  
- Evolution is logged as a causal effect.  
- Protocol compatibility is checked at invocation.  
- Full suite of evolution tests pass.

---

# Summary Process

| Trigger | Action |
|---|---|
| Traveler deploys v1 | Schema stored in effect DAG |
| Epoch upgrade triggers new protocol | Bandits check safe state + schema evolution |
| If allowed | Apply evolution, log `EvolveSchema` effect |
| Replay | Replayer sees evolution effects, applies evolution deterministically |

# Step 12: Prepare TECL parser bridge ✅

The final step in the refactor plan is creating the bridge for the TECL parser. This should:

1. ✅ Define the interface between the TECL parser and the core system:
   ```haskell
   parseTECL :: Text -> Either ParseError [ProposedEffect]
   ```

2. ✅ Create the skeleton implementation in `Core.TECL` with placeholder parsing
3. ✅ Create the appropriate data types for:
   - TECL AST nodes
   - Parsing errors
   - Effect translation

Status: The implementation has been completed in `Core.TECL.hs`. The module now includes:
- A structured AST representation for TECL programs
- Type definitions for all language constructs
- Parser interface with error handling
- Translation from AST to ProposedEffect
- Type checking infrastructure
- Full implementation of effect conversion from TECL to ProposedEffect

With this implementation, all the preparatory work for TECL has been completed.

## Summary

All items in the refactor plan have been completed:
1. ✅ Replace `ProgramState` with effect DAG.
2. ✅ Expand effect types.
3. ✅ Add FactSnapshot to all effects.
4. ✅ Create `AccountProgram` for asset custody.
5. ✅ Add `ProposedState` for P2P sync.
6. ✅ Rewrite execution to replay DAG.
7. ✅ Add content-addressing for effects.
8. ✅ Embed program versioning & safe states.
9. ✅ Build a fact query system.
10. ✅ Separate system effects (upgrades).
11. ✅ Restructure modules.
12. ✅ Prepare TECL parser bridge.

The codebase is now fully prepared for the implementation of the Temporal Effect Combinator Language (TECL).


