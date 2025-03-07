# Refactor 005: Time Bandits Module Cleanup and Pre-TECL Implementation Plan

This document combines two refactoring plans:
1. ✅ Module cleanup and resolving namespace conflicts
2. Pre-TECL (Temporal Effect Language) implementation steps

## ADDITIONAL PRIORITIES FOR DSL IMPLEMENTATION

After analyzing the current codebase, I recommend the following additional priorities to prepare for implementing our domain-specific language (TECL):

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
| 12 | Prepare TECL parser bridge. | TECL needs to parse into `ProposedEffect` directly. |

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
