# PRD 002: Program and Protocol Upgrades

## Context and Motivation

Time Bandits is a system for deploying, executing, and proving the execution of **cross-timeline programs**. These programs interact with diverse blockchains (timelines), depend on external state (prices, balances), and must maintain **causal integrity** across all effects and observations.

Over time, both **the Time Bandits system itself** (the protocol) and individual **program logic** (time traveler-authored programs) will need to evolve. This document defines the requirements, constraints, and proposed design options for enabling **program and protocol upgrades** in a way that:

- Preserves causal traceability.
- Maintains traveler sovereignty — programs **belong to travelers**.
- Ensures system coherence across versions.
- Balances flexibility (travelers can refuse upgrades) with practicality (default path should be smooth auto-upgrades).

## Requirements

### R1 - Traveler Sovereignty
- Travelers must have **full ownership** of their deployed programs.
- Travelers can **refuse a protocol upgrade**, choosing instead to stay pinned to the version they originally deployed with.
- This means programs **must declare and persist their intended runtime version**.

### R2 - Safe Upgrade Default
- The **default experience** should favor automatic upgrades.
- If a traveler does nothing, their program should:
    - Migrate to the new version (if safe to do so).
    - Receive schema migrations automatically (if schema changes are safe).

### R3 - Explicit Safe States for Migration
- Not all program states should be upgradeable.
- Travelers should optionally declare **safe migration points** — states where upgrades can be applied without ambiguity.
- Examples:
    - No in-flight cross-program calls.
    - No pending returns from dependent programs.

### R4 - Schema Evolution without Traveler Involvement
- Schema upgrades should be **automatic and transparent** if possible.
- Travelers should only write explicit migrations if:
    - The upgrade introduces breaking changes to program state.
    - The program uses deeply custom logic.

## New Terminology

| Term | Meaning |
|---|---|
| Epoch | Discrete protocol version checkpoint (e.g., every N blocks). |
| Protocol Version | Version of the Time Bandits protocol itself. |
| Program Version | Version of the traveler-authored program logic. |
| Safe State | Declared state in which upgrades are allowed. |
| Migration Function | Pure function mapping old program state to new program state. |

## Key Mechanism: Program Version Pinning

Every program must embed:

```toml
[program]
name = "CrossChainArb"
version = "v1.2.0"
protocolVersion = "2.3.0"
safeStateStrategy = "no-pending-returns"

[history]
previousVersions = ["sha256-oldversion1", "sha256-oldversion2"]
```

## Key Mechanism: Epoch-Based Protocol Upgrades

The **execution network itself** (the Bandit network) will upgrade in **epochs**. Every Bandit tracks:

```toml
[current]
protocolVersion = "2.3.0"
compatibleProgramVersions = ["1.x", "2.x"]

[upcoming]
activationEpoch = 180000
newVersion = "3.0.0"
```

At epoch 180000, the Bandits automatically switch to version 3.0.0 of the protocol.

## Compatibility Policy

| Program Version | Protocol Version | Behavior |
|---|---|---|
| Program < Compatible Range | Reject new effects. Traveler must upgrade. |
| Program ∈ Compatible Range | Normal execution. |
| Program > Protocol Version | Reject program — Bandits too old. |

## Key Mechanism: Safe State Declaration

Each program declares a **safe upgrade strategy**:

| Strategy | Meaning |
|---|---|
| `always` | Program can be upgraded in any state. |
| `no-pending-returns` | Upgrade only when no cross-program calls are awaiting response. |
| `manual` | Traveler must explicitly signal safe upgrade points. |

## Open Problem: Pending Return Hostage Risk

If **Program A** calls **Program B**, and Program B deliberately **refuses to return** (or returns in a way that prevents A from reaching a safe state), it can **block A from upgrading**.

### Suggested Options to Handle This

| Option | Approach | Tradeoffs |
|---|---|---|
| Timeout Escalation | If B stalls for too long, A can **abort the call unilaterally** and mark the state degraded. | May break valid protocols if B is slow for valid reasons. |
| Fallback State | Programs define a **degraded fallback state** they enter if a dependent call is unresolved. | Complex to define for all programs. |
| Refuse External Calls | Some programs declare they **will not make cross-program calls** (self-contained). | Limits composability. |

## Key Mechanism: Migration Functions

If a program upgrade requires schema changes, Bandits automatically attempt to apply:

1. **Schema Evolution Rules** (default migrator for field additions/removals).
2. **Traveler-Provided Migration Function** if rules aren’t enough.

## Example Migration Effect

```haskell
data EffectType
    = Deposit Resource Amount
    | Withdraw Resource Amount
    | Transfer Resource Amount Recipient
    | UpgradeProgram { oldHash :: ProgramHash
                     , newHash :: ProgramHash
                     , migration :: Maybe MigrationFunction
                     }
```

## Migration Function Format

```haskell
type MigrationFunction = OldProgramState -> Either MigrationError NewProgramState
```

## Full Upgrade Process

| Step | Action |
|---|---|
| Epoch N | Bandits run Protocol 2.3.0 |
| Program P runs | Under Protocol 2.3.0 |
| Epoch N+1 | ProtocolUpgrade to 3.0.0 |
| Program P | Checks safe state |
| If safe | Applies auto-migration |
| If not safe | Stays pinned to 2.3.0 and requires legacy Bandits |
| If schema breaks | Traveler provides migration function |
| Effect Log | Records `UpgradeProgram` effect |

## Consequences of Staying Pinned

If a program **refuses to upgrade**, the traveler may have to:

- **Incentivize Bandits to run a legacy node.**
- **Lose compatibility with newer programs.**
- **Lose access to newer effect types.**
- **Manually operate cross-chain relays (if relays evolve).**

This is acceptable because sovereignty > convenience in this design.

## Visualization

```
Epoch N (2.3.0) ProgramA --Effect--> ProgramB ProgramB calls ProgramC Upgrade announced (3.0.0)

Epoch N+1 (3.0.0 activates) ProgramA checks safeState: ok ProgramB safeState check fails (pending return from ProgramC) ProgramA upgrades ProgramB refuses upgrade ProgramB now requires legacy Bandits
```

## Summary Recommendations

✅ Programs declare pinned versions and safe state strategies.  
✅ Bandits upgrade by epoch, and signal compatibility windows.  
✅ Safe states are a **critical feature** — they allow automatic upgrades without traveler action.  
✅ Travelers only write migration functions if schema changes are non-trivial.  
✅ Programs that refuse upgrades need explicit **legacy Bandit support**.

## Summary Options for Pending Call Hostage

| Option | Recommended? |
|---|---|
| Timeout + Degrade | ✅ Default if safeState = `no-pending-returns` |
| Fallback State | Optional per-program. |
| Self-Contained Only | Optional for simpler programs. |

## Conclusion

This balances:
- Full traveler ownership.
- Practical automatic upgrades.
- Strong causal traceability (every upgrade is an effect).
- Clean separation of **program version** and **protocol version**.

---


## Addendum I: Autonomous Schema Evolution

The goal of **autonomous schema evolution** is to allow **program upgrades to happen without requiring traveler-written migrations** — as long as the schema change falls into **a predictable class of non-breaking changes**.

This fits well with the **default safe upgrade path** where programs automatically evolve along with the protocol, provided they are in a **safe state** at the time of the upgrade.


## What Kinds of Schema Changes Can Be Auto-Evolved?

| Change Type | Example | Automatically Safe? |
|---|---|---|
| Add New Field (Optional) | Add `riskTolerance` to program state | ✅ Yes |
| Add New Field (With Default) | Add `maxSlippage` with default of `0.01` | ✅ Yes |
| Remove Unused Field | Remove deprecated `legacyCounter` | ✅ Yes |
| Change Field Type (Coercible) | Change `ethBalance` from `Int` to `Decimal` | ⚠️ Safe if lossless coercion |
| Rename Field | Rename `price` to `oraclePrice` | ❌ Requires explicit migration |
| Add New Effect Type | Add `ObserveOracle` | ✅ Yes (if old Bandits ignore) |
| Modify Effect Payload | Add `timestamp` to `DepositEffect` | ⚠️ Requires careful coordination |

## Recommended Approach: Schema Evolution Rules

Each **program schema version** includes:

```toml
[schema]
version = "1.2.0"
fields = ["balances", "lastPrice", "riskTolerance"]

[schema.evolution]
allowed = ["add-optional-field", "add-default-field", "remove-unused-field"]
```

This allows Bandits to:
- Detect when schema changes are **within allowed rules**.
- Auto-apply changes to the serialized program state.
- Validate evolution as part of **effect validation**.

## Evolution Engine Example

```haskell
applySchemaEvolution :: ProgramState -> NewSchema -> Either EvolutionError ProgramState
applySchemaEvolution oldState newSchema =
    if evolutionIsSafe oldSchema newSchema
    then Right (migrateFields oldState oldSchema newSchema)
    else Left IncompatibleSchemaChange
```

## Program Declaration Example

```toml
[program]
name = "CrossChainArb"
version = "1.2.0"
schemaVersion = "1.2.0"
safeStateStrategy = "no-pending-returns"
```

## Example: Add a Field Automatically

Original State (v1.2.0):
```json
{
    "balances": { "ETH": 100 },
    "lastPrice": { "ETH/USDC": 2900 }
}
```

Schema Evolution (v1.3.0 adds `riskTolerance`):
```toml
[schema]
version = "1.3.0"
fields = ["balances", "lastPrice", "riskTolerance"]

[schema.defaultValues]
riskTolerance = 0.05
```

Auto-migrated State:
```json
{
    "balances": { "ETH": 100 },
    "lastPrice": { "ETH/USDC": 2900 },
    "riskTolerance": 0.05
}
```

## Benefits of This Approach

- Travelers do nothing for common schema changes.  
- Strong typing and schema evolution rules guarantee compatibility.  
- Every schema change is **documented in the program history**.  
- Bandits refuse to apply unsafe changes (renames, type changes).

## When Migration Functions Are Still Required

| Case | Example |
|---|---|
| Semantic Changes | Change `riskTolerance` from being per-asset to global. |
| Field Renames | Rename `price` to `oraclePrice`. |
| Field Splits | Replace `spread` with `{askPrice, bidPrice}`. |
| Aggregation Logic | Add a rolling average that needs to bootstrap from history. |

For these, travelers provide a **MigrationFunction** that is:
- Pure.
- Content-addressed.
- Logged in the **UpgradeProgram** effect.

---

## Addendum II: Hostage Situation Prevention

### Context

In cross-program workflows, **Program A** may call **Program B**.  
If Program A can only upgrade when it is in a **safe state**, this creates a risk:  
- **Program B could intentionally stall the return value**.
- **Program A becomes stuck — unable to upgrade until B cooperates**.

### Options to Mitigate Hostage Risk

#### Option 1: Strict No Pending Returns (Hard Rule)

- Programs **cannot make cross-program calls** unless they explicitly allow deferring their own upgrade.
- This means programs either:
    - Operate **fully standalone**.
    - Or explicitly handle the risk of depending on others.

✅ Simple and predictable.  
❌ Severely limits composability.  

#### Option 2: Timeout and Auto-Abort (Graceful Degrade)

- If a call to Program B doesn’t return within a deadline, Program A enters a **degraded state**.
- This state logs the timeout and:
    - Either ignores the result permanently.
    - Or substitutes a default result.
- This allows upgrades to proceed from the degraded state.

✅ Keeps composability.  
✅ Avoids permanent deadlock.  
❌ Some loss of flexibility in program logic.

#### Option 3: Optional Lock Contracts

- Programs could register **lock contracts** with Bandits.
- A lock contract governs **under what conditions a program can hold another hostage**.
- Example: A program can only delay another program if both were deployed together (composable pair).

✅ Highly flexible.  
✅ Explicit opt-in.  
❌ Complex to implement.

#### Option 4: Explicit Safe State Handshakes

- When Program A calls Program B, it sends a **safe state intent**.
- This declares:
    - When Program A expects to upgrade.
    - What state it expects to be in at that time.
- Program B either:
    - Acknowledges this (promising to return before then).
    - Or refuses the interaction.

✅ Formalizes expectations up front.  
❌ Adds messaging overhead.

### Recommended Approach

| Component | Approach |
|---|---|
| Default | Option 2: Timeout and Auto-Abort |
| Programs That Never Upgrade Mid-Run | Option 1: Strict No Pending Returns |
| Highly Coupled Programs | Option 3: Lock Contracts |
| Programs with High-Value State Transitions | Option 4: Safe State Handshake |

### Example Timeout Rule

```toml
[safeStateStrategy]
type = "no-pending-returns"
timeout = "15 minutes"
degradedState = { status = "timeout", reason = "awaiting ProgramB" }
```

---

### Example Safe State Handshake

When A calls B:
```json
{
    "call": "doRiskCheck",
    "args": { "balance": 100 },
    "safeStateIntent": {
        "expectedReturnTime": "2025-06-01T12:00:00Z",
        "expectedSafeState": "no-pending-returns"
    }
}
```

Program B either acknowledges:
```json
{
    "ack": true
}
```
Or refuses:
```json
{
    "ack": false,
    "reason": "Program B does not support guaranteed return times"
}
```

## Key Takeaways

| Mechanism | Goal |
|---|---|
| Schema Evolution Rules | Allow no-touch upgrades for travelers. |
| Migration Functions | Only required for semantic shifts. |
| Safe State Policies | Define how and when programs can upgrade. |
| Timeout Handling | Default safeguard against hostage risk. |
| Safe State Handshake | Optional stricter commitment mechanism. |

## Summary Recommendations

- Standardize **schema evolution rules** in the protocol.  
- Make **time travelers opt into a safe state policy** at program deploy time.  
- Use **timeouts by default** for pending return mitigation.  
- Allow optional safe state handshakes for travelers who want stronger guarantees.  
- Document all safe state policies and program version history in the effect log.