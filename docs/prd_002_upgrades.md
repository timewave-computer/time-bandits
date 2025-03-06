# Time Bandits PRD: Program and Protocol Upgrades

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

| Program Versiom [ ] Protocol Version | Behavior |
|---|---|
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

### Problem Statement
When the **Time Bandits protocol** evolves (new effect types, new fact types, new combinators), programs that were written under **older versions** may need their internal state schemas upgraded to match the new environment. Ideally, this should:

- Require no manual intervention from the time traveler.
- Be **safe**, meaning the program is only upgraded if it can clearly migrate to the new schema without risk.
- Be **auditable**, meaning the upgrade itself is a logged effect.
- Be **content-addressed**, meaning the new schema and migration rules are immutable once applied.

### What Makes Evolution Hard
- **Programs own arbitrary user-defined data.**
- Some programs are **in-flight** (cross-program calls are pending).
- Some schema changes are safe (adding fields with defaults), some are not (changing types, removing fields).
- Different programs have **different safe state policies** (some can upgrade at any time, others only at safe checkpoints).

## Autonomous Evolution Model

### Schema Definition

Programs would include a machine-readable schema, versioned alongside the program:
```toml
[schema]
version = "1.2.0"
fields = [
    { name = "balanceSheet", type = "Map<ResourceID, Amount>" },
    { name = "riskMetric", type = "Decimal" },
    { name = "lastRebalanceTime", type = "Timestamp" }
]
```

### System-Level Evolution Rules

The Time Bandits protocol defines **schema evolution rules** that apply if the schema change is simple, such as:
- **Adding a new field** with a clear default value.
- **Adding a new optional field** (which is absent in older states).
- **Adding a new fact type or effect type that does not invalidate older programs’ semantics.**

These rules are protocol-defined, content-addressed, and **do not require traveler participation**.

### Example Evolution Rule

If the new schema adds a `safetyMargin` field, the system rule might say:
```haskell
migrateField "safetyMargin" = Just (DefaultDecimal 0.1)
```

### Traveler-Supplied Migration Functions

If a schema change **removes fields, changes meaning, or breaks assumptions**, a program may require a **traveler-supplied migration function**.

These are content-addressed and optionally registered at program deployment:
```haskell
type MigrationFunction = OldProgramState -> Either MigrationError NewProgramState
```

The `UpgradeProgram` effect logs:
```haskell
UpgradeProgram
    { oldHash = "abc123"
    , newHash = "def456"
    , migration = Just "ghi789"
    }
```

### Full Decision Process for Evolution

| Case | Action |
|---|---|
| Schema is forward-compatible | No change needed |
| Schema change is safe under system rules | Auto-migrate using protocol rules |
| Schema change is unsafe | Require traveler-supplied migration |
| Traveler-supplied migration missing | Program remains pinned, legacy Bandits required |

### Migration Effect in DAG

Every migration becomes a first-class effect in the DAG:
```haskell
Effect
    { effectID = "efg123"
    , parentEffects = ["prevEffectHash"]
    , effectType = UpgradeProgram oldHash newHash (Just migrationHash)
    , payload = empty
    , observedFacts = empty
    }
```

---

## Addendum II: Options for Preventing Hostage Situations (Pending Call Blocking Upgrade)

### Problem Statement

In cross-program workflows, a program (`A`) might call another program (`B`), and wait for a response.  
If `B` **delays indefinitely**, `A` can never enter a **safe state** to upgrade.  
This is a hostage scenario — `B` can hold `A`'s upgrade hostage.

### Goals

- Avoid leaving programs permanently pinned because of **unresponsive dependencies**.
- Avoid breaking **valid workflows** where `B` is just genuinely slow.
- Preserve causal consistency — if `A` aborts the call, this must be recorded.

### Option 1: Hard Timeouts on All Cross-Program Calls (Strict)

Every cross-program call includes a **time-to-live**.
If `B` does not respond in time, `A` enters a **degraded state** (tracked in the DAG), logs the failure, and becomes **upgradeable** again.

**Example Effect**
```haskell
Effect
    { effectType = CallProgram
        { target = "B"
        , function = "getRiskProfile"
        , args = []
        , timeout = Just (Duration "30 minutes")
        }
    }
```

If the timeout expires, `A` logs:
```haskell
Effect
    { effectType = CallTimeout "B" "getRiskProfile"
    }
```

#### Pros
- Strong safety guarantee — no program is permanently blocked.
- Fully auditable — the timeout is logged in causal history.
- Time travelers **opt in** to the timeout policy at program deployment.

#### Cons
- Some workflows might genuinely require **long or indefinite calls**.
- Time travelers must carefully pick **safe timeouts** at design time.

### Option 2: Programmable Fallback Paths (Soft)

Each program defines **fallback logic** if a dependent program fails to respond.
This is essentially **"Plan B if dependency fails"**.

Example:
```haskell
callProgram "B" "getRiskProfile" []
    `timeoutAfter` 30 minutes
    fallback log("Risk profile missing, continuing with defaults")
```

#### Pros
- More flexibility — programs choose graceful failure modes.
- Works well if programs are designed for **robust degraded operation**.

#### Cons
- Requires more work by time travelers.
- Could lead to **combinatorial explosion** of fallback logic if heavily used.

### Option 3: No External Calls Allowed (Isolation Mode)

Some programs could declare themselves **"isolated"**, meaning they will never rely on external programs.
These programs can **always be safely upgraded**.

```toml
[program]
name = "SelfContainedRebalancer"
safeStateStrategy = "always"
noExternalCalls = true
```

#### Pros
- Maximum upgrade flexibility.
- Simple mental model for time travelers.

#### Cons
- Severely limits composability.
- Doesn’t work for programs intended to **coordinate across multiple travelers**.

### Option 4: Traveler-Initiated Aborts (Manual Escape Hatch)

A time traveler can explicitly declare the program **abandoned** if it is blocked by an external dependency.
This is an **irrevocable action**, logged as a causal effect.

```haskell
Effect
    { effectType = ForceUpgradeWithAbandonment
        { abandonedCalls = [("B", "getRiskProfile")] }
    }
```

#### Pros
- Preserves ultimate traveler sovereignty.
- Useful in emergency scenarios (e.g., hacked program B).

#### Cons
- Dangerous if overused — breaks composability if used frequently.
- Requires manual traveler action, so less autonomous.

## Recommendation

| Situation | Recommended Solution |
|---|---|
| Default cross-program call | **Timeout + log degraded state** |
| Long-running workflows | **Explicit fallback paths** |
| Fully isolated programs | **Declare no external calls** |
| Emergency intervention | **Traveler-initiated abandonment** |

## Summary Table

| Option | Flexibility | Complexity | Traveler Effort |
|---|---|---|---|
| Hard Timeouts | ✅ Simple | ✅ Low | ✅ Low |
| Fallback Paths | ✅ Flexible | ❌ More logic | ✅ Medium |
| Isolation Mode | ✅ Very Safe | ✅ Simple | ✅ Low |
| Manual Abandonment | ✅ Sovereign | ✅ Simple | ❌ High (manual action) |

## Final Takeaway

- Default = Timeouts with logged degradation.
- Allow fallback paths for programs that need resilience.
- Allow isolation mode for simple programs.
- Reserve abandonment for true emergencies.
