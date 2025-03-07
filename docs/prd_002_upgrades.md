# PRD 002: Program and Protocol Upgrades (Updated - 2025-03-07)


## Context and Motivation

Time Bandits is a system for deploying, executing, and proving the execution of **cross-timeline programs** — programs that operate across multiple independent blockchains (timelines). These programs depend on external state (prices, balances, external messages) and must preserve **causal integrity** across all effects, facts, and observations.

Over time, both the **Time Bandits protocol itself** (the infrastructure operated by Time Bandits nodes) and **individual programs** (authored and owned by time travelers) must evolve to support:

- New chains and effect types.
- New proof formats.
- New cryptographic primitives.
- New user-defined program logic.

This document defines the requirements, constraints, and design decisions for enabling safe and secure program and protocol upgrades.


## Requirements

### R1 - Traveler Sovereignty
- **Travelers fully own and control their programs.**
- No upgrade — to either program logic or protocol rules — can be **forced** on a program.
- Travelers can **opt out** of protocol upgrades and choose to stay pinned to a prior version — as long as they can find Bandits willing to run the legacy version.

### R2 - Safe Upgrade Defaults
- The **default path** favors **automatic upgrades** to the latest protocol version.
- If a program is in a **safe state**, upgrades should:
    - Automatically apply schema migrations (where possible).
    - Migrate to the new version without traveler intervention.

### R3 - Explicit Safe States for Migration
- Travelers should optionally declare **safe state strategies** defining when upgrades are permissible.
- Safe states can be:
    - **Always Safe:** Upgrade at any time.
    - **No Pending Returns:** Only upgrade when no cross-program calls are unresolved.
    - **Manual Trigger:** Traveler explicitly signals a safe state.

### R4 - Schema Evolution without Manual Migration
- Schema changes should, whenever possible, be **automatic**.
- Only **semantic shifts** (meaning changes, not format changes) should require traveler-authored migration code.
- Schema evolution rules should cover:
    - Adding fields.
    - Removing unused fields.
    - Adding default fields.


## Core Concepts

### Program Version Pinning
Every program declares:
```toml
[program]
name = "CrossChainArb"
version = "1.2.0"
protocolVersion = "2.3.0"
safeStateStrategy = "no-pending-returns"

[history]
previousVersions = ["sha256-oldversion1", "sha256-oldversion2"]
```

This allows Bandits to validate **which runtime version applies** for every effect.


### Epoch-Based Protocol Upgrades
The Time Bandits **protocol version** evolves in discrete **epochs**. All Bandits agree on:

```toml
[current]
protocolVersion = "2.3.0"
compatibleProgramVersions = ["1.x", "2.x"]

[upcoming]
activationEpoch = 180000
newVersion = "3.0.0"
```

This ensures coordinated, deterministic network upgrades.


### Compatibility Policy

| Program Version | Protocol Version | Behavior |
|---|---|---|
| Older than compatible range | Rejected until upgraded. |
| Within compatible range | Runs normally. |
| Newer than Bandit version | Rejected (Bandits must upgrade). |


### Safe State Declaration
Programs declare their safe state policy at deployment:

| Strategy | Meaning |
|---|---|
| `always` | Safe to upgrade anytime. |
| `no-pending-returns` | Safe only if no pending cross-program calls. |
| `manual` | Traveler explicitly signals safe points. |


### Migration Functions
For **schema changes not covered by automatic evolution rules**, travelers provide:

```haskell
type MigrationFunction = OldProgramState -> Either MigrationError NewProgramState
```

This is a logged effect in the program’s unified log.


## Example Migration Effect

```haskell
data EffectType
    = Deposit Resource Amount
    | Withdraw Resource Amount
    | Transfer Resource Amount Recipient
    | UpgradeProgram
        { oldHash :: ProgramHash
        , newHash :: ProgramHash
        , migration :: Maybe MigrationFunction
        }
```


## Autonomous Schema Evolution
By default, Time Bandits applies **schema evolution rules** when upgrading programs.

Example schema evolution:

```toml
[schema]
version = "1.2.0"
fields = ["balances", "riskTolerance"]

[evolution]
allowed = ["add-optional-field", "remove-unused-field"]

[defaultValues]
riskTolerance = 0.05
```

This allows Bandits to automatically evolve program states **without traveler action** if the changes are non-breaking.


## Pending Return Hostage Risk
A special risk arises when **Program A** calls **Program B**, but B deliberately **refuses to return** — blocking A’s upgrade. Options to mitigate:

| Option | Description |
|---|---|
| Timeout + Degrade | After timeout, auto-abort call and enter degraded state. |
| Fallback State | Define a fallback state for incomplete calls. |
| Self-Contained Programs | Programs can opt to avoid cross-program calls entirely. |
| Safe State Handshake | Programs negotiate return guarantees before calls. |

Default: **Timeout + Degrade**


## Migration Flow

| Step | Action |
|---|---|
| Epoch N | Bandits run Protocol 2.3.0 |
| Program P runs | Under Protocol 2.3.0 |
| Epoch N+1 | ProtocolUpgrade to 3.0.0 |
| Program P | Checks safe state |
| If safe | Applies auto-migration |
| If not safe | Stays pinned to 2.3.0 |
| If schema breaks | Traveler provides migration function |
| Unified Log | Records `UpgradeProgram` effect |


## Consequences of Staying Pinned
Travelers who **refuse upgrades** may need to:
- Incentivize Bandits to run legacy versions.
- Lose access to newer program capabilities.
- Manually maintain effect adapters (if external timelines evolve).
- Handle all cross-program interop manually.

This is intentional — sovereignty > convenience.


## Replay and Audit
- Every upgrade is an **effect** in the unified log.
- Effects are tied to time map observations.
- Replay must:
    - Re-run migrations.
    - Reconstruct safe state checks.
    - Validate schema evolution rules.


## Benefits

- Programs fully own their upgrade path.  
- Default path favors safe automatic evolution.  
- Bandits remain compatible across epochs.  
- Schema evolution is strongly typed and rule-driven.  
- All upgrades leave a **permanent audit trail**.  
- External auditors can verify upgrade correctness by replaying logs.


## Visual Timeline


```
Epoch N: Bandits v2.3.0

ProgramA runs with ProgramVersion 1.2.0
Epoch N+1: Bandits upgrade to v3.0.0

ProgramA checks safe state (no pending returns)
ProgramA auto-upgrades to 1.3.0
Unified log records UpgradeProgram effect

```


## Open Questions

| Question | Current Answer |
|---|---|
| How long do Bandits support old protocols? | Configurable — minimum N epochs. |
| Can travelers pre-sign migration functions? | Yes — pre-approved migrations can run automatically. |
| Can a program downgrade? | No — effects are forward-only. |
| Can program and protocol upgrades be decoupled? | Partially — programs choose to accept or reject protocol upgrades. |


## Summary

This upgrade model:

- Respects traveler sovereignty.  
- Supports automatic schema evolution.  
- Preserves causal traceability for all upgrades.  
- Provides flexible safe state strategies.  
- Uses time map snapshots to anchor all upgrade effects in causal time.

This strikes a careful balance between:

- Protecting program sovereignty.
- Ensuring Bandits can safely upgrade.
- Preserving seamless replay and auditability.
