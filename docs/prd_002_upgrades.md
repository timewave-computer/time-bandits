# PRD 002: Program and Protocol Upgrades (Updated - 2025-03-07)


## Context and Motivation

Time Bandits is a system for deploying, executing, and proving the execution of **cross-timeline programs** — programs that operate across multiple independent blockchains (timelines). These programs depend on external state (prices, balances, external messages) and must preserve **causal integrity** across all effects, facts, and observations.

With the introduction of the formalized resource model (ADR_018), programs also track formalized resources with explicit tuples, controller labels, and conservation laws. This new model adds complexity to the upgrade process, particularly for programs that manage cross-chain resources.

Over time, both the **Time Bandits protocol itself** (the infrastructure operated by Time Bandits nodes) and **individual programs** (authored and owned by time travelers) must evolve to support:

- New chains and effect types.
- New proof formats.
- New cryptographic primitives.
- New user-defined program logic.
- New resource types and controller mechanisms.
- Updates to resource formalization rules.
- Changes in dual validation requirements.

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
    - Migrate formalized resources to new schema formats.
    - Update controller labels to comply with new requirements.
    - Migrate to the new version without traveler intervention.

### R3 - Explicit Safe States for Migration
- Travelers should optionally declare **safe state strategies** defining when upgrades are permissible.
- Safe states can be:
    - **Always Safe:** Upgrade at any time.
    - **No Pending Returns:** Only upgrade when no cross-program calls are unresolved.
    - **No Cross-Chain Resources:** Only upgrade when no cross-chain resources are in transit.
    - **Resource Quiescence:** Only upgrade when all resources are in a stable state (not in process of creation/consumption).
    - **Manual Trigger:** Traveler explicitly signals a safe state.

### R4 - Schema Evolution without Manual Migration
- Schema changes should, whenever possible, be **automatic**.
- Only **semantic shifts** (meaning changes, not format changes) should require traveler-authored migration code.
- Schema evolution rules should cover:
    - Adding fields.
    - Removing unused fields.
    - Adding default fields.
    - Updating resource formalizations.
    - Migrating controller labels.

### R5 - Resource Conservation During Upgrades
- Resource conservation laws (ΔTX = 0) must be maintained during and after upgrades.
- Upgrades must not create, destroy, or duplicate formalized resources.
- Controller labels must maintain valid provenance information across upgrades.


## Core Concepts

### Program Version Pinning
Every program declares:
```toml
[program]
name = "CrossChainArb"
version = "1.2.0"
protocolVersion = "2.3.0"
safeStateStrategy = "no-pending-returns"
resourceStrategy = "quiescent-resources"

[history]
previousVersions = ["sha256-oldversion1", "sha256-oldversion2"]

[resourceFormalization]
version = "1.0.0"
controllerLabelVersion = "1.1.0"
```

This allows Bandits to validate **which runtime version applies** for every effect and which resource formalization rules to use.


### Epoch-Based Protocol Upgrades
The Time Bandits **protocol version** evolves in discrete **epochs**. All Bandits agree on:

```toml
[current]
protocolVersion = "2.3.0"
compatibleProgramVersions = ["1.x", "2.x"]
resourceFormalizationVersion = "1.0.0"

[upcoming]
activationEpoch = 180000
newVersion = "3.0.0"
newResourceFormalizationVersion = "1.1.0"
```

This ensures coordinated, deterministic network upgrades.


### Compatibility Policy

| Program Version | Protocol Version | Resource Formalization | Behavior |
|---|---|---|---|
| Older than compatible range | Any | Rejected until upgraded. |
| Within compatible range | Compatible | Runs normally. |
| Within compatible range | Incompatible | Resource migration required. |
| Newer than Bandit version | Any | Rejected (Bandits must upgrade). |


### Safe State Declaration
Programs declare their safe state policy at deployment:

| Strategy | Meaning |
|---|---|
| `always` | Safe to upgrade anytime. |
| `no-pending-returns` | Safe only if no pending cross-program calls. |
| `no-cross-chain-resources` | Safe only when no resources are in transit across chains. |
| `quiescent-resources` | Safe only when all resources are in stable states. |
| `manual` | Traveler explicitly signals safe points. |


### Migration Functions
For **schema changes not covered by automatic evolution rules**, travelers provide:

```haskell
type MigrationFunction = OldProgramState -> Either MigrationError NewProgramState

-- For resource-specific migrations
type ResourceMigrationFunction = OldResource -> Either MigrationError NewResource

-- For controller label migrations
type ControllerLabelMigrationFunction = OldControllerLabel -> Either MigrationError NewControllerLabel
```

This is a logged effect in the program's unified log.


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
        , resourceMigrations :: Maybe (Map ResourceType ResourceMigrationFunction)
        , controllerLabelMigration :: Maybe ControllerLabelMigrationFunction
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

[resourceFormalization]
autoMigrateResources = true
preserveControllerHistory = true
updateFungibilityDomains = { "ERC20:USDT": "MultiAsset:USDT" }
```

This allows Bandits to automatically evolve program states **without traveler action** if the changes are non-breaking.


## Resource Migration During Upgrades

When upgrading programs that use the formalized resource model, special care must be taken:

```haskell
-- Example resource migration function
migrateResource :: OldResource -> Either MigrationError NewResource
migrateResource old = do
    -- Ensure resource conservation
    let oldDelta = calculateDelta [old]
    
    -- Create new resource with updated fields
    let new = Resource {
        resourceLogic = upgradeLogic old.resourceLogic,
        fungibilityDomain = old.fungibilityDomain,
        quantity = old.quantity,
        metadata = addVersionField old.metadata,
        ephemeral = old.ephemeral,
        nonce = old.nonce,
        nullifierPubKey = upgradeNullifierKey old.nullifierPubKey,
        randomnessSeed = old.randomnessSeed
    }
    
    -- Verify conservation law is maintained
    let newDelta = calculateDelta [new]
    if newDelta /= oldDelta
        then Left $ ConservationViolation oldDelta newDelta
        else Right new
```

### Controller Label Migration

```haskell
-- Example controller label migration
migrateControllerLabel :: OldControllerLabel -> Either MigrationError NewControllerLabel
migrateControllerLabel old = do
    -- Check if creating controller is still valid
    let newCreatingController = upgradeController old.creatingController
    
    -- Update terminal controller if needed
    let newTerminalController = upgradeController old.terminalController
    
    -- Update all affecting controllers
    let newAffectingControllers = map upgradeController old.affectingControllers
    
    -- Create new controller label
    Right $ ControllerLabel {
        creatingController = newCreatingController,
        terminalController = newTerminalController,
        affectingControllers = newAffectingControllers,
        backupControllers = map upgradeController old.backupControllers
    }
```

## Pending Return Hostage Risk
A special risk arises when **Program A** calls **Program B**, but B deliberately **refuses to return** — blocking A's upgrade. Options to mitigate:

| Option | Description |
|---|---|
| Timeout + Degrade | After timeout, auto-abort call and enter degraded state. |
| Fallback State | Define a fallback state for incomplete calls. |
| Self-Contained Programs | Programs can opt to avoid cross-program calls entirely. |
| Safe State Handshake | Programs negotiate return guarantees before calls. |
| Resource Timeout Nullifiers | Resources locked in pending calls can be nullified and recreated after timeout. |

Default: **Timeout + Degrade with Resource Conservation**


## Cross-Chain Resource Risk During Upgrades

For programs that manage cross-chain resources, additional risks during upgrades include:

1. **In-Transit Resources**: Resources that are in the process of crossing chains during an upgrade.
2. **Controller Label Incompatibility**: A new version changes controller label requirements.
3. **Conservation Violations**: Upgrade migrations that would violate ΔTX = 0.
4. **Dual Validation Changes**: Modifications to temporal or ancestral validation.

Solutions to these risks:

| Risk | Mitigation |
|---|---|
| In-Transit Resources | Wait for quiescence or implement resource recovery mechanisms. |
| Label Incompatibility | Provide migration functions for controller labels. |
| Conservation Violations | Enforce static verification of migration functions. |
| Validation Changes | Ensure backward compatibility for validation rules. |


## Migration Flow

| Step | Action |
|---|---|
| Epoch N | Bandits run Protocol 2.3.0 |
| Program P runs | Under Protocol 2.3.0 with Resource Formalization 1.0.0 |
| Epoch N+1 | ProtocolUpgrade to 3.0.0 with Resource Formalization 1.1.0 |
| Program P | Checks safe state + resource quiescence |
| If safe | Applies auto-migration + resource migration |
| If not safe | Stays pinned to 2.3.0 |
| If schema breaks | Traveler provides migration function |
| If resource schema changes | Apply resource migration functions |
| If controller labels change | Apply controller label migration |
| Unified Log | Records `UpgradeProgram` effect with resource details |
| Verify | Confirm resource conservation laws maintained (ΔTX = 0) |


## Consequences of Staying Pinned
Travelers who **refuse upgrades** may need to:
- Incentivize Bandits to run legacy versions.
- Lose access to newer program capabilities.
- Manually maintain effect adapters (if external timelines evolve).
- Handle all cross-program interop manually.
- Maintain compatibility with older resource formalization models.
- Miss security improvements in dual validation.

This is intentional — sovereignty > convenience.


## Replay and Audit
- Every upgrade is an **effect** in the unified log.
- Effects are tied to time map observations.
- Replay must:
    - Re-run migrations.
    - Reconstruct safe state checks.
    - Validate schema evolution rules.
    - Verify resource conservation (ΔTX = 0) across migrations.
    - Confirm controller label validity after migration.
    - Ensure dual validation rules are properly applied.


## Benefits

- Programs fully own their upgrade path.  
- Default path favors safe automatic evolution.  
- Bandits remain compatible across epochs.  
- Schema evolution is strongly typed and rule-driven.  
- All upgrades leave a **permanent audit trail**.  
- External auditors can verify upgrade correctness by replaying logs.
- Resource conservation is maintained across upgrades.
- Controller labels preserve resource provenance during migrations.
- Dual validation ensures cross-chain security after upgrades.


## Visual Timeline


```
Epoch N: Bandits v2.3.0, Resource Model v1.0.0

ProgramA runs with ProgramVersion 1.2.0
ProgramA manages cross-chain resources with controller labels

Epoch N+1: Bandits upgrade to v3.0.0, Resource Model v1.1.0

ProgramA checks safe state (no pending returns)
ProgramA checks resource quiescence (no cross-chain transfers in progress)
ProgramA applies resource migration functions to update resource tuples
ProgramA updates controller labels to new format
ProgramA auto-upgrades to 1.3.0
Unified log records UpgradeProgram effect with resource migration details

```


## Open Questions

| Question | Current Answer |
|---|---|
| How long do Bandits support old protocols? | Configurable — minimum N epochs. |
| Can travelers pre-sign migration functions? | Yes — pre-approved migrations can run automatically. |
| Can a program downgrade? | No — effects are forward-only. |
| Can program and protocol upgrades be decoupled? | Partially — programs choose to accept or reject protocol upgrades. |
| How to handle resources that span multiple programs during upgrades? | Coordinated upgrades or resource quiescence requirements. |
| How to migrate controller labels when controller types change? | Explicit controller mapping functions in migration. |
| Can resource formalization changes be separated from protocol upgrades? | Yes, through versioned resource formalization rules. |


## Summary

This upgrade model:

- Respects traveler sovereignty.  
- Supports automatic schema evolution.  
- Preserves causal traceability for all upgrades.  
- Provides flexible safe state strategies.  
- Uses time map snapshots to anchor all upgrade effects in causal time.
- Maintains resource conservation (ΔTX = 0) across upgrades.
- Preserves controller label provenance during migrations.
- Ensures dual validation integrity for cross-chain resources.

This strikes a careful balance between:

- Protecting program sovereignty.
- Ensuring Bandits can safely upgrade.
- Preserving seamless replay and auditability.
- Maintaining resource integrity across versions.
- Supporting the full resource formalization model (ADR_018).
