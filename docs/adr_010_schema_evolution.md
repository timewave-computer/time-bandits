# ADR 010: Autonomous Schema Evolution and Hostage Prevention in Time Bandits


## Autonomous Schema Evolution

The goal of **autonomous schema evolution** is to allow **program upgrades to happen without requiring traveler-written migrations** — as long as the schema change falls into **a predictable class of non-breaking changes**.

This fits well with the **default safe upgrade path** where programs automatically evolve along with the protocol, provided they are in a **safe state** at the time of the upgrade.


## What Kinds of Schema Changes Can Be Auto-Evolved?

| Change Type | Example | Automatically Safe? |
|---|---|---|
| Add New Field (Optional) | Add `riskTolerance` to program state | - Yes |
| Add New Field (With Default) | Add `maxSlippage` with default of `0.01` | - Yes |
| Remove Unused Field | Remove deprecated `legacyCounter` | - Yes |
| Change Field Type (Coercible) | Change `ethBalance` from `Int` to `Decimal` | ⚠️ Safe if lossless coercion |
| Rename Field | Rename `price` to `oraclePrice` | ❌ Requires explicit migration |
| Add New Effect Type | Add `ObserveOracle` | - Yes (if old Bandits ignore) |
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


# Hostage Situation Prevention


## Context

In cross-program workflows, **Program A** may call **Program B**.  
If Program A can only upgrade when it is in a **safe state**, this creates a risk:  
- **Program B could intentionally stall the return value**.
- **Program A becomes stuck — unable to upgrade until B cooperates**.


## Options to Mitigate Hostage Risk

### Option 1: Strict No Pending Returns (Hard Rule)

- Programs **cannot make cross-program calls** unless they explicitly allow deferring their own upgrade.
- This means programs either:
    - Operate **fully standalone**.
    - Or explicitly handle the risk of depending on others.

- Simple and predictable.  
❌ Severely limits composability.  


### Option 2: Timeout and Auto-Abort (Graceful Degrade)

- If a call to Program B doesn’t return within a deadline, Program A enters a **degraded state**.
- This state logs the timeout and:
    - Either ignores the result permanently.
    - Or substitutes a default result.
- This allows upgrades to proceed from the degraded state.

- Keeps composability.  
- Avoids permanent deadlock.  
❌ Some loss of flexibility in program logic.


### Option 3: Optional Lock Contracts

- Programs could register **lock contracts** with Bandits.
- A lock contract governs **under what conditions a program can hold another hostage**.
- Example: A program can only delay another program if both were deployed together (composable pair).

- Highly flexible.  
- Explicit opt-in.  
❌ Complex to implement.


### Option 4: Explicit Safe State Handshakes

- When Program A calls Program B, it sends a **safe state intent**.
- This declares:
    - When Program A expects to upgrade.
    - What state it expects to be in at that time.
- Program B either:
    - Acknowledges this (promising to return before then).
    - Or refuses the interaction.

- Formalizes expectations up front.  
❌ Adds messaging overhead.


## Recommended Approach

| Component | Approach |
|---|---|
| Default | Option 2: Timeout and Auto-Abort |
| Programs That Never Upgrade Mid-Run | Option 1: Strict No Pending Returns |
| Highly Coupled Programs | Option 3: Lock Contracts |
| Programs with High-Value State Transitions | Option 4: Safe State Handshake |


## Example Timeout Rule

```toml
[safeStateStrategy]
type = "no-pending-returns"
timeout = "15 minutes"
degradedState = { status = "timeout", reason = "awaiting ProgramB" }
```


## Example Safe State Handshake

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


# Key Takeaways

| Mechanism | Goal |
|---|---|
| Schema Evolution Rules | Allow no-touch upgrades for travelers. |
| Migration Functions | Only required for semantic shifts. |
| Safe State Policies | Define how and when programs can upgrade. |
| Timeout Handling | Default safeguard against hostage risk. |
| Safe State Handshake | Optional stricter commitment mechanism. |


# Summary Recommendations

- Standardize **schema evolution rules** in the protocol.  
- Make **time travelers opt into a safe state policy** at program deploy time.  
- Use **timeouts by default** for pending return mitigation.  
- Allow optional safe state handshakes for travelers who want stronger guarantees.  
- Document all safe state policies and program version history in the effect log.


# Addendum to ADR 010: Compiler and Deployment Architecture

## Context

This addendum extends ADR 010 to address compiler implementation, deployment workflows, and extension mechanisms within the Time Bandits system. Following our core principle of traveler sovereignty, we need a cohesive architecture that enables travelers to compile, deploy, and extend programs while maintaining the system's integrity guarantees.

## Key Decisions

### 1. Account Programs as Deployment Gateways

All deployment operations must flow through account programs, maintaining our "programs talk to programs" invariant:

```haskell
data AccountProgramEffect
  = Deposit Asset Amount
  | Withdraw Asset Amount
  | DeployProgram CompiledProgram DeploymentConfig
  | DeployEffect CustomEffect
  | DeployTimelineAdapter TimelineAdapter
  | ActivateProgram ProgramID ActivationStrategy
```

This unified approach enables both traveler-initiated and program-initiated deployments to follow the same pipeline, preserving causality and audit trails.

### 2. Explicit Compiler Architecture

The TEL compiler transforms source programs into deployable artifacts through a series of validated transformations:

1. **Parse** TEL source into AST
2. **Type Check** program structure and resource usage
3. **Effect Validate** causal relationships and temporal properties
4. **Transform** to an Effect-Based IR
5. **Optimize** for targeted execution environment
6. **Generate** content-addressed deployment artifacts

Each stage enforces guarantees that ensure program correctness before deployment.

### 3. Configurable Deployment Activation Strategies

Programs may specify their preferred activation strategy:

```haskell
data ActivationStrategy
  = ManualStepwise                -- Activate each component separately after verification
  | FloodActivation               -- Last component triggers parallel activation messages
  | ChainedActivation OrderSpec   -- Activation proceeds in specified order
  | ConditionBased [Condition]    -- Activate when specific conditions are met
```

This enables complex cross-timeline coordination patterns while preserving traveler control over the activation sequence.

### 4. System Extensibility via Custom Effects and Timeline Adapters

Travelers may extend the system by deploying:
- **Custom Effects** that add new capabilities to the effect system
- **Timeline Adapters** that enable interaction with new blockchains

These extensions are deployed through account programs and tracked using the content-addressable code system, enabling composition and reuse.

### 5. Version Compatibility Management for Custom Extensions

Custom effects and adapters must declare their compatibility with protocol versions:

```toml
[effect]
name = "ObserveCustomOracle"
version = "1.2.0"

[effect.compatibility]
protocol_versions = ["2.x", "3.x"]
evolution_rules = ["add-optional-parameter", "refine-return-type"]

[effect.handler]
hash = "bafy123..."

[effect.fallbacks]
protocol = "2.x"
strategy = "use-alternative-implementation"
alternative_handler = "bafy456..."
```

This ensures that extensions can evolve alongside the protocol without breaking existing programs.

## Implementation Details

### Compiler Pipeline

The compiler will output artifacts in a uniform format that includes:

```haskell
data CompiledProgram = CompiledProgram
  { programHash :: ContentHash           -- Content hash of the entire program
  , effectDAG :: EffectDAG               -- The core effect structure
  , dependencies :: Set ContentHash      -- All code/effect dependencies
  , schema :: Schema                     -- Program state schema
  , schemaEvolutionRules :: [EvolutionRule] -- Allowed schema changes
  , compatibleProtocolVersions :: VersionRange -- Compatible protocol versions
  }
```

The content-addressable nature of this representation ensures that programs are immutable and can be precisely referenced.

### Deployment Flow

The deployment sequence will follow this pattern:

1. Traveler **compiles** program locally
2. Traveler **initiates deployment** through their account program
3. Account program **distributes** program to Bandits
4. Bandits **register** program components on relevant timelines
5. Time Keepers **observe and verify** successful deployment
6. Traveler or program **activates** deployment according to chosen strategy

### Custom Effect Registration

Custom effects will be registered in a global effect registry that tracks:

```haskell
data EffectRegistry = EffectRegistry
  { effectDefinitions :: Map EffectID EffectDefinition
  , effectHandlers :: Map (EffectID, Version) ContentHash
  , effectCompatibility :: Map EffectID [ProtocolCompatibility]
  }
```

This registry ensures that effects are consistently available across the Bandit network.

## Consequences

### Positive

- **Uniform Deployment Model**: Same mechanism for all deployments maintains simplicity
- **Extensibility**: Travelers can extend the system with new effects and timeline adapters
- **Sovereignty Preservation**: All deployments remain under traveler control
- **Upgrade Safety**: Version compatibility declarations prevent breaking changes
- **Composability**: Content-addressed components enable safe composition and reuse

### Challenges

- **Partial Deployment Handling**: We need robust recovery mechanisms for partial deployment failures
- **Effect Compatibility Matrix**: Managing compatibility between protocol versions and custom effects will grow in complexity
- **Testing Requirements**: Comprehensive testing of custom components requires sophisticated simulation capabilities

## Implementation Plan

We recommend implementing this architecture in phases:

1. **Compiler Foundation**: Basic TEL compiler with effect validation
2. **Deployment Pipeline**: Core deployment flow through account programs
3. **Activation Strategies**: Support for different activation patterns
4. **Custom Effects**: Extension mechanism for effect system
5. **Timeline Adapters**: Extension mechanism for blockchain support

Each phase should be implemented with full testing and documentation before proceeding to the next.

## Additional Considerations

The interaction between custom effects and schema evolution requires careful consideration. While schema evolution applies to program state, effect evolution applies to the protocol's extension points. We should develop clear guidelines for when to evolve an effect versus creating a new one.

When extending the system with new timeline adapters, we should consider a vetting or reputation system to ensure that adapters correctly implement blockchain interfaces, particularly for security-critical operations like signature verification.

As the ecosystem grows, we may need a discovery mechanism for custom components, potentially including a registry or marketplace for travelers to find and evaluate extensions.