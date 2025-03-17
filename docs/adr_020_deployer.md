# ADR 020: Program Deployment and Activation System

## Status

Proposed

## Context

Time Bandits programs operate across multiple independent timelines, requiring a robust system for deploying compiled programs, activating them safely, and extending the system's capabilities. Currently, we lack a formalized deployment model that maintains our core invariants:

- Traveler sovereignty (program owners retain full control)
- Programs talk to programs (not directly to actors)
- Account programs as gateways for actor-initiated actions
- Content-addressed, causally-linked execution history

We need a deployment system that preserves these principles while providing a consistent experience across simulation and production environments.

## Decision

We will implement a comprehensive deployment system that routes all deployment actions through account programs, supports flexible activation strategies, and enables safe extension through custom effects and timeline adapters.

### Account Programs as Deployment Gateways

All deployments will flow through account programs, preserving our "programs talk to programs" invariant:

```haskell
data AccountProgramEffect
  = Deposit Asset Amount
  | Withdraw Asset Amount
  | DeployProgram CompiledProgram DeploymentConfig
  | DeployEffect CustomEffect
  | DeployTimelineAdapter TimelineAdapter
  | ActivateProgram ProgramID ActivationStrategy
```

This maintains our security model where actions flow through account programs as proxies for traveler identity. Programs can also deploy other programs through the same mechanism, creating a uniform deployment model regardless of initiator.

### Deployment Process

The deployment process will follow these steps:

1. Traveler compiles a program locally
2. Traveler submits a `DeployProgram` effect to their account program
3. Account program validates the deployment request
4. Account program forwards the deployment to Bandits via the effect system
5. Bandits distribute program to relevant timelines
6. Time Keepers observe and verify successful deployment
7. Traveler or program initiates activation

This ensures that deployments maintain the same security and auditability guarantees as other effects in our system.

### Activation Strategies

Programs may specify different activation strategies depending on their requirements:

```haskell
data ActivationStrategy
  = ManualStepwise                -- Activate each component separately after verification
  | FloodActivation               -- Last component triggers parallel activation messages
  | ChainedActivation OrderSpec   -- Activation proceeds in specified order
  | ConditionBased [Condition]    -- Activate when specific conditions are met
```

This flexibility supports different deployment patterns, from conservative manual verification to fully automated activation.

Activation is conceptually similar to "linking" in traditional compilation. While deployment places program components on their respective timelines, activation establishes connections between these components and makes the program operational as a cohesive unit.

### Custom Effects

The system will support custom effects that extend capabilities:

```haskell
data CustomEffect = CustomEffect
  { effectName :: Text
  , effectVersion :: Version
  , effectSignature :: EffectType
  , effectParameters :: [Parameter]
  , effectReturnType :: Type
  , effectValidator :: ContentHash  -- Content-addressed validation function
  , effectHandler :: ContentHash    -- Content-addressed handler implementation
  , compatibilityRules :: CompatibilityRules
  }
```

Custom effects will be deployed through account programs and registered in a global effect registry. Custom effects must declare compatibility with protocol versions:

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

This ensures that custom effects can evolve alongside the protocol without breaking existing programs.

### Timeline Adapters

Timeline adapters will enable interaction with new blockchains:

```haskell
data TimelineAdapter = TimelineAdapter
  { timelineID :: TimelineID
  , timelineSchema :: TimelineSchema
  , adapterCode :: ContentHash
  , proofValidators :: Map ProofType ContentHash
  , rpcBindings :: Map RPCMethod ContentHash
  }
```

Timeline adapters translate between the Time Bandits effect system and blockchain-specific APIs. Since they validate proofs and handle signatures, they're security-critical components.

To maintain security and composability, the ability to deploy timeline adapters that receive mainstream recognition will initially be controlled through a whitelist. However, this doesn't prevent travelers from deploying their own custom adapters for their specific programs.

### Version Compatibility Management

For protocol version compatibility, we'll consider several dimensions:

1. **Protocol Version Compatibility Declaration**: Each effect declares which protocol versions it's compatible with.

2. **Graceful Degradation**: When running on an older protocol version, effects can specify fallback behaviors:

```toml
[effect.fallbacks]
protocol = "2.x"
strategy = "use-alternative-implementation"
alternative_handler = "bafy456..."
```

3. **Compatibility Verification**: During compilation, the system verifies that all used effects are compatible with the targeted protocol version.

The content-addressable nature of effect handlers ensures that Bandits can always access the exact handler implementation a program was compiled against, even if newer versions exist.

### Deployment Status and Observation

Deployment status will be tracked through the Time Keeper observation system. Time Keepers sign observations of successful deployments, creating verifiable facts:

```haskell
data DeploymentObservation = DeploymentObservation
  { deploymentID :: DeploymentID
  , timeline :: TimelineID
  , status :: DeploymentStatus
  , observedAt :: Timestamp
  , signature :: TimeKeeperSignature
  }
```

These observations form part of the Map of Time, enabling programs to reason about deployment status and coordinate cross-timeline activations.

### Integration with Simulation System

The deployment system will integrate with our existing simulation infrastructure:

```toml
[scenario]
name = "CrossChainSwap_Test"
mode = "LocalProcesses"

[[programs]]
id = "swap_program"
compiled_path = "swap.tbp"
activation = "flood"

[[actors]]
id = "traveler1"
type = "Traveler"
```

Programs verified in simulation can be deployed to production with the same configuration:

```bash
# First simulate
tb-sim run swap_scenario.toml

# Then deploy to production
tb-deploy swap.tbp --config swap_config.toml --env production
```

### Deployment CLI Interface

The deployment system will expose a CLI interface:

```bash
# Deploy a compiled program
tb-deploy deploy swap.tbp --account alice --config deploy-config.toml

# Check deployment status
tb-deploy status dep-123456

# Activate a deployed program
tb-deploy activate dep-123456 --strategy flood

# Deploy a custom effect
tb-deploy effect custom-oracle.tef --account alice
```

### Transient Deployment Timelines

A pattern we anticipate is that travelers may spin up temporary timelines as transient gateways for coordinating program deployment. These timelines serve as both a convenience and transaction cost optimization, especially for complex multi-timeline programs. They can be decommissioned after program activation, as they've served their purpose as deployment coordinators.

This pattern is particularly valuable when deploying programs that span multiple high-cost timelines, as it centralizes deployment orchestration on a low-cost timeline that can be discarded after use.

## Consequences

### Positive

- **Uniform Deployment Model**: Same mechanism for all deployments maintains conceptual simplicity
- **Extensibility**: Travelers can extend the system with new effects and timeline adapters
- **Sovereignty Preservation**: All deployments remain under traveler control
- **Upgrade Safety**: Version compatibility declarations prevent breaking changes
- **Composability**: Content-addressed components enable safe composition and reuse
- **Simulation Integration**: Seamless path from simulation to production deployment

### Challenges

- **Partial Deployment Handling**: Requires robust recovery mechanisms for partial failures
- **Effect Compatibility Matrix**: Managing compatibility between protocol versions and custom effects grows in complexity
- **Timeline Adapter Security**: Must carefully validate timeline adapters for security implications
- **Cross-Timeline Coordination**: The deployment system must respect causal relationships between timelines

### Deployment Design Implications

- **Consistency Models**: Different timeline consistency models must be accommodated
- **Activation Safety**: Activation strategies must handle partial failures gracefully
- **Cross-Timeline Coordination**: The deployment system must respect causal dependencies

## Implementation Plan

We will implement the deployment system in phases:

1. **Core Deployment Pipeline**: Deploy programs through account programs
2. **Activation Strategies**: Support for different activation patterns
3. **Custom Effects**: Extension mechanism for effect system
4. **Timeline Adapters**: Extension mechanism for blockchain support

Each phase will include integration with the simulation system to ensure a consistent experience across development and production environments.

## Additional Considerations

### Effect Discovery via the Map of Time

Custom effect discovery will leverage the Map of Time as the foundational discovery mechanism, allowing travelers to find and evaluate extensions based on their causal relationships and usage patterns.

### Social Reputation for Custom Components

The initial discovery mechanism for custom components will be social reputation. A core set of effects will be included in the mainline distribution, with trusted parties curating their own lists of trusted effects and handlers.

### Relationship to Schema Evolution

This system builds on our existing schema evolution mechanisms (ADR-010), extending them to handle the evolution of effects and timeline adapters. The same principles of safe, controlled evolution apply, with explicit rules about what changes are permitted.

### Program Upgrade Coordination

The system will track program upgrade epoch boundaries, providing insights into when and how programs transition between versions. This is crucial for coordinating upgrades across multiple timelines with different finality guarantees.