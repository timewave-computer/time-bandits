# ADR 016: Program Observability System

## Status

Proposed

## Context

Time Bandits programs operate across multiple timelines with complex causal relationships, resource flows, and state transitions. Understanding program behavior in such a distributed environment presents significant challenges:

- It's difficult to visualize causal relationships spanning multiple chains
- Debugging cross-timeline issues requires correlated views of distributed state
- Developers need insights into resource flows and program execution paths
- Timeline-specific issues require visibility into protocol-level details
- Compilation errors and deployment status need clear, actionable feedback

Our current approach lacks a comprehensive observability system that provides visibility across the full lifecycle from compilation through simulation to production execution.

## Decision

We will implement a dedicated observability system that provides materialized views of compilation, simulation, deployment, and runtime execution. This system will serve as a core pillar of the Time Bandits architecture, complementing the compiler, deployment, and simulation components.

### System Architecture

The observability system will have these core components:

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  Event Sources  │───▶│ Event Collectors │───▶│ Event Storage   │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                                        │
                                                        ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  Visualizers    │◀───│ Query Engine    │◀───│ Materializers   │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

The system leverages our existing content-addressable, append-only logs while adding:
1. Specialized collectors to extract and correlate events
2. Materializers that transform raw events into coherent views
3. A query engine for retrieving and filtering observability data
4. Visualization components for rendering different aspects of program behavior

### Event Sources

The observability system will collect events from multiple sources:

#### 1. Compilation Events

The compiler will emit structured events during compilation, capturing:
- Type errors and warnings with source context
- Effect validation results with causality checks
- Resource usage analysis including flow paths
- Dependency resolution and version compatibility issues

```haskell
data CompilationEvent
  = TypeErrorEvent SourceSpan TypeError
  | EffectValidationEvent EffectID ValidationResult
  | ResourceFlowEvent ResourceID FlowPath
  | DependencyEvent DependencyID Resolution
```

#### 2. Deployment Events

The deployment system will emit events tracking:
- Deployment initiation and progress
- Timeline-specific deployment status
- Activation progress and coordination events
- Cross-timeline synchronization points
- Version compatibility checks

```haskell
data DeploymentEvent
  = DeployInitiatedEvent DeploymentID ProgramID
  | TimelineDeploymentEvent DeploymentID TimelineID DeployStatus
  | ActivationEvent DeploymentID ActivationStrategy ActivationStatus
  | SynchronizationEvent DeploymentID [TimelineID] SyncStatus
```

#### 3. Runtime Events

The runtime will generate events capturing program execution:
- Effect application and results
- Resource transfers and transformations
- Timeline observations and fact consumption
- Cross-program invocations
- Schema transformations

```haskell
data RuntimeEvent
  = EffectApplicationEvent EffectID Result
  | ResourceTransformEvent ResourceID TransformationType
  | FactObservationEvent FactID Timeline
  | InvocationEvent ProgramID EntryPoint Arguments
  | SchemaTransformationEvent OldSchema NewSchema
```

#### 4. Simulation Events

The simulation system will emit events specific to simulation contexts:
- Actor startup and shutdown
- Injected faults and delays
- Timeline state manipulations
- Synthetic facts and mocked responses
- Scenario progress markers

```haskell
data SimulationEvent
  = ActorLifecycleEvent ActorID LifecycleStatus
  | FaultInjectionEvent FaultType Target
  | TimelineManipulationEvent TimelineID ManipulationType
  | SyntheticFactEvent FactID FactType
  | ScenarioProgressEvent ScenarioID Milestone
```

### Event Collectors

Event collectors will extract and correlate events from multiple sources:
- **Log Collectors**: Process unified logs and factlogs
- **Compiler Collectors**: Extract diagnostics from compiler outputs
- **Simulation Collectors**: Gather events from simulation runs
- **Deployment Collectors**: Track deployment status across timelines

Each collector will normalize events into a consistent format with:
- Unique event ID
- Timestamp (wall clock + logical/Lamport)
- Source identifier
- Event type
- Event payload
- Causal parent references
- Contextual metadata

### Event Storage

Events will be stored in:
1. **Append-only logs**: Content-addressed, immutable event history
2. **Materialized views**: Derived representations optimized for specific queries
3. **Indexed repositories**: Searchable stores with relationship metadata

All storage components will maintain causal consistency, allowing for accurate representation of happens-before relationships across different timelines and execution contexts.

### Materializers

Materializers will transform raw events into meaningful views:

#### 1. Causal Graphs

Materializers will construct causal graphs showing:
- Effect dependencies and execution order
- Cross-program invocation chains
- Timeline observation dependencies
- Resource flow paths

These graphs will be rendered as directed acyclic graphs (DAGs) with:
- Nodes representing effects, facts, and operations
- Edges representing causal dependencies
- Timeline boundaries clearly marked
- Resource flows visualized along edges

#### 2. State Transition Views

State transition views will show how program state evolves:
- Schema transformations during upgrades
- Resource balance changes
- Effect application results
- Observed fact integration

#### 3. Timeline-Specific Views

Timeline-specific views will highlight:
- Block-by-block progress on each chain
- Confirmation status of transactions
- Fork detection and resolution
- Timeline-specific error conditions
- Protocol-level details for debugging

#### 4. Resource Flow Maps

Resource flow maps will visualize:
- Asset movements between actors
- Token flow across timelines
- Balance changes over time
- Resource creation and destruction points

```haskell
data MaterializedView
  = CausalGraph GraphRepresentation
  | StateTransitionView ProgramID StateSequence
  | TimelineView TimelineID BlockSequence
  | ResourceFlowMap [ResourceID] FlowRepresentation
```

### Query Engine

The query engine will provide SQL-like capabilities for observability data:

```sql
-- Example queries

-- Find all events related to a specific effect
SELECT * FROM events WHERE effect_id = 'bafy123...' ORDER BY timestamp;

-- Find resource flow path from source to destination
SELECT * FROM resource_flows 
WHERE source_id = 'account1' AND destination_id = 'account2';

-- Find cross-timeline causality chains
SELECT * FROM causal_chains 
WHERE spans_timelines('ethereum', 'solana') 
ORDER BY start_time;

-- Find all errors during deployment
SELECT * FROM events 
WHERE deployment_id = 'dep-123' AND event_type = 'error' 
ORDER BY timestamp;
```

The engine will support:
- Complex filters and projections
- Aggregations across event streams
- Temporal queries with time windows
- Causal relationship traversal
- Cross-timeline correlations

### Visualization Layer

The observability system will include multiple visualization components:

#### 1. Compiler Diagnostic Visualizers

- Type error explainers with source context
- Resource flow diagrams showing linearity violations
- Effect causality graphs highlighting ordering issues
- Dependency graphs showing version constraints

#### 2. Runtime Flow Visualizers

- Live program execution traces
- Timelines showing effect application sequence
- Resource movement animations
- Cross-timeline causality maps
- State transition diagrams

#### 3. Deployment Status Dashboards

- Cross-timeline deployment progress
- Activation status and synchronization points
- Version compatibility matrices
- Timeline-specific deployment details

#### 4. Simulation Visualizers

- Actor interaction diagrams
- Injected fault impacts
- Timeline manipulations
- Scenario progression visualization

All visualizations will be available through:
- CLI-based ASCII representations
- Web dashboards with interactive elements
- Exportable SVG/PNG formats for documentation

### Cross-Phase Integration

The observability system will integrate information across different phases of the program lifecycle:

- **Compilation ↔ Simulation**: Link compiler warnings to runtime issues
- **Simulation ↔ Deployment**: Compare simulated vs. actual behavior
- **Deployment ↔ Runtime**: Track how deployment choices affect execution
- **Runtime ↔ Compilation**: Identify patterns that could be caught statically

This cross-phase integration provides a comprehensive view of program behavior from source code to execution.

### CLI Interface

The observability system will provide a CLI interface:

```bash
# Get real-time visualization of program execution
tb-observe trace program-123

# Show resource flow for a specific asset
tb-observe resource-flow USDC --program program-123

# Compare simulation vs. production behavior
tb-observe compare --sim sim-456 --prod program-123

# Generate a causal graph for a deployment
tb-observe causal-graph dep-789 --output graph.svg

# Generate a full execution report
tb-observe report program-123 --format markdown --output report.md
```

### Integration with Content-Addressable System

The observability system will leverage our content-addressable storage:
- Each event is content-addressed for immutability and verifiability
- Materializers produce content-addressed views
- Visualization outputs are content-addressed for reproducibility
- Queries reference content-addressed data via hash

This ensures that observability data maintains the same trust and verification properties as the core system.

## Consequences

### Positive

- **Comprehensive Visibility**: Unified view across the entire program lifecycle
- **Improved Debugging**: Rich context for identifying and resolving issues
- **Cross-Timeline Clarity**: Clear visualization of complex causal relationships
- **Resource Transparency**: Explicit tracking of all resource movements
- **Developer Experience**: Faster feedback cycles and clearer error resolution
- **Auditability**: Improved validation of correctness properties

### Challenges

- **Storage Overhead**: Comprehensive observability generates substantial data
- **Performance Implications**: Collection may impact runtime performance
- **Complexity Management**: Users may be overwhelmed by information
- **Privacy Considerations**: Some programs may not want all details visible
- **Scaling Visualization**: Representing complex distributed programs is difficult

### Visualization Challenges

The observability system faces several visualization challenges:

- **Causal Complexity**: Representing happens-before relationships across many timelines
- **Resource Flow Clarity**: Showing resource movement without overwhelming detail
- **Time Representation**: Balancing wall clock, logical, and block-based time
- **Error Context**: Providing sufficient context for effective debugging
- **Scale Management**: Handling very large programs with many effects

These challenges require careful UI/UX design and progressive disclosure of information.

## Implementation Plan

We will implement the observability system in phases:

1. **Foundation**: Event collection and storage infrastructure
2. **Compiler Integration**: Link compiler diagnostics to observability system
3. **Runtime Tracing**: Implement runtime event collection
4. **Basic Visualizations**: Core visualizers for common patterns
5. **Query Engine**: Advanced querying capabilities
6. **Cross-Phase Integration**: Link information across compilation/simulation/runtime
7. **Advanced Visualizations**: Complex visualizations for specialized use cases

Each phase will focus on delivering immediate developer value while building toward the complete system.

## Additional Considerations

### Privacy and Control

Programs may have different observability requirements. The system will support:
- **Selective Disclosure**: Control over which events are published
- **Permissioned Access**: Fine-grained control over who can access observability data
- **Local-Only Mode**: Option to keep observability data entirely local

### Integration with Time Map

The observability system will integrate deeply with the Map of Time, providing:
- Timeline-specific context for each observation
- Causal ordering information across timelines
- Fact verification for observed external state

### Artifact Relationship Tracking

The system will track relationships between artifacts:
- Compiled programs linked to their source code
- Deployed programs linked to their compiled artifacts
- Runtime instances linked to their deployed versions

This enables tracing issues from runtime back to source code and vice versa.