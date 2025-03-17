# ADR 017: AST and Resource Graph Correspondence

## Status

Proposed

## Context

In our content-addressable execution system, we're implementing two conceptually different but related graphs:

1. **Abstract Syntax Tree (AST)** - The static representation of program structure derived from source code
2. **Resource Relationship Graph** - The dynamic graph of resource allocations and hierarchical relationships that emerges during execution

These two graphs model different aspects of program behavior, yet they interact in ways that affect debugging, optimization, and observability. We need to understand this relationship to build effective developer tooling and ensure efficient resource utilization.

## Problem

The relationship between a program's AST and its resource allocation pattern is complex and not well-defined in our current architecture. This creates several challenges:

1. **Debugging Complexity**: When programs behave unexpectedly, developers must mentally map between syntactic structure and runtime resource behavior with limited tooling support.

2. **Resource Attribution**: It's difficult to attribute resource consumption to specific parts of the program, making optimization challenging.

3. **Execution Visualization**: Our time-travel debugging tools need a coherent way to visualize both program structure and resource flow.

4. **Effect Tracing**: When effects propagate through the system, the path they take through the resource graph may diverge significantly from what's suggested by the AST.

5. **Resource Balancing**: With the formal resource model introduced in ADR_018, we need to track which AST nodes create and consume resources to verify delta calculations.

6. **Controller Tracing**: For cross-timeline resources, we need to track how controller labels evolve through the execution graph.

## Decision

We will explicitly model and track the relationship between AST nodes and resource allocations by implementing a **bidirectional mapping system** with the following components:

1. **AST Node Tagging**: Add unique identifiers to AST nodes during parsing/compilation.

2. **Resource Attribution**: Enhance the `ResourceGrant` type to include a reference to the AST node responsible for the allocation and track formalized resource properties:

```haskell
data ResourceGrant = ResourceGrant
  { -- existing fields
  , sourceAstNodeId :: Maybe AstNodeId
  , sourceLocation :: Maybe SourceLocation
  , resource :: Resource  -- Formalized resource from ADR_018
  , controllerLabel :: Maybe ControllerLabel  -- For cross-timeline resources
  , consumptionDelta :: Delta  -- Track resource delta for this allocation
  }
```

3. **Graph Correlation API**: Implement an API for analyzing the relationship between AST and resource graphs:

```haskell
data GraphCorrelation = GraphCorrelation
  { astToResources :: Map AstNodeId [ResourceGrantId]
  , resourceToAst :: Map ResourceGrantId AstNodeId
  , divergencePoints :: [DivergencePoint]
  , resourceDeltas :: Map AstNodeId Delta  -- Added per ADR_018
  , controllerTransitions :: [ControllerTransition]  -- Added per ADR_018
  }
```

4. **Divergence Analysis**: Create algorithms to identify points where the AST and resource graph structures diverge significantly, highlighting potential areas of interest for optimization or debugging.

5. **Unified Visualization**: Develop a visualization approach that can render both graphs and their relationships, making it easier to understand program behavior holistically.

6. **Resource Delta Tracking**: Add mechanisms to track resource deltas by AST node to ensure conservation laws are maintained.

7. **Controller Transition Tracking**: Monitor how controller labels transform as resources move through the execution graph.

## Implementation Strategy

### 1. AST Node Tagging

During parsing and AST construction, we'll add:

```haskell
data AstNode = AstNode
  { nodeId :: AstNodeId
  , nodeType :: AstNodeType
  , sourceLocation :: SourceLocation
  , children :: [AstNode]
  -- Other node-specific fields
  }
```

This provides a stable identifier we can reference throughout execution.

### 2. Resource Allocation Instrumentation

Modify the resource allocator to capture the source of allocation requests and track formal resource properties:

```haskell
allocate :: ResourceAllocator a => a -> ResourceRequest -> AstContext -> IO (Either AllocationError ResourceGrant)
```

Where `AstContext` provides the necessary AST node information and `ResourceRequest` now includes formal resource definitions:

```haskell
data ResourceRequest = ResourceRequest
  { resourceType :: ResourceType
  , resourceLogic :: Logic  -- From ADR_018
  , fungibilityDomain :: Label  -- From ADR_018
  , quantity :: Quantity  -- From ADR_018
  , metadata :: Value  -- From ADR_018
  , controllerLabel :: Maybe ControllerLabel  -- For cross-timeline resources
  }
```

### 3. Correlation Tracking

Implement dedicated data structures for tracking the relationship:

```haskell
-- Track resource allocations by AST node
recordAllocation :: AstNodeId -> ResourceGrantId -> Resource -> CorrelationTracker -> CorrelationTracker

-- Find all resources allocated by a given AST node (including children)
resourcesForAstNode :: AstNodeId -> CorrelationTracker -> [ResourceGrantId]

-- Find the AST node responsible for a resource allocation
astNodeForResource :: ResourceGrantId -> CorrelationTracker -> Maybe AstNodeId

-- Track resource delta for an AST node
recordDelta :: AstNodeId -> Delta -> CorrelationTracker -> CorrelationTracker

-- Compute total delta for a subtree
subtreeDelta :: AstNodeId -> CorrelationTracker -> Delta
```

### 4. Divergence Analysis

Implement metrics and algorithms to quantify structural differences:

```haskell
-- Types of divergence between AST and resource graph
data DivergenceType
  = LoopUnrolling         -- A loop in AST becomes multiple allocations
  | ConcurrentExecution   -- A single node forks into parallel branches
  | HigherOrderDivergence -- Function passed to another context
  | EffectHandlerJump     -- Effect handler causes non-local execution
  | ResourceResharing     -- Resources reallocated to different AST nodes
  | ControllerTransition  -- Resource moves between controllers (per ADR_018)

-- Point where graphs diverge significantly
data DivergencePoint = DivergencePoint
  { astNode :: AstNodeId
  , resourceNodes :: [ResourceGrantId]
  , divergenceType :: DivergenceType
  , divergenceMagnitude :: Float  -- How different the structures are
  , deltaImbalance :: Maybe Delta  -- Resource imbalance if any (per ADR_018)
  }

-- Controller transition in the resource graph
data ControllerTransition = ControllerTransition
  { resourceId :: ResourceGrantId
  , sourceController :: ControllerID
  , targetController :: ControllerID
  , astNode :: AstNodeId  -- AST node responsible for the transition
  }

-- Find points where the graphs diverge significantly
findDivergencePoints :: CorrelationTracker -> [DivergencePoint]

-- Find controller transitions in the resource graph
findControllerTransitions :: CorrelationTracker -> [ControllerTransition]
```

### 5. Visual Debugging Tools

Extend our time-travel debugger to visualize both graphs:

```haskell
-- Generate a combined visualization
visualizeCorrelation :: ExecutionContext -> GraphCorrelation -> Visualization

-- Highlight active AST nodes based on resource usage
highlightActiveNodes :: ResourceUsage -> GraphCorrelation -> [AstNodeId]

-- Visualize resource flow with controller transitions
visualizeResourceFlow :: ResourceGrantId -> GraphCorrelation -> Visualization

-- Visualize resource delta balance
visualizeDeltaBalance :: AstNodeId -> GraphCorrelation -> Visualization
```

### 6. Resource Delta Validation

Implement validation to ensure resource conservation across AST nodes:

```haskell
-- Validate resource conservation for a subtree
validateSubtreeDeltas :: AstNodeId -> CorrelationTracker -> Either ValidationError ()

-- Validate resource conservation for the entire program
validateProgramDeltas :: Program -> CorrelationTracker -> Either ValidationError ()
```

## Expected Correspondence Patterns

Based on language constructs, we expect certain predictable patterns of correspondence:

1. **Sequential Code**: Direct 1:1 mapping between AST and resource graph.

2. **Conditional Branches**: AST shows all branches, resource graph shows only taken paths.

3. **Loops**: Single AST node, multiple resource nodes (one per iteration).

4. **Higher-Order Functions**: Complex many-to-many relationships as functions move between contexts.

5. **Concurrent Primitives** (`fork`, `race`, `parallel`): AST shows the operation, resource graph shows the actual parallelism.

6. **Effect Handlers**: Resource graph may show non-local jumps not evident in the AST.

7. **Controller Transitions**: Resource graph shows controller label changes as resources cross timelines.

8. **Resource Creation/Consumption**: Resource graph tracks deltas to ensure conservation across operations.

## Examples

### Example 1: Sequential Execution

```haskell
let a = heavyComputation1 input -- AST Node A
    b = heavyComputation2 a     -- AST Node B
in combineResults a b           -- AST Node C
```

Resource graph will typically match AST structure:
```
ResourceGrant(for A) → ResourceGrant(for B) → ResourceGrant(for C)
```

With delta tracking:
```
Node A: delta = 0 (creates and consumes internal resources)
Node B: delta = 0 (creates and consumes internal resources)
Node C: delta = 0 (creates and consumes internal resources)
Total program delta = 0 (conservation verified)
```

### Example 2: Loop Unrolling Divergence

```haskell
forEach items $ \item -> 
    processItem item              -- AST Node D (loop body)
```

AST shows a single loop node, but resource graph shows multiple allocations:
```
ResourceGrant(for D, item1) → ResourceGrant(for D, item2) → ResourceGrant(for D, item3)
```

This creates a 1:N relationship between the AST and resource graph, with delta tracking:
```
Node D (iteration 1): delta = 0
Node D (iteration 2): delta = 0
Node D (iteration 3): delta = 0
Total loop delta = 0 (conservation verified across all iterations)
```

### Example 3: Higher-Order Function Divergence

```haskell
let processor = buildProcessor config   -- AST Node E
    mapper = createMapper rules         -- AST Node F
in runPipeline processor mapper input   -- AST Node G
```

When `runPipeline` executes, it creates a complex resource graph where resources are allocated based on AST nodes from multiple sources:

```
ResourceGrant(for G) → ResourceGrant(for E, inside G) → ResourceGrant(for F, inside E)
```

This creates M:N relationships that are difficult to visualize without explicit tracking.

### Example 4: Cross-Timeline Resource Transfer

```haskell
-- Transfer token from Ethereum to Solana
transferCrossChain token amount -- AST Node H
```

The resource graph would show:
```
ResourceGrant(for H, on Ethereum) → ResourceGrant(for H, on Solana)
```

With controller tracking:
```
ControllerTransition {
  resourceId = "token123",
  sourceController = EthereumController,
  targetController = SolanaController,
  astNode = NodeH
}
```

And delta validation:
```
Node H (on Ethereum): delta = -amount
Node H (on Solana): delta = +amount
Total transfer delta = 0 (conservation verified across timelines)
```

## Benefits

1. **Improved Debugging**: Developers can see which parts of their code are consuming resources.

2. **Better Performance Analysis**: Identify code structures that cause unexpected resource patterns.

3. **Enhanced Observability**: Monitor resource utilization with context about the responsible code.

4. **Optimized JIT**: Target optimization efforts at high-impact AST nodes that consume disproportionate resources.

5. **Effect Transparency**: Make non-local control flow from effects more visible and understandable.

6. **Resource Conservation Verification**: Ensure that resources are conserved across program execution.

7. **Controller Transition Tracking**: Identify and monitor cross-timeline resource transfers.

## Drawbacks

1. **Execution Overhead**: Tracking this correspondence adds some runtime overhead.

2. **Implementation Complexity**: Maintaining bidirectional mapping adds complexity.

3. **Storage Requirements**: Additional metadata increases memory usage.

4. **Visualization Challenges**: Representing two interrelated graphs is non-trivial.

5. **Delta Computation Overhead**: Calculating and validating resource deltas adds computational cost.

## Alternatives Considered

### 1. Implicit Correlation Only

We could rely on execution order alone to correlate AST nodes with resource allocations, without explicit tracking.

**Rejected because**: This approach breaks down with concurrent execution, higher-order functions, and effect handlers. The correlation becomes too tenuous to be useful.

### 2. Enhanced AST Only

We could enhance the AST to include resource estimates and usage patterns directly, merging the concepts.

**Rejected because**: This conflates static and dynamic properties, making the AST less reusable and more complex. It also doesn't handle runtime adaptations well.

### 3. Sampling-Based Approach

We could use statistical sampling to infer correlations without comprehensive tracking.

**Rejected because**: While efficient, this provides incomplete information that may miss critical patterns, especially in non-deterministic executions.

### 4. Separate Resource Tracking

We could keep resource formalization entirely separate from AST tracking.

**Rejected because**: This would miss the opportunity to link resource conservation to program structure, making it harder to identify which parts of the program are causing resource imbalances.

## Implementation Plan

1. **Phase 1**: Implement AST node tagging and basic resource attribution (2 weeks)
2. **Phase 2**: Build correlation tracking infrastructure (2 weeks)
3. **Phase 3**: Develop divergence analysis algorithms (3 weeks)
4. **Phase 4**: Integrate resource formalization and delta tracking (2 weeks) - new from ADR_018
5. **Phase 5**: Implement controller transition tracking (2 weeks) - new from ADR_018
6. **Phase 6**: Create visualization tools for the debugger (3 weeks)
7. **Phase 7**: Optimization and performance tuning (2 weeks)

## Open Questions

1. **Granularity**: At what level of AST granularity should we track correlation? Expression, statement, function?
2. **Persistence**: Should correlation data be persisted as part of execution traces or computed on demand?
3. **Concurrent Execution**: How do we handle attribution when resources are shared across concurrent branches?
4. **Effect System Integration**: What additional metadata is needed to properly track effect handling?
5. **Resource Reuse**: How do we handle cases where resources are recycled and reused by different AST nodes?
6. **Delta Precision**: How precise should delta tracking be for complex operations? Should we use exact or approximate tracking?
7. **Controller Validation**: How do we validate controller transitions when resources cross timelines?
8. **Performance Impact**: What is the performance impact of tracking resource deltas and controller transitions?

## Conclusion

By explicitly modeling the correspondence between AST structure and resource allocation patterns, we can provide developers with deeper insights into program behavior, especially for complex scenarios involving concurrency, higher-order functions, and effects. This approach bridges the gap between static program understanding and dynamic execution behavior, making our content-addressable execution system more transparent and debuggable.

With the integration of resource formalization from ADR_018, we gain additional capabilities to track resource conservation and controller transitions, enhancing our ability to reason about cross-timeline operations and ensure resource integrity throughout the system.
