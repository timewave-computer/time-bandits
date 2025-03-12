# ADR 016: Content-Addressable Execution

## Status

**Proposed**

## Context

Time Bandits has implemented a content-addressable code storage system (as detailed in ADR 011), where code is identified by its content hash rather than by name. This approach provides significant benefits including dependency precision, safe refactoring, and immutable definitions.

However, storing code is only half the challenge. We also need mechanisms to:

1. Load and execute code referenced by content hash
2. Manage execution context across invocations
3. Handle dependencies between content-addressed code fragments
4. Provide appropriate sandboxing and security boundaries
5. Ensure deterministic execution for replay and verification

The current implementation has elements of these concerns spread across several components, but lacks a cohesive architectural model for content-addressable execution.

## Decision

We will implement a comprehensive **Content-Addressable Execution** model with the following components:

### 1. Execution Context

```haskell
-- | A context for executing content-addressed code
data ExecutionContext = ExecutionContext
  { contextId :: ContextId
  , contextState :: IORef ContextState
  , parentContext :: Maybe ExecutionContext
  , contextRepository :: CodeRepository
  , contextLogger :: Logger
  }

-- | The internal state of an execution context
data ContextState = ContextState
  { variables :: Map Name Value
  , callStack :: [CallFrame]
  , executionTrace :: [ExecutionEvent]
  }

-- | A single frame in the call stack
data CallFrame = CallFrame
  { frameHash :: CodeHash
  , frameName :: Maybe Name
  , frameArguments :: [Value]
  }
```

### 2. Content-Addressable Executor

```haskell
-- | The main executor interface
data ContentAddressableExecutor = ContentAddressableExecutor
  { executeByHash :: CodeHash -> [Value] -> ExecutionContext -> IO (Either ExecutionError Value)
  , executeByName :: Name -> [Value] -> ExecutionContext -> IO (Either ExecutionError Value)
  , createContext :: Maybe ExecutionContext -> IO ExecutionContext
  , destroyContext :: ExecutionContext -> IO ()
  }
```

### 3. Execution Events and Tracing

```haskell
-- | Events that occur during execution
data ExecutionEvent
  = FunctionInvocation CodeHash [Value]
  | FunctionReturn CodeHash Value
  | EffectApplication Effect
  | ExternalDependencyCall Text [Value] Value
  | ExecutionError Error
```

### 4. Dependency Resolution

```haskell
-- | Interface for resolving dependencies in content-addressed code
data DependencyResolver = DependencyResolver
  { resolveByHash :: CodeHash -> IO (Either ResolutionError CodeDefinition)
  , resolveByName :: Name -> IO (Either ResolutionError CodeDefinition)
  , resolveTransitive :: CodeHash -> IO (Either ResolutionError [CodeDefinition])
  }
```

### 5. Security Sandbox and Resource Management

```haskell
-- | Security boundaries for code execution
data SecuritySandbox = SecuritySandbox
  { allowedEffects :: Set EffectType
  , resourceAllocator :: ResourceAllocator
  , timeoutMillis :: Int
  , allowedExternalCalls :: Set ExternalCallType
  }

-- | Resource allocation type class allowing for pluggable allocation strategies
class ResourceAllocator a where
  allocate :: a -> ResourceRequest -> IO (Either AllocationError ResourceGrant)
  release :: a -> ResourceGrant -> IO ()
  checkUsage :: a -> ResourceGrant -> IO ResourceUsage
  subdivide :: a -> ResourceGrant -> [ResourceRequest] -> IO (Either AllocationError [ResourceGrant])

-- | A resource request specifies the resources needed for execution
data ResourceRequest = ResourceRequest
  { requestMemoryBytes :: Int
  , requestCpuMillis :: Int
  , requestIoOperations :: Int
  , requestEffectCount :: Int
  }

-- | A resource grant represents allocated resources
data ResourceGrant = ResourceGrant
  { grantId :: GrantId
  , grantedMemoryBytes :: Int
  , grantedCpuMillis :: Int
  , grantedIoOperations :: Int
  , grantedEffectCount :: Int
  , grantMetadata :: Map Text Value
  }

-- | Static hierarchical resource allocator (initial implementation)
data StaticAllocator = StaticAllocator
  { availableResources :: TVar Resources
  , allocationHistory :: TVar (Map GrantId ResourceUsage)
  }

instance ResourceAllocator StaticAllocator where
  -- Implementation using the simple hierarchical model
  -- ...
```

## Implementation Strategy

### Execution Flow

1. **Hash-Based Invocation**
   - When a program invokes a function by hash, the executor:
     - Retrieves the code definition from the repository
     - Creates a new call frame
     - Adds the frame to the context's call stack
     - Interprets or executes the code with the provided arguments
     - Records the execution events in the trace
     - Returns the result value (or error)

2. **Name-Based Invocation**
   - When a program invokes a function by name, the executor:
     - Looks up the associated hash from the name registry
     - Proceeds with hash-based invocation as above
     - Records both the name and hash in the call frame for traceability

3. **Dependency Resolution**
   - When code references other definitions (by hash or name):
     - The dependency resolver retrieves the required definitions
     - Dependent code is loaded into the execution context
     - References are resolved at runtime when needed

4. **Effect Handling**
   - When code produces effects:
     - The effect is validated against the security sandbox
     - If allowed, the effect is applied through the effect system
     - The effect application is recorded in the execution trace
     - The result is returned to the executing code

### Key Interfaces

The key interfaces for content-addressable execution are:

```haskell
-- | Create a new executor with the given repository
newExecutor :: CodeRepository -> IO ContentAddressableExecutor

-- | Execute code by its content hash
executeByHash :: ContentAddressableExecutor -> CodeHash -> [Value] -> ExecutionContext -> IO (Either ExecutionError Value)

-- | Execute code by its name
executeByName :: ContentAddressableExecutor -> Name -> [Value] -> ExecutionContext -> IO (Either ExecutionError Value)

-- | Create a new execution context
createContext :: ContentAddressableExecutor -> Maybe ExecutionContext -> IO ExecutionContext

-- | Get the execution trace from a context
getExecutionTrace :: ExecutionContext -> IO [ExecutionEvent]

-- | Create a security sandbox for execution
newSecuritySandbox :: [EffectType] -> ResourceLimits -> IO SecuritySandbox

-- | Execute within a security sandbox
executeWithSandbox :: ContentAddressableExecutor -> SecuritySandbox -> CodeHash -> [Value] -> IO (Either ExecutionError Value)
```

### Integration with Temporal Effect Language (TEL)

The content-addressable execution system will integrate with the Temporal Effect Language (TEL) as follows:

1. TEL expressions will be compiled to content-addressable code fragments
2. Each fragment will be stored in the code repository with its content hash
3. References between fragments will use content hashes
4. The TEL interpreter will use the content-addressable executor for evaluation

## Determinism and Replay

For deterministic execution and replay, we will:

1. Capture all inputs, outputs, and effects in the execution trace
2. Use the trace to replay execution exactly as it occurred
3. Verify that replay produces identical results to the original execution
4. Flag any divergence as a potential determinism violation

To ensure determinism, the execution environment will:

1. Use fixed random seeds for any randomness
2. Control access to external resources and time functions
3. Serialize access to shared resources
4. Handle floating-point operations consistently across platforms

## Security Considerations

The content-addressable execution system must protect against:

1. **Infinite loops or resource exhaustion** - via resource limits and timeouts
2. **Unauthorized effect application** - via the security sandbox
3. **Information leakage between execution contexts** - via strict context isolation
4. **Code injection attacks** - via validation of loaded code
5. **Determinism violations** - via controlled access to non-deterministic operations

## Architectural Decisions

1. **Execution Strategy**: We will implement a JIT (Just-In-Time) hybrid approach. Code will initially be interpreted for flexibility during development, but frequently executed paths will be compiled to an optimized intermediate representation for performance in production.

2. **Dependency Management**: Dependencies will be managed through an immutable directed graph with explicit hash pinning. Each function will reference the exact content hashes of its dependencies, and the resolver will build a flattened dependency tree at resolution time to avoid diamond dependency problems.

3. **Persistence Model**: We will implement a snapshot-based persistence model inspired by CRIU (Checkpoint/Restore In Userspace). Execution contexts will be serialized at effect boundaries, capturing any in-flight state in the snapshot.

4. **Upgradeability**: Both code formats and executor versions will be separately versioned with a compatibility matrix. New executor versions will support all previous code formats, while new code formats may require executor upgrades.

5. **Cross-Platform Determinism**: All execution will be performed in a RISC-V ZK co-processor, providing hardware-enforced determinism across platforms. This eliminates concerns about floating-point variations, memory ordering, and other sources of non-determinism.

6. **Optimization**: We will leverage content-addressing for aggressive memoization. Since functions and their inputs are content-addressed, we can use hashes as cache keys to retrieve previously computed results for identical inputs.

7. **Debug Tooling**: We will implement time-travel debugging based on execution traces. The immutable nature of content-addressed execution allows stepping forward and backward through program history, similar to Git's version control model but for program state.

8. **Resource Governance**: We will implement a capabilities-based hierarchical resource model using a flexible type class design. This allows different resource allocation strategies to be plugged in while maintaining the core invariant that a context cannot use more resources than it has been allocated by its parent.

## Benefits

This content-addressable execution model provides several key benefits:

1. **Immutability and Verification**: Code behavior is directly tied to its content hash, making verification straightforward.

2. **Safe Evolution**: New versions of functions can be deployed without breaking existing references.

3. **Dependency Precision**: Dependencies are resolved exactly as specified, eliminating "works on my machine" problems.

4. **Auditability**: Every execution can be traced and replayed based on its execution trace.

5. **Distributed Execution**: Code can be executed on any node that has access to the content-addressed repository.

6. **Security Boundaries**: The sandbox model provides clear security boundaries and resource limits.

## Consequences

Implementing this content-addressable execution model will have these consequences:

1. **Increased Complexity**: The execution system becomes more complex, requiring careful design and testing.

2. **Performance Overhead**: Content-addressed execution may introduce some performance overhead compared to direct function calls.

3. **Tooling Requirements**: Developers will need specialized tools to work effectively with content-addressed code execution.

4. **Learning Curve**: Teams will need to understand the content-addressed execution model and its implications.

5. **Deployment Changes**: Deployment processes will need to account for content-addressed code storage and execution.

## Implementation Plan

1. **Phase 1**: Implement the core executor and context management
   - Develop the base ExecutionContext structure
   - Implement the ResourceAllocator type class with a StaticAllocator implementation
   - Create the ContentAddressableExecutor interface

2. **Phase 2**: Implement dependency resolution and security sandbox
   - Build the immutable directed graph for dependency resolution
   - Implement the security sandbox with the capability model
   - Integrate with the RISC-V ZK co-processor for deterministic execution

3. **Phase 3**: Integrate with the Temporal Effect Language
   - Connect TEL expressions to content-addressed code fragments
   - Implement the TEL interpreter using the content-addressable executor
   - Develop the JIT compilation strategy for hot code paths

4. **Phase 4**: Implement tracing, replay, and verification
   - Create the execution event tracing system
   - Implement snapshot-based persistence at effect boundaries
   - Develop replay and verification tools

5. **Phase 5**: Develop tooling and documentation
   - Build time-travel debugging tools
   - Implement memoization and optimization infrastructure
   - Create developer documentation and examples

## Appendix: Current Implementation Analysis and Extensions

The current implementation has several components related to content-addressable code and execution:

1. **CodeAddress**: Core content-addressable storage mechanisms that handle the hashing and retrieval of code definitions.
2. **CodeRepository**: Storage and retrieval of code definitions with basic versioning support.
3. **NameRegistry**: Association of human-readable names with content hashes, enabling refactoring without breaking references.
4. **ExecutionContext**: Basic context for execution, but with limited support for nested contexts and resource tracking.
5. **Effect System**: Handling of effects, but not fully integrated with the content-addressed execution model.

Our proposed extensions build upon this foundation:

1. **ResourceAllocator Type Class**: Replacing the current fixed resource limits with a flexible type class approach that can evolve from static allocation to more dynamic strategies as the system matures.

2. **Hierarchical Context Management**: Extending the ExecutionContext to support explicit parent-child relationships with proper resource subdivision and isolation.

3. **JIT Compilation Strategy**: Adding support for optimizing frequently executed code paths while preserving the content-addressed nature of the code.

4. **RISC-V ZK Co-processor Integration**: Leveraging hardware-enforced determinism for cross-platform consistency, rather than trying to solve this purely in software.

5. **Snapshot-based Persistence**: Implementing checkpoint/restore capabilities at effect boundaries to support long-running executions and recovery.

6. **Time-Travel Debugging**: Building on the immutable nature of content-addressed code to enable bidirectional stepping through execution history.

These extensions maintain the core design philosophy of the existing implementation while addressing the areas that need further development:

1. **Comprehensive Execution Model**: A complete model that spans interpretation, compilation, and hardware acceleration.
2. **Context Management**: Sophisticated hierarchical context management with resource isolation.
3. **Dependency Resolution**: Robust immutable directed graph approach for dependencies.
4. **Security Boundaries**: Clear security boundaries through the capability model and resource allocation.
5. **Tracing and Replay**: First-class support for execution tracing and deterministic replay.
