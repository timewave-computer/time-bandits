# ADR 019: Temporal Effect Language Compiler Architecture

## Status

Proposed

## Context

As Time Bandits matures, we need a formal compiler architecture for the Temporal Effect Language (TEL). Our current approach lacks a clearly defined compiler pipeline that integrates with our content-addressable code system while preserving core properties required for cross-timeline programming:

- Strong typing with effect tracking
- Resource linearity (preventing double-spend)
- Timeline-aware causality validation
- Integration with content-addressable code
- Support for formalized resource models and controller patterns

A consistent and powerful compiler architecture is essential for ensuring programs behave predictably across multiple timelines and can be reasoned about statically before deployment. Furthermore, the formalized resource model outlined in ADR 018 introduces new requirements for the compiler to validate resource properties, conservation laws, and controller label integrity.

## Decision

We will implement a TEL compiler with a multi-stage pipeline that transforms source code into deployable artifacts while enforcing Time Bandits' semantic guarantees. The compiler will integrate deeply with our content-addressable code system to enable incremental compilation, precise dependency management, and reproducible builds.

### Compiler Pipeline Architecture

The compiler will follow this pipeline structure:

```
┌─────────┐   ┌────────────┐   ┌──────────────┐   ┌───────────────┐   ┌───────────────┐
│ Parsing │──▶│ Type Check │──▶│ Effect Check │──▶│ IR Generation │──▶│ Optimization  │
└─────────┘   └────────────┘   └──────────────┘   └───────────────┘   └───────┬───────┘
                                                                              │
                                                                              ▼
                                                                     ┌───────────────┐
                                                                     │   Artifact    │
                                                                     │  Generation   │
                                                                     └───────────────┘
```

Each stage builds on the previous one, gradually transforming raw source into verified and optimized deployment artifacts.

### 1. Parser and AST

The TEL parser will transform source text into a typed abstract syntax tree (AST) that captures domain-specific concerns:

```haskell
-- Core AST Structure
data TELExpression
  = Literal LiteralValue
  | Variable Identifier
  | Apply TELExpression [TELExpression]
  | Lambda [Pattern] TELExpression
  | Let [(Identifier, TELExpression)] TELExpression
  | If TELExpression TELExpression TELExpression
  | Match TELExpression [(Pattern, TELExpression)]
  | TimeExpr TimeExpression
  | EffectExpr EffectExpression
  | ResourceExpr ResourceExpression  -- New for resource formalization
  | ControllerExpr ControllerExpression  -- New for controller operations
  | Do [DoStatement]
  | HashRef ContentHash -- Direct reference by content hash

-- Timeline-specific expressions
data TimeExpression
  = After Duration TELExpression        -- Execute after a time delay
  | Within Duration TELExpression       -- Execute within time bound
  | At TimePoint TELExpression          -- Execute at specific time
  | Race [TELExpression]                -- First-to-complete wins
  | Barrier [TELExpression] Condition   -- Wait for all conditions

-- Effect expressions
data EffectExpression
  = Deposit Asset Amount Timeline
  | Withdraw Asset Amount Timeline
  | Transfer Asset Amount Source Destination
  | Observe FactType Timeline
  | Invoke ProgramID EntryPoint [TELExpression]
  | Watch Condition TELExpression       -- Wait for condition, then execute
  | EndorseState ControllerID StateRoot Proof  -- New for controller endorsement

-- Resource expressions (new for formalized resources)
data ResourceExpression
  = DefineResource ResourceLogic FungibilityDomain Quantity Value
  | NullifyResource ResourceID NullifierKey
  | CreateResource ResourceDefinition ControllerLabel
  | ComputeDelta [ResourceID]
  | ValidateResourceBalance [ResourceID]

-- Controller expressions (new for controller pattern)
data ControllerExpression
  = DefineController ControllerID ControllerType FinalizationRules
  | EndorseController ControllerID StateRoot Proof
  | ValidateControllerChain ControllerLabel
  | ApplyEndorsements [ControllerID] [Endorsement]

-- Pattern matching
data Pattern
  = VarPattern Identifier
  | LiteralPattern LiteralValue
  | ConstructorPattern Identifier [Pattern]
  | WildcardPattern
  | AsPattern Pattern Identifier
  | OrPattern [Pattern]
  | GuardPattern Pattern TELExpression
  | ResourcePattern ResourceID  -- New for resource pattern matching
```

The AST will preserve source location information for error reporting and debugging, with annotations for:
- Type information
- Effect sets (which effects each expression may trigger)
- Resource usage tracking and delta analysis
- Timeline dependencies
- Controller label and ancestry tracking
- Endorsement relationships

### 2. Type System

TEL's type system will combine:

```haskell
data Type
  = TyBase BaseType
  | TyVar TypeVar
  | TyArrow Type Type
  | TyEffect Type
  | TyTimeline Type
  | TyResource ResourceType
  | TyList Type
  | TyTuple [Type]
  | TySchema SchemaType
  | TyController ControllerType        -- New for controller types
  | TyResourceLogic LogicType          -- New for resource logic
  | TyDelta DeltaType                  -- New for resource deltas
  | TyControllerLabel LabelType        -- New for controller labels
  | TyEndorsement EndorsementType      -- New for endorsements
```

The type checker will enforce:
1. Resources are never duplicated or lost (linear types)
2. Effects are used consistently with their declared signatures
3. Timeline-specific constraints are respected
4. Cross-timeline causality is preserved
5. Resource delta calculations sum to zero (conservation laws)
6. Controller labels maintain valid ancestry chains
7. Endorsements are properly validated
8. Dual validation constraints are satisfied

Type inference will use a modified Hindley-Milner algorithm extended with:
- Linear type constraints for resource tracking
- Effect tracking for causality
- Resource delta inference for conservation law checking
- Controller label propagation for ancestral validity
- Temporal constraint analysis for dual validation

### 3. Effect and Resource Validation

The validation phase will analyze both causal relationships between effects and resource conservation properties:

```haskell
data EffectGraph = EffectGraph
  { nodes :: Map EffectNodeID EffectNode
  , edges :: Map EffectNodeID [EffectNodeID]
  , resourceFlow :: ResourceFlowMap
  , controllerMap :: ControllerMap       -- New for controller tracking
  , deltaTracker :: ResourceDeltaTracker -- New for resource delta tracking
  }

-- Enhanced validation that incorporates both temporal and resource aspects
validateEffectGraph :: EffectGraph -> Either ValidationError ValidatedGraph

-- Resource delta validation
validateResourceDeltas :: EffectGraph -> Either DeltaError ValidatedDeltas

-- Controller label validation
validateControllerChains :: EffectGraph -> Either ControllerError ValidatedControllers

-- Dual validation (combining temporal and ancestral)
validateDual :: EffectGraph -> Either ValidationError DualValidationResult
```

This phase will:
- Verify temporal consistency (correct ordering of events)
- Validate resource flows (no duplication or loss)
- Check that preconditions are satisfied
- Identify potential non-determinism
- Generate warnings for race conditions and hazards
- Calculate and verify resource deltas sum to zero
- Validate controller label ancestry chains
- Verify endorsement applications are valid
- Implement dual validation (temporal + ancestral) for cross-timeline operations

### 4. Intermediate Representation (IR)

The compiler will use a specialized IR optimized for effect-based programs with formalized resources:

```haskell
data TELIR = TELIR
  { effectDAG :: EffectDAG               -- Core effect structure
  , resourceMappings :: ResourceMap       -- Resource tracking
  , timeConstraints :: TimeMap            -- Temporal constraints
  , factDependencies :: FactMap           -- External fact dependencies
  , resourceDefinitions :: ResourceDefMap -- Formalized resource definitions
  , controllerLabels :: ControllerLabelMap -- Controller ancestry tracking
  , deltaBalances :: DeltaMap             -- Resource delta calculations
  , endorsements :: EndorsementMap        -- Controller endorsements
  , dualValidation :: DualValidationMap   -- Results of dual validation
  }

data EffectDAG = EffectDAG
  { nodes :: Map EffectID Effect
  , edges :: Map EffectID [EffectID]
  }

data ResourceDefMap = ResourceDefMap
  { resources :: Map ResourceID ResourceDefinition
  , commitments :: Map ResourceID Commitment
  , nullifiers :: Map ResourceID Nullifier
  , kinds :: Map ResourceID Kind
  }

data ControllerLabelMap = ControllerLabelMap
  { labels :: Map ResourceID ControllerLabel
  , reductions :: Map [ControllerID] [ControllerID] -- Possible reductions via endorsements
  }
```

The IR preserves the full effect graph while abstracting syntax details, making it ideal for optimization and code generation. The enhanced IR also captures resource definitions, controller labels, and delta calculations needed for the formalized resource model.

### 5. Optimization

The optimizer will apply several passes:

- **Effect fusion**: Combining compatible effects to reduce overhead
- **Dead effect elimination**: Removing effects with no observable impact
- **Resource localization**: Keeping resources in local scopes when possible
- **Timeline batching**: Grouping operations on the same timeline
- **Fact deduplication**: Consolidating identical fact observations
- **Controller label reduction**: Applying endorsements to simplify controller chains
- **Delta computation fusion**: Combining delta calculations for efficiency
- **Dual validation optimization**: Minimizing redundant checks between temporal and ancestral validation
- **Resource commitment precomputation**: Precomputing resource commitments when possible

All optimizations must preserve causal ordering, effect semantics, resource conservation laws, and controller ancestry integrity. The optimizer must be particularly careful with optimizations that affect resource delta calculations or controller label validity.

### 6. Artifact Generation

The final stage generates deployable program artifacts:

```haskell
data CompiledProgram = CompiledProgram
  { programHash :: ContentHash           -- Content hash of the entire program
  , effectDAG :: EffectDAG               -- The core effect structure
  , dependencies :: Set ContentHash      -- All code/effect dependencies
  , schema :: Schema                     -- Program state schema
  , schemaEvolutionRules :: [EvolutionRule] -- Allowed schema changes
  , compatibleProtocolVersions :: VersionRange -- Compatible protocol versions
  , irDebugInfo :: Maybe IRDebugInfo     -- Optional debug information
  , resourceDefinitions :: ResourceDefSet -- Formalized resource definitions
  , controllerMappings :: ControllerMappingSet -- Controller configurations used
  , deltaVerification :: DeltaVerificationProof -- Proof that resource deltas balance
  , dualValidationEvidence :: DualValidationProof -- Evidence of temporal + ancestral validation
  }
```

The enhanced program artifact includes the formalized resource definitions, controller mappings, and proofs of both delta verification and dual validation. These additions ensure that any program operating with formalized resources can be verified for correctness both at compile time and runtime.

### Content-Addressable Integration

The compiler will leverage our content-addressable code system for:

- **Dependency management**: Precise versioning of imported modules
- **Code deduplication**: Identical functions share storage
- **Incremental compilation**: Reusing already-compiled components
- **Immutable artifacts**: Guaranteeing reproducible builds

Every compiler artifact receives a unique content hash, making compilation deterministic and verifiable.

### Separate Compilation

To support large programs, the compiler will implement separate compilation:

1. Each module compiles separately into a content-addressed artifact
2. Modules reference dependencies by content hash
3. The final program links these components precisely

This enables incremental compilation where only changed modules need recompiling.

### Debugging Support

The compiler will generate debugging information:

- **Source maps**: Mapping between IR elements and source locations
- **Type annotations**: Full type information for variables and expressions
- **Effect flow graphs**: Visualizable representations of effect causality
- **Resource flow traces**: Tracking how resources move through the program

### Compiler CLI Interface

```bash
# Build a TEL program
tel-compiler build swap.tel --output swap.tbp

# Check a program for errors without building
tel-compiler check swap.tel

# Generate a visualization of program flow
tel-compiler viz swap.tel --output swap-flow.svg

# Generate a simulation scenario from a program
tel-compiler gen-scenario swap.tel --output swap-scenario.toml
```

## Consequences

### Positive

- **Strong correctness guarantees**: Static verification prevents many runtime errors
- **Precise dependency management**: Content-addressable artifacts ensure reproducibility
- **Visual development**: Generated flow diagrams improve understanding
- **Incremental compilation**: Separate compilation improves development velocity
- **Deep integration**: Compiler leverages existing Time Bandits architectural features
- **Conservation proofs**: Resource delta validation ensures conservation laws are respected
- **Controller chain validation**: Ancestral validity proofs for cross-timeline resources
- **Dual validation**: Combining temporal and ancestral validation for stronger security
- **Optimized controller chains**: Endorsement-based reductions of controller ancestry

### Challenges

- **Compiler complexity**: Linear types, effect analysis, and resource formalization require sophisticated techniques
- **Performance concerns**: Linear type checking and resource delta calculation may become bottlenecks for large programs
- **Extensibility**: We must design for effect system extensibility from the start
- **Learning curve**: Developers will need to understand new type system concepts and formalized resource model
- **Validation overhead**: Dual validation increases compilation and verification time
- **Controller abstraction complexity**: Reasoning about controller labels and endorsements adds cognitive load
- **Optimized pattern discovery**: Finding opportunities for controller chain reduction requires advanced analysis

### Compiler Design Implications

The compiler architecture has several long-term implications:

- **Language evolution**: The design constrains how TEL can evolve
- **Performance characteristics**: Trade-offs between compilation time and runtime performance
- **Debugging experience**: Output quality shapes developer experience
- **Tooling requirements**: Rich diagnostics and visualizations are essential

## Implementation Plan

We will implement the compiler in phases:

1. **Core pipeline**: AST, basic type checking, and artifact generation
2. **Effect validation**: Causality checking and resource flow analysis
3. **Resource formalization**: Formal resource model implementation and delta validation
4. **Controller integration**: Controller label tracking and ancestry validation
5. **Dual validation**: Combined temporal and ancestral validation
6. **Optimization**: Performance-focused transformations including controller chain reduction
7. **Integration**: Content-addressable code system integration
8. **Tooling**: Visualizations and developer experience improvements

Each phase will include thorough testing and documentation to ensure both correctness and usability.

### Integration with ADR 018

The implementation of this compiler architecture will closely coordinate with the resource formalization work described in ADR 018. Specifically:

1. The AST and type system will be extended to support the formalized resource model
2. Resource delta calculation and validation will be integrated into the effect validation phase
3. Controller label tracking and validation will be implemented as part of the type system
4. Dual validation (temporal + ancestral) will be built into the validator
5. Optimizations for controller chain reduction via endorsements will be added

This ensures that TEL programs can fully leverage the formal guarantees provided by the enhanced resource model while maintaining the familiar temporal effect programming model.