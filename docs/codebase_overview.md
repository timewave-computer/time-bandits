# Time Bandits: Codebase Overview

This document provides a high-level overview of the Time Bandits codebase structure, the key modules, and their dependencies. Understanding these relationships is essential for navigating the codebase and contributing effectively.

## Key Components

Time Bandits includes the following key components:

1. **Core Effect System**: The foundation for representing and managing temporal effects.
2. **Timeline Management**: Handling divergent timelines and their relationships.
3. **Resource Accounting**: Managing resources across timelines.
4. **Temporal Effect Language (TEL)**: The language for declaring cross-timeline workflows, unifying and replacing the earlier TECL implementation.
5. **Schema Evolution**: Supporting schema changes across timeline evolutions.
6. **Program Model**: The programming model for creating time-aware applications.

## Architecture

Time Bandits is designed with a modular architecture that separates concerns and minimizes coupling between subsystems:

```
+-----------+     +------------+     +------------+
|           |     |            |     |            |
|   Core    |<----+  Programs  |<----+  Execution |
|           |     |            |     |            |
+-----------+     +------------+     +------------+
      ^                 ^                  ^
      |                 |                  |
      v                 v                  v
+-----------+     +------------+     +------------+
|           |     |            |     |            |
|  Adapters |<--->| Simulation |<--->|   Actors   |
|           |     |            |     |            |
+-----------+     +------------+     +------------+
                        ^
                        |
                        v
                  +------------+
                  |            |
                  |    Proofs  |
                  |            |
                  +------------+
                        ^
                        |
                        v
                  +------------+
                  |            |
                  |    CLI     |
                  |            |
                  +------------+
```

## Source Code Structure

The Time Bandits codebase is organized into the following main directories:

```
time-bandits/
├── src/                  Main source code
│   ├── TimeBandits.hs    Main entry point
│   ├── Actors/           Actor system implementation
│   ├── Adapters/         Timeline and protocol adapters
│   ├── CLI/              Command line interface
│   ├── Core/             Core system components
│   │   ├── Common/       Shared utilities and types
│   │   ├── Effect/       Effect system implementation
│   │   ├── TEL/          Temporal Effect Language
│   │   ├── Timeline/     Timeline management
│   │   └── Resource/     Resource handling system
│   ├── Execution/        Execution engine
│   └── Types/            Common type definitions
├── test/                 Test suite
├── docs/                 Documentation
└── scripts/              Utility scripts
```

## Module Dependencies

### Core Module

The `Core` module contains fundamental components that form the backbone of the Time Bandits system, including:

- **Common**: Shared utilities, types, and functions used throughout the codebase.
- **Effect**: The effect system implementation that defines and manages temporal effects.
- **TEL**: The Temporal Effect Language implementation, including parser, interpreter, and type checker. This module unifies and replaces the earlier TECL (Temporal Effect Combinator Language) implementation.
- **Timeline**: Timeline management components for tracking and manipulating timelines.
- **Resource**: Resource handling system for managing assets across timelines.

### Content-Addressable Code System

The Content-Addressable Code system is a key architectural component that enables code immutability, eliminates dependency conflicts, and simplifies refactoring:

```
Core/
├── CodeAddress.hs       # Core content-addressable storage system
├── CodeAddressUtil.hs   # Utilities for working with content-addressable code
└── ...

Execution/
├── ContentAddressableExecutor.hs  # Execution engine for content-addressable code
└── ...
```

This system provides:

- **Hash-Based Identification**: Code is uniquely identified by its content hash
- **Immutable Code Storage**: Modifications create new versions with unique hashes
- **Decoupled Naming**: Names are metadata pointing to content hashes
- **Dependency Resolution**: Different versions of the same code can coexist

This design enables several powerful features:
- Dependencies are resolved with perfect precision
- Code can be reliably shared across different contexts
- Refactoring is safer and more straightforward
- Exact code versions are preserved indefinitely

### Temporal Effect Language (TEL)

The Temporal Effect Language (TEL) implementation is a core component of the Time Bandits system, providing a specialized language for cross-timeline programming. TEL unifies and replaces the earlier TECL (Temporal Effect Combinator Language) implementation, combining its strengths while providing a more comprehensive feature set.

```
Core/
├── TEL/
│   ├── AST.hs              # Abstract Syntax Tree definitions
│   ├── Parser.hs           # Parser for TEL code
│   ├── TypeChecker.hs      # Type checking and inference
│   ├── Interpreter.hs      # Expression evaluation and effect handling
│   ├── PrettyPrinter.hs    # Code formatting and display
│   ├── REPL.hs             # Interactive Read-Eval-Print Loop
│   ├── ContentAddressable.hs # Integration with content-addressable code
│   └── ...
├── TEL.hs                  # Main entry point for TEL functionality
└── ...
```

The TEL implementation includes:

- **Parser**: Converts TEL source code into an Abstract Syntax Tree (AST)
- **Type Checker**: Ensures type safety and correctness before execution
- **Interpreter**: Evaluates expressions and manages effects
- **Pretty Printer**: Formats TEL code for display and debugging
- **REPL**: Interactive environment for testing and experimenting with TEL
- **Content-Addressable Integration**: Connects TEL with the content-addressable code system

TEL is designed to be:
- **Expression-oriented**: Everything evaluates to a value
- **Strongly typed**: Type errors are caught at compile time
- **Effect-aware**: Effects are explicitly declared and managed
- **Pattern-matching focused**: Comprehensive support for pattern matching
- **Content-addressable**: All code is identified by its content hash

The TEL implementation is tightly integrated with other Time Bandits components, particularly the effect system, resource management, and timeline operations.

### Programs Layer

The Programs layer implements the programming model:

```
Programs/
├── Program.hs           # Program definition and state
├── ProgramDefinition.hs # Program structure and interfaces
├── ProgramState.hs      # Program state management
├── ProgramEffect.hs     # Program-specific effects
├── ProgramUpgrade.hs    # Program upgrade mechanisms
├── AccountProgram.hs    # Account program implementation
├── Scenario.hs          # Scenario definition and execution
└── Types.hs             # Program-specific types
```

**Dependencies**: Core

### Execution Layer

The Execution layer handles runtime execution:

```
Execution/
├── EffectInterpreter.hs       # Effect interpreter
├── EffectExecutor.hs          # Effect execution engine
├── EffectLogger.hs            # Effect logging
├── EffectAdapterGenerator.hs  # Adapter generation for effects
├── PreconditionEvaluator.hs   # Precondition evaluation
├── LogStore.hs                # Execution logging
├── ResourceLedger.hs          # Resource accounting
├── ExecutionLog.hs            # Execution log management
├── Events.hs                  # Event handling
├── LocalMultiProcess.hs       # Local execution process management
└── DistributedLog.hs          # Distributed logging system
```

**Dependencies**: Core, Programs

### Adapters Layer

The Adapters layer interfaces with external systems:

```
Adapters/
├── Network.hs           # Network communication primitives
├── TimelineAdapter.hs   # Timeline management adapter
├── NetworkQUIC.hs       # QUIC protocol implementation
├── NetworkAdapter.hs    # Network adapter interface
├── MockAdapter.hs       # Mock adapter for testing
├── EthereumAdapter.hs   # Ethereum blockchain adapter
└── CelestiaAdapter.hs   # Celestia blockchain adapter
```

**Dependencies**: Core

### Actors Layer

The Actors layer implements the actor model:

```
Actors/
├── Actor.hs               # Base actor functionality
├── ActorTypes.hs          # Actor type definitions
├── ActorCoordination.hs   # Coordination between actors
├── ActorCommunication.hs  # Communication protocol
├── TransitionMessage.hs   # Message transition model
├── TimeTraveler.hs        # Timeline manipulation actor
├── TimeKeeper.hs          # Timeline consistency actor
└── TimeBandit.hs          # Security testing actor
```

**Dependencies**: Core, Programs, Execution

### Simulation Layer

The Simulation layer provides the simulation framework:

```
Simulation/
├── Controller.hs       # Simulation control
├── Scenario.hs         # Scenario definition
├── ScenarioLoader.hs   # Scenario loading and initialization
├── Replay.hs           # Replay functionality
├── Observer.hs         # Event observation
├── Messaging.hs        # Actor-to-actor messaging
└── Scenarios.hs        # Predefined scenarios
```

**Dependencies**: Core, Programs, Execution, Actors

### Proofs Layer

The Proofs layer handles cryptographic proofs:

```
Proofs/
├── TimelineProof.hs     # Timeline state proofs
├── ZKProof.hs           # Zero-knowledge proofs
└── SecurityVerifier.hs  # Security property verification
```

**Dependencies**: Core, Programs, Simulation

### CLI Layer

The CLI layer provides the command-line interface:

```
CLI/
├── Main.hs         # Main entry point
├── Controller.hs   # Command handlers and control flow
└── Deployment.hs   # Deployment management
```

**Dependencies**: Core, Programs, Execution, Simulation, Proofs

### Types Module

The Types module provides common type definitions used throughout the system:

```
Types/
├── Core.hs           # Core type definitions
├── Effect.hs         # Effect-related types
├── EffectTypes.hs    # Types for effect system
├── EffectPayload.hs  # Payload definitions for effects
├── Guard.hs          # Guard conditions and verification
├── Scenario.hs       # Scenario type definitions
├── Deployment.hs     # Deployment configuration types
└── Actor.hs          # Actor-related type definitions
```

**Dependencies**: None (except standard libraries)

## Dependency Guidelines

To maintain a clean architecture:

1. **Lower layers should not depend on higher layers**:
   - Core should not import from any other subsystem
   - Programs should only import from Core
   - Execution can import from Core and Programs
   - Adapters should only import from Core
   - Actors can import from Core, Programs, and Execution
   - Simulation can import from Core, Programs, Execution, and Actors
   - Proofs can import from Core, Programs, and Simulation
   - CLI can import from any subsystem

2. **Avoid circular dependencies**:
   - If module A imports module B, module B should not import module A
   - Use interface abstractions when bidirectional communication is needed

3. **Use typeclasses for abstraction**:
   - Define interfaces in lower layers
   - Implement in higher layers
   - Use dependency injection via the Effects system

4. **Keep imports explicit**:
   - Avoid wildcard imports (`import X.*`)
   - Name imports clearly (`import qualified X as X`)

## Dependency Management

Dependencies are managed through:

1. **The Cabal file**: External dependencies and version constraints
2. **The Nix flake**: Development environment with fixed dependency versions
3. **Module wiring**: Internal dependency relationships

By following these guidelines, we maintain a codebase that is modular, testable, and easy to understand and extend. 