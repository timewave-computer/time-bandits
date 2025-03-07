# Time Bandits: Codebase Overview

This document provides a high-level overview of the Time Bandits codebase structure, the key modules, and their dependencies. Understanding these relationships is essential for navigating the codebase and contributing effectively.

## Key Components

Time Bandits includes the following key components:

1. **Core Effect System**: The foundation for representing and managing temporal effects.
2. **Timeline Management**: Handling divergent timelines and their relationships.
3. **Resource Accounting**: Managing resources across timelines.
4. **TECL Language**: The Temporal Effect Combinator Language for declaring cross-timeline workflows.
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

## Directory Structure

The codebase is organized as follows:

```
time-bandits/
├── src/           # Source code
│   ├── Core/      # Core types and data structures
│   ├── Programs/  # Program model and state management
│   ├── Execution/ # Program execution and effect handling
│   ├── Adapters/  # External system interfaces
│   ├── Actors/    # Actor model implementations
│   ├── Simulation/# Simulation framework
│   ├── Proofs/    # Cryptographic proofs and verification
│   └── CLI/       # Command-line interface
├── test/          # Test suite
├── docs/          # Documentation
└── examples/      # Example programs and scenarios
```

## Module Dependencies

### Core Layer

The Core layer forms the foundation of the Time Bandits system:

```
Core/
├── Timeline.hs       # Timeline representation and operations
├── Resource.hs       # Resource management primitives
├── TimeMap.hs        # Timeline relationship tracking
├── Types.hs          # Fundamental type definitions
├── TECL.hs           # Temporal Effect Combinator Language parser and interpreter
├── Schema.hs         # Schema definition and evolution
├── Effect.hs         # Effect representation and processing
├── AccountProgram.hs # Account program implementation
├── Common.hs         # Common utilities and functions
├── Log.hs            # Logging infrastructure
└── Utils.hs          # General utility functions
```

**Dependencies**: None (except standard libraries)

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