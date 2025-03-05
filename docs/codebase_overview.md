# Time Bandits: Codebase Overview

This document provides a high-level overview of the Time Bandits codebase structure, the key modules, and their dependencies. Understanding these relationships is essential for navigating the codebase and contributing effectively.

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
└── Hash.hs           # Cryptographic hashing utilities
```

**Dependencies**: None (except standard libraries)

### Programs Layer

The Programs layer implements the programming model:

```
Programs/
├── Program.hs        # Program definition and state
├── Effect.hs         # Effect data structures
├── Types.hs          # Program-specific types
├── Memory.hs         # Program memory model
└── Precondition.hs   # Precondition evaluation
```

**Dependencies**: Core

### Execution Layer

The Execution layer handles runtime execution:

```
Execution/
├── Interpreter.hs    # Effect interpreter
├── Controller.hs     # Execution control
├── LogStore.hs       # Execution logging
└── ResourceLedger.hs # Resource accounting
```

**Dependencies**: Core, Programs

### Adapters Layer

The Adapters layer interfaces with external systems:

```
Adapters/
├── Storage/          # Storage backends
├── Network/          # Network communication
├── TimeSource.hs     # External time sources
└── Crypto.hs         # Cryptographic operations
```

**Dependencies**: Core

### Actors Layer

The Actors layer implements the actor model:

```
Actors/
├── Actor.hs          # Base actor functionality
├── TimeTraveler.hs   # Timeline manipulation actor
├── TimeKeeper.hs     # Timeline consistency actor
└── TimeBandit.hs     # Security testing actor
```

**Dependencies**: Core, Programs, Execution

### Simulation Layer

The Simulation layer provides the simulation framework:

```
Simulation/
├── Controller.hs     # Simulation control
├── Scenario.hs       # Scenario definition and loading
└── Messaging.hs      # Actor-to-actor messaging
```

**Dependencies**: Core, Programs, Execution, Actors

### Proofs Layer

The Proofs layer handles cryptographic proofs:

```
Proofs/
├── TimelineProof.hs  # Timeline state proofs
├── ZKProof.hs        # Zero-knowledge proofs
└── SecurityVerifier.hs # Security property verification
```

**Dependencies**: Core, Programs, Simulation

### CLI Layer

The CLI layer provides the command-line interface:

```
CLI/
├── Main.hs           # Main entry point
└── Commands.hs       # Command handlers
```

**Dependencies**: Core, Programs, Execution, Simulation, Proofs

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