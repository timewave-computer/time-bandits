# Time-Bandits Codebase Overview

This document provides an overview of the Time-Bandits codebase, explaining the main components, what each folder contains, and the typical flow of data and control during program execution.

## Main Components

The Time-Bandits system is organized into several high-level functional areas:

### Core

The `core/` directory contains the shared types, data structures, and foundational logic that is used throughout the system. This includes:

- **Effect.hs**: Defines the core effect system that enables time-travel operations
- **Resource.hs**: Defines the resources that can be manipulated across timelines
- **Timeline.hs**: Core timeline abstractions and operations
- **TimeMap.hs**: Manages mappings between timeline states and resources
- **ResourceLedger.hs**: Tracks and manages resources across timelines
- **TimelineDescriptor.hs**: Describes the structure and properties of timelines

### Programs

The `programs/` directory contains program-specific logic for the Time-Bandits system:

- **Program.hs**: Defines the core program structure and operations
- **ProgramMemory.hs**: Manages program state and memory across executions
- **Preconditions.hs**: Defines and evaluates preconditions for program execution
- **Scenario.hs**: Defines scenarios that combine multiple programs for testing and demonstration

### Actors

The `actors/` directory contains actor-specific logic:

- **TimeTraveler.hs**: Implements the time traveler actor that navigates across timelines
- **TimeKeeper.hs**: Implements the time keeper actor that maintains timeline consistency
- **TimeBandit.hs**: Implements the time bandit actor that attempts to exploit timeline vulnerabilities
- **Actor.hs**: Common actor functionality and abstractions
- **ActorCommunication.hs**: Handles communication between actors
- **ActorCoordination.hs**: Coordinates activities between multiple actors

### Execution

The `execution/` directory handles the unified interpreter, logging, and state transitions:

- **EffectInterpreter.hs**: Interprets effects across the system
- **ExecutionLog.hs**: Logs execution events and state changes
- **EffectExecutor.hs**: Executes effects in the appropriate context
- **EffectAdapterGenerator.hs**: Generates adapters for effects
- **DistributedLog.hs**: Manages logs in a distributed setting
- **LocalMultiProcess.hs**: Handles multi-process execution locally

### Adapters

The `adapters/` directory contains external timeline adapters:

- **TimelineAdapter.hs**: Base timeline adapter functionality
- **NetworkAdapter.hs**: Adapters for network communication
- **Network.hs**: Core network functionality
- **NetworkQUIC.hs**: QUIC protocol implementation for network communication

### Proofs

The `proofs/` directory handles proof generation and verification:

- **ZKProof.hs**: Zero-knowledge proof implementation
- **TimelineProof.hs**: Proofs for timeline operations
- **SecurityVerifier.hs**: Verifies security properties

### CLI

The `cli/` directory contains the command-line interface and entry points:

- **Main.hs**: The main entry point for the CLI application
- **Controller.hs**: Handles the simulation controller logic
- **Deployment.hs**: Manages deployment of the system

## Typical Flow of Data and Control

1. **Initialization**: The system starts with the `Main.hs` entry point in the CLI module, which initializes the simulation environment and actors.

2. **Program Definition**: Users define programs that specify operations to be performed on timelines.

3. **Actor Execution**: Time Travelers, Time Keepers, and Time Bandits execute programs by generating effects.

4. **Effect Interpretation**: Effects are interpreted by the Effect Interpreter and executed by the Effect Executor.

5. **Timeline Interaction**: Timeline adapters facilitate interaction with external timelines (e.g., blockchain systems).

6. **Logging and Verification**: All operations are logged in the Execution Log and can be verified using the Proof system.

7. **Resource Management**: Resources are tracked and managed by the Resource Ledger throughout the system.

This flow allows for complex time-travel operations across multiple timelines, with proper validation, security, and consistency guarantees. 