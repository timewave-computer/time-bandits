# Time-Bandits Glossary

## Core Concepts

### Timeline
A sequence of states that represents the evolution of a system over time. Timelines can be branched, merged, and traversed by Time Travelers.

### Time Traveler
An actor that can navigate between states in a timeline and execute programs that manipulate resources.

### Time Keeper
An actor responsible for maintaining the consistency and integrity of timelines. Time Keepers validate operations and prevent invalid state transitions. Time Keepers implement the Controller interface for their respective timelines.

### Time Bandit
An actor that attempts to exploit vulnerabilities in timelines, often trying to execute double-spend attacks or other manipulation of timeline states.

### Effect
A primitive operation that can be performed on a timeline, such as reading a resource, writing a resource, or branching a timeline.

### Resource
A formalized tuple with defined properties including resource logic, fungibility domain, quantity, metadata, and unique identifiers. Resources track their provenance through controller labels and are subject to conservation laws that ensure their total amount remains constant across operations.

### Controller
An entity responsible for managing a timeline and enforcing its rules. Controllers are classified as Safe, Live, or Byzantine based on their security properties and can endorse other controllers' states to enable state reduction optimizations.

### Controller Label
A data structure that tracks the history of a resource as it crosses between timelines, recording the creating controller, terminal controller, affecting controllers, and backup controllers.

### Resource Delta
The net change in resource quantity during an operation. The system enforces that all operations must have a total delta of zero, ensuring conservation of resources.

### Time Map
A data structure that maps timeline states to resources, tracking where resources exist across different timeline states.

### Execution Log
A record of all operations performed on timelines, used for debugging, auditing, and verification purposes.

### Program Memory
The state maintained by a program as it executes across different timeline states.

## Advanced Concepts

### Timeline Branch
A fork in a timeline that creates a new potential future state path.

### Timeline Merge
The operation of reconciling two divergent timelines back into a single consistent timeline.

### Causality Violation
An inconsistent state where an effect depends on a future state that hasn't occurred yet or is in a different branch.

### Double-Spend Attack
An attack where a Time Bandit attempts to use the same resource twice by exploiting branching timelines.

### Temporal Consistency
The property that effects in a timeline are ordered in a way that respects causality.

### Ancestral Validation
A validation mechanism that verifies the provenance of resources by checking their controller history through controller labels.

### Dual Validation
The combination of temporal validation (ensuring causal consistency via time maps) and ancestral validation (verifying controller history), providing defense in depth for cross-timeline operations.

### Resource Commitment
A cryptographic commitment to a resource's existence, derived from its properties. Used to prove resource existence without revealing all details.

### Resource Nullifier
A unique value that marks a resource as consumed, preventing double-spending. Created using the resource and a nullifier key.

### Timeline Proof
A cryptographic proof that verifies the validity of a timeline state or state transition.

### Resource Ledger
A component that tracks resource ownership and transfers across timelines.

### Timeline Descriptor
A specification of a timeline's properties, including its branching model, consensus mechanism, and security parameters.

### Effect Interpreter
A component that translates abstract effects into concrete operations on a specific timeline.

### Program Precondition
A condition that must be satisfied before a program can be executed on a timeline.

## Content-Addressable Code System

### Content Hash
A cryptographic hash derived from the content of a code definition, used as a unique identifier independent of names.

### Code Definition
A unit of code (function or module) stored in the content-addressable system, identified by its content hash.

### Content-Addressable Repository
A storage system that organizes code by content hash rather than by name, enabling immutability and precise dependency resolution.

### Name Registration
The process of associating a human-readable name with a content hash, allowing code to be referenced by name while maintaining hash-based dependencies.

### Content-Addressable Executor
A runtime component that can execute code retrieved by its content hash, maintaining execution context across invocations.

## Temporal Effect Language (TEL)

### Temporal Effect Language (TEL)
A specialized programming language designed for cross-timeline programming in the Time Bandits system, with explicit effects, strong typing, and causal consistency.

### Expression
The basic unit of computation in TEL, which always evaluates to a value.

### Pattern Matching
A mechanism in TEL for destructuring and analyzing values, enabling conditional logic and data transformation.

### Effect Expression
A specialized TEL expression that describes an interaction with external timelines or resources, such as deposit, withdraw, transfer, observe, or emit.

### TEL Interpreter
The component that evaluates TEL expressions, manages effects, and integrates with the Time Bandits runtime.

### TEL Type System
The static type checking system that ensures TEL programs are well-formed and type-safe before execution.

### TEL Program
A collection of function definitions in the Temporal Effect Language that can be deployed to the Time Bandits network. 