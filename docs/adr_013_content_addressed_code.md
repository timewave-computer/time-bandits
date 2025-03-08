# ADR 013: Content-Addressed Code

## Status

Proposed​

## Context

In our current system, code modules and functions are identified by names, leading to challenges such as dependency conflicts, mutable codebases, complex refactoring processes, and build process overheads. To address these issues, we propose adopting a content-addressable code storage system, inspired by the Unison programming language's approach, where each code definition is identified by a hash of its syntax tree, ensuring immutability and simplifying dependency management. ​

## Decision

We will implement a content-addressable code storage mechanism with the following components:​

## Hash-Based Code Identification:

Each function or module will be assigned a unique identifier derived from its content hash, ensuring that any change results in a new identifier.​
Immutable Code Definitions:

Once defined, code cannot be altered. Any modification will create a new entity with a distinct identifier, preserving the stability of existing code.​

## Decoupled Naming System:

Names will be treated as metadata associated with immutable content hashes, allowing for straightforward refactoring without affecting dependencies.​
Content-Aware Execution Environment:

Develop or integrate a system that understands and executes code based on its content-addressed structure, eliminating the need for a traditional build process.​

## Consequences

### Positive Impacts:

- Elimination of Dependency Conflicts: By referencing code through unique content hashes, we prevent naming collisions and ensure precise dependency resolution.​
- Immutable Codebase: Once defined, code cannot be altered. Any modification results in a new version with a distinct identifier, preserving the stability of existing code.​
- Simplified Refactoring: Renaming functions or modules becomes straightforward, as names are metadata associated with immutable content hashes, minimizing the risk of errors during refactoring.​
- No Traditional Build Process: The system can directly execute code based on its content hash, eliminating the need for a traditional build process and enhancing development efficiency.​

## Potential Challenges:

- Integration Complexity: Transitioning to a content-addressable system may require significant changes to existing development workflows and tooling.​
- Learning Curve: Developers will need to adapt to a new paradigm of code management, which may involve a learning period.​
- Storage Considerations: Storing multiple versions of code modules could increase storage requirements, necessitating efficient storage management strategies.​

By implementing content-addressable code storage, we aim to enhance the robustness, maintainability, and efficiency of our system, aligning with modern best practices in software architecture.

---

## Note On Relationship to Combinator-based Language

Utilizing a combinator-based Domain-Specific Language (DSL) provides several benefits that align with the goals of content-addressable code storage, particularly in terms of modularity and composability. However, to fully achieve the advantages such as elimination of dependency conflicts, an immutable codebase, simplified refactoring, and removal of the build process, additional mechanisms are necessary.​

### Benefits Provided by a Combinator-Based DSL:

Modularity and Composability: Combinator-based DSLs allow for building complex functionality by combining simpler, reusable components. This modular approach enhances code clarity and maintainability.​

### Additional Mechanisms Needed:

#### Content-Addressable Storage:

Hash-Based Identification: Implement a system where each combinator or function is assigned a unique identifier based on its content hash. This ensures that any change results in a new identifier, maintaining code integrity.​

#### Immutable Code Definitions:

Enforce Immutability: Ensure that once a combinator or function is defined, it cannot be altered. Any modification should create a new entity with a distinct identifier, preserving the stability of existing code.​
Decoupled Naming System:

#### Metadata-Based Naming: Treat names as metadata associated with immutable content hashes. This allows for straightforward refactoring, such as renaming, without affecting dependencies.​
Content-Aware Execution Environment:

#### Direct Execution: Develop or integrate an execution environment that understands and executes code based on its content-addressed structure, eliminating the need for a traditional build process.​

While a combinator-based DSL offers a strong foundation for modular and composable code, achieving the full spectrum of benefits associated with content-addressable code storage requires implementing additional mechanisms that ensure immutability, precise dependency management, and an optimized execution environment.