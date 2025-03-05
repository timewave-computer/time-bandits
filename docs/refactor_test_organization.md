# Refactor: Test Organization

## Current Issues

After reviewing the current organization of the test-related directories, I've identified the following issues:

1. **Scattered Test Files**: Test files are placed in a flat structure in the `test/` directory with inconsistent naming
2. **Unclear Purpose for `examples/`**: The `examples/` directory contains scenario files mixed with timeline descriptors
3. **Overlapping Functionality**: The `scenarios/` directory seems to overlap with examples
4. **Confusing `timeline_specs/` Directory**: This stands separate but contains similar content to `examples/timeline_descriptors/`

## Proposed Directory Structure

I propose reorganizing these directories into a more coherent and consistent structure:

```
time-bandits/
├── test/                        # All automated tests
│   ├── unit/                    # Unit tests for individual components
│   │   ├── core/                # Core component tests
│   │   ├── programs/            # Program component tests
│   │   ├── actors/              # Actor component tests
│   │   ├── execution/           # Execution component tests
│   │   ├── adapters/            # Adapter component tests
│   │   └── proofs/              # Proof component tests
│   ├── integration/             # Tests involving multiple components
│   │   ├── program_execution/   # Tests for complete program execution flows
│   │   ├── actor_interactions/  # Tests for interactions between actors
│   │   └── timeline_sync/       # Tests for timeline synchronization
│   ├── property/                # Property-based tests
│   │   ├── invariants/          # System invariant tests
│   │   └── security/            # Security property tests
│   ├── fixtures/                # Shared test data and helpers
│   └── TestMain.hs              # Main test runner
├── fixtures/                    # External data files for tests and examples
│   ├── timelines/               # Timeline specification files
│   │   ├── ethereum.toml        # Ethereum timeline specification
│   │   ├── solana.toml          # Solana timeline specification
│   │   └── ...                  # Other timeline specifications
│   └── resources/               # Sample resource definitions
├── examples/                    # Executable examples of system usage
│   ├── scenarios/               # Example usage scenarios
│   │   ├── simple_transfer.toml        # Simple resource transfer scenario
│   │   ├── execution_log.toml          # Execution log demonstration
│   │   └── causal_ordering.toml        # Causal ordering demonstration
│   ├── simulations/             # More complex simulations
│   │   ├── multi_timeline.toml         # Cross-timeline simulation
│   │   └── distributed_actors.toml     # Distributed actor simulation
│   └── README.md                # Documentation for the examples
```

## Rationale for Changes

### 1. Structured Test Organization

The new structure organizes tests by their type (unit, integration, property) and then by the component they're testing. This makes it easier to:

- Find tests related to a specific component
- Understand the test coverage of the system
- Run only certain types of tests when needed

### 2. Clear Separation of Fixtures

Moving all external data files to a dedicated `fixtures/` directory:

- Clarifies that these files are inputs to tests and examples
- Allows for better organization of different types of fixtures
- Makes it easier to find and update these files

### 3. Purposeful Examples Directory

The new `examples/` directory is focused exclusively on demonstrating how to use the system:

- Contains runnable examples with clear purposes
- Organizes examples by complexity and use case
- Provides documentation for each example

### 4. Consistent Naming

This proposal establishes consistent naming conventions:

- Test files are named after what they test with a `Test` suffix
- Fixture files are named descriptively by their purpose
- Example files use clear, action-oriented names

## Implementation Plan

1. Create the new directory structure
2. Move files to their appropriate locations (preserving git history)
3. Update import paths in all affected files
4. Update build scripts to reflect the new structure
5. Add READMEs to each major directory explaining its purpose
6. Update documentation to reference the new structure

## Impact on Current Code

The proposed changes will require updates to:

- Import statements in test files
- Path references in CI/CD scripts
- Documentation references to test files and examples
- Commands in the developer workflow guides

However, these changes will improve maintainability and make the codebase more approachable for new developers. 