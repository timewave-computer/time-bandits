# Time Bandits Test Suite

This directory contains the automated test suite for the Time Bandits system.

## Directory Structure

- `unit/`: Unit tests for individual components
  - `core/`: Core component tests
  - `programs/`: Program component tests 
  - `actors/`: Actor component tests
  - `execution/`: Execution component tests
  - `adapters/`: Adapter component tests
  - `proofs/`: Proof component tests

- `integration/`: Tests involving multiple components
  - `program_execution/`: Tests for complete program execution flows
  - `actor_interactions/`: Tests for interactions between actors
  - `timeline_sync/`: Tests for timeline synchronization

- `property/`: Property-based tests
  - `invariants/`: System invariant tests
  - `security/`: Security property tests

- `fixtures/`: Shared test data and helpers

- `TestMain.hs`: Main test runner

## Running Tests

The tests can be run using the Nix-based build system:

```bash
# Run all tests
nix build .#checks.aarch64-darwin.time-bandits-tests

# Run specific test groups
cabal test unit-tests
cabal test integration-tests
cabal test property-tests
```

## Adding New Tests

When adding new tests:

1. Place the test in the appropriate directory based on its type (unit, integration, property)
2. Use the `Test` suffix for test files
3. Add the test to the appropriate test group in the test runner
4. Update the cabal file to include the new test file 