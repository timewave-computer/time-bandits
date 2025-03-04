# System Invariant Property Tests

This directory contains property-based tests that verify key invariants of the Time Bandits system.

Tests in this directory focus on invariants such as:

- Single-ownership guarantee: Resources have exactly one owner at all times
- Causal consistency: Events are observed in a causally consistent order
- Resource conservation: Resources cannot be created or destroyed except by defined rules
- Timeline validity: Timeline state transitions follow consensus rules
- Log integrity: Execution logs are append-only and tamper-evident

## Adding Tests

When adding tests to this directory, follow these guidelines:

1. Use property-based testing libraries like QuickCheck or Hedgehog
2. Generate diverse system states and transitions to test invariants extensively
3. Name test files after the invariant being tested (e.g., `SingleOwnershipTest.hs`)
4. Document the formal definition of the invariant in the test file
5. Include both positive tests (invariant holds) and negative tests (invariant violations are detected) 