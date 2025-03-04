# Actor Unit Tests

This directory contains unit tests for the actor components of the Time Bandits system.

Tests in this directory focus on:

- `TimeTraveler` implementations
- `TimeKeeper` implementations
- `TimeBandit` implementations
- Actor messaging and coordination
- Actor capabilities and permissions

## Adding Tests

When adding tests to this directory, follow these guidelines:

1. Name test files after the component they test with a `Test` suffix (e.g., `TimeTravelerTest.hs`)
2. Import the test module in `test/TestMain.hs` and add it to the appropriate test group
3. Include both positive tests (expected behavior) and negative tests (error handling)
4. Use mocks and stubs where appropriate to isolate the actor behavior 