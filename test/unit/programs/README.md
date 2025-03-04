# Program Unit Tests

This directory contains unit tests for the program components of the Time Bandits system.

Tests in this directory focus on:

- Program creation and initialization
- Program memory management
- Program effect generation
- Precondition evaluation
- Scenario composition

## Adding Tests

When adding tests to this directory, follow these guidelines:

1. Name test files after the component they test with a `Test` suffix (e.g., `ProgramMemoryTest.hs`)
2. Import the test module in `test/TestMain.hs` and add it to the appropriate test group
3. Include tests for both valid and invalid program states
4. Use mocks for dependencies when testing program component behavior in isolation 