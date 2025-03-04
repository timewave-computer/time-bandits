# Adapter Unit Tests

This directory contains unit tests for the adapter components of the Time Bandits system.

Tests in this directory focus on:

- Timeline adapter implementations
- Network adapter functionality
- Ethereum adapter specific tests
- Celestia adapter specific tests
- Adapter error handling

## Adding Tests

When adding tests to this directory, follow these guidelines:

1. Name test files after the component they test with a `Test` suffix (e.g., `EthereumAdapterTest.hs`)
2. Import the test module in `test/TestMain.hs` and add it to the appropriate test group
3. Use mocks for external systems (blockchains, networks) to test adapter behavior
4. Test both successful operations and error/edge cases
5. For blockchain-specific adapters, include tests with representative blockchain data 