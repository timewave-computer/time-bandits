# Actor Interactions Integration Tests

This directory contains integration tests that verify the correct interactions between different actor types in the Time Bandits system.

Tests in this directory focus on:

- Communication between Time Travelers and Time Keepers
- Message passing between actors
- Coordination of multiple actors in a system
- Multi-actor consensus scenarios
- Actor role transitions and handoffs

## Adding Tests

When adding tests to this directory, follow these guidelines:

1. Name test files descriptively after the interaction pattern being tested (e.g., `TravelerKeeperMessagingTest.hs`)
2. Set up test environments that include multiple actor instances
3. Test both normal operation flows and failure recovery scenarios
4. Verify that actor interactions maintain system invariants 