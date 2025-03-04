# Timeline Synchronization Integration Tests

This directory contains integration tests that verify the synchronization of multiple timelines in the Time Bandits system.

Tests in this directory focus on:

- Cross-timeline operations
- Timeline state consistency
- Causal ordering across multiple timelines
- Timeline fork handling and reconciliation
- Resource ownership tracking across timelines

## Adding Tests

When adding tests to this directory, follow these guidelines:

1. Name test files descriptively after the synchronization scenario being tested (e.g., `CrossTimelineTransferTest.hs`)
2. Set up test environments with multiple simulated timelines
3. Test eventual consistency properties
4. Verify that causal ordering is maintained across timeline boundaries
5. Include tests for conflict resolution mechanisms 