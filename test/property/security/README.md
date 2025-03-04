# Security Property Tests

This directory contains property-based tests that verify the security properties of the Time Bandits system.

Tests in this directory focus on security properties such as:

- No double-spending: Resources cannot be spent twice
- No unauthorized access: Resources cannot be accessed by non-owners
- No backdating: Past states cannot be altered
- Proof validity: Cryptographic proofs are unforgeable
- Fault tolerance: System remains secure under various failure scenarios

## Adding Tests

When adding tests to this directory, follow these guidelines:

1. Use property-based testing to explore potential attack vectors
2. Implement adversarial models that attempt to violate security properties
3. Name test files after the security property being tested (e.g., `NoDoubleSpendTest.hs`)
4. Consider fuzzing techniques to find edge cases
5. Document the threat model assumptions for each test
6. Test both legitimate and malicious actor behaviors 