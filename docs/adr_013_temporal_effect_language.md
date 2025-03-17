# ADR 013: Temporal Effect Language

## Status

Accepted, with register-based extensions

## Context

Time Bandits programs need to express complex **effects across timelines**. Currently, effects are expressed using several incompatible approaches:

1. Ad-hoc JSON structures
2. Serialized protocol buffers
3. Chain-specific formats

This has led to:

- Complex integration code in adapters
- Inconsistent error handling
- Difficulties in testing and verification
- Limited expressiveness for cross-timeline operations

## Decision

We will develop a unified **Temporal Effect Language (TEL)** that can express effects across timelines. TEL will be:

1. **Declarative**: Focus on what effects should happen, not how
2. **Timeline-agnostic**: Same language for all chains
3. **Strongly typed**: Catch errors at compile time
4. **Composable**: Build complex effects from simpler ones
5. **Register-aware**: Express register operations and ZK proofs

### Core Language Features

```haskell
module TEL where

-- Base types
type Asset = String
type Amount = Integer
type Address = String
type Timeline = String
type RegisterID = String
type VerificationKey = String
type Proof = ByteString
type RegisterContents = Value

-- Core effects
data Effect
  = Deposit Timeline Asset Amount
  | Withdraw Timeline Asset Amount Address
  | Transfer Address Asset Amount
  | Invoke Address [Effect]
  | Observe FactType
  | Sequence [Effect]
  | RegisterCreate RegisterContents
  | RegisterUpdate RegisterID RegisterContents
  | RegisterTransfer RegisterID Timeline
  | RegisterMerge [RegisterID] RegisterID
  | RegisterSplit RegisterID [RegisterID]
  | VerifyProof VerificationKey Proof
  | ExecuteCircuit CircuitType Inputs

-- Register authorization
data Authorization
  = Signature Address
  | ZKProof VerificationKey Proof
  | TokenOwnership Asset Amount
  | MultiSig [Address] Int
  | Timelock Timestamp

-- Combinators
sequence :: [Effect] -> Effect
sequence = Sequence

parallel :: [Effect] -> [Effect]
parallel effects = effects

withAuth :: Authorization -> Effect -> Effect
withAuth auth effect = effect { authorization = Just auth }

withCondition :: Condition -> Effect -> Effect
withCondition cond effect = effect { condition = Just cond }

withTimeout :: Timestamp -> Effect -> Effect
withTimeout ts effect = effect { timeout = Just ts }

-- ZK Operations
verifyProof :: VerificationKey -> Proof -> Effect
verifyProof vk proof = VerifyProof vk proof

executeCircuit :: CircuitType -> Inputs -> Effect
executeCircuit circuit inputs = ExecuteCircuit circuit inputs

-- Register Operations
createRegister :: RegisterContents -> Effect
createRegister contents = RegisterCreate contents

updateRegister :: RegisterID -> RegisterContents -> Effect
updateRegister regId contents = RegisterUpdate regId contents

transferRegister :: RegisterID -> Timeline -> Effect
transferRegister regId timeline = RegisterTransfer regId timeline

mergeRegisters :: [RegisterID] -> RegisterID -> Effect
mergeRegisters sources target = RegisterMerge sources target

splitRegister :: RegisterID -> [RegisterID] -> Effect
splitRegister source targets = RegisterSplit source targets
```

### Example Usage

```haskell
-- Simple token transfer
tokenTransfer :: Effect
tokenTransfer = sequence [
  Withdraw "Ethereum" "USDC" 1000 "0x1234...",
  withAuth (Signature "0x1234...") $ 
    RegisterUpdate "reg123" (TokenBalance "USDC" 1000)
]

-- Cross-chain bridge with register transfer
crossChainBridge :: Effect
crossChainBridge = sequence [
  Withdraw "Ethereum" "ETH" 1 "0x1234...",
  withAuth (ZKProof "vk123" proofBytes) $
    RegisterTransfer "reg123" "Solana"
]

-- Complex ZK-based operation
zkOperation :: Effect
zkOperation = sequence [
  RegisterCreate (TokenBalance "USDC" 1000),
  ExecuteCircuit "TokenSwap" swapInputs,
  withAuth (ZKProof "vk456" proofBytes) $
    RegisterUpdate "reg456" (TokenBalance "ETH" 0.5)
]

-- Execute a sequence of register operations
registerSequence :: Effect
registerSequence = sequence [
  RegisterCreate (TokenBalance "USDC" 1000),
  RegisterCreate (TokenBalance "ETH" 1),
  withAuth (ZKProof "vk789" proofBytes) $
    RegisterMerge ["reg1", "reg2"] "reg3"
]
```

### Compilation and Execution

TEL effects will be compiled to timeline-specific formats by adapters:

1. **TEL Parser**: Convert text to AST
2. **Type Checker**: Verify types and constraints
3. **Timeline Adapter**: Convert to chain-specific format
4. **Executor**: Submit to blockchain

Register operations will have special handling:

1. **ZK Circuit Generation**: For operations requiring ZK proofs
2. **Proof Generation**: Generate proofs for register operations
3. **Register State Tracking**: Track register state for verification
4. **Cross-Timeline Coordination**: Coordinate register transfers

## Consequences

### Positive

- Unified language for expressing effects across timelines
- Improved type safety and error handling
- Better testing and verification
- Enhanced expressiveness for cross-timeline operations
- Direct support for register operations and ZK proofs
- Declarative syntax for complex operations

### Negative

- Learning curve for new language
- Complexity in implementing adapters for all chains
- Performance overhead from abstraction
- Increased complexity for ZK circuit integration

### Neutral

- Requires standardization of effect types
- May evolve as new chains and register types emerge
- Needs companion libraries for each supported language

## Related ADRs

- [ADR 006: Resource Ownership](adr_006_resource_ownership.md)
- [ADR 007: Fact Management](adr_007_fact_management.md)
- [ADR 018: Resource Formalization](adr_018_resource_formalization.md)

## Implementation Plan

1. Design the core language grammar and syntax.
2. Implement a parser and type checker.
3. Build a compiler targeting the Time Bandits runtime.
4. Create developer tools and documentation.
5. Implement resource tracking and verification in the compiler.
6. Add formal verification capabilities.
7. Build IDE integrations.