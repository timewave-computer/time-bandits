# ADR-003: Effect Adapter Generation

## Status

Accepted

## Context

Time Bandits programs interact with multiple types of external blockchains, requiring a consistent way to encode blockchain-specific logic. Each blockchain has unique APIs, transaction formats, and security models, which would typically require extensive custom code for integration.

Currently, each interaction with an external blockchain or rollup is manually coded, which is error-prone, difficult to maintain, and challenging to secure. We need a unified approach to handle these interactions while maintaining the security properties of the Time Bandits system.

## Decision

We will implement a generated effect adapter model to simplify interaction with external blockchains. The adapter will:

1. Define the scope of which effects can be handled by which adapter.
2. Generate from a schema that specifies blockchain-specific details.
3. Implement the necessary transforms and validations required for the effects.

### Adapter Scope

Each adapter will handle specific effects for a designated timeline (e.g., Ethereum, Solana, etc.). The adapter will:

- Only handle effects for its designated timeline.
- Transform Time Bandits effects into blockchain-specific transactions.
- Validate external proofs for incoming effects.
- Maintain consistent time map observations.

### Generation Process

Adapters will be generated from a schema that defines:

- Timeline-specific details (chain ID, block format, etc.)
- Effect type mappings (deposit, withdraw, etc.)
- Proof validation requirements
- External API specifications

This generation process ensures consistent handling across all supported blockchains while allowing blockchain-specific optimizations.

### Changes Required

To support the new model, we will need to:

1. Update how effects flow through account programs.
2. Add the ability to include observed time maps in proofs.
3. Allow the adapter to query the external timeline.
4. Create a clear separation between the abstract effect model and timeline-specific implementations.

## Consequences

### Security Model

The adapter generation model provides several security benefits:

- Standardized proof validation across all timelines
- Clear separation between program logic and blockchain-specific code
- Guaranteed replayability for all adapted effects
- Consistent time map observation enforcement

### API Simplification

Programs will be able to interact with any supported blockchain using the same Time Bandits effect model, without knowledge of underlying blockchain-specific details.

### Timeline Extensibility

Adding support for new blockchains will become more straightforward, requiring only a new adapter schema rather than extensive code changes throughout the system.

## Implementation

### Example Generated Adapter

```haskell
-- Generated adapter for applying a deposit effect
applyDepositEffect :: DepositEffect -> AccountProgram -> ExternalTimeline -> IO (Either Error Receipt)
applyDepositEffect effect accountProgram timeline = do
  -- 1. Verify the effect is targeting this adapter's timeline
  unless (effectTimeline effect == timelineId timeline) $
    return $ Left $ WrongTimeline (effectTimeline effect) (timelineId timeline)
  
  -- 2. Query the account program address
  accountAddr <- getAccountProgramAddress accountProgram
  
  -- 3. Verify the deposit proof against the timeline
  proofValid <- verifyDepositProof (depositProof effect) timeline accountAddr
  unless proofValid $
    return $ Left $ InvalidProof (depositProof effect)
  
  -- 4. Check the observed time map matches the proof's context
  timeMapValid <- verifyTimeMapMatchesProof (observedTimeMap effect) (depositProof effect)
  unless timeMapValid $
    return $ Left $ TimeMapMismatch (observedTimeMap effect)
  
  -- 5. Get the actual time map hash to embed in the proof
  timeMapHash <- hashTimeMap (observedTimeMap effect)
  
  -- 6. Apply the deposit to the account program
  receipt <- applyToAccountProgram accountProgram effect timeMapHash
  
  -- 7. Return the receipt with the time map hash included
  return $ Right $ receipt { receiptTimeMapHash = timeMapHash }
```

### Summary of Required Changes

1. New requirements for the adapter system:
   - Query the account program address
   - Verify proofs against the external timeline
   - Embed observed time map hashes in proofs
   - Apply effects to account programs with time map context

2. Schema-driven adapter generation:
   - Effect type mappings
   - Proof validation rules
   - Timeline-specific API bindings

3. Account program integration:
   - Update inbox/outbox model to include adapter-specific information
   - Add time map observation to all effect applications

### Open Questions

- Should we include simulation stubs in the generated adapters?
- How do we enforce effect schema validation at the adapter level?