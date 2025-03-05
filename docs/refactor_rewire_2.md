# Time Bandits Refactor Rewire - Phase 2

This document contains the remaining tasks from the original refactor_rewire.md plan that need to be completed.

## Remaining Tasks

### Module Restructuring

1. [ ] Move all simulation control logic to `Simulation/Controller.hs`, with `Main.hs` only responsible for parsing CLI arguments and calling `runController`
2. [ ] Create `ResourceLedger.hs` and move ownership checks from `Resource.hs` there, with `Resource.hs` only defining the data type
3. [ ] Introduce `ExecutionLog.hs` for structured logging of applied effects, ensuring effect handlers return structured data instead of calling `putStrLn` directly
4. [ ] Isolate actor communication by moving message formats into `Messaging.hs`, ensuring actors only depend on this for communication
5. [ ] Create a shared interface for all actor types in `Actor.hs` to allow uniform spawning of actors
6. [ ] Consolidate effect-specific logic into `EffectInterpreter.hs`, which will handle precondition checking, state mutation, and logging
7. [ ] Move scenario loading logic into `Scenarios.hs`, with the controller only responsible for calling a load function

### Timeline-Specific Code

8. [ ] Complete the `TimelineAdapter` typeclass implementation to prevent timeline logic from bleeding into core effect processing
9. [ ] Implement the following adapters:
   - [x] `MockAdapter.hs` - for testing purposes (simulates blockchain without external connections)
   - [ ] `EthereumAdapter.hs` - for Ethereum chain interactions
   - [ ] `CelestiaAdapter.hs` - for Celestia chain interactions
   - [ ] `NetworkAdapter.hs` - for P2P network operations

### Core Module Cleanup

10. [ ] Ensure all foundational data types are properly moved to `Core/`:
    - [ ] Verify `Core/Effect.hs` only defines effect types with no logic or precondition checks
    - [ ] Ensure `Core/Resource.hs` only defines the Resource type without ownership checking
    - [ ] Check that `Core/Timeline.hs` only contains type definitions without RPC logic
    - [ ] Confirm `Core/TimeMap.hs` defines only the time map structure without logic to advance time
    - [ ] Make sure `Core/Program.hs` defines only the immutable program definition

### Import Policy Enforcement

11. [ ] Audit imports to ensure:
    - [ ] Core imports nothing above it (pure foundation layer)
    - [ ] No circular dependencies exist
    - [ ] Proper abstraction boundaries are maintained
    - [ ] Minimal coupling between subsystems

### Documentation Updates

12. [ ] Update the codebase documentation to reflect the new structure
13. [ ] Create diagrams showing the updated module dependencies
14. [ ] Update onboarding guide with the new architecture 

## Testing Strategy

For each refactoring step:

1. Run the test suite to ensure everything still works
2. Verify scenarios still run correctly
3. Check for any unintended side effects of the changes

## Success Criteria

The refactoring will be considered successful when:

1. All tasks above are completed
2. All tests pass
3. The codebase has clear separation of concerns
4. There are no circular dependencies
5. The documentation accurately reflects the new structure 