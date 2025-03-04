# Developer Workflow Guide

This document describes common workflows for developing the Time-Bandits codebase.

## Adding a New Effect

1. **Define the Effect Type**: Add a new constructor to the `Effect` type in `core/src/TimeBandits/Core/Effects.hs`:

   ```haskell
   data Effect r where
     -- ... existing effects
     NewEffect :: Param1 -> Param2 -> Effect Result
   ```

2. **Implement the Interpreter**: Extend the effect interpreter in `execution/src/TimeBandits/Execution/EffectInterpreter.hs` to handle the new effect:

   ```haskell
   runEffect :: Effect a -> Interpreter a
   runEffect = \case
     -- ... existing effect handlers
     NewEffect p1 p2 -> do
       -- Implementation of the effect
       pure result
   ```

3. **Add Tests**: Add test cases in the test suite to verify the new effect works as expected.

4. **Update Documentation**: Document the new effect in the appropriate places.

## Onboarding a New Timeline

1. **Define the Timeline Descriptor**: Create a new timeline descriptor in `core/src/TimeBandits/Core/TimelineDescriptor.hs`:

   ```haskell
   newTimelineDescriptor :: TimelineDescriptor
   newTimelineDescriptor = TimelineDescriptor
     { name = "NewTimeline"
     , properties = [...]
     , ...
     }
   ```

2. **Create the Timeline Adapter**: Implement a new adapter in `adapters/src/TimeBandits/Adapters/` that conforms to the `TimelineAdapter` interface:

   ```haskell
   module TimeBandits.Adapters.NewTimelineAdapter where
   
   import TimeBandits.Adapters.TimelineAdapter
   
   data NewTimelineAdapter = NewTimelineAdapter { ... }
   
   instance TimelineAdapter NewTimelineAdapter where
     -- Implement required functions
     ...
   ```

3. **Register the Timeline**: Add the new timeline to the available timelines list in the appropriate configuration or registry module.

4. **Test the Timeline**: Create a simple test scenario that uses the new timeline to verify it works correctly.

## Updating the System Contract

1. **Modify the Core Types**: If necessary, update the core types in `core/src/TimeBandits/Core/Types.hs`.

2. **Update Effect Handlers**: Modify effect handlers in `execution/src/TimeBandits/Execution/EffectInterpreter.hs` to align with the new contract.

3. **Update Actors**: Modify actor implementations to comply with the new system contract.

4. **Update Tests**: Modify tests to verify the new contract is correctly implemented.

5. **Document Changes**: Update the relevant documentation to explain the contract changes.

## Adding a Property Test for an Invariant

1. **Identify the Invariant**: Clearly define the invariant you want to test.

2. **Create a Generator**: Add a generator for the test data in the appropriate test module:

   ```haskell
   genTestCase :: Gen TestCase
   genTestCase = do
     -- Generate test case data
     ...
   ```

3. **Write the Property**: Define the property that should hold:

   ```haskell
   prop_invariantHolds :: TestCase -> Property
   prop_invariantHolds testCase =
     -- Property that should be true for the invariant
     ...
   ```

4. **Add to Test Suite**: Add the property to the test suite in `test/Spec.hs`:

   ```haskell
   testGroup "Invariants"
     [ testProperty "New invariant holds" prop_invariantHolds
     , ...
     ]
   ```

5. **Run the Tests**: Verify that the property tests pass:

   ```bash
   cabal test
   ```

## Common Development Tasks

### Running the REPL

```bash
cabal repl
```

### Running Linters

```bash
hlint src/ test/
```

### Formatting Code

```bash
ormolu -i $(find src test -name "*.hs")
```

### Building Documentation

```bash
cabal haddock
```

Documentation will be available in `dist-newstyle/build/.../doc/html/`. 