# ADR 018: Resource Formalization and Controller Adaptation from Anoma

## Status

Proposed

## Context

Time Bandits provides a powerful framework for cross-timeline programming and effect-based state transitions. However, our current resource model lacks formal rigor in several areas:

1. Resources are implicitly defined through account programs without a standardized mathematical model
2. Cross-timeline asset transfers rely on ad-hoc validation rules rather than formal conservation laws
3. Timeline interactions lack a formalized controller pattern for tracking integrity across domains

Meanwhile, the Anoma project has developed rigorous formalizations around:
- The Resource Machine (ARM) with precise resource definitions
- Resource balancing mechanisms for ensuring conservation laws
- Controller patterns with explicit endorsement for cross-chain integrity

Integrating these concepts with Time Bandits could strengthen our formal guarantees while maintaining our unique temporal approach to cross-chain programming.

## Decision

We will enhance Time Bandits with formalized resource definitions and controller patterns inspired by Anoma, while preserving our temporal effect model. Specifically:

### 1. Formalized Resource Definition

Resources will be defined as tuples with formal properties:

```haskell
data Resource = Resource
    { resourceLogic      :: Logic         -- Predicate controlling resource consumption
    , fungibilityDomain  :: Label         -- Determines equivalence classes
    , quantity           :: Quantity      -- Numerical representation of amount
    , metadata           :: Value         -- Associated resource data
    , ephemeral          :: Bool          -- Whether existence must be verified
    , nonce              :: Nonce         -- Uniqueness identifier
    , nullifierPubKey    :: NullifierPK   -- For verifying consumption
    , randomnessSeed     :: Seed          -- For deriving randomness
    }
```

With derived fields:

```haskell
-- Resource commitment (hash-based proof of existence)
commitment :: Resource -> Commitment
commitment r = hashCommitment r

-- Resource nullifier (unique value marking consumption)
nullifier :: NullifierKey -> Resource -> Nullifier  
nullifier nk r = hashNullifier nk r

-- Resource kind (category for balancing)
kind :: Resource -> Kind
kind r = hashKind (resourceLogic r) (fungibilityDomain r)

-- Resource delta (net change in system)
delta :: Resource -> Delta
delta r = hashDelta (kind r) (quantity r)
```

### 2. Resource Balancing in Effects

We'll enhance the effect system with explicit resource delta calculation and balancing:

```haskell
data Effect r where
    -- Existing effect constructors...
    -- Each effect will track consumed and created resources

effectDelta :: Effect a -> Delta
effectDelta e = sum (map delta (createdResources e)) - sum (map delta (consumedResources e))

validateDelta :: Effect a -> Bool
validateDelta e = effectDelta e == 0  -- Must balance to zero
```

This ensures strong conservation laws across all effect applications.

### 3. Controller Pattern for Timelines

We'll formalize our timeline concept through controllers:

```haskell
data ControllerType = Safe | Live | Byzantine

data Controller = Controller
    { controllerID       :: ControllerID
    , controllerType     :: ControllerType
    , stateRoots         :: Map Height Hash
    , endorsements       :: Map ControllerID StateRoot
    , finalityDepth      :: Int            -- Consider final after n blocks
    , backupControllers  :: [ControllerID] -- Fallback if controller halts
    }

-- Time Keepers will implement the Controller interface
instance Controller TimeKeeper where
    -- Implementation details
```

### 4. Resource Label Extension for Cross-Timeline Transfers

Resources transferred across timelines will maintain a controller history:

```haskell
data ControllerLabel = ControllerLabel
    { creatingController  :: ControllerID
    , terminalController  :: ControllerID
    , affectingControllers :: [ControllerID]  -- DAG of controllers that affected the resource
    , backupControllers   :: [ControllerID]   -- In case terminal fails
    }

-- Extended resource with controller label
data LabeledResource = LabeledResource
    { resource :: Resource
    , controllerLabel :: ControllerLabel
    }
```

### 5. Endorsement Mechanism for Time Keepers

Time Keepers will gain the ability to endorse other controller's states:

```haskell
endorseController :: TimeKeeper -> ControllerID -> StateRoot -> Proof -> IO ()
endorseController keeper targetID root proof = do
    -- Validate proof
    -- Record endorsement
    -- Update Time Keeper's endorsement record
```

This enables state-reduction optimizations for cross-timeline resources.

### 6. Dual Validation: Temporal and Ancestral

The key innovation in our approach is combining Time Bandits' temporal validation with Anoma's ancestral validity through controller labels:

```haskell
-- The core validation function that combines both approaches
validateCrossChainResource :: Effect a -> Resource -> TimeMap -> ControllerLabel -> Either ValidationError ValidationResult
validateCrossChainResource effect resource timeMap controllerLabel = do
    -- First check temporal validity
    temporalResult <- validateTemporalConsistency effect resource timeMap
    
    -- Then check ancestral validity
    ancestralResult <- validateControllerAncestry effect resource controllerLabel
    
    -- Both must pass for the resource to be valid
    return $ ValidationResult temporalResult ancestralResult
```

#### Temporal Validation

```haskell
-- Temporal validity ensures causal consistency via time maps
validateTemporalConsistency :: Effect a -> Resource -> TimeMap -> Either ValidationError TemporalResult
validateTemporalConsistency effect resource timeMap = do
    -- Get the source timeline for this resource
    let sourceTimeline = getResourceTimeline resource
    
    -- Check if the effect's time map includes this timeline at a sufficiently advanced state
    let sourceHeight = getTimelineHeight timeMap sourceTimeline
    let requiredHeight = getResourceRequiredHeight resource
    
    -- The effect must have observed the timeline at or past the resource creation
    if sourceHeight >= requiredHeight
        then Right $ TemporallyValid sourceHeight
        else Left $ TemporalInconsistency 
                { observed = sourceHeight
                , required = requiredHeight 
                }
```

#### Ancestral Validation

```haskell
-- Ancestral validity checks the controller history
validateControllerAncestry :: Effect a -> Resource -> ControllerLabel -> Either ValidationError AncestralResult
validateControllerAncestry effect resource controllerLabel = do
    -- Get the current terminal controller
    let terminalController = controllerLabel.terminalController
    
    -- Verify this effect is being processed by the right controller
    let effectController = getEffectController effect
    if effectController /= terminalController
        then Left $ WrongController effectController terminalController
        else do
            -- Check if this controller is in the affecting controllers list
            let affectingControllers = controllerLabel.affectingControllers
            if effectController `elem` affectingControllers
                then do
                    -- Check for any endorsements that would reduce the controller chain
                    let reducedChain = applyEndorsements affectingControllers (getEndorsements effect)
                    Right $ AncestrallyValid reducedChain
                else Left $ InvalidControllerChain effectController affectingControllers
```

#### Endorsement-Based Reduction

```haskell
-- Apply endorsements to potentially reduce the controller chain
applyEndorsements :: [ControllerID] -> Map ControllerID [ControllerID] -> [ControllerID]
applyEndorsements chain endorsements = 
    -- For each controller pair (A,B) in the chain
    -- If A endorses B, we can remove B from the chain
    foldl removeEndorsedControllers chain (Map.toList endorsements)
  where
    removeEndorsedControllers chain' (endorser, endorsed) =
        if endorser `elem` chain' && any (`elem` chain') endorsed
        then filter (\c -> not (c `elem` endorsed)) chain'
        else chain'
```

## Consequences

### Positive Impacts

1. **Stronger Formal Guarantees**: Mathematical properties of resources can now be proven rather than assumed
2. **Explicit Conservation Laws**: Resource balancing ensures no resources are created or destroyed inappropriately
3. **Enhanced Cross-Timeline Integrity**: Controller labels make resource history explicit and verifiable
4. **Better Optimization Opportunities**: Endorsement patterns allow for state reduction and history pruning
5. **More Rigorous Effect Validation**: Delta calculations provide an additional validation check in the effect pipeline
6. **Defense in Depth**: Dual validation (temporal and ancestral) catches a wider range of potential attacks
7. **Resource History Compression**: Controller DAG with endorsement provides a principled way to bound history size

### Negative Impacts

1. **Increased Complexity**: The resource model becomes more complex, requiring additional developer understanding
2. **Performance Overhead**: Computing deltas, commitments, and nullifiers adds computational overhead
3. **Migration Challenges**: Existing account programs need adaptation to the new resource model
4. **Learning Curve**: Developers will need to understand the new concepts and how they relate to the existing model

### Implementation Strategy

We propose a phased approach:

1. **Phase 1**: Implement the formal resource model and adapt account programs
2. **Phase 2**: Add resource delta computation and validation to the effect pipeline
3. **Phase 3**: Extend Time Keepers to implement the controller interface
4. **Phase 4**: Implement controller labeling for cross-timeline resources
5. **Phase 5**: Add endorsement mechanisms for controller state reduction
6. **Phase 6**: Integrate dual validation (temporal and ancestral) into the effect pipeline

## Discussion: Integration with Time Bandits

### Relationship to Time Bandits Architecture

The careful reader might wonder how this resource formalization relates to our existing temporal effect system. Rather than replacing our effect model, these Anoma-inspired formalisms enhance it in specific ways:

1. **Resource Definition vs. Effect Execution**: The resource model provides formal rigor around "what" is being manipulated, while our effect system continues to control "how" and "when" those manipulations happen.

2. **Resource Balancing as an Invariant**: Adding resource delta validation (ΔTX = 0) to our effects adds mathematical certainty without changing the execution model.

3. **Controllers as Enhanced Time Keepers**: The controller pattern formalizes what's currently implicit in our Time Keeper model, providing explicit modeling of trust assumptions and chain behavior.

4. **Time Maps + Controller Labels**: Our time maps ensure operations happen in the right causal order, while controller labels ensure resources have valid ancestry. They solve orthogonal but complementary problems.

This approach preserves the unique temporal focus of Time Bandits while enhancing it with formal resource guarantees. Think of it as upgrading our type system rather than changing our programming paradigm.

### The Value of Dual Validation

The combination of temporal and ancestral validation provides defense in depth against various attack vectors:

- **Temporal Replay Attacks**: Caught by time map validation
- **Ancestry Forgery**: Caught by controller label validation
- **"Flash Loan"-Style Attacks**: Caught by either or both validations
- **Controller Halt/Fork Scenarios**: Explicitly modeled and handled

Most bridge exploits in the wild would have been caught by either temporal or ancestral validation, but many systems implement just one or the other. Our dual approach provides comprehensive protection.

## Alternative Approaches Considered

### Alternative 1: Full Anoma ARM Integration

We considered a deeper integration by directly adopting the Anoma Resource Machine as our core state model. This would provide maximum compatibility with Anoma's ecosystem but would require a complete overhaul of our effect system and temporal model, disrupting existing use cases.

#### Why Not a Full Anoma ARM Integration

1. **Fundamental Architecture Shift**: Moving from our effect-based execution model to ARM's resource-centric state machine would require rewriting our core execution pipeline. The ARM uses a different execution model focused around resource commitments and nullifiers rather than effects and facts.

2. **Execution Paradigm Shift**: Time Bandits uses a temporal effect model where time relationships are explicit, while ARM uses a state-replacement model based on resource consumption and creation. These are conceptually different approaches to state management.

3. **Developer Experience Impact**: Developers would need to learn a different paradigm focused on resource balancing rather than temporal effects, representing a significant shift in how programs are written and reasoned about.

While full ARM integration would provide theoretical elegance and closer alignment with Anoma's research, the practical disruption to our existing architecture, and use cases would be substantial. The selected approach of adopting key concepts while preserving our temporal model offers a better balance of enhanced formalism without requiring a ground-up redesign.

## Examples

### Example 1: Cross-Timeline Token Transfer

```haskell
-- Transfer token from Ethereum to Solana
transferCrossChain :: Token -> Amount -> Account -> Account -> Effect TransferResult
transferCrossChain token amount fromAccount toAccount = do
    -- Consume resource on Ethereum (with delta -amount)
    nullifyResource (makeResource token amount EthereumController)
    
    -- Create resource on Solana (with delta +amount)
    createResource (makeResource token amount SolanaController)
        { controllerLabel = ControllerLabel 
            { creatingController = EthereumController
            , terminalController = SolanaController
            , affectingControllers = [EthereumController, SolanaController]
            }
        }
        
    -- Effect delta = 0, validating conservation
```

### Example 2: Controller Endorsement

```haskell
-- Ethereum Keeper endorses Celestia's state after validation
endorseCelestiaState :: TimeKeeper -> StateRoot -> Effect EndorsementResult
endorseCelestiaState ethereumKeeper celestiaRoot = do
    -- Verify and endorse Celestia state
    proof <- validateCelestiaStateRoot celestiaRoot
    
    -- Record endorsement (enables state reduction)
    endorsement <- endorseController ethereumKeeper CelestiaController celestiaRoot proof
    
    -- Resources can now use reduced controller labels
    -- [Ethereum, Celestia, Solana] can become [Ethereum, Solana]
    -- if Ethereum endorses Celestia
```

### Example 3: Dual Validation in Practice

```haskell
-- Validate a cross-chain resource transfer
validateTransfer :: Effect Transfer -> Either ValidationError TransferResult
validateTransfer effect = do
    -- Get the resource and associated metadata
    let resource = effect.resource
    let timeMap = effect.timeMapSnapshot
    let controllerLabel = effect.controllerLabels Map.! resource.id
    
    -- Perform dual validation
    validationResult <- validateCrossChainResource effect resource timeMap controllerLabel
    
    -- If both temporal and ancestral validation pass, process the transfer
    case validationResult of
        ValidationResult (TemporallyValid _) (AncestrallyValid _) ->
            Right $ ValidTransfer resource
        ValidationResult temporalResult ancestralResult ->
            Left $ ValidationFailed temporalResult ancestralResult
```

### Example 4: Controller Classification for Real Chains

```haskell
-- Define controllers with appropriate classifications
controllers = [
    Controller 
        { controllerID = "ethereum"
        , controllerType = Safe  -- Strong security guarantees
        , finalityDepth = 15     -- Consider final after 15 blocks
        },
    Controller 
        { controllerID = "optimism"
        , controllerType = Live  -- May occasionally halt
        , finalityDepth = 1      -- Fast finality
        , backupControllers = ["ethereum"]  -- Fallback if halted
        },
    Controller 
        { controllerID = "bsc"
        , controllerType = Byzantine  -- May potentially fork
        , finalityDepth = 30     -- Need more confirmations
        }
]
```

## Integration with Existing Systems

### Account Programs

Account programs will need to be extended to handle the formalized resource model. This includes:

```haskell
data AccountProgramState = AccountProgramState
    { -- Existing fields...
      balances :: Map (TimelineID, Asset) Amount
    , inbox :: [ReceivedMessage]
    , outbox :: [SentMessage]
    , effectDAG :: EffectDAG
      -- New fields...
    , resources :: Map ResourceID Resource
    , controllerLabels :: Map ResourceID ControllerLabel
    }
```

### Time Keepers and the Map of Time

Time Keepers will implement the controller interface:

```haskell
instance Controller TimeKeeper where
    getControllerID keeper = keeper.timelineID
    getControllerType keeper = 
        -- Determine type based on timeline characteristics
        classifyTimeline keeper.timelineID
    getStateRoots keeper = keeper.observedBlockHeaders
    getEndorsements keeper = keeper.endorsedStates
```

The Time Map will continue to track causal time, while controller labels track resource ancestry:

```haskell
-- Each effect references both time maps and controller labels
data Effect a = Effect {
    -- existing fields
    timeMapSnapshot :: TimeMap,
    -- new field
    controllerLabels :: Map ResourceID ControllerLabel
}
```

## Open Questions

1. What is the performance impact of resource delta computation?
2. How do we handle migration of existing resources to the new model?
3. Should endorsement be automatic or require explicit authorization?
4. How do we handle controllers with different security assumptions in the same transaction?
5. Can we formalize the relationship between time maps and controller labels as a categorical dual?

## Conclusion

Adopting formalized resources and the controller pattern from Anoma represents an enhancement to Time Bandits' theoretical foundation. This approach preserves our unique temporal focus while strengthening our formal guarantees around cross-timeline interactions. 

The dual validation strategy, combining temporal consistency with ancestral validation, provides a comprehensive security model that addresses many of the vulnerabilities that have plagued cross-chain systems. By carefully integrating these concepts while respecting the existing architecture, we can achieve the best of both worlds: the mathematical rigor of ARM's resource model with the intuitive temporal semantics of Time Bandits.