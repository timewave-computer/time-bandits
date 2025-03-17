# Example 002: Adding a Custom External Bridge Effect to Time Bandits with Refund Handling

## Step 1: Ensure Required Timeline Adapters Exist

Before implementing a custom bridge effect, the developer needs to check if adapters for both the source and destination timelines already exist, and create any missing ones:

```bash
# Check available timeline adapters
time-bandits adapters list
```

If an adapter is missing, the developer would implement it:

```haskell
module TimeBandits.Adapters.NewTimelineAdapter where

import TimeBandits.Adapters.TimelineAdapter
import TimeBandits.Resources.Formalized
import TimeBandits.Controllers.Interface

data NewTimelineAdapter = NewTimelineAdapter {
  rpcEndpoint :: Text,
  networkId :: Int,
  controllerType :: ControllerType  -- Safe, Live, or Byzantine
}

instance TimelineAdapter NewTimelineAdapter where
  applyEffect effect accountState timeline = 
    case effect of
      -- Handle basic effects (existing implementation)
      Deposit params -> handleDeposit params accountState timeline
      Withdraw params -> handleWithdraw params accountState timeline
      
      -- Add handler for our custom effect
      CustomEffect "ExternalBridge" params -> handleBridgeEffect params accountState timeline
      
      -- Other effects handled by default implementation
      _ -> defaultEffectHandler effect accountState timeline
  
  -- Resource delta validation for the timeline    
  validateResourceDelta :: Effect -> Bool
  validateResourceDelta effect = 
    let delta = computeEffectDelta effect
    in delta == 0  -- Ensure all resources are balanced
      
  -- Other required interface methods with default implementations
  validateProof = defaultProofValidator
  observeFact = defaultFactObserver
  
-- Implement Controller interface for the Timeline Adapter
instance Controller NewTimelineAdapter where
  getControllerID adapter = ControllerID $ "timeline:" <> show adapter.networkId
  getControllerType adapter = adapter.controllerType
  getStateRoots adapter = fetchLatestBlockHeaders adapter.rpcEndpoint
  getEndorsements adapter = fetchStoredEndorsements adapter.rpcEndpoint
```

## Step 2: Define the Bridge Effect in TEL with Refund Handling and Resource Formalization

With adapters in place, the developer defines the external bridge effect with refund support and formalized resources:

```haskell
-- Define the bridge effect type with refund monitoring and resource formalization
type ExternalBridge = {
  sourceTimeline :: Timeline,
  destinationTimeline :: Timeline,
  sourceResource :: Resource,  -- Formalized resource definition
  destinationResource :: Resource,  -- Formalized resource definition
  bridgeAddress :: Address,
  observationTimeout :: Int,  -- In seconds
  refundMonitoringPeriod :: Int,  -- In seconds, how long to monitor for refunds
  controllerLabel :: ControllerLabel  -- Track resource provenance across chains
}

-- Formalized resource definition
type Resource = {
  resourceLogic :: Logic,         -- Predicate controlling resource consumption
  fungibilityDomain :: Label,     -- Determines equivalence classes (e.g., "USDC")
  quantity :: Quantity,           -- Numerical representation of amount
  metadata :: Value,              -- Associated resource data
  ephemeral :: Bool,              -- Whether existence must be verified
  nonce :: Nonce,                 -- Uniqueness identifier
  nullifierPubKey :: NullifierPK, -- For verifying consumption
  randomnessSeed :: Seed          -- For deriving randomness
}

-- Controller label for tracking resource provenance
type ControllerLabel = {
  creatingController :: ControllerID,
  terminalController :: ControllerID,
  affectingControllers :: [ControllerID],
  backupControllers :: [ControllerID]
}

-- Define enhanced result type with refund information and dual validation
type BridgeResult = {
  success :: Bool,
  sourceTransactionHash :: Text,
  destinationTransactionHash :: Maybe Text,
  refundTransactionHash :: Maybe Text,
  resourceCommitment :: Commitment,  -- Commitment to the bridged resource
  validationResult :: ValidationResult  -- Results of temporal and ancestral validation
}

-- Dual validation result
type ValidationResult = {
  temporalResult :: TemporalResult,
  ancestralResult :: AncestralResult
}

-- Implementation of the bridge effect
externalBridge :: ExternalBridge -> Effect BridgeResult
externalBridge params = do
  -- Check if source resource is valid
  let sourceResource = params.sourceResource
  let sourceCommitment = commitment sourceResource
  let sourceNullifier = nullifier sourceResource.nullifierPubKey sourceResource
  
  -- Create controller label for cross-chain transfer
  let controllerLabel = params.controllerLabel {
    affectingControllers = params.destinationTimeline : params.controllerLabel.affectingControllers
  }
  
  -- Get current time map for temporal validation
  timeMap <- getCurrentTimeMap
  
  -- Execute the bridge transaction on source chain
  sourceTx <- execBridgeTx params.sourceTimeline params.bridgeAddress sourceResource
  
  -- Record the nullifier to mark the source resource as consumed
  recordNullifier sourceNullifier
  
  -- Monitor for transaction confirmation on source chain
  sourceReceipt <- monitorTx params.sourceTimeline sourceTx params.observationTimeout
  
  -- If source transaction failed or timed out, return failure
  if not sourceReceipt.success
    then return { 
      success = False, 
      sourceTransactionHash = sourceTx,
      destinationTransactionHash = Nothing,
      refundTransactionHash = Nothing,
      resourceCommitment = sourceCommitment,
      validationResult = ValidationResult 
        { temporalResult = TemporallyValid (getTimelineHeight timeMap params.sourceTimeline) 
        , ancestralResult = AncestrallyInvalid "Source transaction failed"
        }
    }
    else do
      -- Monitor destination chain for incoming bridge transfer
      destTx <- monitorBridgeDestination 
        params.destinationTimeline 
        params.sourceResource.fungibilityDomain 
        params.sourceResource.quantity 
        sourceReceipt.blockHeight
        params.observationTimeout
      
      -- Create destination resource with updated controller label
      let destResource = params.destinationResource {
        controllerLabel = controllerLabel {
          terminalController = params.destinationTimeline
        }
      }
      
      -- Perform dual validation
      validationResult <- validateCrossChainResource 
        (BridgeEffect sourceResource destResource) 
        destResource 
        timeMap 
        controllerLabel
        
      case destTx of
        -- Bridge transfer completed successfully
        Just txHash -> return { 
          success = True, 
          sourceTransactionHash = sourceTx,
          destinationTransactionHash = Just txHash,
          refundTransactionHash = Nothing,
          resourceCommitment = commitment destResource,
          validationResult = validationResult
        }
        
        -- Bridge transfer not detected, monitor for refund
        Nothing -> do
          refundTx <- monitorForRefund 
            params.sourceTimeline 
            params.sourceResource.fungibilityDomain
            params.sourceResource.quantity
            params.refundMonitoringPeriod
          
          return { 
            success = False, 
            sourceTransactionHash = sourceTx,
            destinationTransactionHash = Nothing,
            refundTransactionHash = refundTx,
            resourceCommitment = sourceCommitment,
            validationResult = ValidationResult 
              { temporalResult = TemporallyValid (getTimelineHeight timeMap params.sourceTimeline)
              , ancestralResult = AncestrallyInvalid "Bridge transfer failed, refund detected" 
              }
          }
```

## Step 3: Set Up Observability and Retries

With resource formalization in place, the developer adds observability patterns, including delta calculation and validation:

```haskell
-- Monitor a bridge transaction with full observability
monitorBridgeDestination :: Timeline -> Label -> Quantity -> Int -> Int -> Effect (Maybe Text)
monitorBridgeDestination destTimeline asset amount sourceHeight timeout = do
  -- Set up initial resource delta tracker
  let initialDelta = Delta (-amount)  -- Initial negative delta from source chain
  
  -- Start observation loop
  startTime <- now
  loop startTime initialDelta
  where
    loop startTime currentDelta = do
      -- Check if we've detected the transfer
      transfers <- queryCrossChainTransfers destTimeline asset
      
      -- Filter transfers by approximate amount and time window
      relevantTransfers <- filterRelevant transfers amount sourceHeight
      
      case relevantTransfers of
        -- Found a transfer, calculate the positive delta to balance the negative source delta
        (transfer:_) -> do
          let positiveDelta = Delta amount
          let totalDelta = combineDelta currentDelta positiveDelta
          
          -- Validate that resource deltas balance to zero
          if totalDelta == Delta 0
            then return (Just transfer.txHash)
            else throwError $ DeltaImbalance currentDelta positiveDelta
            
        -- No transfer found, check timeout
        [] -> do
          currentTime <- now
          if diffTime currentTime startTime > timeout
            then return Nothing
            else do
              sleep 10  -- Sleep and retry
              loop startTime currentDelta
```

## Step 4: Use in a Program with Dual Validation

Finally, the developer can use the custom bridge effect within a program:

```haskell
-- Example usage in a program
crossChainSwap :: Token -> Amount -> Account -> Account -> Effect SwapResult
crossChainSwap token amount fromAccount toAccount = do
  -- Get the timeline controllers
  ethController <- getController "ethereum"
  polyController <- getController "polygon"
  
  -- Create source resource
  let sourceResource = Resource {
    resourceLogic = TokenLogic,
    fungibilityDomain = token,
    quantity = amount,
    metadata = emptyValue,
    ephemeral = False,
    nonce = generateNonce,
    nullifierPubKey = fromAccount.nullifierKey,
    randomnessSeed = generateSeed
  }
  
  -- Create destination resource (same properties but different controller)
  let destResource = sourceResource {
    nonce = generateNonce,  -- New nonce for destination
    nullifierPubKey = toAccount.nullifierKey
  }
  
  -- Create controller label for cross-chain transfer
  let controllerLabel = ControllerLabel {
    creatingController = ethController,
    terminalController = polyController,
    affectingControllers = [ethController],
    backupControllers = []
  }
  
  -- Execute the bridge with resource formalization
  bridgeResult <- externalBridge {
    sourceTimeline = "ethereum",
    destinationTimeline = "polygon",
    sourceResource = sourceResource,
    destinationResource = destResource,
    bridgeAddress = "0x1234...5678",
    observationTimeout = 3600,  -- 1 hour
    refundMonitoringPeriod = 86400,  -- 24 hours
    controllerLabel = controllerLabel
  }
  
  -- Validate the result using dual validation
  case bridgeResult.validationResult of
    ValidationResult (TemporallyValid _) (AncestrallyValid _) ->
      -- Both validations passed, continue with the post-bridge operation
      -- The final delta will be 0, ensuring resource conservation
      performPostBridgeAction destResource amount toAccount
      
    -- Handle validation failures
    _ -> throwError $ ValidationFailure bridgeResult.validationResult
```

This example demonstrates how to implement a custom cross-chain bridge effect with the formalized resource model, including:

1. Resource formalization with proper tuples
2. Controller labels for tracking resource provenance
3. Dual validation (temporal and ancestral)
4. Resource delta tracking to ensure conservation
5. Resource commitments and nullifiers for security