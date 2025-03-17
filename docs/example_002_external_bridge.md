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

data NewTimelineAdapter = NewTimelineAdapter {
  rpcEndpoint :: Text,
  networkId :: Int
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
      
  -- Other required interface methods with default implementations
  validateProof = defaultProofValidator
  observeFact = defaultFactObserver
```

## Step 2: Define the Bridge Effect in TEL with Refund Handling

With adapters in place, the developer defines the external bridge effect with refund support:

```haskell
-- Define the bridge effect type with refund monitoring
type ExternalBridge = {
  sourceTimeline :: Timeline,
  destinationTimeline :: Timeline,
  sourceAsset :: Asset,
  destinationAsset :: Asset,
  amount :: Amount,
  bridgeAddress :: Address,
  observationTimeout :: Int,  -- In seconds
  refundMonitoringPeriod :: Int  -- In seconds, how long to monitor for refunds
}

-- Define enhanced result type with refund information
type BridgeResult = 
  | BridgeSuccess { txHash :: Hash }
  | BridgeFailed { reason :: Text }
  | BridgeTimedOut
  | BridgeRefunded { refundTxHash :: Hash }

-- Define the bridge function
bridgeViaExternal :: ExternalBridge -> Effect BridgeResult
bridgeViaExternal bridge = emit (CustomEffect "ExternalBridge" bridge)
```

## Step 3: Implement Fact Observation Rules for Both Mint and Refund

The developer creates fact observation rules for both the mint event and potential refund:

```toml
# Rule for observing mints on destination chain
[[rules]]
rule_id = "external-bridge-mint-observation"
fact_type = "MintObservation"
proof = "InclusionProof"
enabled = true
description = "Observes token mints on destination chain from external bridge"

path.source = "${destinationTimeline}"
path.selector = "contract.event"

[path.parameters]
contract_address = "${bridgeAddress}"
event_name = "TokensMinted"

[[conditions]]
field = "token"
operator = "=="
value = "${destinationAsset}"

[[conditions]]
field = "origin_chain"
operator = "=="
value = "${sourceTimeline}"

[[conditions]]
field = "amount"
operator = "=="
value = "${amount}"

# Rule for observing refunds on source chain
[[rules]]
rule_id = "external-bridge-refund-observation"
fact_type = "RefundObservation" 
proof = "InclusionProof"
enabled = true
description = "Observes token refunds on source chain from external bridge"

path.source = "${sourceTimeline}"
path.selector = "contract.event"

[path.parameters]
contract_address = "${bridgeAddress}"
event_name = "BridgeRefunded"

[[conditions]]
field = "token"
operator = "=="
value = "${sourceAsset}"

[[conditions]]
field = "destination_chain"
operator = "=="
value = "${destinationTimeline}"

[[conditions]]
field = "amount"
operator = "=="
value = "${amount}"
```

## Step 4: Create a Bridge Module using TEL with Refund Handling

The developer creates a reusable TEL module with comprehensive refund handling:

```haskell
module ExternalBridges where

import TimeBandits.Core.TEL

-- Higher-level utility function for bridging tokens with refund monitoring
bridgeToken :: Asset -> Asset -> Timeline -> Timeline -> Amount -> Effect BridgeResult
bridgeToken sourceAsset destAsset sourceChain destChain amount = do
  -- 1. Get the appropriate bridge address based on assets and chains
  bridgeAddr <- getBridgeAddress sourceAsset destAsset sourceChain destChain
  
  -- 2. Withdraw the source asset from the account program
  withdrawResult <- withdraw amount sourceAsset sourceChain
  
  case withdrawResult of
    Left err -> return $ BridgeFailed { reason = "Withdrawal failed: " ++ show err }
    Right _ -> do
      -- 3. Set up the bridge parameters
      let bridge = ExternalBridge {
        sourceTimeline = sourceChain,
        destinationTimeline = destChain,
        sourceAsset = sourceAsset,
        destinationAsset = destAsset,
        amount = amount,
        bridgeAddress = bridgeAddr,
        observationTimeout = 3600,  -- 1 hour timeout for mint
        refundMonitoringPeriod = 86400  -- 24 hours to monitor for refunds
      }
      
      -- 4. Execute the bridge effect
      bridgeResult <- bridgeViaExternal bridge
      
      case bridgeResult of
        BridgeSuccess txHash -> do
          -- 5. Race between watching for mint and watching for refund
          raceResult <- race
            -- Watch for mint on destination chain
            (do
              mintFact <- observe (MintObservation destChain destAsset amount) on destChain
              return $ Left mintFact)
            
            -- Watch for refund on source chain
            (do
              refundFact <- observe (RefundObservation sourceChain sourceAsset amount) on sourceChain
              return $ Right refundFact)
          
          case raceResult of
            Left mintFact -> do
              -- 6a. Mint succeeded - deposit tokens to account program on destination
              deposit amount destAsset destChain
              return $ BridgeSuccess { txHash = txHash }
              
            Right refundFact -> do
              -- 6b. Bridge refunded tokens - handle the refund
              let refundTxHash = getRefundTxHash refundFact
              -- Process the refund (add to account balance)
              processRefund amount sourceAsset sourceChain refundTxHash
              return $ BridgeRefunded { refundTxHash = refundTxHash }
        
        other -> return other

-- Helper function to get appropriate bridge address
getBridgeAddress :: Asset -> Asset -> Timeline -> Timeline -> Effect Address
getBridgeAddress sourceAsset destAsset sourceChain destChain = do
  -- This could query a registry or use a predefined mapping
  return $ case (sourceAsset, destAsset, sourceChain, destChain) of
    ("USDC", "USDC", "ethereum", "celestia") -> "0x1234...5678"
    ("USDC", "USDC", "ethereum", "solana") -> "0xabcd...efgh"
    -- Add more bridge addresses as needed
    _ -> error "Unsupported bridge combination"

-- Helper to extract refund transaction hash from fact
getRefundTxHash :: Fact -> Hash
getRefundTxHash fact = fact.refundTxHash  -- Simplified; actual implementation would extract from fact payload

-- Process refund by updating account program balance
processRefund :: Amount -> Asset -> Timeline -> Hash -> Effect ()
processRefund amount asset timeline refundTxHash = do
  -- Create a deposit effect to account program for the refunded tokens
  deposit amount asset timeline
  -- Emit refund received event
  emit $ "Refund received: " ++ show amount ++ " " ++ asset ++ " on " ++ timeline
```

## Step 5: Add Fallback and Timeout Handling

The developer enhances the bridge module with comprehensive timeout and fallback logic:

```haskell
-- Enhanced bridging function with comprehensive fallback handling
bridgeTokenWithFallbacks :: Asset -> Asset -> Timeline -> Timeline -> Amount -> Effect BridgeResult
bridgeTokenWithFallbacks sourceAsset destAsset sourceChain destChain amount = do
  -- First try regular bridging
  bridgeResult <- bridgeToken sourceAsset destAsset sourceChain destChain amount
  
  case bridgeResult of
    BridgeSuccess txHash -> return $ BridgeSuccess txHash
    BridgeRefunded refundTxHash -> return $ BridgeRefunded refundTxHash
    
    -- For failures or timeouts, implement fallback logic
    BridgeFailed reason -> do
      -- Log the failure
      emit $ "Bridge failed: " ++ reason
      
      -- Monitor for possible refund for an extended period
      extendedResult <- timeout (7 * 24 * 3600) seconds $ do
        refundFact <- observe (RefundObservation sourceChain sourceAsset amount) on sourceChain
        let refundTxHash = getRefundTxHash refundFact
        processRefund amount sourceAsset sourceChain refundTxHash
        return $ BridgeRefunded { refundTxHash = refundTxHash }
      
      -- If no refund observed after extended period, report final failure
      case extendedResult of
        Some result -> return result
        None -> do
          -- Final failure - might need manual intervention
          emit $ "Bridge failed and no refund detected after extended monitoring"
          return $ BridgeFailed { reason = reason ++ " (no refund detected)" }
    
    BridgeTimedOut -> do
      -- For timeouts, continue monitoring for either success or refund
      emit "Bridge timed out, continuing to monitor for completion or refund"
      
      extendedRaceResult <- race
        -- Keep watching for mint for extended period
        (do
          mintFact <- timeout (48 * 3600) seconds $ 
            observe (MintObservation destChain destAsset amount) on destChain
          case mintFact of
            Some fact -> do
              deposit amount destAsset destChain
              return $ Left $ BridgeSuccess { txHash = extractTxHash fact }
            None -> return $ Left $ BridgeTimedOut)
        
        -- Keep watching for refund for extended period
        (do
          refundFact <- timeout (7 * 24 * 3600) seconds $ 
            observe (RefundObservation sourceChain sourceAsset amount) on sourceChain
          case refundFact of
            Some fact -> do
              let refundTxHash = getRefundTxHash fact
              processRefund amount sourceAsset sourceChain refundTxHash
              return $ Right $ BridgeRefunded { refundTxHash = refundTxHash }
            None -> return $ Right $ BridgeTimedOut)
      
      case extendedRaceResult of
        Left result -> return result
        Right result -> return result

-- Helper to extract transaction hash from fact
extractTxHash :: Fact -> Hash
extractTxHash fact = fact.txHash  -- Simplified; actual implementation would extract from fact payload
```

## Step 6: Register and Publish the Bridge Module

The developer registers the module in the content-addressable code system:

```haskell
import Core.CodeAddress

-- Store the bridge module
storeAndRegisterBridgeModule :: CodeRepository -> IO CodeHash
storeAndRegisterBridgeModule repo = do
  let moduleName = "ExternalBridges"
      moduleSource = "-- Full module source code here..."
  
  moduleHash <- hashModule moduleName [] moduleSource
  moduleDef <- CodeDefinition moduleHash moduleSource ModuleDef
  
  _ <- storeDefinition repo moduleDef
  registerName repo moduleName moduleHash
  
  -- Notify users that the module is available
  putStrLn $ "Bridge module published with hash: " ++ show moduleHash
  
  return moduleHash
```

## Step 7: Use the Bridge in a Program with Refund Handling

Other developers can now use the bridge module with comprehensive refund handling:

```haskell
-- Import the bridge module by name or content hash
import ExternalBridges

-- Cross-chain arbitrage with refund handling
crossChainArbitrageWithRefundHandling :: Amount -> Effect Profit
crossChainArbitrageWithRefundHandling amount = do
  -- 1. Bridge USDC from Ethereum to Celestia with refund handling
  bridgeResult <- ExternalBridges.bridgeTokenWithFallbacks "USDC" "USDC" "ethereum" "celestia" amount
  
  case bridgeResult of
    BridgeSuccess _ -> do
      -- 2. Continue with arbitrage on destination chain
      arbitrageOnDestination amount
      
    BridgeRefunded refundTxHash -> do
      -- Handle refund case - maybe try an alternative route or strategy
      emit $ "Bridge was refunded with tx: " ++ show refundTxHash
      -- Maybe try an alternative arbitrage path
      alternativeArbitrage amount
      
    BridgeFailed reason -> do
      emit $ "Bridge failed permanently: " ++ reason
      return 0  -- No profit
      
    BridgeTimedOut -> do
      emit "Bridge timed out even after extended monitoring"
      return 0  -- No profit

-- Arbitrage implementation on destination chain
arbitrageOnDestination :: Amount -> Effect Profit
arbitrageOnDestination amount = do
  -- Swap and arbitrage implementation
  -- ...
  return calculatedProfit

-- Alternative arbitrage path when primary route fails
alternativeArbitrage :: Amount -> Effect Profit
alternativeArbitrage amount = do
  -- Try different route
  -- ...
  return altProfit
```

## Real-World Example: USDC Bridge with Refund Handling

Here's a concrete example of implementing a bridge for USDC between Ethereum and Celestia with comprehensive refund handling:

```haskell
-- Safe USDC bridge that handles all possible outcomes
safeUSDCBridge :: Amount -> Effect (Either BridgeResult Amount)
safeUSDCBridge amount = do
  -- Use the enhanced bridge function with fallbacks
  bridgeResult <- bridgeTokenWithFallbacks "USDC" "USDC" "ethereum" "celestia" amount
  
  case bridgeResult of
    BridgeSuccess _ -> do
      -- Bridge succeeded - return success with the bridged amount
      return $ Right amount
      
    BridgeRefunded refundTxHash -> do
      -- Bridge refunded - return the refunded amount
      emit $ "Bridge refunded: " ++ show refundTxHash
      return $ Right amount
      
    _ -> do
      -- Bridge failed or timed out - return the failure
      return $ Left bridgeResult
```

## How This Handles External Bridge Failures

This implementation handles the following scenarios:

1. **Normal Success Path**: Tokens are locked on source chain and minted on destination chain
2. **Immediate Refund Path**: Bridge detects an issue and immediately refunds on source chain
3. **Delayed Refund Path**: Bridge attempts to process but eventually refunds after some time
4. **Extended Monitoring**: Continues to watch for either completion or refund for days/weeks
5. **Ultimate Timeout**: After exhaustive monitoring, reports final status for manual resolution

The content-addressable nature of the code ensures that users can trust the exact implementation they're using, and the bridge module can evolve over time with new versions without breaking existing programs.

This approach allows third-party developers to build robust bridge integrations that gracefully handle all potential failure modes, including extended downtime of external systems.