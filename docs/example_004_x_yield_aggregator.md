# Example 004: Cross-Chain Yield Rotation Strategy in Time Bandits

Yield rotation strategy that automatically moves funds between two remote vaults on different chains to maximize returns. The strategy monitors yield rates, executes cross-chain transfers when advantageous, and handles all the complexities of cross-chain vault management.

## Yield Rotation Strategy Implementation

```haskell
module CrossChainYieldRotator where

import TimeBandits.Core.TEL
import ExternalBridges       -- For bridge functionality
import Erc4626Vault          -- For vault interactions
import CrossChainVault       -- From previous implementation

-- Configuration for a remote vault
type RemoteVaultConfig = {
  chain :: Timeline,              -- Chain where the vault is deployed
  vaultAddress :: Address,        -- Address of the vault contract
  assetAddress :: Address,        -- Address of the underlying asset
  name :: Text,                   -- Human-readable name for the vault
  minYieldAdvantage :: Decimal,   -- Minimum yield difference to trigger rotation (in percentage points)
  coolingPeriod :: Int,           -- Minimum time between rotations (in seconds)
  maxAllocation :: Decimal        -- Maximum percentage of total assets to allocate to this vault (0.0-1.0)
}

-- The yield rotator state
type YieldRotatorState = {
  sourceChain :: Timeline,        -- The chain with the source/controller vault
  sourceVault :: Address,         -- Address of the source/controller vault
  remoteVaults :: [RemoteVaultConfig],  -- Configurations for remote vaults
  lastRotationTimestamp :: Int,   -- When the last rotation happened
  currentAllocations :: [(Address, Decimal)],  -- Current allocation percentages for each vault
  totalAssets :: Amount,          -- Total assets under management
  yieldHistory :: [(Int, [(Address, Decimal)])]  -- History of yields by timestamp and vault
}

-- Initialize a new yield rotator
initYieldRotator :: 
  Timeline ->                  -- Source chain
  Address ->                   -- Source vault address
  [RemoteVaultConfig] ->       -- Remote vault configurations
  Effect YieldRotatorState
initYieldRotator sourceChain sourceVault remoteVaults = do
  -- Get current timestamp
  currentTime <- getCurrentTimestamp
  
  -- Get total assets
  totalAssets <- Erc4626Vault.getTotalAssets sourceChain sourceVault
  
  -- Initialize with empty allocations
  let initialAllocations = map (\v -> (v.vaultAddress, 0.0)) remoteVaults
      
  -- Create initial state
  let rotatorState = YieldRotatorState {
    sourceChain = sourceChain,
    sourceVault = sourceVault,
    remoteVaults = remoteVaults,
    lastRotationTimestamp = 0,  -- No rotations yet
    currentAllocations = initialAllocations,
    totalAssets = totalAssets,
    yieldHistory = []
  }
  
  -- Initialize monitoring for each remote vault
  forM_ remoteVaults $ \vault -> do
    fork $ initCrossChainVaultMonitor 
      sourceChain vault.chain sourceVault vault.vaultAddress vault.assetAddress 3600
    
    fork $ trackVaultPerformance 
      sourceChain vault.chain sourceVault vault.vaultAddress 86400
  
  -- Start yield monitoring and rotation process
  fork $ monitorYieldsAndRotate rotatorState
  
  -- Start risk monitoring
  fork $ monitorRisks rotatorState
  
  emit $ "Yield rotator initialized with " ++ show (length remoteVaults) ++ " remote vaults"
  
  return rotatorState

-- Monitor yields and rotate funds when advantageous
monitorYieldsAndRotate :: YieldRotatorState -> Effect ()
monitorYieldsAndRotate initialState = do
  -- Create mutable state reference
  stateRef <- makeStateRef initialState
  
  while True $ do
    -- Get current state
    state <- readStateRef stateRef
    
    -- Get current yields from all vaults
    vaultYields <- mapM getVaultYield state.remoteVaults
    
    -- Current timestamp
    currentTime <- getCurrentTimestamp
    
    -- Update yield history
    let yieldData = zip (map (.vaultAddress) state.remoteVaults) vaultYields
        newYieldHistory = (currentTime, yieldData) : state.yieldHistory
        updatedState = state { yieldHistory = take 30 newYieldHistory }  -- Keep last 30 data points
    
    -- Update state with yield history
    writeStateRef stateRef updatedState
    
    -- Log current yields
    logYields vaultYields state.remoteVaults
    
    -- Check if we should consider rotation
    let coolingPeriodElapsed = currentTime - state.lastRotationTimestamp > 
                               maximumCoolingPeriod state.remoteVaults
    
    when coolingPeriodElapsed $ do
      -- Find best vault
      let vaultYieldPairs = zip state.remoteVaults vaultYields
          (bestVault, bestYield) = maximumBy (\(_, y1) (_, y2) -> compare y1 y2) vaultYieldPairs
          
      -- Find current highest allocation vault
      let (currentVault, currentAllocation) = maximumBy (\(_, a1) (_, a2) -> compare a1 a2) state.currentAllocations
          currentVaultConfig = findVaultConfig currentVault state.remoteVaults
          currentVaultYield = findVaultYield currentVault vaultYieldPairs
      
      -- Check if yield difference is significant
      let yieldAdvantage = bestYield - currentVaultYield
          minAdvantageNeeded = bestVault.minYieldAdvantage
      
      -- Log analysis
      emit $ "Yield analysis:"
      emit $ "- Best vault: " ++ bestVault.name ++ " with " ++ show bestYield ++ "% APY"
      emit $ "- Current highest allocation: " ++ currentVaultConfig.name ++ 
             " with " ++ show currentVaultYield ++ "% APY"
      emit $ "- Yield advantage: " ++ show yieldAdvantage ++ "% (min needed: " ++ 
             show minAdvantageNeeded ++ "%)"
      
      -- If the best vault has significantly better yield, rotate funds
      when (yieldAdvantage > minAdvantageNeeded) $ do
        emit $ "Yield advantage sufficient, initiating rotation"
        
        -- Calculate optimal allocation
        newAllocations <- calculateOptimalAllocations vaultYieldPairs
        
        -- Execute the rotation
        rotationResult <- executeFundRotation state newAllocations
        
        case rotationResult of
          Right newState -> do
            emit "Rotation completed successfully"
            writeStateRef stateRef newState
          
          Left err -> do
            emit $ "Rotation failed: " ++ err
    
    -- Wait before next check (daily)
    wait 86400 seconds

-- Calculate optimal allocations based on yields and constraints
calculateOptimalAllocations :: 
  [(RemoteVaultConfig, Decimal)] ->  -- Vaults and their yields
  Effect [(Address, Decimal)]        -- New allocation percentages
calculateOptimalAllocations vaultYieldPairs = do
  -- Sort by yield (highest first)
  let sortedPairs = sortBy (\(_, y1) (_, y2) -> compare y2 y1) vaultYieldPairs
  
  -- Allocate to highest yield first, respecting max allocation constraints
  let (allocations, _) = foldl allocateOptimally ([], 100.0) sortedPairs
  
  return allocations
  where
    allocateOptimally (accAllocations, remainingPercentage) (vault, _) =
      -- Determine allocation for this vault
      let desiredAllocation = min remainingPercentage (vault.maxAllocation * 100.0)
          newAllocation = (vault.vaultAddress, desiredAllocation / 100.0)
          newRemaining = remainingPercentage - desiredAllocation
      in
        (newAllocation : accAllocations, newRemaining)

-- Execute fund rotation based on new allocations
executeFundRotation :: 
  YieldRotatorState ->               -- Current state
  [(Address, Decimal)] ->            -- New target allocations
  Effect (Either Text YieldRotatorState)  -- Updated state or error
executeFundRotation state newAllocations = do
  -- Get current assets in each vault
  currentVaultAssets <- mapM (getCurrentVaultAssets state) state.remoteVaults
  
  -- Calculate total assets across all vaults
  let totalAssets = sum (map snd currentVaultAssets)
  
  -- Calculate target assets for each vault
  let targetAssets = map (\(addr, alloc) -> (addr, floor (alloc * fromIntegral totalAssets))) newAllocations
  
  -- Calculate transfers needed
  let transfers = calculateTransfers (zip (map (.vaultAddress) state.remoteVaults) currentVaultAssets) targetAssets
  
  -- Execute transfers
  transferResults <- executeTransfers state transfers
  
  if all isRight transferResults
    then do
      -- All transfers succeeded, update state
      currentTime <- getCurrentTimestamp
      
      let newState = state {
        lastRotationTimestamp = currentTime,
        currentAllocations = newAllocations,
        totalAssets = totalAssets
      }
      
      -- Log the new allocations
      emit "New allocations after rotation:"
      forM_ newAllocations $ \(addr, alloc) -> do
        let vaultConfig = findVaultConfig addr state.remoteVaults
        emit $ "- " ++ vaultConfig.name ++ ": " ++ show (alloc * 100.0) ++ "%"
      
      return $ Right newState
    
    else do
      -- Some transfers failed
      let errors = concatMap extractError transferResults
      return $ Left $ "Transfer failures: " ++ errors

-- Execute a set of transfers between vaults
executeTransfers ::
  YieldRotatorState ->
  [(Address, Address, Amount)] ->  -- (from vault, to vault, amount)
  Effect [Either Text ()]
executeTransfers state transfers = do
  -- For each transfer, execute it
  forM transfers $ \(fromVault, toVault, amount) -> do
    -- Skip zero or negligible amounts
    if amount < 100  -- Assuming minimum meaningful amount
      then return $ Right ()
      else do
        -- Get vault configs
        let fromVaultConfig = findVaultConfig fromVault state.remoteVaults
            toVaultConfig = findVaultConfig toVault state.remoteVaults
        
        -- Execute cross-chain transfer
        emit $ "Transferring " ++ show amount ++ " from " ++ 
               fromVaultConfig.name ++ " to " ++ toVaultConfig.name
        
        transferResult <- transferBetweenVaults 
                           fromVaultConfig.chain 
                           toVaultConfig.chain
                           fromVault
                           toVault
                           fromVaultConfig.assetAddress
                           toVaultConfig.assetAddress
                           amount
        
        case transferResult of
          Right txHash -> do
            emit $ "Transfer completed: " ++ show txHash
            return $ Right ()
          
          Left err -> do
            emit $ "Transfer failed: " ++ err
            return $ Left err

-- Transfer funds between vaults (possibly cross-chain)
transferBetweenVaults ::
  Timeline ->    -- Source chain
  Timeline ->    -- Destination chain
  Address ->     -- Source vault
  Address ->     -- Destination vault
  Address ->     -- Source asset
  Address ->     -- Destination asset
  Amount ->      -- Amount to transfer
  Effect (Either Text Hash)
transferBetweenVaults sourceChain destChain sourceVault destVault sourceAsset destAsset amount = do
  -- Check if this is a cross-chain transfer
  if sourceChain == destChain
    then do
      -- Same chain transfer (simpler)
      sameChainTransfer sourceChain sourceVault destVault sourceAsset amount
    else do
      -- Cross-chain transfer (more complex)
      crossChainTransfer sourceChain destChain sourceVault destVault sourceAsset destAsset amount

-- Execute same-chain transfer between vaults
sameChainTransfer ::
  Timeline -> Address -> Address -> Address -> Amount -> Effect (Either Text Hash)
sameChainTransfer chain sourceVault destVault assetAddress amount = do
  -- Withdraw from source vault
  withdrawResult <- Erc4626Vault.withdrawFromVault chain sourceVault assetAddress amount 0.01
  
  case withdrawResult of
    WithdrawSuccess receivedAssets txHash -> do
      -- Deposit to destination vault
      depositResult <- Erc4626Vault.depositToVault chain destVault assetAddress receivedAssets 0.01
      
      case depositResult of
        DepositSuccess _ finalTxHash ->
          return $ Right finalTxHash
        
        _ -> return $ Left "Deposit to destination vault failed"
    
    _ -> return $ Left "Withdrawal from source vault failed"

-- Execute cross-chain transfer between vaults
crossChainTransfer ::
  Timeline -> Timeline -> Address -> Address -> Address -> Address -> Amount -> 
  Effect (Either Text Hash)
crossChainTransfer sourceChain destChain sourceVault destVault sourceAsset destAsset amount = do
  -- Determine bridge parameters
  bridgeParams <- getBridgeParameters sourceChain destChain sourceAsset
  
  -- Withdraw from source vault
  withdrawResult <- Erc4626Vault.withdrawFromVault 
                     sourceChain sourceVault sourceAsset amount 0.01
  
  case withdrawResult of
    WithdrawSuccess receivedAssets _ -> do
      -- Bridge assets to destination chain
      bridgeResult <- ExternalBridges.bridgeToken
                        sourceAsset destAsset
                        sourceChain destChain
                        receivedAssets
      
      case bridgeResult of
        BridgeSuccess txHash -> do
          -- Deposit to destination vault
          depositResult <- Erc4626Vault.depositToVault 
                            destChain destVault destAsset receivedAssets 0.01
          
          case depositResult of
            DepositSuccess _ finalTxHash ->
              return $ Right finalTxHash
            
            _ -> return $ Left "Deposit to destination vault failed"
        
        BridgeRefunded refundTxHash -> do
          -- Handle refund - attempt to return assets to source vault
          emit $ "Bridge refunded: " ++ show refundTxHash
          reDepositResult <- Erc4626Vault.depositToVault 
                              sourceChain sourceVault sourceAsset receivedAssets 0.01
          
          case reDepositResult of
            DepositSuccess _ _ -> 
              return $ Left "Bridge refunded, assets returned to source vault"
            
            _ -> return $ Left "Bridge refunded but failed to return assets to source vault"
        
        _ -> return $ Left "Bridge to destination chain failed"
    
    _ -> return $ Left "Withdrawal from source vault failed"

-- Monitor risks across the system
monitorRisks :: YieldRotatorState -> Effect ()
monitorRisks state = do
  -- Start various risk monitors
  fork $ monitorBridgeHealth state
  fork $ monitorVaultTVL state
  fork $ monitorYieldAnomalies state
  fork $ monitorAllocationDrift state
  
  emit "Risk monitoring initialized"

-- Monitor bridge health
monitorBridgeHealth :: YieldRotatorState -> Effect ()
monitorBridgeHealth state = do
  -- Get all unique chain pairs
  let chainPairs = getUniqueBridgePairs state.remoteVaults
  
  -- Monitor each bridge
  forM_ chainPairs $ \(chain1, chain2) -> do
    fork $ do
      while True $ do
        -- Test small bridge transaction in both directions
        health1to2 <- testBridgeHealth chain1 chain2
        health2to1 <- testBridgeHealth chain2 chain1
        
        -- Log and alert on issues
        case (health1to2, health2to1) of
          (BridgeHealthy _, BridgeHealthy _) ->
            emit $ "Bridge between " ++ chain1 ++ " and " ++ chain2 ++ " is healthy"
          
          (BridgeIssue desc1, _) -> do
            emit $ "ALERT: Bridge from " ++ chain1 ++ " to " ++ chain2 ++ " has issues: " ++ desc1
            notifyAdministrators $ "Bridge issue: " ++ desc1
          
          (_, BridgeIssue desc2) -> do
            emit $ "ALERT: Bridge from " ++ chain2 ++ " to " ++ chain1 ++ " has issues: " ++ desc2
            notifyAdministrators $ "Bridge issue: " ++ desc2
        
        -- Check weekly
        wait (7 * 86400) seconds

-- Monitor vault TVL for anomalies
monitorVaultTVL :: YieldRotatorState -> Effect ()
monitorVaultTVL state = do
  -- For each vault, monitor TVL
  forM_ state.remoteVaults $ \vault -> do
    fork $ do
      -- Get initial TVL
      initialTVL <- Erc4626Vault.getTotalAssets vault.chain vault.vaultAddress
      
      -- Store it
      tvlRef <- makeStateRef initialTVL
      
      while True $ do
        -- Get current TVL
        currentTVL <- Erc4626Vault.getTotalAssets vault.chain vault.vaultAddress
        
        -- Get last TVL
        lastTVL <- readStateRef tvlRef
        
        -- Calculate change
        let tvlChange = abs(fromIntegral (currentTVL - lastTVL) / fromIntegral lastTVL * 100.0)
        
        -- Alert on significant changes
        when (tvlChange > 20.0) $ do  -- 20% change threshold
          emit $ "ALERT: Significant TVL change in " ++ vault.name
          emit $ "Previous TVL: " ++ show lastTVL
          emit $ "Current TVL: " ++ show currentTVL
          emit $ "Change: " ++ show tvlChange ++ "%"
          
          notifyAdministrators $ "Significant TVL change in " ++ vault.name ++ ": " ++ show tvlChange ++ "%"
        
        -- Update reference
        writeStateRef tvlRef currentTVL
        
        -- Check daily
        wait 86400 seconds

-- Monitor yield anomalies
monitorYieldAnomalies :: YieldRotatorState -> Effect ()
monitorYieldAnomalies state = do
  -- Create a reference to track yield history
  yieldHistoryRef <- makeStateRef []
  
  while True $ do
    -- For each vault
    forM_ state.remoteVaults $ \vault -> do
      -- Get current yield
      currentYield <- getVaultYield vault
      
      -- Get yield history
      yieldHistory <- readStateRef yieldHistoryRef
      
      -- Add to history
      let newHistory = updateYieldHistory vault.vaultAddress currentYield yieldHistory
      writeStateRef yieldHistoryRef newHistory
      
      -- Calculate moving average
      let movingAvg = calculateMovingAverage vault.vaultAddress newHistory
          stdDev = calculateStdDev vault.vaultAddress newHistory
      
      -- Check for anomalies
      let zScore = (currentYield - movingAvg) / stdDev
      
      when (abs zScore > 3.0) $ do  -- 3 standard deviations
        emit $ "ALERT: Yield anomaly detected for " ++ vault.name
        emit $ "Current yield: " ++ show currentYield ++ "%"
        emit $ "Average yield: " ++ show movingAvg ++ "%"
        emit $ "Z-score: " ++ show zScore
        
        notifyAdministrators $ "Yield anomaly in " ++ vault.name ++ " (z-score: " ++ show zScore ++ ")"
    
    -- Check daily
    wait 86400 seconds

-- Monitor allocation drift from target
monitorAllocationDrift :: YieldRotatorState -> Effect ()
monitorAllocationDrift state = do
  while True $ do
    -- Get current state reference
    stateRef <- makeStateRef state
    currentState <- readStateRef stateRef
    
    -- Get current allocations
    currentVaultAssets <- mapM (getCurrentVaultAssets currentState) currentState.remoteVaults
    let totalAssets = sum (map snd currentVaultAssets)
    
    -- Calculate current allocation percentages
    let currentAllocationPercentages = map (\(addr, assets) -> 
                                           (addr, fromIntegral assets / fromIntegral totalAssets))
                                       currentVaultAssets
    
    -- Compare with target allocations
    forM_ currentState.currentAllocations $ \(addr, targetAlloc) -> do
      let vaultConfig = findVaultConfig addr currentState.remoteVaults
          currentAlloc = findAllocation addr currentAllocationPercentages
          driftPercentage = abs(currentAlloc - targetAlloc) * 100.0
      
      when (driftPercentage > 10.0) $ do  -- 10% drift threshold
        emit $ "ALERT: Allocation drift detected for " ++ vaultConfig.name
        emit $ "Target allocation: " ++ show (targetAlloc * 100.0) ++ "%"
        emit $ "Current allocation: " ++ show (currentAlloc * 100.0) ++ "%"
        emit $ "Drift: " ++ show driftPercentage ++ "%"
        
        notifyAdministrators $ "Allocation drift in " ++ vaultConfig.name ++ ": " ++ show driftPercentage ++ "%"
    
    -- Check weekly
    wait (7 * 86400) seconds

-- Helper functions

-- Get the current yield rate for a vault
getVaultYield :: RemoteVaultConfig -> Effect Decimal
getVaultYield vault = do
  -- This would typically fetch the current annualized yield
  -- Could be calculated from the vault's share price changes over time
  sharePrice1 <- Erc4626Vault.getExchangeRate vault.chain vault.vaultAddress
  
  -- Wait a short time and check again to calculate an instant rate
  wait 3600 seconds  -- 1 hour
  
  sharePrice2 <- Erc4626Vault.getExchangeRate vault.chain vault.vaultAddress
  
  -- Calculate hourly rate and annualize
  let hourlyRate = (sharePrice2 / sharePrice1) - 1.0
      annualizedRate = hourlyRate * 8760 * 100.0  -- 8760 hours in a year, convert to percentage
  
  return annualizedRate

-- Get current assets in a specific vault
getCurrentVaultAssets :: YieldRotatorState -> RemoteVaultConfig -> Effect (Address, Amount)
getCurrentVaultAssets state vault = do
  assets <- Erc4626Vault.getTotalAssets vault.chain vault.vaultAddress
  return (vault.vaultAddress, assets)

-- Calculate transfers needed to reach target allocations
calculateTransfers :: 
  [(Address, Amount)] ->  -- Current assets per vault
  [(Address, Amount)] ->  -- Target assets per vault
  [(Address, Address, Amount)]  -- (from vault, to vault, amount)
calculateTransfers currentAssets targetAssets = do
  -- Calculate excess and deficit for each vault
  let excessDeficit = map calculateExcessDeficit (zip currentAssets targetAssets)
      
      -- Separate into surpluses and deficits
      surpluses = filter (\(_, amount) -> amount > 0) excessDeficit
      deficits = filter (\(_, amount) -> amount < 0) excessDeficit
      
  -- Match surpluses to deficits (greedy algorithm)
  matchSurplusesToDeficits surpluses deficits
  where
    calculateExcessDeficit ((addr, current), (_, target)) = (addr, current - target)
    
    matchSurplusesToDeficits [] _ = []
    matchSurplusesToDeficits _ [] = []
    matchSurplusesToDeficits ((sAddr, sAmount):ss) ((dAddr, dAmount):ds)
      | sAmount + dAmount >= 0 =  -- Surplus covers deficit with excess
          (sAddr, dAddr, abs dAmount) : matchSurplusesToDeficits [(sAddr, sAmount + dAmount)] ds
      | otherwise =  -- Deficit partially covered
          (sAddr, dAddr, sAmount) : matchSurplusesToDeficits ss [(dAddr, dAmount + sAmount)]

-- Find a vault configuration by address
findVaultConfig :: Address -> [RemoteVaultConfig] -> RemoteVaultConfig
findVaultConfig addr configs = 
  head $ filter (\v -> v.vaultAddress == addr) configs

-- Find a vault's yield from the pairs list
findVaultYield :: Address -> [(RemoteVaultConfig, Decimal)] -> Decimal
findVaultYield addr pairs =
  snd $ head $ filter (\(v, _) -> v.vaultAddress == addr) pairs

-- Find allocation for a vault
findAllocation :: Address -> [(Address, Decimal)] -> Decimal
findAllocation addr allocs =
  snd $ head $ filter (\(a, _) -> a == addr) allocs

-- Get the maximum cooling period from all vault configs
maximumCoolingPeriod :: [RemoteVaultConfig] -> Int
maximumCoolingPeriod configs = maximum $ map (.coolingPeriod) configs

-- Get unique bridge pairs from vault configs
getUniqueBridgePairs :: [RemoteVaultConfig] -> [(Timeline, Timeline)]
getUniqueBridgePairs configs = do
  let chains = map (.chain) configs
  [(a, b) | a <- chains, b <- chains, a < b]  -- Use < to avoid duplicates

-- Update yield history for a specific vault
updateYieldHistory :: 
  Address ->             -- Vault address
  Decimal ->             -- Current yield
  [(Int, [(Address, Decimal)])] ->  -- Existing history
  [(Int, [(Address, Decimal)])]    -- Updated history
updateYieldHistory addr yield [] = do
  -- No history yet, create first entry
  currentTime <- getCurrentTimestamp
  [(currentTime, [(addr, yield)])]
  
updateYieldHistory addr yield ((timestamp, entries):rest) = do
  -- Update or add to existing entry
  let updatedEntries = updateOrAddEntry addr yield entries
  (timestamp, updatedEntries) : rest
  where
    updateOrAddEntry a y [] = [(a, y)]
    updateOrAddEntry a y ((addr', y'):rest)
      | addr' == a = (addr', y) : rest  -- Update existing
      | otherwise = (addr', y') : updateOrAddEntry a y rest  -- Keep looking

-- Calculate moving average for a vault's yield
calculateMovingAverage :: 
  Address ->                        -- Vault address
  [(Int, [(Address, Decimal)])] ->  -- Yield history
  Decimal                           -- Moving average
calculateMovingAverage addr history = do
  -- Extract yields for this address
  let yields = extractYields addr history
  
  -- Calculate average
  if null yields
    then 0.0  -- No data
    else sum yields / fromIntegral (length yields)
  where
    extractYields a h = [y | (_, entries) <- h, (addr', y) <- entries, addr' == a]

-- Calculate standard deviation for a vault's yield
calculateStdDev :: 
  Address ->                        -- Vault address
  [(Int, [(Address, Decimal)])] ->  -- Yield history
  Decimal                           -- Standard deviation
calculateStdDev addr history = do
  -- Extract yields for this address
  let yields = extractYields addr history
  
  -- Calculate standard deviation
  if null yields || length yields < 2
    then 1.0  -- No data or not enough data, return a default
    else do
      let mean = sum yields / fromIntegral (length yields)
          squaredDiffs = map (\y -> (y - mean) ^ 2) yields
          variance = sum squaredDiffs / fromIntegral (length yields - 1)
      sqrt variance
  where
    extractYields a h = [y | (_, entries) <- h, (addr', y) <- entries, addr' == a]

-- Log current yields
logYields :: [Decimal] -> [RemoteVaultConfig] -> Effect ()
logYields yields configs = do
  emit "Current vault yields:"
  forM_ (zip configs yields) $ \(vault, yield) -> do
    emit $ "- " ++ vault.name ++ ": " ++ show yield ++ "% APY"

-- Create a state reference
makeStateRef :: a -> Effect (IORef a)
makeStateRef initialValue = do
  -- In real implementation, this would create a mutable reference
  newIORef initialValue

-- Read from a state reference
readStateRef :: IORef a -> Effect a
readStateRef ref = do
  -- In real implementation, this would read from a mutable reference
  readIORef ref

-- Write to a state reference
writeStateRef :: IORef a -> a -> Effect ()
writeStateRef ref newValue = do
  -- In real implementation, this would update a mutable reference
  writeIORef ref newValue

-- Check if a result is Right
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

-- Extract error from a result
extractError :: Either Text a -> Text
extractError (Right _) = ""
extractError (Left err) = err

-- Notify administrators
notifyAdministrators :: Text -> Effect ()
notifyAdministrators message = do
  -- In real implementation, this would send alerts through appropriate channels
  emit $ "ADMIN NOTIFICATION: " ++ message
```

## Usage Example: Deploying the Yield Rotator Between Two Vaults

```haskell
-- Deploy a yield rotator between an Ethereum and a Arbitrum vault
deployEthereumArbitrumYieldRotator :: Effect YieldRotatorState
deployEthereumArbitrumYieldRotator = do
  -- Set up source vault on Ethereum
  let sourceChain = "ethereum"
      sourceVault = "0x1234...5678"  -- Source vault address
      
      -- Ethereum vault config
      ethereumVault = RemoteVaultConfig {
        chain = "ethereum",
        vaultAddress = "0xabcd...efgh",  -- Ethereum yield vault
        assetAddress = "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48",  -- USDC on Ethereum
        name = "Ethereum Aave USDC Vault",
        minYieldAdvantage = 0.5,  -- Require 0.5% better yield to rotate
        coolingPeriod = 7 * 86400,  -- 7 days between rotations
        maxAllocation = 0.8  -- Max 80% allocation
      }
      
      -- Arbitrum vault config
      ArbitrumVault = RemoteVaultConfig {
        chain = "Arbitrum",
        vaultAddress = "0x9876...5432",  -- Arbitrum yield vault
        assetAddress = "0xdcba...9876",  -- USDC on Arbitrum
        name = "Arbitrum USDC Yield Vault",
        minYieldAdvantage = 0.5,  -- Require 0.5% better yield to rotate
        coolingPeriod = 7 * 86400,  -- 7 days between rotations
        maxAllocation = 0.8  -- Max 80% allocation
      }
  
  -- Initialize the yield rotator
  rotator <- initYieldRotator sourceChain sourceVault [ethereumVault, ArbitrumVault]
  
  -- Log initialization 
  emit "Ethereum-Arbitrum yield rotator deployed"