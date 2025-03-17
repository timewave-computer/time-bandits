# Example 004: Cross-Chain Yield Rotation Strategy in Time Bandits

Yield rotation strategy that automatically moves funds between two remote vaults on different chains to maximize returns. The strategy monitors yield rates, executes cross-chain transfers when advantageous, and handles all the complexities of cross-chain vault management using the formalized resource model.

## Yield Rotation Strategy Implementation

```haskell
module CrossChainYieldRotator where

import TimeBandits.Core.TEL
import ExternalBridges       -- For bridge functionality
import Erc4626Vault          -- For vault interactions
import CrossChainVault       -- From previous implementation
import TimeBandits.Resources.Formalized  -- For formalized resource model
import TimeBandits.Controllers.Interface -- For controller interface and labels

-- Configuration for a remote vault
type RemoteVaultConfig = {
  chain :: Timeline,              -- Chain where the vault is deployed
  vaultAddress :: Address,        -- Address of the vault contract
  assetAddress :: Address,        -- Address of the underlying asset
  name :: Text,                   -- Human-readable name for the vault
  minYieldAdvantage :: Decimal,   -- Minimum yield difference to trigger rotation (in percentage points)
  coolingPeriod :: Int,           -- Minimum time between rotations (in seconds)
  maxAllocation :: Decimal,       -- Maximum percentage of total assets to allocate to this vault (0.0-1.0)
  controllerType :: ControllerType -- Safe, Live, or Byzantine classification
}

-- The yield rotator state
type YieldRotatorState = {
  sourceChain :: Timeline,        -- The chain with the source/controller vault
  sourceVault :: Address,         -- Address of the source/controller vault
  remoteVaults :: [RemoteVaultConfig],  -- Configurations for remote vaults
  lastRotationTimestamp :: Int,   -- When the last rotation happened
  currentAllocations :: [(Address, Decimal)],  -- Current allocation percentages for each vault
  totalAssets :: Resource,        -- Total assets under management as formalized resource
  yieldHistory :: [(Int, [(Address, Decimal)])],  -- History of yields by timestamp and vault
  controllerLabels :: Map Timeline ControllerLabel, -- Controller labels for each chain
  commitments :: [Commitment]     -- Resource commitments for tracking
}

-- Define the resource model for stable assets (e.g., USDC)
resource StableCoin(quantity: Decimal) {
  resourceLogic = TokenLogic
  fungibilityDomain = "USDC"
  ephemeral = false
  metadata = { "decimals" = 6 }
}

-- Define the resource model for vault shares
resource VaultShare(quantity: Decimal, vaultAddress: Address, chain: Timeline) {
  resourceLogic = TokenLogic
  fungibilityDomain = "VaultShare:" <> toText vaultAddress
  ephemeral = false
  metadata = { 
    "vaultAddress" = toText vaultAddress,
    "chain" = toText chain,
    "decimals" = 18
  }
}

-- Initialize a new yield rotator with formalized resources
initYieldRotator :: 
  Timeline ->                  -- Source chain
  Address ->                   -- Source vault address
  [RemoteVaultConfig] ->       -- Remote vault configurations
  Effect YieldRotatorState
initYieldRotator sourceChain sourceVault remoteVaults = do
  -- Get current timestamp
  currentTime <- getCurrentTimestamp
  
  -- Get total assets
  rawTotalAssets <- Erc4626Vault.getTotalAssets sourceChain sourceVault
  
  -- Create formalized resource for the assets
  let assetResource = Resource {
    resourceLogic = TokenLogic,
    fungibilityDomain = "USDC",
    quantity = rawTotalAssets,
    metadata = encodeMetadata ("chain", sourceChain),
    ephemeral = false,
    nonce = generateNonce,
    nullifierPubKey = deriveNullifierKey sourceVault,
    randomnessSeed = generateSeed
  }
  
  -- Get controller for source chain
  sourceController <- getController sourceChain
  let sourceControllerLabel = sourceController.createControllerLabel
  
  -- Initialize controller labels for all chains
  controllerLabels <- foldM
    (\acc config -> do
      controller <- getController config.chain
      let label = controller.createControllerLabel
      return $ Map.insert config.chain label acc
    )
    (Map.singleton sourceChain sourceControllerLabel)
    remoteVaults
  
  -- Calculate resource commitment
  let commitment = hashCommitment assetResource
    
  -- Return initial state
  return YieldRotatorState {
    sourceChain = sourceChain,
    sourceVault = sourceVault,
    remoteVaults = remoteVaults,
    lastRotationTimestamp = currentTime,
    currentAllocations = [(sourceVault, 1.0)],  -- Start with all assets in source vault
    totalAssets = assetResource,
    yieldHistory = [(currentTime, [(sourceVault, 0.0)])],
    controllerLabels = controllerLabels,
    commitments = [commitment]
  }

-- Check if rotation is needed based on yields
shouldRotate :: YieldRotatorState -> Map Address Decimal -> Effect Bool
shouldRotate state currentYields = do
  -- Get current timestamp
  currentTime <- getCurrentTimestamp
  
  -- Check if cooling period has elapsed
  if currentTime - state.lastRotationTimestamp < (minimumCoolingPeriod state.remoteVaults)
    then return False
    else do
      -- Find highest yielding vault
      let (highestYieldVault, highestYield) = findHighestYield currentYields
      
      -- Find current vault with highest allocation
      let (currentVault, _) = findHighestAllocation state.currentAllocations
      
      -- Get current yield for that vault
      let currentYield = Map.findWithDefault 0.0 currentVault currentYields
      
      -- Calculate yield advantage
      let yieldAdvantage = highestYield - currentYield
      
      -- Find minimum required advantage in configs
      let minRequiredAdvantage = findMinYieldAdvantage state.remoteVaults highestYieldVault
      
      -- Decide if rotation is needed
      return (yieldAdvantage > minRequiredAdvantage && highestYieldVault /= currentVault)

-- Get current yields from all vaults
getCurrentYields :: YieldRotatorState -> Effect (Map Address Decimal)
getCurrentYields state = do
  -- Get yield for source vault
  sourceYield <- getVaultYield state.sourceChain state.sourceVault
  
  -- Get yield for each remote vault
  remoteYields <- forM state.remoteVaults $ \config -> do
    yield <- getVaultYield config.chain config.vaultAddress
    return (config.vaultAddress, yield)
  
  -- Combine yields into a map
  return $ Map.fromList $ (state.sourceVault, sourceYield) : remoteYields

-- Execute rotation to a target vault with formalized resources
executeRotation :: YieldRotatorState -> Address -> Timeline -> Effect YieldRotatorState
executeRotation state targetVault targetChain = do
  -- Get current timestamp
  currentTime <- getCurrentTimestamp
  
  -- Get source vault chain and address
  let (sourceVault, sourceAllocation) = findHighestAllocation state.currentAllocations
      sourceChain = getVaultChain state sourceVault
  
  -- Calculate amount to move (50% of current allocation)
  let sourceAmount = floor $ state.totalAssets.quantity * sourceAllocation * 0.5
  
  -- Create formalized resource for the amount to move
  let sourceResource = Resource {
        resourceLogic = state.totalAssets.resourceLogic,
        fungibilityDomain = state.totalAssets.fungibilityDomain,
        quantity = sourceAmount,
        metadata = state.totalAssets.metadata,
        ephemeral = false,
        nonce = generateNonce,
        nullifierPubKey = deriveNullifierKey sourceVault,
        randomnessSeed = generateSeed
      }
  
  -- Get controller labels for source and target chains
  let sourceControllerLabel = Map.findWithDefault 
                              (error "Missing source controller") 
                              sourceChain 
                              state.controllerLabels
      
      targetControllerLabel = Map.findWithDefault 
                              (error "Missing target controller") 
                              targetChain 
                              state.controllerLabels
  
  -- Update source controller label for cross-chain transfer
  let updatedSourceLabel = sourceControllerLabel {
        terminalController = targetControllerLabel.terminalController,
        affectingControllers = targetControllerLabel.terminalController : 
                              sourceControllerLabel.affectingControllers
      }
  
  -- Get current time map for temporal validation
  timeMap <- getCurrentTimeMap
  
  -- Perform dual validation
  validationResult <- validateCrossChainResource 
    (TransferEffect sourceResource)
    sourceResource
    timeMap
    updatedSourceLabel
  
  case validationResult of
    ValidationResult (TemporallyValid _) (AncestrallyValid _) -> do
      -- Withdraw from source vault, creating a nullifier
      let nullifier = hashNullifier sourceResource.nullifierPubKey sourceResource
      recordNullifier nullifier
      
      withdrawResult <- Erc4626Vault.withdrawFromVault
                          sourceChain
                          sourceVault
                          sourceAmount
                          0.01
                          updatedSourceLabel
      
      case withdrawResult of
        WithdrawSuccess receivedAmount _ -> do
          -- Track resource delta from source chain (negative)
          recordResourceDelta (Delta (-sourceAmount)) sourceChain
          
          -- Bridge assets to target chain
          bridgeResult <- ExternalBridges.bridgeTokenWithResource
                            receivedAmount
                            sourceChain
                            targetChain
                            updatedSourceLabel
          
          case bridgeResult of
            BridgeSuccess bridgedAmount _ -> do
              -- Deposit to target vault
              depositResult <- Erc4626Vault.depositToVault
                                targetChain
                                targetVault
                                bridgedAmount
                                0.01
                                updatedSourceLabel
              
              case depositResult of
                DepositSuccess sharesReceived _ -> do
                  -- Track resource delta in target chain (positive)
                  recordResourceDelta (Delta bridgedAmount.quantity) targetChain
                  
                  -- Update allocations
                  let remainingSourceAlloc = sourceAllocation * 0.5
                      targetAlloc = sourceAllocation * 0.5  -- Moved half the allocation
                      newAllocations = updateAllocations 
                                      state.currentAllocations
                                      sourceVault
                                      targetVault
                                      remainingSourceAlloc
                                      targetAlloc
                  
                  -- Calculate resource commitment for shares
                  let shareCommitment = hashCommitment sharesReceived
                  
                  -- Return updated state
                  return state {
                    lastRotationTimestamp = currentTime,
                    currentAllocations = newAllocations,
                    yieldHistory = (currentTime, allocationsToYields newAllocations) : state.yieldHistory,
                    commitments = shareCommitment : state.commitments
                  }
                
                _ -> return state  -- Keep current state on deposit failure
              
            _ -> return state  -- Keep current state on bridge failure
          
        _ -> return state  -- Keep current state on withdraw failure
    
    _ -> do
      -- Log validation failure
      logError $ "Cross-chain validation failed for resource transfer"
      return state  -- Keep current state on validation failure

-- Main yield rotation function
runYieldRotator :: YieldRotatorState -> Effect YieldRotatorState
runYieldRotator state = do
  -- Get current yields
  currentYields <- getCurrentYields state
  
  -- Check if rotation is needed
  rotationNeeded <- shouldRotate state currentYields
  
  if rotationNeeded
    then do
      -- Find highest yielding vault
      let (highestYieldVault, _) = findHighestYield currentYields
          targetChain = getVaultChain state highestYieldVault
      
      -- Execute rotation to highest yielding vault
      executeRotation state highestYieldVault targetChain
    else do
      -- No rotation needed, return current state
      return state

-- Helper function to get vault chain from config
getVaultChain :: YieldRotatorState -> Address -> Timeline
getVaultChain state vaultAddress
  | vaultAddress == state.sourceVault = state.sourceChain
  | otherwise = 
      case find (\config -> config.vaultAddress == vaultAddress) state.remoteVaults of
        Just config -> config.chain
        Nothing -> error $ "Unknown vault address: " ++ show vaultAddress

-- Helper for finding minimum yield advantage
findMinYieldAdvantage :: [RemoteVaultConfig] -> Address -> Decimal
findMinYieldAdvantage configs vaultAddress =
  case find (\config -> config.vaultAddress == vaultAddress) configs of
    Just config -> config.minYieldAdvantage
    Nothing -> error $ "Unknown vault address: " ++ show vaultAddress

-- Helper for finding minimum cooling period
minimumCoolingPeriod :: [RemoteVaultConfig] -> Int
minimumCoolingPeriod configs =
  case configs of
    [] -> 86400  -- Default: 1 day
    _  -> minimum $ map (.coolingPeriod) configs

-- Helper to find highest yielding vault
findHighestYield :: Map Address Decimal -> (Address, Decimal)
findHighestYield yields =
  case Map.toList yields of
    [] -> error "No yields provided"
    xs -> maximumBy (\a b -> compare (snd a) (snd b)) xs

-- Helper to find highest allocation
findHighestAllocation :: [(Address, Decimal)] -> (Address, Decimal)
findHighestAllocation allocations =
  case allocations of
    [] -> error "No allocations provided"
    xs -> maximumBy (\a b -> compare (snd a) (snd b)) xs

-- Update allocations when rotating
updateAllocations :: 
  [(Address, Decimal)] -> 
  Address ->            -- Source vault
  Address ->            -- Target vault
  Decimal ->            -- New source allocation
  Decimal ->            -- New target allocation
  [(Address, Decimal)]
updateAllocations allocations sourceVault targetVault sourceAlloc targetAlloc =
  let withoutSource = filter (\(addr, _) -> addr /= sourceVault) allocations
      withoutTarget = filter (\(addr, _) -> addr /= targetVault) withoutSource
      existingTarget = find (\(addr, _) -> addr == targetVault) allocations
      newTargetAlloc = case existingTarget of
                        Just (_, existing) -> existing + targetAlloc
                        Nothing -> targetAlloc
  in (sourceVault, sourceAlloc) : (targetVault, newTargetAlloc) : withoutTarget

-- Convert allocations to yield history format
allocationsToYields :: [(Address, Decimal)] -> [(Address, Decimal)]
allocationsToYields = id  -- In this case they're the same format

-- Get vault yield using the APY function from vault module
getVaultYield :: Timeline -> Address -> Effect Decimal
getVaultYield chain vaultAddress = do
  Erc4626Vault.getAnnualizedYield chain vaultAddress
```

## Main Loop Implementation and Integration with Cross-Chain Vault

```haskell
module YieldRotatorMain where

import TimeBandits.Core.TEL
import CrossChainYieldRotator
import CrossChainVault
import TimeBandits.Resources.Formalized
import TimeBandits.Controllers.Interface

-- Configuration for supported vaults
config :: [RemoteVaultConfig]
config = [
  RemoteVaultConfig {
    chain = "arbitrum",
    vaultAddress = "0xabcd1234...",  -- Arbitrum USDC vault
    assetAddress = "0x1234abcd...",  -- USDC on Arbitrum
    name = "Arbitrum USDC Vault",
    minYieldAdvantage = 0.5,         -- Need at least 0.5% higher yield to rotate
    coolingPeriod = 86400 * 3,       -- 3 days between rotations
    maxAllocation = 0.7,             -- Max 70% allocation
    controllerType = Live            -- Arbitrum is a L2 rollup (Live)
  },
  RemoteVaultConfig {
    chain = "optimism",
    vaultAddress = "0xfedc5678...",  -- Optimism USDC vault
    assetAddress = "0x5678fedc...",  -- USDC on Optimism
    name = "Optimism USDC Vault",
    minYieldAdvantage = 0.5,         -- Need at least 0.5% higher yield to rotate
    coolingPeriod = 86400 * 3,       -- 3 days between rotations
    maxAllocation = 0.7,             -- Max 70% allocation
    controllerType = Live            -- Optimism is a L2 rollup (Live)
  },
  RemoteVaultConfig {
    chain = "base",
    vaultAddress = "0x9876dcba...",  -- Base USDC vault
    assetAddress = "0xdcba9876...",  -- USDC on Base
    name = "Base USDC Vault",
    minYieldAdvantage = 0.7,         -- Need a bit more yield to rotate here
    coolingPeriod = 86400 * 5,       -- 5 days between rotations
    maxAllocation = 0.5,             -- Max 50% allocation
    controllerType = Live            -- Base is a L2 rollup (Live)
  }
]

-- Main program entry point
main :: Effect ()
main = do
  -- Set controller classifications for all chains
  setControllerType "ethereum" Safe      -- Ethereum is considered Safe
  setControllerType "arbitrum" Live      -- Arbitrum is Live
  setControllerType "optimism" Live      -- Optimism is Live
  setControllerType "base" Live          -- Base is Live
  
  -- Source chain and vault
  let sourceChain = "ethereum"
      sourceVault = "0x1324354657687980..."  -- Ethereum USDC vault

  -- Initialize rotator
  state <- initYieldRotator sourceChain sourceVault config
  
  -- Store state in persistent storage
  storeState "yield_rotator_state" state
  
  -- Start the main loop
  scheduleRecurring 86400 "yield_rotator_daily" $ do
    -- Retrieve current state
    currentState <- loadState "yield_rotator_state"
    
    -- Run one iteration
    newState <- runYieldRotator currentState
    
    -- Persist updated state
    storeState "yield_rotator_state" newState
    
    -- Log current allocations
    logAllocations newState

-- Log current allocations and yields
logAllocations :: YieldRotatorState -> Effect ()
logAllocations state = do
  -- Get current yields
  currentYields <- getCurrentYields state
  
  -- Log each allocation with yield
  forM_ state.currentAllocations $ \(vaultAddress, allocation) -> do
    let chain = getVaultChain state vaultAddress
        yield = Map.findWithDefault 0.0 vaultAddress currentYields
        vaultConfig = find (\c -> c.vaultAddress == vaultAddress) state.remoteVaults
        vaultName = case vaultConfig of
                      Just config -> config.name
                      Nothing -> if vaultAddress == state.sourceVault
                                then "Source Vault (Ethereum)"
                                else show vaultAddress
        
        controllerLabel = Map.findWithDefault 
                          (error "Missing controller label") 
                          chain state.controllerLabels
        
        controllerType = determineControllerType controllerLabel
      
    -- Log allocation with controller info
    logInfo $ "Vault: " <> vaultName <> 
              "\n  Chain: " <> chain <>
              "\n  Allocation: " <> show (allocation * 100) <> "%" <>
              "\n  Current APY: " <> show (yield * 100) <> "%" <>
              "\n  Controller: " <> controllerType
  
  -- Validate resource conservation
  validateResourceConservation state

-- Determine controller type for display
determineControllerType :: ControllerLabel -> Text
determineControllerType label =
  let controllerID = show label.terminalController
      isSource = label.creatingController == label.terminalController
      affectingCount = length label.affectingControllers
  in
    controllerID <> 
    (if isSource then " (Source)" else " (Remote)") <>
    " - Affected by " <> show affectingCount <> " controllers"

-- Validate that resources are conserved
validateResourceConservation :: YieldRotatorState -> Effect ()
validateResourceConservation state = do
  -- Get all resources across all chains
  resources <- getAllResources state
  
  -- Calculate total resources by fungibility domain
  let totals = foldl' addResourceQuantity Map.empty resources
  
  -- Get the original total from state
  let originalTotal = state.totalAssets.quantity
      originalDomain = state.totalAssets.fungibilityDomain
      currentTotal = Map.findWithDefault 0 originalDomain totals
  
  -- Allow for small rounding errors (0.01%)
  let delta = abs (originalTotal - currentTotal)
      tolerance = originalTotal * 0.0001
  
  -- Log validation result
  if delta <= tolerance
    then logInfo $ "Resource conservation validated: " <> 
                  show originalTotal <> " ≈ " <> show currentTotal
    else logError $ "Resource conservation violation detected: " <> 
                  "Expected " <> show originalTotal <> 
                  " but found " <> show currentTotal <> 
                  " (delta: " <> show delta <> ")"

-- Helper to add resource quantities by fungibility domain
addResourceQuantity :: Map Text Quantity -> Resource -> Map Text Quantity
addResourceQuantity acc resource =
  let domain = resource.fungibilityDomain
      qty = resource.quantity
      current = Map.findWithDefault 0 domain acc
  in Map.insert domain (current + qty) acc

-- Get all resources across all chains
getAllResources :: YieldRotatorState -> Effect [Resource]
getAllResources state = do
  -- Get resources from source vault
  sourceResources <- getVaultResources 
                      state.sourceChain 
                      state.sourceVault
  
  -- Get resources from each remote vault
  remoteResources <- foldM 
    (\acc config -> do
      resources <- getVaultResources config.chain config.vaultAddress
      return (acc ++ resources)
    )
    []
    state.remoteVaults
  
  -- Combine all resources
  return (sourceResources ++ remoteResources)

-- Get resources in a specific vault
getVaultResources :: Timeline -> Address -> Effect [Resource]
getVaultResources chain vaultAddress = do
  -- Get asset and share value
  assetValue <- Erc4626Vault.getTotalAssets chain vaultAddress
  
  -- Create resource
  let resource = Resource {
        resourceLogic = TokenLogic,
        fungibilityDomain = "USDC",
        quantity = assetValue,
        metadata = encodeMetadata [("chain", chain), ("vault", vaultAddress)],
        ephemeral = false,
        nonce = generateNonce,
        nullifierPubKey = deriveNullifierKey vaultAddress,
        randomnessSeed = generateSeed
      }
  
  return [resource]
```

## Handling Cross-Chain Recovery with Resource Nullifiers

```haskell
-- Recovery mechanism for stuck cross-chain transfers
recoverStuckTransfer :: YieldRotatorState -> Address -> Timeline -> Timeline -> Effect YieldRotatorState
recoverStuckTransfer state vaultAddress sourceChain targetChain = do
  -- Find transfer in the commitments
  let transferCommitment = findTransferCommitment 
                          state.commitments 
                          sourceChain 
                          targetChain
  
  case transferCommitment of
    Nothing -> do
      logError "No matching transfer commitment found"
      return state
    
    Just commitment -> do
      -- Get controller labels
      let sourceLabel = Map.findWithDefault 
                        (error "Missing source controller") 
                        sourceChain 
                        state.controllerLabels
          
          targetLabel = Map.findWithDefault 
                        (error "Missing target controller") 
                        targetChain 
                        state.controllerLabels
      
      -- Check controller status
      sourceStatus <- checkControllerStatus sourceLabel.terminalController
      targetStatus <- checkControllerStatus targetLabel.terminalController
      
      case (sourceStatus, targetStatus) of
        (ControllerActive, ControllerActive) ->
          -- Both controllers active, try normal recovery
          recoverActiveTransfer state commitment sourceChain targetChain
        
        (ControllerActive, ControllerHalted) ->
          -- Target controller halted, use backup controller
          recoverWithBackupController state commitment targetLabel.backupControllers
        
        (ControllerHalted, ControllerActive) ->
          -- Source controller halted, cancel transfer and reclaim funds
          recoverSourceHalted state commitment sourceLabel.backupControllers
        
        (ControllerHalted, ControllerHalted) ->
          -- Both controllers halted, use ultimate backup
          recoverBothHalted state commitment
          
        _ -> do
          logError "Unrecoverable controller state"
          return state

-- Recovery when both controllers are active
recoverActiveTransfer :: YieldRotatorState -> Commitment -> Timeline -> Timeline -> Effect YieldRotatorState
recoverActiveTransfer state commitment sourceChain targetChain = do
  -- Get the resource from commitment
  resource <- getResourceFromCommitment commitment
  
  -- Create new transfer with extended timeout
  bridgeResult <- ExternalBridges.retryBridgeTransfer
                    resource
                    sourceChain
                    targetChain
                    (extendedTimeout 3600)  -- 1 hour extended timeout
  
  case bridgeResult of
    BridgeSuccess bridgedAmount _ -> do
      -- Create new commitment for the bridged amount
      let newCommitment = hashCommitment bridgedAmount
      
      -- Return updated state with new commitment
      return state {
        commitments = newCommitment : 
                      filter (/= commitment) state.commitments
      }
    
    _ -> return state  -- Keep current state on bridge failure

-- Recovery using backup controller
recoverWithBackupController :: YieldRotatorState -> Commitment -> [ControllerID] -> Effect YieldRotatorState
recoverWithBackupController state commitment backupControllers = do
  -- Get the resource from commitment
  resource <- getResourceFromCommitment commitment
  
  -- Find first working backup controller
  workingBackup <- findWorkingController backupControllers
  
  case workingBackup of
    Nothing -> do
      logError "No working backup controllers available"
      return state
    
    Just backupController -> do
      -- Create recovery transaction through backup controller
      recoveryResult <- ExternalBridges.recoverThroughBackup
                          resource
                          backupController
                          (generateRecoveryProof commitment)
      
      case recoveryResult of
        RecoverySuccess recoveredAmount _ -> do
          -- Create new commitment for the recovered amount
          let newCommitment = hashCommitment recoveredAmount
          
          -- Return updated state with new commitment
          return state {
            commitments = newCommitment : 
                          filter (/= commitment) state.commitments
          }
        
        _ -> return state  -- Keep current state on recovery failure

-- Find the first working controller
findWorkingController :: [ControllerID] -> Effect (Maybe ControllerID)
findWorkingController [] = return Nothing
findWorkingController (controller:rest) = do
  status <- checkControllerStatus controller
  if status == ControllerActive
    then return (Just controller)
    else findWorkingController rest

-- Helper for extended timeout
extendedTimeout :: Int -> Int
extendedTimeout baseTimeout = baseTimeout * 3

-- Generate recovery proof from commitment
generateRecoveryProof :: Commitment -> Proof
generateRecoveryProof commitment = 
  createRecoveryProof commitment (getCurrentTime)

-- Check controller status
checkControllerStatus :: ControllerID -> Effect ControllerStatus
checkControllerStatus controllerID = do
  -- Check if controller is responsive
  isResponsive <- pingController controllerID
  
  if isResponsive
    then return ControllerActive
    else return ControllerHalted

-- Find a transfer commitment between chains
findTransferCommitment :: [Commitment] -> Timeline -> Timeline -> Maybe Commitment
findTransferCommitment commitments sourceChain targetChain =
  find (\c -> isTransferCommitment c sourceChain targetChain) commitments
```

## Sample Usage with Cross-Chain Vault

This yield rotator can be deployed in a Time Bandits account program as follows:

```haskell
-- Deploy the yield rotator as an account program
deployYieldRotator :: Amount -> Timeline -> Effect ProgramID
deployYieldRotator initialAmount sourceChain = do
  -- Create account program
  accountProgram <- createAccountProgram "CrossChainYieldRotator"
  
  -- Convert raw amount to formalized resource
  let usdcResource = Resource {
        resourceLogic = TokenLogic,
        fungibilityDomain = "USDC",
        quantity = initialAmount,
        metadata = encodeMetadata sourceChain,
        ephemeral = false,
        nonce = generateNonce,
        nullifierPubKey = deriveNullifierKey accountProgram.address,
        randomnessSeed = generateSeed
      }
  
  -- Perform initial deposit
  depositResult <- CrossChainVault.depositToVault 
                    sourceChain
                    "0x1324354657687980..."  -- Source vault address
                    usdcResource
  
  case depositResult of
    DepositSuccess shares _ -> do
      -- Initialize yield rotator
      state <- initYieldRotator 
                sourceChain 
                "0x1324354657687980..."  -- Source vault address
                config
      
      -- Store state in account program
      accountProgram.storeState "yield_rotator_state" state
      
      -- Set up recurring job
      accountProgram.scheduleRecurring 86400 "yield_rotator_daily" $ do
        -- Run one iteration
        currentState <- accountProgram.loadState "yield_rotator_state"
        newState <- runYieldRotator currentState
        accountProgram.storeState "yield_rotator_state" newState
      
      -- Return program ID
      return accountProgram.id
    
    _ -> throwError "Initial deposit failed"
```

This implementation demonstrates a sophisticated cross-chain yield rotation strategy that leverages the formalized resource model introduced in ADR_018. The strategy:

1. **Tracks resources formally**: Uses the formalized resource tuple model to represent assets, with proper commitments and nullifiers.
2. **Enforces conservation**: Validates that resources are conserved across all chains and operations.
3. **Uses controller labels**: Classifies controllers and maintains resource provenance across chains.
4. **Implements dual validation**: Performs both temporal and ancestral validation for cross-chain transfers.
5. **Handles controller failures**: Provides recovery mechanisms using backup controllers when chains halt.

By utilizing the formalized resource model, this yield rotator offers stronger security guarantees while still providing the flexibility needed for a dynamic yield optimization strategy.