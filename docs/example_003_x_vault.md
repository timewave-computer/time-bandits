# Example 003: Implementing Cross-Chain Vaults in Time Bandits

This document outlines how a developer would implement a cross-chain vault system using Time Bandits, inspired by the Valence cross-chain vault architecture. In this system, a vault contract on one chain (source chain) manages assets that are actually deployed to a different vault on another chain (destination chain) to generate yield, utilizing the formalized resource model for tracking and validating cross-chain assets.

## Architecture Overview

The cross-chain vault system consists of:

1. **Source Chain Vault Contract**: Deployed on the source chain (e.g., Ethereum), this contract:
   - Accepts user deposits in a specific token (e.g., USDC)
   - Issues vault shares to depositors
   - Tracks the total assets under management
   - Communicates with the Time Bandits system for cross-chain operations
   - Maintains resource commitments for cross-chain assets

2. **Destination Chain Vault**: An externally-owned vault on the destination chain (e.g., Celestia) that:
   - Receives assets from the source chain via bridges
   - Generates yield from these assets
   - Can return assets to the source chain when withdrawals are requested
   - Validates resource conservation across chains

3. **Time Bandits Program**: A program that:
   - Observes deposit/withdrawal events on the source chain
   - Executes cross-chain transfers between source and destination vaults
   - Monitors yield generation on the destination chain
   - Updates the source chain vault with current value information
   - Enforces resource conservation laws using formalized resource model
   - Applies dual validation for cross-chain operations
   - Tracks resources using controller labels

## Step 1: Define the Smart Contract Architecture

First, let's outline the required smart contracts and their interfaces.

### Source Chain Vault (Ethereum)

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "@openzeppelin/contracts/token/ERC20/extensions/ERC4626.sol";
import "@openzeppelin/contracts/access/Ownable.sol";

contract CrossChainVault is ERC4626, Ownable {
    // Address of the Time Bandits bridge controller
    address public timeBanditsController;
    
    // Information about the destination vault
    string public destinationChain;
    string public destinationVault;
    
    // Tracking values
    uint256 public lastKnownTotalAssetsRemote;
    uint256 public lastUpdateTimestamp;
    
    // Resource tracking for cross-chain assets
    bytes32 public resourceCommitment;
    bytes32 public controllerLabel;
    
    // Events for resource tracking
    event ResourceCommitmentUpdated(bytes32 commitment);
    event ControllerLabelUpdated(bytes32 label);
    event CrossChainTransferInitiated(uint256 amount, bytes32 resourceId, bytes32 controllerLabel);
    event CrossChainTransferCompleted(uint256 amount, bytes32 resourceId, bytes32 controllerLabel);
    
    event DepositForBridge(address indexed sender, uint256 assets);
    event WithdrawRequestForBridge(address indexed receiver, uint256 assets);
    event RemoteAssetsUpdated(uint256 newTotalAssets, uint256 timestamp);
    
    constructor(
        IERC20 _asset,
        string memory _name,
        string memory _symbol,
        address _controller,
        string memory _destChain,
        string memory _destVault
    ) ERC4626(_asset) ERC20(_name, _symbol) Ownable(msg.sender) {
        timeBanditsController = _controller;
        destinationChain = _destChain;
        destinationVault = _destVault;
    }
    
    // Override totalAssets to include remote assets
    function totalAssets() public view override returns (uint256) {
        return lastKnownTotalAssetsRemote;
    }
    
    // Called by Time Bandits to update the vault with remote information
    function updateRemoteAssets(uint256 totalAssetsRemote, bytes32 newResourceCommitment, bytes32 newControllerLabel) external {
        require(msg.sender == timeBanditsController, "Unauthorized");
        lastKnownTotalAssetsRemote = totalAssetsRemote;
        lastUpdateTimestamp = block.timestamp;
        
        // Update resource tracking information
        resourceCommitment = newResourceCommitment;
        controllerLabel = newControllerLabel;
        
        emit ResourceCommitmentUpdated(newResourceCommitment);
        emit ControllerLabelUpdated(newControllerLabel);
    }
    
    // Override deposit to emit events for Time Bandits
    function deposit(uint256 assets, address receiver) public override returns (uint256) {
        uint256 shares = super.deposit(assets, receiver);
        emit DepositForBridge(receiver, assets);
        
        // Emit resource tracking event
        emit CrossChainTransferInitiated(assets, keccak256(abi.encode(assets, block.timestamp, receiver)), controllerLabel);
        
        return shares;
    }
    
    // Override withdraw to emit events for Time Bandits
    function withdraw(uint256 assets, address receiver, address owner) public override returns (uint256) {
        emit WithdrawRequestForBridge(receiver, assets);
        
        // Emit resource tracking event with a nullifier
        bytes32 nullifier = keccak256(abi.encode(assets, block.timestamp, owner));
        emit CrossChainTransferInitiated(assets, nullifier, controllerLabel);
        
        return super.withdraw(assets, receiver, owner);
    }
}
```

## Step 2: Implement the Time Bandits Program

Next, implement the Time Bandits program that will manage the cross-chain vault operations.

```haskell
module CrossChainVault where

import TimeBandits.Core.TEL
import TimeBandits.Adapters.EthereumAdapter
import TimeBandits.Adapters.CelestiaAdapter
import TimeBandits.Resources.Formalized
import TimeBandits.Controllers.Interface

-- Define resource models
resource StableCoin(quantity: Decimal) {
  resourceLogic = TokenLogic
  fungibilityDomain = "USDC"
  ephemeral = false
  metadata = { "decimals" = 6 }
}

resource VaultShares(quantity: Decimal) {
  resourceLogic = TokenLogic
  fungibilityDomain = "Vault:Shares"
  ephemeral = false
  metadata = { "decimals" = 18 }
}

-- State for cross-chain vault management
data CrossChainVaultState = CrossChainVaultState {
  -- Source chain info
  sourceChain :: Timeline,
  sourceVaultAddress :: Address,
  sourceAssetAddress :: Address,
  
  -- Destination chain info
  destChain :: Timeline,
  destVaultAddress :: Address,
  
  -- Asset tracking
  totalAssetsRemote :: Decimal,
  totalSharesIssued :: Decimal,
  lastUpdateTimestamp :: Int,
  
  -- Resource tracking
  resources :: [Resource],
  controllerLabels :: Map Timeline ControllerLabel,
  commitments :: [Commitment],
  nullifiers :: [Nullifier]
}

-- Initialize the cross-chain vault manager
initCrossChainVault :: 
  Timeline ->    -- Source chain (e.g., Ethereum)
  Address ->     -- Source vault address
  Address ->     -- Source asset address
  Timeline ->    -- Destination chain (e.g., Celestia)
  Address ->     -- Destination vault address
  Effect CrossChainVaultState
initCrossChainVault sourceChain sourceVault sourceAsset destChain destVault = do
  -- Get current time
  currentTime <- getCurrentTimestamp
  
  -- Initialize controller labels for both chains
  sourceController <- getController sourceChain
  destController <- getController destChain
  
  let sourceControllerLabel = sourceController.createControllerLabel
      destControllerLabel = destController.createControllerLabel
      
      -- For cross-chain operations, create a linked controller label
      crossChainLabel = sourceControllerLabel {
        terminalController = destControllerLabel.terminalController,
        affectingControllers = destControllerLabel.terminalController : 
                              sourceControllerLabel.affectingControllers
      }
  
  -- Store the controller labels
  let controllerLabels = Map.fromList [
        (sourceChain, sourceControllerLabel),
        (destChain, destControllerLabel)
      ]
  
  -- Return initial state
  return CrossChainVaultState {
    sourceChain = sourceChain,
    sourceVaultAddress = sourceVault,
    sourceAssetAddress = sourceAsset,
    destChain = destChain,
    destVaultAddress = destVault,
    totalAssetsRemote = 0,
    totalSharesIssued = 0,
    lastUpdateTimestamp = currentTime,
    resources = [],
    controllerLabels = controllerLabels,
    commitments = [],
    nullifiers = []
  }

-- Handle deposit event from source chain vault
handleDeposit :: CrossChainVaultState -> Address -> Decimal -> Effect CrossChainVaultState
handleDeposit state sender amount = do
  -- Get current time for resource creation
  currentTime <- getCurrentTimestamp
  
  -- Create a formalized resource for the deposit
  let depositResource = Resource {
        resourceLogic = TokenLogic,
        fungibilityDomain = "USDC",
        quantity = amount,
        metadata = encodeMetadata [
          ("chain", state.sourceChain), 
          ("sender", sender)
        ],
        ephemeral = false,
        nonce = generateNonce,
        nullifierPubKey = deriveNullifierKey state.sourceVaultAddress,
        randomnessSeed = generateSeed
      }
  
  -- Get controller labels for source and destination chains
  let sourceLabel = Map.findWithDefault 
                    (error "Missing source controller") 
                    state.sourceChain 
                    state.controllerLabels
      
      destLabel = Map.findWithDefault 
                  (error "Missing destination controller") 
                  state.destChain 
                  state.controllerLabels
      
      -- Create cross-chain controller label
      crossChainLabel = sourceLabel {
        terminalController = destLabel.terminalController,
        affectingControllers = destLabel.terminalController : 
                              sourceLabel.affectingControllers
      }
  
  -- Create resource commitment
  let commitment = hashCommitment depositResource
  
  -- Bridge the assets to the destination chain
  bridgeResult <- bridgeAssets 
                  state.sourceChain 
                  state.destChain
                  state.sourceAssetAddress
                  depositResource
                  crossChainLabel
  
  case bridgeResult of
    BridgeSuccess bridgedResource -> do
      -- Record resource delta on source chain (negative)
      recordResourceDelta (Delta (-amount)) state.sourceChain
      
      -- Record resource delta on destination chain (positive)
      recordResourceDelta (Delta amount) state.destChain
      
      -- Get current time map for temporal validation
      timeMap <- getCurrentTimeMap
      
      -- Perform dual validation
      validationResult <- validateCrossChainResource 
        (TransferEffect bridgedResource)
        bridgedResource
        timeMap
        crossChainLabel
      
      case validationResult of
        ValidationResult (TemporallyValid _) (AncestrallyValid _) -> do
          -- Update state with new resources
          let newResources = bridgedResource : state.resources
              newCommitments = commitment : state.commitments
              
          -- Update total assets on remote chain
          newTotalAssetsRemote <- getDestinationVaultBalance state
          
          -- Update source chain contract with new information
          updateSourceVaultInfo 
            state.sourceChain 
            state.sourceVaultAddress 
            newTotalAssetsRemote 
            commitment 
            crossChainLabel
          
          -- Return updated state
          return state {
            totalAssetsRemote = newTotalAssetsRemote,
            lastUpdateTimestamp = currentTime,
            resources = newResources,
            commitments = newCommitments
          }
        
        _ -> do
          -- Log validation failure
          logError $ "Cross-chain validation failed for deposit"
          return state
      
    BridgeFailure error -> do
      logError $ "Bridge failure: " <> error
      return state

-- Handle withdrawal request event from source chain
handleWithdrawRequest :: CrossChainVaultState -> Address -> Decimal -> Effect CrossChainVaultState
handleWithdrawRequest state receiver amount = do
  -- Current time for operations
  currentTime <- getCurrentTimestamp
  
  -- Find a resource to withdraw
  let resourceToWithdraw = findAvailableResource state.resources amount
  
  case resourceToWithdraw of
    Nothing -> do
      logError $ "Not enough resources available for withdrawal"
      return state
    
    Just resource -> do
      -- Get controller labels for destination and source chains
      let destLabel = Map.findWithDefault 
                      (error "Missing destination controller") 
                      state.destChain 
                      state.controllerLabels
          
          sourceLabel = Map.findWithDefault 
                        (error "Missing source controller") 
                        state.sourceChain 
                        state.controllerLabels
          
          -- Create cross-chain controller label (dest -> source)
          crossChainLabel = destLabel {
            terminalController = sourceLabel.terminalController,
            affectingControllers = sourceLabel.terminalController : 
                                  destLabel.affectingControllers
          }
      
      -- Create a nullifier for this resource
      let nullifier = hashNullifier resource.nullifierPubKey resource
      
      -- Check that nullifier hasn't been used
      if nullifier `elem` state.nullifiers
        then do
          logError $ "Resource already spent"
          return state
        else do
          -- Record the nullifier
          recordNullifier nullifier
          
          -- Bridge assets back to source chain
          bridgeResult <- bridgeAssets 
                          state.destChain 
                          state.sourceChain
                          state.destVaultAddress
                          resource
                          crossChainLabel
          
          case bridgeResult of
            BridgeSuccess bridgedResource -> do
              -- Record resource delta on destination chain (negative)
              recordResourceDelta (Delta (-amount)) state.destChain
              
              -- Record resource delta on source chain (positive)
              recordResourceDelta (Delta amount) state.sourceChain
              
              -- Get current time map for temporal validation
              timeMap <- getCurrentTimeMap
              
              -- Perform dual validation
              validationResult <- validateCrossChainResource 
                (TransferEffect bridgedResource)
                bridgedResource
                timeMap
                crossChainLabel
              
              case validationResult of
                ValidationResult (TemporallyValid _) (AncestrallyValid _) -> do
                  -- Update destination vault balance
                  newTotalAssetsRemote <- getDestinationVaultBalance state
                  
                  -- Create new commitment for the returned resource
                  let commitment = hashCommitment bridgedResource
                  
                  -- Update source chain contract with new information
                  updateSourceVaultInfo 
                    state.sourceChain 
                    state.sourceVaultAddress 
                    newTotalAssetsRemote 
                    commitment 
                    crossChainLabel
                  
                  -- Update state
                  return state {
                    totalAssetsRemote = newTotalAssetsRemote,
                    lastUpdateTimestamp = currentTime,
                    resources = resource : state.resources,
                    commitments = commitment : state.commitments,
                    nullifiers = nullifier : state.nullifiers
                  }
                
                _ -> do
                  -- Log validation failure
                  logError $ "Cross-chain validation failed for withdrawal"
                  return state
            
            BridgeFailure error -> do
              logError $ "Bridge failure: " <> error
              return state

## Step 3: Value Calculation and Yield Tracking

Enhance the program with value calculation and yield tracking capabilities:

```haskell
-- Track the performance of the cross-chain vault
trackVaultPerformance :: 
  Timeline ->           -- Source timeline 
  Timeline ->           -- Destination timeline
  Address ->            -- Source vault address
  Address ->            -- Destination vault address
  Int ->                -- Tracking interval in seconds
  Effect ()
trackVaultPerformance sourceChain destChain sourceVault destVault trackingInterval = do
  -- Initialize tracking data
  initialTotalAssets <- Erc4626Vault.getTotalAssets destChain destVault
  initialTimestamp <- getCurrentTimestamp
  
  -- Create initial checkpoint
  let initialCheckpoint = VaultCheckpoint {
    timestamp = initialTimestamp,
    totalAssets = initialTotalAssets,
    sharePrice = await calculateSharePrice()
  }
  
  -- Store the checkpoint
  storeCheckpoint sourceVault initialCheckpoint
  
  -- Start performance tracking loop
  while True $ do
    -- Wait for tracking interval
    wait trackingInterval seconds
    
    -- Get current values
    currentTotalAssets <- Erc4626Vault.getTotalAssets destChain destVault
    currentTimestamp <- getCurrentTimestamp
    currentSharePrice <- calculateSharePrice()
    
    -- Create new checkpoint
    let newCheckpoint = VaultCheckpoint {
      timestamp = currentTimestamp,
      totalAssets = currentTotalAssets,
      sharePrice = currentSharePrice
    }
    
    -- Store the checkpoint
    storeCheckpoint sourceVault newCheckpoint
    
    -- Calculate performance metrics
    lastCheckpoint <- getLastCheckpoint sourceVault
    dailyYield <- calculateDailyYield lastCheckpoint newCheckpoint
    annualizedYield <- calculateAnnualizedYield lastCheckpoint newCheckpoint
    
    -- Log performance
    emit $ "Cross-chain vault performance:"
    emit $ "- Total Assets: " ++ show currentTotalAssets
    emit $ "- Share Price: " ++ show currentSharePrice
    emit $ "- Daily Yield: " ++ show dailyYield ++ "%"
    emit $ "- Annualized Yield: " ++ show annualizedYield ++ "%"
    
    -- Update the source vault
    _ <- updateSourceVaultValue sourceChain destChain sourceVault destVault

-- Calculate the current share price
calculateSharePrice :: Effect Decimal
calculateSharePrice = do
  -- Implementation would get total assets and total supply, then divide
  totalAssets <- getTotalAssets()
  totalSupply <- getTotalSupply()
  return $ if totalSupply == 0 then 1.0 else totalAssets / totalSupply

-- Calculate yield between checkpoints
calculateDailyYield :: VaultCheckpoint -> VaultCheckpoint -> Effect Decimal
calculateDailyYield oldCp newCp = do
  -- Calculate percentage change in share price
  let priceDiff = newCp.sharePrice - oldCp.sharePrice
      priceChange = priceDiff / oldCp.sharePrice
      
      -- Calculate time difference in days
      timeDiffSeconds = newCp.timestamp - oldCp.timestamp
      timeDiffDays = timeDiffSeconds / (24 * 3600)
      
      -- Calculate daily yield rate
      dailyYield = (priceChange / timeDiffDays) * 100
      
  return dailyYield

-- Calculate annualized yield between checkpoints
calculateAnnualizedYield :: VaultCheckpoint -> VaultCheckpoint -> Effect Decimal
calculateAnnualizedYield oldCp newCp = do
  dailyYield <- calculateDailyYield oldCp newCp
  return $ dailyYield * 365
```

## Step 4: Use Case - Cross-Chain USDC Yield Strategy

Here's a concrete example of a USDC yield strategy using vaults across Ethereum and Celestia:

```haskell
-- Deploy and initialize a cross-chain USDC yield strategy
deployUsdcYieldStrategy :: Amount -> Effect ()
deployUsdcYieldStrategy initialDeposit = do
  -- Constants
  let ethereumChain = "ethereum"
      celestiaChain = "celestia"
      usdcAddress = "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"  -- USDC on Ethereum
      sourceVaultAddress = "0x1234...5678"  -- Our source vault on Ethereum
      destVaultAddress = "0xabcd...efgh"    -- High-yield vault on Celestia
      updateInterval = 3600                 -- Update value hourly
      trackingInterval = 86400              -- Track performance daily
  
  -- Initialize the cross-chain vault monitor
  initCrossChainVaultMonitor 
    ethereumChain celestiaChain sourceVaultAddress destVaultAddress usdcAddress updateInterval
  
  -- Start performance tracking
  fork $ trackVaultPerformance 
    ethereumChain celestiaChain sourceVaultAddress destVaultAddress trackingInterval
  
  -- Make initial deposit if provided
  when (initialDeposit > 0) $ do
    emit $ "Making initial deposit of " ++ show initialDeposit ++ " USDC"
    
    -- Approve the source vault to spend USDC
    _ <- approve ethereumChain usdcAddress sourceVaultAddress initialDeposit
    
    -- Deposit to the source vault
    depositResult <- callContract ethereumChain sourceVaultAddress "deposit" [initialDeposit, getAddress()]
    
    case depositResult of
      Left err -> 
        emit $ "Initial deposit failed: " ++ err
      Right txHash -> 
        emit $ "Initial deposit successful: " ++ show txHash
  
  emit "Cross-chain USDC yield strategy deployed and initialized"
```

## Step 5: Risk Management and Monitoring

Add risk management and monitoring capabilities:

```haskell
-- Monitor cross-chain vault for risks and anomalies
monitorVaultRisks :: 
  Timeline ->           -- Source timeline 
  Timeline ->           -- Destination timeline
  Address ->            -- Source vault address
  Address ->            -- Destination vault address
  Decimal ->            -- Maximum slippage allowed (as percentage)
  Decimal ->            -- Maximum value deviation allowed (as percentage)
  Effect ()
monitorVaultRisks sourceChain destChain sourceVault destVault maxSlippage maxDeviation = do
  -- Start monitoring loops
  fork $ monitorSlippage sourceChain destChain sourceVault destVault maxSlippage
  fork $ monitorValueDeviation sourceChain destChain sourceVault destVault maxDeviation
  fork $ monitorBridgeHealth sourceChain destChain
  
  emit "Risk monitoring initialized"

-- Monitor for excessive slippage
monitorSlippage :: Timeline -> Timeline -> Address -> Address -> Decimal -> Effect ()
monitorSlippage sourceChain destChain sourceVault destVault maxSlippage = do
  while True $ do
    -- Check source chain price
    sourcePrice <- Erc4626Vault.getExchangeRate sourceChain sourceVault
    
    -- Check destination chain price 
    destPrice <- Erc4626Vault.getExchangeRate destChain destVault
    
    -- Calculate slippage
    let expectedPrice = sourcePrice  -- Ideally these should be very close
        actualSlippage = abs((destPrice - expectedPrice) / expectedPrice) * 100
    
    -- Alert if slippage exceeds threshold
    when (actualSlippage > maxSlippage) $ do
      emit $ "ALERT: Excessive slippage detected!"
      emit $ "Source price: " ++ show sourcePrice
      emit $ "Destination price: " ++ show destPrice
      emit $ "Slippage: " ++ show actualSlippage ++ "% (threshold: " ++ show maxSlippage ++ "%)"
      
      -- Notify administrators
      notifyAdministrators "Excessive slippage detected in cross-chain vault"
    
    -- Check every hour
    wait 3600 seconds

-- Monitor for unexpected value deviations
monitorValueDeviation :: Timeline -> Timeline -> Address -> Address -> Decimal -> Effect ()
monitorValueDeviation sourceChain destChain sourceVault destVault maxDeviation = do
  while True $ do
    -- Get source vault's view of total assets
    sourceTotalAssets <- callContract sourceChain sourceVault "totalAssets" []
    
    -- Get actual destination vault total assets
    destTotalAssets <- Erc4626Vault.getTotalAssets destChain destVault
    
    -- Calculate deviation
    let deviation = abs((destTotalAssets - sourceTotalAssets) / sourceTotalAssets) * 100
    
    -- Alert if deviation exceeds threshold
    when (deviation > maxDeviation) $ do
      emit $ "ALERT: Value deviation detected!"
      emit $ "Source vault reports: " ++ show sourceTotalAssets
      emit $ "Actual destination value: " ++ show destTotalAssets
      emit $ "Deviation: " ++ show deviation ++ "% (threshold: " ++ show maxDeviation ++ "%)"
      
      -- Update the source vault
      _ <- updateSourceVaultValue sourceChain destChain sourceVault destVault
      
      -- Notify administrators
      notifyAdministrators "Value deviation detected in cross-chain vault"
    
    -- Check every 4 hours
    wait 14400 seconds

-- Monitor bridge health
monitorBridgeHealth :: Timeline -> Timeline -> Effect ()
monitorBridgeHealth sourceChain destChain = do
  while True $ do
    -- Perform a small test bridge transaction
    testResult <- testBridgeHealth sourceChain destChain
    
    case testResult of
      BridgeHealthy stats -> 
        emit $ "Bridge health check passed: " ++ stats
      
      BridgeIssue description -> do
        emit $ "ALERT: Bridge health issue detected: " ++ description
        notifyAdministrators $ "Bridge health issue: " ++ description
    
    -- Check daily
    wait 86400 seconds
```

## Conclusion

This implementation demonstrates how to create a cross-chain vault using the Time Bandits framework with formalized resource model. The key aspects of this implementation are:

1. **Resource formalization**: Assets are represented as formalized resources with explicit properties, ensuring conservation laws are upheld.

2. **Dual validation**: Both temporal and ancestral validation are performed for cross-chain operations, preventing invalid states.

3. **Controller labels**: Resources are tracked across chains using controller labels that identify which timelines control the resources.

4. **Resource commitments**: Cryptographic commitments to resources are maintained and verified, ensuring integrity.

5. **Nullifiers**: Resources that have been spent are tracked with nullifiers to prevent double-spending.

This architecture provides several security benefits:

- Strong guarantees about asset conservation across chains
- Protection against replay attacks using nullifiers
- Clear ownership tracking with controller labels
- Ability to recover from chain failures through backup controllers
- Auditability of resource movements through commitments

By using the Time Bandits formalized resource model, this cross-chain vault ensures that assets are properly tracked and validated as they move between chains, providing strong security guarantees for users' funds.
