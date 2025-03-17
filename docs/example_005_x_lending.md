# Exaple 005: Cross-Chain Lending Protocol with Unified Collateral and Risk Management

This implementation demonstrates a sophisticated DeFi lending protocol that operates across multiple blockchains, enabling users to borrow assets on one chain while using collateral deposited on another. The system maintains a unified risk and health factor assessment across all chains, ensuring proper collateralization without requiring latency-sensitive operations.

## Architectural Overview

The system coordinates these key components across multiple chains:

1. **Collateral Vaults**: Deployed on Ethereum, Arbitrum, Base, and Optimism
2. **Lending Pools**: Providing borrowable assets on each chain
3. **Risk Management System**: Tracking global user health factors across all chains
4. **Cross-Chain Debt Positions**: Allowing borrowing on one chain against collateral on another
5. **Liquidation Orchestrator**: Coordinating liquidations across chains when necessary
6. **Resource Conservation System**: Ensuring all assets are properly accounted for across chains using formalized resources

## Core Implementation

```haskell
module CrossChainLendingProtocol where

import TimeBandits.Core.TEL
import ExternalBridges
import PriceOracle
import RiskEngine
import TimeBandits.Resources.Formalized
import TimeBandits.Controllers.Interface

-- Supported asset types with resource formalization
type Asset = {
  symbol :: Text,
  address :: Address,
  decimals :: Int,
  chainId :: Timeline,
  oracleAddress :: Address,
  controllerType :: ControllerType  -- Safe, Live, or Byzantine
}

-- Formalized resource definition for cross-chain assets
data Resource = Resource
  { resourceLogic      :: Logic         -- Predicate controlling resource consumption
  , fungibilityDomain  :: Label         -- Asset type (e.g., "USDC")
  , quantity           :: Quantity      -- Amount
  , metadata           :: Value         -- Additional asset information
  , ephemeral          :: Bool          -- Whether existence must be verified
  , nonce              :: Nonce         -- Uniqueness identifier
  , nullifierPubKey    :: NullifierPK   -- For verifying consumption
  , randomnessSeed     :: Seed          -- For deriving randomness
  }

-- Controller label for tracking resource provenance
data ControllerLabel = ControllerLabel
  { creatingController  :: ControllerID  -- Original chain where asset was deposited
  , terminalController  :: ControllerID  -- Current chain where asset resides
  , affectingControllers :: [ControllerID]  -- History of controllers that affected this resource
  , backupControllers   :: [ControllerID]   -- Fallback controllers if terminal fails
  }

-- User's collateral position with resource formalization
type CollateralPosition = {
  user :: Address,
  resource :: Resource,
  controllerLabel :: ControllerLabel,
  chain :: Timeline,
  lastUpdated :: Int,
  utilizationRatio :: Decimal,  -- How much of this collateral is being utilized for borrowing
  commitment :: Commitment      -- Cryptographic commitment to the resource
}

-- User's borrowing position with resource formalization
type BorrowPosition = {
  user :: Address,
  resource :: Resource,
  controllerLabel :: ControllerLabel,
  chain :: Timeline,
  interestRate :: Decimal,
  lastAccrued :: Int,
  collateralSources :: [(Timeline, Resource, Commitment)],  -- Which collateral positions back this borrow
  commitment :: Commitment      -- Cryptographic commitment to the borrowed resource
}

-- User's overall position across all chains
type UserPosition = {
  user :: Address,
  collateralPositions :: [CollateralPosition],
  borrowPositions :: [BorrowPosition],
  healthFactor :: Decimal,
  totalResourceDelta :: Delta  -- Should always be zero for proper conservation
}

-- Create a formalized resource for a given asset
createAssetResource :: Asset -> Amount -> Address -> Resource
createAssetResource asset amount owner = Resource
  { resourceLogic = TokenLogic
  , fungibilityDomain = asset.symbol
  , quantity = amount
  , metadata = encodeMetadata asset
  , ephemeral = False
  , nonce = generateNonce
  , nullifierPubKey = deriveNullifierKey owner
  , randomnessSeed = generateSeed
  }

-- Create a controller label for a given chain
createControllerLabel :: Timeline -> ControllerLabel
createControllerLabel chain = ControllerLabel
  { creatingController = timelineToControllerID chain
  , terminalController = timelineToControllerID chain
  , affectingControllers = [timelineToControllerID chain]
  , backupControllers = determineBackupControllers chain
  }

-- Convert timeline to controller ID
timelineToControllerID :: Timeline -> ControllerID
timelineToControllerID timeline = ControllerID $ "chain:" <> timeline

-- Determine appropriate backup controllers based on chain security properties
determineBackupControllers :: Timeline -> [ControllerID]
determineBackupControllers chain = 
  case chain of
    "optimism" -> [timelineToControllerID "ethereum"]  -- L2 rollup fallback to L1
    "arbitrum" -> [timelineToControllerID "ethereum"]  -- L2 rollup fallback to L1
    "base" -> [timelineToControllerID "ethereum"]      -- L2 rollup fallback to L1
    _ -> []  -- Main chains have no fallback

-- Function to update a controller label when resource crosses chains
updateControllerLabel :: ControllerLabel -> Timeline -> ControllerLabel
updateControllerLabel label newChain = 
  let newController = timelineToControllerID newChain
  in label 
    { terminalController = newController
    , affectingControllers = newController : label.affectingControllers
    }
```

## Deposit and Withdrawal with Resource Formalization

```haskell
-- Deposit collateral with resource formalization and conservation
depositCollateral :: Address -> Asset -> Amount -> Timeline -> Effect CollateralPosition
depositCollateral userAddress asset amount chain = do
  -- Create formalized resource for this deposit
  let resource = createAssetResource asset amount userAddress
  let controllerLabel = createControllerLabel chain
  
  -- Calculate resource commitment
  let commitment = hashCommitment resource
  
  -- Verify the deposit on the blockchain (via Time Keeper)
  depositFact <- observeDeposit userAddress asset.address amount chain
  
  -- Record deposit with formalized resource
  position <- createCollateralPosition userAddress resource controllerLabel chain
  
  -- Ensure resource conservation (deposit adds to user's resources)
  recordResourceDelta (Delta amount) userAddress
  
  pure position

-- Withdraw with resource conservation and nullifier tracking
withdrawCollateral :: CollateralPosition -> Address -> Effect WithdrawalResult
withdrawCollateral position destinationAddress = do
  -- Create nullifier to prevent double-withdrawal
  let nullifier = hashNullifier position.resource.nullifierPubKey position.resource
  
  -- Record nullifier to mark resource as consumed
  recordNullifier nullifier
  
  -- Execute the withdrawal on chain
  withdrawResult <- executeWithdrawal 
    position.user 
    position.resource.fungibilityDomain 
    position.resource.quantity 
    position.chain 
    destinationAddress
  
  -- Ensure resource conservation (withdrawal subtracts from user's resources)
  recordResourceDelta (Delta (- position.resource.quantity)) position.user
  
  pure withdrawResult
```

## Cross-Chain Borrowing with Dual Validation

```haskell
-- Borrow against collateral on another chain, using dual validation
borrowCrossChain :: CollateralPosition -> Asset -> Amount -> Timeline -> Effect BorrowPosition
borrowCrossChain collateral borrowAsset borrowAmount borrowChain = do
  -- Verify collateral exists and is sufficient
  let collateralValue = getCollateralValue collateral
  let borrowValue = getBorrowValue borrowAsset borrowAmount
  
  -- Check if collateral covers the borrow with required margin
  unless (collateralValue >= borrowValue * requiredCollateralRatio) $
    throwError InsufficientCollateral
  
  -- Create resource for the borrowed tokens
  let borrowResource = createAssetResource borrowAsset borrowAmount collateral.user
  
  -- Update controller label to track cross-chain relationship
  let borrowControllerLabel = updateControllerLabel 
        collateral.controllerLabel 
        borrowChain
  
  -- Get current time map for temporal validation
  timeMap <- getCurrentTimeMap
  
  -- Perform dual validation
  validationResult <- validateCrossChainResource 
    (BorrowEffect collateral.resource borrowResource) 
    borrowResource 
    timeMap 
    borrowControllerLabel
  
  case validationResult of
    ValidationResult (TemporallyValid _) (AncestrallyValid _) -> do
      -- Both validations passed, proceed with borrowing
      -- Calculate borrow commitment
      let borrowCommitment = hashCommitment borrowResource
      
      -- Execute borrow on the target chain
      executeBorrow 
        collateral.user 
        borrowAsset.address 
        borrowAmount 
        borrowChain
      
      -- Create borrow position
      borrowPosition <- createBorrowPosition 
        collateral.user 
        borrowResource 
        borrowControllerLabel 
        borrowChain 
        [(collateral.chain, collateral.resource, collateral.commitment)]
      
      -- Update collateral utilization
      updateCollateralUtilization collateral borrowValue
      
      -- Ensure resource conservation (borrow adds to user's debt resources)
      recordResourceDelta (Delta borrowAmount) collateral.user
      
      pure borrowPosition
      
    _ -> throwError ValidationFailed

-- Calculate collateral-to-debt ratio across all chains with resource tracking
calculateHealthFactor :: Address -> Effect Decimal
calculateHealthFactor userAddress = do
  -- Get all user positions across chains
  userPosition <- getUserPosition userAddress
  
  -- Calculate total collateral value in USD
  totalCollateralValue <- sum <$> mapM getCollateralValue userPosition.collateralPositions
  
  -- Calculate total borrow value in USD
  totalBorrowValue <- sum <$> mapM getBorrowValue userPosition.borrowPositions
  
  -- Validate resource conservation across all positions
  unless (userPosition.totalResourceDelta == 0) $
    throwError ResourceConservationViolation
  
  -- Calculate and return health factor
  if totalBorrowValue == 0
    then pure infinity  -- No debt
    else pure (totalCollateralValue / totalBorrowValue)
```

## Liquidation with Controller Classification

```haskell
-- Liquidate an underwater position with controller fallback handling
liquidatePosition :: Address -> Effect LiquidationResult
liquidatePosition userAddress = do
  -- Get user's current position
  position <- getUserPosition userAddress
  
  -- Check if position is underwater
  healthFactor <- calculateHealthFactor userAddress
  unless (healthFactor < minimumHealthFactor) $
    throwError PositionNotLiquidatable
  
  -- Find most efficient liquidation path based on controller types
  liquidationPaths <- determineLiquidationPaths position
  
  -- Execute liquidation across chains
  results <- mapM executeLiquidationPath liquidationPaths
  
  -- Handle any Byzantine controllers specially
  byzantineResults <- handleByzantineControllers results
  
  -- Ensure final resource conservation
  validateFinalResourceBalance position
  
  pure $ aggregateLiquidationResults results byzantineResults

-- Determine liquidation paths considering controller types
determineLiquidationPaths :: UserPosition -> Effect [LiquidationPath]
determineLiquidationPaths position = do
  -- Get controller classifications for all chains involved
  controllers <- mapM getControllerClassification (getAllChains position)
  
  -- Prioritize Safe controllers over Live over Byzantine
  let prioritizedCollateral = sortCollateralByControllerType position.collateralPositions controllers
  let prioritizedBorrows = sortBorrowsByControllerType position.borrowPositions controllers
  
  -- Create optimal liquidation paths
  createLiquidationPaths prioritizedCollateral prioritizedBorrows
```

This implementation demonstrates how a cross-chain lending protocol can leverage the formalized resource model to ensure proper conservation of assets across multiple chains. The dual validation approach combines temporal validation via time maps with ancestral validation via controller labels, providing comprehensive security against various attack vectors. Controller classification enables the system to handle chains with different trust assumptions appropriately, enhancing overall robustness.