# Exaple 005: Cross-Chain Lending Protocol with Unified Collateral and Risk Management

This implementation demonstrates a sophisticated DeFi lending protocol that operates across multiple blockchains, enabling users to borrow assets on one chain while using collateral deposited on another. The system maintains a unified risk and health factor assessment across all chains, ensuring proper collateralization without requiring latency-sensitive operations.

## Architectural Overview

The system coordinates these key components across multiple chains:

1. **Collateral Vaults**: Deployed on Ethereum, Arbitrum, Base, and Optimism
2. **Lending Pools**: Providing borrowable assets on each chain
3. **Risk Management System**: Tracking global user health factors across all chains
4. **Cross-Chain Debt Positions**: Allowing borrowing on one chain against collateral on another
5. **Liquidation Orchestrator**: Coordinating liquidations across chains when necessary

## Core Implementation

```haskell
module CrossChainLendingProtocol where

import TimeBandits.Core.TEL
import ExternalBridges
import PriceOracle
import RiskEngine

-- Supported asset types
type Asset = {
  symbol :: Text,
  address :: Address,
  decimals :: Int,
  chainId :: Timeline,
  oracleAddress :: Address
}

-- User's collateral position
type CollateralPosition = {
  user :: Address,
  asset :: Asset,
  amount :: Amount,
  chain :: Timeline,
  lastUpdated :: Int,
  utilizationRatio :: Decimal  -- How much of this collateral is being utilized for borrowing
}

-- User's borrowing position
type BorrowPosition = {
  user :: Address,
  asset :: Asset,
  amount :: Amount,
  chain :: Timeline,
  interestRate :: Decimal,
  lastAccrued :: Int,
  collateralSources :: [(Timeline, Asset, Amount)]  -- Which collateral positions back this borrow
}

-- User's overall position across all chains
type UserPosition = {
  user :: Address,
  collateralPositions :: [CollateralPosition],
  borrowPositions :: [BorrowPosition],
  healthFactor :: Decimal,
  lastCalculated :: Int
}

-- Liquidation threshold configuration
type LiquidationConfig = {
  asset :: Asset,
  ltv :: Decimal,  -- Loan to Value ratio
  liquidationThreshold :: Decimal,
  liquidationBonus :: Decimal,
  borrowFactor :: Decimal  -- How this asset is weighted when borrowed
}

-- Protocol configuration
type ProtocolConfig = {
  supportedAssets :: [Asset],
  liquidationConfigs :: [LiquidationConfig],
  minHealthFactor :: Decimal,
  liquidationHealthFactor :: Decimal,
  interestRateModels :: [(Asset, InterestRateModel)],
  treasuryAddress :: Address,
  operatorAddress :: Address,
  bridges :: [Bridge],
  updateInterval :: Int
}

-- Interest rate model
data InterestRateModel = 
  LinearModel { 
    baseRate :: Decimal, 
    slopeRate :: Decimal, 
    optimalUtilization :: Decimal 
  }
  | JumpRateModel { 
    baseRate :: Decimal, 
    jumpRate :: Decimal, 
    jumpUtilization :: Decimal, 
    maxRate :: Decimal 
  }

-- Chain-specific pool data
type PoolData = {
  chain :: Timeline,
  assets :: [(Asset, Amount, Amount)],  -- (Asset, totalSupply, totalBorrow)
  utilizationRates :: [(Asset, Decimal)],
  interestRates :: [(Asset, Decimal)]
}

-- Protocol state
type ProtocolState = {
  config :: ProtocolConfig,
  userPositions :: [UserPosition],
  pools :: [PoolData],
  liquidationQueue :: [LiquidationEvent],
  lastGlobalUpdate :: Int,
  totalValueLocked :: Amount,
  pendingOperations :: [PendingOperation]
}

-- Liquidation event
type LiquidationEvent = {
  user :: Address,
  borrowPosition :: BorrowPosition,
  collateralToLiquidate :: [(Timeline, Asset, Amount)],
  liquidator :: Maybe Address,
  healthFactorAtTrigger :: Decimal,
  timestamp :: Int,
  status :: LiquidationStatus
}

-- Status of liquidation
data LiquidationStatus = 
  Pending | 
  InProgress | 
  Completed | 
  Failed Text | 
  Cancelled

-- Pending cross-chain operation
data PendingOperation = 
  CollateralUpdate { 
    operationId :: Text, 
    user :: Address, 
    chain :: Timeline, 
    asset :: Asset, 
    amount :: Amount, 
    isIncrease :: Bool,
    startTime :: Int
  }
  | BorrowUpdate { 
    operationId :: Text, 
    user :: Address, 
    chain :: Timeline, 
    asset :: Asset, 
    amount :: Amount, 
    isIncrease :: Bool,
    startTime :: Int
  }
  | LiquidationOperation { 
    operationId :: Text, 
    liquidationEvent :: LiquidationEvent,
    currentStep :: Int,
    totalSteps :: Int,
    startTime :: Int
  }

-- Initialize the cross-chain lending protocol
initLendingProtocol :: ProtocolConfig -> Effect ProtocolState
initLendingProtocol config = do
  currentTime <- getCurrentTimestamp
  
  -- Create empty pools for each chain
  let chains = getUniqueChains config.supportedAssets
      emptyPools = map (\chain -> 
        PoolData {
          chain = chain,
          assets = [],
          utilizationRates = [],
          interestRates = []
        }
      ) chains
  
  -- Create initial state
  let initialState = ProtocolState {
    config = config,
    userPositions = [],
    pools = emptyPools,
    liquidationQueue = [],
    lastGlobalUpdate = currentTime,
    totalValueLocked = 0,
    pendingOperations = []
  }
  
  -- Start the main processes
  fork $ startInterestAccrual initialState
  fork $ startHealthFactorMonitoring initialState
  fork $ startLiquidationProcessor initialState
  fork $ startCrossChainOperationProcessor initialState
  
  emit "Cross-chain lending protocol initialized"
  
  return initialState
```

## Why Time Bandits is Uniquely Suited for Cross-Chain Lending

### 1. Unified Collateral Management Across Chains

**Problem**: Traditional lending protocols are confined to single chains, forcing users to maintain separate positions with inefficient capital utilization.

**Why Time Bandits Solves This**: Time Bandits' **chain-spanning state management** allows the protocol to:

- Track a user's collateral positions across all supported chains in a unified way
- Calculate a single global health factor that incorporates assets on multiple chains
- Let users leverage their entire portfolio regardless of which chains hold their assets

This is only possible because Time Bandits provides a unified view of the user's position that spans all chains while maintaining causal consistency between operations.

### 2. Causally Consistent Interest Accrual

**Problem**: Interest accrual across multiple chains must maintain proper temporal ordering to ensure fairness and prevent exploitation.

**Why Time Bandits Solves This**: The **time map and fact observation system** ensures that:

- Interest calculations use precise timestamps from each chain
- Accrual operates on a consistent global state, even when chain clocks differ
- Historical interest rates are properly preserved in the audit trail

## Periodic Interest Accrual Process

This process manages interest accrual across all chains, showcasing how Time Bandits can handle periodic financial updates without requiring low-latency responses:

```haskell
-- Start interest accrual process
startInterestAccrual :: ProtocolState -> Effect ()
startInterestAccrual initialState = do
  stateRef <- makeStateRef initialState
  
  while True $ do
    state <- readStateRef stateRef
    currentTime <- getCurrentTimestamp
    
    -- Only accrue interest once per hour
    when (currentTime - state.lastGlobalUpdate >= 3600) $ do
      emit "Beginning global interest accrual across all chains"
      
      -- Process each chain's pools
      updatedPools <- forM state.pools $ \pool -> do
        -- For each asset in the pool
        updatedAssets <- forM pool.assets $ \(asset, totalSupply, totalBorrow) -> do
          -- Calculate new interest
          let interestModel = lookupInterestModel asset state.config.interestRateModels
              utilizationRate = if totalSupply == 0 then 0 else 
                                fromIntegral totalBorrow / fromIntegral totalSupply
              interestRate = calculateInterestRate interestModel utilizationRate
              timeElapsed = currentTime - state.lastGlobalUpdate
              interestAccrued = calculateInterestAccrued totalBorrow interestRate timeElapsed
          
          -- Apply interest to total borrow amount
          let newTotalBorrow = totalBorrow + interestAccrued
          
          -- Record some interest for the protocol treasury
          let protocolFee = interestAccrued `div` 10  -- 10% of interest goes to protocol
          
          -- Add protocol fee to treasury
          _ <- addToTreasury pool.chain asset protocolFee
          
          -- Update interest rates
          let newUtilizationRate = if totalSupply == 0 then 0 else 
                                   fromIntegral newTotalBorrow / fromIntegral totalSupply
              newInterestRate = calculateInterestRate interestModel newUtilizationRate
          
          -- Return updated asset data
          return (asset, totalSupply, newTotalBorrow)
        
        -- Update utilization and interest rates
        let newUtilizationRates = map (\(asset, totalSupply, totalBorrow) -> 
                                      (asset, if totalSupply == 0 then 0 else 
                                              fromIntegral totalBorrow / fromIntegral totalSupply)) 
                                 updatedAssets
            
            newInterestRates = map (\(asset, _, _) -> 
                                    (asset, calculateInterestRate 
                                           (lookupInterestModel asset state.config.interestRateModels)
                                           (lookup asset newUtilizationRates)))
                                   updatedAssets
        
        -- Return updated pool
        return pool {
          assets = updatedAssets,
          utilizationRates = newUtilizationRates,
          interestRates = newInterestRates
        }
      
      -- Update all user borrow positions with accrued interest
      updatedUserPositions <- forM state.userPositions $ \userPosition -> do
        updatedBorrowPositions <- forM userPosition.borrowPositions $ \borrowPos -> do
          -- Find the interest rate for this asset
          let chain = borrowPos.chain
              asset = borrowPos.asset
              pool = find (\p -> p.chain == chain) updatedPools
              interestRate = maybe 
                borrowPos.interestRate 
                (\p -> lookupAssetInterestRate asset p.interestRates) 
                pool
              
              -- Calculate accrued interest
              timeElapsed = currentTime - borrowPos.lastAccrued
              interestAccrued = calculateInterestAccrued borrowPos.amount interestRate timeElapsed
              newAmount = borrowPos.amount + interestAccrued
          
          -- Return updated borrow position
          return borrowPos {
            amount = newAmount,
            lastAccrued = currentTime,
            interestRate = interestRate
          }
        
        -- Return updated user position
        return userPosition {
          borrowPositions = updatedBorrowPositions
        }
      
      -- Recalculate health factors after interest accrual
      updatedHealthUserPositions <- updateAllHealthFactors updatedUserPositions state.config
      
      -- Check for any positions that need liquidation after interest accrual
      let positionsToLiquidate = filter (\p -> p.healthFactor < state.config.liquidationHealthFactor) 
                                updatedHealthUserPositions
      
      -- Create liquidation events for unhealthy positions
      newLiquidationEvents <- forM positionsToLiquidate $ \position -> do
        -- Find the most underwater borrow position
        let sortedBorrows = sortBy (\b1 b2 -> 
                                   compare 
                                     (calculateBorrowValue b1 state.config) 
                                     (calculateBorrowValue b2 state.config))
                           position.borrowPositions
            worstBorrow = if null sortedBorrows then error "No borrow positions" else head sortedBorrows
        
        -- Create liquidation event
        return LiquidationEvent {
          user = position.user,
          borrowPosition = worstBorrow,
          collateralToLiquidate = [], -- Will be calculated by liquidation processor
          liquidator = Nothing,
          healthFactorAtTrigger = position.healthFactor,
          timestamp = currentTime,
          status = Pending
        }
      
      -- Update state with all changes
      modifyStateRef stateRef $ \s -> s {
        pools = updatedPools,
        userPositions = updatedHealthUserPositions,
        lastGlobalUpdate = currentTime,
        liquidationQueue = s.liquidationQueue ++ newLiquidationEvents
      }
      
      -- Log update summary
      let totalPositions = length updatedHealthUserPositions
          newLiquidations = length newLiquidationEvents
      
      emit $ "Interest accrual completed across all chains:" ++
             "\n- Updated " ++ show totalPositions ++ " user positions" ++
             "\n- Added " ++ show newLiquidations ++ " new liquidation events"
    
    -- Wait before next interest accrual check
    wait 900 seconds  -- Check every 15 minutes
```

### 3. Cross-Chain Risk Management

**Problem**: Managing risk across multiple chains requires a unified view of prices, positions, and liquidation thresholds.

**Why Time Bandits Solves This**: The **account program architecture** enables:

- A global risk model that considers all assets across all chains
- Consistent price oracle data incorporation from multiple sources
- Unified liquidation threshold enforcement

## Cross-Chain Health Factor Monitoring

This process demonstrates Time Bandits' ability to coordinate risk management across multiple chains by continuously calculating user health factors:

```haskell
-- Start health factor monitoring process
startHealthFactorMonitoring :: ProtocolState -> Effect ()
startHealthFactorMonitoring initialState = do
  stateRef <- makeStateRef initialState
  
  while True $ do
    state <- readStateRef stateRef
    currentTime <- getCurrentTimestamp
    
    -- Get latest prices for all assets from oracles
    prices <- getAllAssetPrices state.config.supportedAssets
    
    -- Process user positions in batches
    let userBatches = chunk 100 state.userPositions  -- Process 100 users at a time
    
    updatedAllUsers <- forM userBatches $ \userBatch -> do
      -- Update health factors for this batch
      updatedBatch <- forM userBatch $ \userPosition -> do
        -- Calculate total collateral value (in USD)
        totalCollateralValue <- sum <$> forM userPosition.collateralPositions $ \collPos -> do
          let asset = collPos.asset
              price = lookupAssetPrice asset prices
              collateralConfig = lookupLiquidationConfig asset state.config.liquidationConfigs
              ltv = collateralConfig.ltv
              
              -- Apply LTV to get adjusted collateral value
              collateralValueRaw = fromIntegral collPos.amount * price / (10 ^ asset.decimals)
              collateralValueAdjusted = collateralValueRaw * ltv
          
          return collateralValueAdjusted
        
        -- Calculate total borrow value (in USD)
        totalBorrowValue <- sum <$> forM userPosition.borrowPositions $ \borrowPos -> do
          let asset = borrowPos.asset
              price = lookupAssetPrice asset prices
              borrowFactor = (lookupLiquidationConfig asset state.config.liquidationConfigs).borrowFactor
              
              -- Apply borrow factor to get adjusted borrow value
              borrowValueRaw = fromIntegral borrowPos.amount * price / (10 ^ asset.decimals)
              borrowValueAdjusted = borrowValueRaw * borrowFactor
          
          return borrowValueAdjusted
        
        -- Calculate new health factor
        let newHealthFactor = if totalBorrowValue == 0 
                             then 100.0  -- Arbitrary high number for no borrows
                             else totalCollateralValue / totalBorrowValue
        
        -- Return updated user position
        return userPosition {
          healthFactor = newHealthFactor,
          lastCalculated = currentTime
        }
      
      return updatedBatch
    
    -- Flatten batches back to a single list
    let updatedUsers = concat updatedAllUsers
    
    -- Find any positions that now need liquidation
    let newLiquidations = filter (\p -> p.healthFactor < state.config.liquidationHealthFactor &&
                                      not (any (\l -> l.user == p.user && 
                                                    l.status `elem` [Pending, InProgress]) 
                                            state.liquidationQueue))
                         updatedUsers
    
    -- Create liquidation events for newly unhealthy positions
    liquidationEvents <- forM newLiquidations $ \position -> do
      -- Find the most underwater borrow position
      let sortedBorrows = sortBy (\b1 b2 -> 
                                 compare 
                                   (calculateBorrowValue b1 state.config) 
                                   (calculateBorrowValue b2 state.config))
                         position.borrowPositions
          worstBorrow = if null sortedBorrows then error "No borrow positions" else head sortedBorrows
      
      -- Create liquidation event
      return LiquidationEvent {
        user = position.user,
        borrowPosition = worstBorrow,
        collateralToLiquidate = [], -- Will be calculated by liquidation processor
        liquidator = Nothing,
        healthFactorAtTrigger = position.healthFactor,
        timestamp = currentTime,
        status = Pending
      }
    
    -- Update state with all changes
    modifyStateRef stateRef $ \s -> s {
      userPositions = updatedUsers,
      liquidationQueue = s.liquidationQueue ++ liquidationEvents
    }
    
    -- Log monitoring summary
    let totalUsers = length updatedUsers
        newLiquidationsCount = length liquidationEvents
        avgHealthFactor = sum (map (.healthFactor) updatedUsers) / fromIntegral totalUsers
    
    emit $ "Health factor monitoring completed:" ++
           "\n- Processed " ++ show totalUsers ++ " user positions" ++
           "\n- Average health factor: " ++ show avgHealthFactor ++
           "\n- New liquidation events: " ++ show newLiquidationsCount
    
    -- Wait before next health check
    wait 300 seconds  -- Check every 5 minutes
```

### 4. Cross-Chain Liquidation Orchestration

**Problem**: Liquidating positions with collateral and debt on different chains requires complex orchestration while maintaining solvency.

**Why Time Bandits Solves This**: The **choreographic programming model** allows the protocol to:

- Coordinate multi-step liquidations across chains
- Handle partial liquidations when one chain experiences delays
- Ensure that liquidation bonuses are properly distributed
- Maintain protocol solvency throughout the entire process

## Cross-Chain Liquidation Orchestration

This code shows how Time Bandits orchestrates complex multi-chain liquidations when a user's position becomes under-collateralized:

```haskell
-- Start liquidation processor
startLiquidationProcessor :: ProtocolState -> Effect ()
startLiquidationProcessor initialState = do
  stateRef <- makeStateRef initialState
  
  while True $ do
    state <- readStateRef stateRef
    
    -- Find pending liquidations
    let pendingLiquidations = filter (\l -> l.status == Pending) state.liquidationQueue
    
    -- Process each pending liquidation
    forM_ pendingLiquidations $ \liquidation -> do
      -- Mark liquidation as in progress
      modifyStateRef stateRef $ \s -> s {
        liquidationQueue = updateLiquidationStatus liquidation.user InProgress s.liquidationQueue
      }
      
      -- Get the user position
      let maybeUserPosition = find (\p -> p.user == liquidation.user) state.userPositions
      
      case maybeUserPosition of
        Nothing -> do
          -- User position not found (should not happen)
          modifyStateRef stateRef $ \s -> s {
            liquidationQueue = updateLiquidationStatus 
                               liquidation.user 
                               (Failed "User position not found") 
                               s.liquidationQueue
          }
        
        Just userPosition -> do
          -- Calculate which collateral to liquidate
          liquidationPlan <- planLiquidation userPosition liquidation state.config
          
          -- Create operation ID for this liquidation
          operationId <- generateOperationId "liquidation"
          
          -- Create pending operation for the liquidation
          let pendingOperation = LiquidationOperation {
            operationId = operationId,
            liquidationEvent = liquidation { 
              collateralToLiquidate = liquidationPlan.collateralToLiquidate
            },
            currentStep = 0,
            totalSteps = length liquidationPlan.steps,
            startTime = getCurrentTimestamp
          }
          
          -- Add to pending operations
          modifyStateRef stateRef $ \s -> s {
            pendingOperations = pendingOperation : s.pendingOperations
          }
          
          -- Log liquidation plan
          emit $ "Liquidation plan created for user " ++ show liquidation.user ++
                 "\n- Borrow position: " ++ liquidation.borrowPosition.asset.symbol ++ 
                 " on " ++ liquidation.borrowPosition.chain ++
                 "\n- Health factor: " ++ show liquidation.healthFactorAtTrigger ++
                 "\n- Collateral to liquidate across " ++ 
                 show (length liquidationPlan.collateralToLiquidate) ++ " positions"
    
    -- Wait before checking for more liquidations
    wait 60 seconds  -- Check every minute
```

### 5. Cross-Chain Operation Processing

**Problem**: Coordinating operations across chains requires careful sequencing and handling of delays or failures on any chain.

**Why Time Bandits Solves This**: Time Bandits' **effect log and time map** system ensures:

- Operations are always executed in the correct causal sequence
- Failed operations can be retried or rolled back safely
- The system maintains a complete audit trail of all cross-chain activities

## Cross-Chain Operation Processor

This component demonstrates how Time Bandits coordinates operations across chains with proper sequencing:

```haskell
-- Start cross-chain operation processor
startCrossChainOperationProcessor :: ProtocolState -> Effect ()
startCrossChainOperationProcessor initialState = do
  stateRef <- makeStateRef initialState
  
  while True $ do
    state <- readStateRef stateRef
    currentTime <- getCurrentTimestamp
    
    -- Find pending operations to process
    let pendingOps = filter (\op -> 
          case op of
            CollateralUpdate {} -> True
            BorrowUpdate {} -> True
            LiquidationOperation { currentStep = cs, totalSteps = ts } -> cs < ts
        ) state.pendingOperations
    
    -- Process operations
    forM_ pendingOps $ \op -> do
      result <- case op of
        CollateralUpdate {} -> 
          processCollateralUpdate state op
          
        BorrowUpdate {} -> 
          processBorrowUpdate state op
          
        LiquidationOperation {} -> 
          processLiquidationStep state op
      
      -- Update pending operations based on result
      case result of
        Right (completed, newState) -> do
          if completed
            then do
              -- Remove from pending operations if completed
              modifyStateRef stateRef $ \s -> s {
                pendingOperations = filter (\p -> getOperationId p /= getOperationId op) s.pendingOperations
              }
              
              -- Update state with new state
              modifyStateRef stateRef $ \s -> 
                applyStateUpdates newState s
            else do
              -- Update the operation with new progress
              modifyStateRef stateRef $ \s -> s {
                pendingOperations = updateOperation op result s.pendingOperations
              }
        
        Left err -> do
          -- Mark operation as failed
          emit $ "Operation " ++ getOperationId op ++ " failed: " ++ err
          
          -- Remove from pending operations
          modifyStateRef stateRef $ \s -> s {
            pendingOperations = filter (\p -> getOperationId p /= getOperationId op) s.pendingOperations
          }
          
          -- If it was a liquidation, update liquidation queue
          case op of
            LiquidationOperation { liquidationEvent = le } ->
              modifyStateRef stateRef $ \s -> s {
                liquidationQueue = updateLiquidationStatus le.user (Failed err) s.liquidationQueue
              }
            
            _ -> return ()
    
    -- Wait before processing more operations
    wait 30 seconds  -- Check every 30 seconds
```

## User Interaction: Cross-Chain Borrowing

Here's how a user would interact with the protocol to borrow assets on one chain using collateral on another:

```haskell
-- Deposit collateral on one chain
depositCollateral :: 
  ProtocolState -> 
  Address ->     -- User
  Timeline ->    -- Chain
  Asset ->       -- Asset
  Amount ->      -- Amount
  Effect (Either Text Text)  -- Error or operation ID
depositCollateral state user chain asset amount = do
  -- Validate asset is supported
  unless (isAssetSupported asset state.config) $
    return $ Left $ "Asset " ++ asset.symbol ++ " not supported on " ++ chain
  
  -- Generate operation ID
  operationId <- generateOperationId "deposit"
  
  -- Create pending operation
  let pendingOp = CollateralUpdate {
    operationId = operationId,
    user = user,
    chain = chain,
    asset = asset,
    amount = amount,
    isIncrease = True,
    startTime = getCurrentTimestamp
  }
  
  -- Add to pending operations
  modifyStateRef state $ \s -> s {
    pendingOperations = pendingOp : s.pendingOperations
  }
  
  -- Execute transfer to protocol
  transferResult <- transferFromUserToProtocol user chain asset amount
  
  case transferResult of
    Left err -> 
      return $ Left $ "Transfer failed: " ++ err
      
    Right _ -> do
      -- Log deposit
      emit $ "Collateral deposit initiated: " ++ 
             show amount ++ " " ++ asset.symbol ++ 
             " on " ++ chain ++ " by " ++ show user
      
      return $ Right operationId

-- Borrow assets on another chain using cross-chain collateral
borrowCrossChain :: 
  ProtocolState -> 
  Address ->     -- User
  Timeline ->    -- Chain to borrow on
  Asset ->       -- Asset to borrow
  Amount ->      -- Amount to borrow
  Effect (Either Text Text)  -- Error or operation ID
borrowCrossChain state user chain asset amount = do
  -- Get user position
  let maybeUserPosition = find (\p -> p.user == user) state.userPositions
  
  case maybeUserPosition of
    Nothing ->
      return $ Left "User has no collateral positions"
      
    Just userPosition -> do
      -- Calculate current health factor
      currentHealthFactor <- recalculateHealthFactor userPosition state.config
      
      -- Calculate new health factor after potential borrow
      let borrowValue = calculateAssetValueInUSD amount asset state.config
          simulatedPosition = addBorrowToPosition userPosition chain asset amount state.config
      
      newHealthFactor <- recalculateHealthFactor simulatedPosition state.config
      
      -- Check if health factor would remain above minimum
      if newHealthFactor < state.config.minHealthFactor
        then return $ Left $ "Borrow would put health factor below minimum threshold. " ++
                           "Current: " ++ show currentHealthFactor ++ 
                           ", After borrow: " ++ show newHealthFactor ++
                           ", Minimum: " ++ show state.config.minHealthFactor
        else do
          -- Generate operation ID
          operationId <- generateOperationId "borrow"
          
          -- Create pending operation
          let pendingOp = BorrowUpdate {
            operationId = operationId,
            user = user,
            chain = chain,
            asset = asset,
            amount = amount,
            isIncrease = True,
            startTime = getCurrentTimestamp
          }
          
          -- Add to pending operations
          modifyStateRef state $ \s -> s {
            pendingOperations = pendingOp : s.pendingOperations
          }
          
          -- Log borrow request
          emit $ "Cross-chain borrow initiated: " ++ 
                 show amount ++ " " ++ asset.symbol ++ 
                 " on " ++ chain ++ " by " ++ show user ++
                 "\n- Current health factor: " ++ show currentHealthFactor ++
                 "\n- Projected health factor: " ++ show newHealthFactor
          
          return $ Right operationId
```

## Example: Cross-Chain Debt Position

This example demonstrates how a user might interact with the protocol to manage a debt position across multiple chains:

```haskell
-- Example: Create a multi-chain position with ETH on Ethereum, borrowing USDC on Arbitrum
createCrossChainPosition :: Effect ()
createCrossChainPosition = do
  -- Initialize protocol
  protocolConfig <- defaultProtocolConfig
  state <- initLendingProtocol protocolConfig
  
  -- User address
  let user = "0x1234567890123456789012345678901234567890"
  
  -- First deposit ETH on Ethereum as collateral
  let ethAsset = findAsset "ETH" "ethereum" protocolConfig.supportedAssets
      ethAmount = 10 * 10^18  -- 10 ETH
  
  depositResult <- depositCollateral state user "ethereum" ethAsset ethAmount
  
  case depositResult of
    Left err ->
      emit $ "Deposit failed: " ++ err
      
    Right operationId -> do
      emit $ "Collateral deposit initiated with operation ID: " ++ operationId
      
      -- Wait for deposit to be processed
      waitForOperationCompletion state operationId
      
      -- Now borrow USDC on Arbitrum against the ETH collateral
      let usdcAsset = findAsset "USDC" "arbitrum" protocolConfig.supportedAssets
          usdcAmount = 15000 * 10^6  -- 15,000 USDC
      
      borrowResult <- borrowCrossChain state user "arbitrum" usdcAsset usdcAmount
      
      case borrowResult of
        Left err ->
          emit $ "Borrow failed: " ++ err
          
        Right borrowOpId -> do
          emit $ "Cross-chain borrow initiated with operation ID: " ++ borrowOpId
          
          -- Wait for borrow to be processed
          waitForOperationCompletion state borrowOpId
          
          -- Get final position state
          finalPosition <- getUserPosition state user
          
          emit $ "Cross-chain position created successfully:" ++
                 "\n- Collateral: " ++ show ethAmount ++ " ETH on Ethereum" ++
                 "\n- B