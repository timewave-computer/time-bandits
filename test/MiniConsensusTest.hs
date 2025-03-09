{-# LANGUAGE OverloadedStrings #-}

-- A minimal standalone test for Consensus functionality, with minimal dependencies

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sortOn, foldl')
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Maybe (isJust, fromJust)
import qualified Data.ByteString as BS
import Control.Monad (foldM)

-- Simplified Consensus types
newtype NodeID = NodeID Text
  deriving (Show, Eq, Ord)

newtype BlockID = BlockID Text
  deriving (Show, Eq, Ord)

data Vote = Vote 
  { voteNodeId :: NodeID
  , voteBlockId :: BlockID
  , voteTimestamp :: UTCTime
  } deriving (Show, Eq)

data ConsensusState = ConsensusState
  { csNodes :: Set NodeID
  , csVotes :: Map NodeID Vote
  , csLatestBlock :: BlockID
  , csVotingThreshold :: Double  -- Percentage of nodes required for consensus (0.0-1.0)
  } deriving (Show, Eq)

-- Create a new consensus state
createConsensusState :: [NodeID] -> BlockID -> Double -> ConsensusState
createConsensusState nodes latestBlock threshold =
  ConsensusState
    { csNodes = Set.fromList nodes
    , csVotes = Map.empty
    , csLatestBlock = latestBlock
    , csVotingThreshold = threshold
    }

-- Register a vote for a block
registerVote :: ConsensusState -> Vote -> ConsensusState
registerVote state vote =
  -- Only count votes from known nodes
  if voteNodeId vote `Set.member` csNodes state
    then state { csVotes = Map.insert (voteNodeId vote) vote (csVotes state) }
    else state

-- Calculate vote percentages for each block
calculateVotes :: ConsensusState -> Map BlockID Double
calculateVotes state =
  let totalNodes = fromIntegral $ Set.size (csNodes state)
      votesByBlock = foldl' countVote Map.empty (Map.elems (csVotes state))
      countVote acc vote = 
        let blockId = voteBlockId vote
            count = Map.findWithDefault 0 blockId acc
        in Map.insert blockId (count + 1) acc
  in Map.map (\count -> fromIntegral count / totalNodes) votesByBlock

-- Check if consensus is reached for a specific block
hasConsensus :: ConsensusState -> BlockID -> Bool
hasConsensus state blockId =
  let votePercentages = calculateVotes state
      blockVotes = Map.findWithDefault 0.0 blockId votePercentages
  in blockVotes >= csVotingThreshold state

-- Get the block with the most votes (if any have reached threshold)
getConsensusBlock :: ConsensusState -> Maybe BlockID
getConsensusBlock state =
  let votePercentages = calculateVotes state
      threshold = csVotingThreshold state
      validBlocks = Map.filter (>= threshold) votePercentages
  in if Map.null validBlocks
       then Nothing
       else Just $ fst $ Map.foldrWithKey findMax (BlockID "", 0.0) validBlocks
  where
    findMax blockId votes (maxBlock, maxVotes) =
      if votes > maxVotes
        then (blockId, votes)
        else (maxBlock, maxVotes)

-- Create a vote
createVote :: NodeID -> BlockID -> IO Vote
createVote nodeId blockId = do
  now <- getCurrentTime
  return Vote
    { voteNodeId = nodeId
    , voteBlockId = blockId
    , voteTimestamp = now
    }

-- Test basic consensus functionality
testBasicConsensus :: IO ()
testBasicConsensus = do
  putStrLn "Testing basic consensus functionality..."
  
  -- Create nodes and consensus state
  let nodes = [NodeID "node1", NodeID "node2", NodeID "node3", NodeID "node4", NodeID "node5"]
  let initialBlock = BlockID "block0"
  let state = createConsensusState nodes initialBlock 0.6  -- 60% threshold
  
  putStrLn $ "Initial state: " ++ show (Set.size $ csNodes state) ++ " nodes, 0 votes"
  
  -- Create votes for two competing blocks
  vote1 <- createVote (NodeID "node1") (BlockID "block1")
  vote2 <- createVote (NodeID "node2") (BlockID "block1")
  vote3 <- createVote (NodeID "node3") (BlockID "block1")
  vote4 <- createVote (NodeID "node4") (BlockID "block2")
  vote5 <- createVote (NodeID "node5") (BlockID "block2")
  
  -- Register votes
  let stateWithVotes = foldl' registerVote state [vote1, vote2, vote3, vote4, vote5]
  
  -- Calculate percentages
  let percentages = calculateVotes stateWithVotes
  putStrLn "Vote percentages:"
  mapM_ (\(blockId, percentage) -> 
          putStrLn $ "  " ++ T.unpack (let (BlockID id) = blockId in id) ++ 
                    ": " ++ show (percentage * 100) ++ "%") 
        (Map.toList percentages)
  
  -- Check if consensus is reached for specific blocks
  let block1Consensus = hasConsensus stateWithVotes (BlockID "block1")
  let block2Consensus = hasConsensus stateWithVotes (BlockID "block2")
  
  putStrLn $ "Consensus for block1: " ++ show block1Consensus
  putStrLn $ "Consensus for block2: " ++ show block2Consensus
  
  -- Get the block with the most votes
  case getConsensusBlock stateWithVotes of
    Nothing -> putStrLn "No block has reached consensus"
    Just blockId -> putStrLn $ "Consensus reached for: " ++ 
                               T.unpack (let (BlockID id) = blockId in id)

-- Test changing votes
testChangingVotes :: IO ()
testChangingVotes = do
  putStrLn "\nTesting changing votes..."
  
  -- Create nodes and consensus state
  let nodes = [NodeID "node1", NodeID "node2", NodeID "node3"]
  let initialBlock = BlockID "block0"
  let state = createConsensusState nodes initialBlock 0.67  -- 67% threshold
  
  -- Create initial votes - all nodes vote for block1
  vote1a <- createVote (NodeID "node1") (BlockID "block1")
  vote2a <- createVote (NodeID "node2") (BlockID "block1")
  vote3a <- createVote (NodeID "node3") (BlockID "block1")
  
  -- Register initial votes
  let stateWithInitialVotes = foldl' registerVote state [vote1a, vote2a, vote3a]
  putStrLn "Initial voting state:"
  let initialPercentages = calculateVotes stateWithInitialVotes
  mapM_ (\(blockId, percentage) -> 
          putStrLn $ "  " ++ T.unpack (let (BlockID id) = blockId in id) ++ 
                    ": " ++ show (percentage * 100) ++ "%") 
        (Map.toList initialPercentages)
  
  -- Now node2 and node3 change their votes to block2
  vote2b <- createVote (NodeID "node2") (BlockID "block2")
  vote3b <- createVote (NodeID "node3") (BlockID "block2")
  
  -- Register changed votes
  let stateWithChangedVotes = foldl' registerVote stateWithInitialVotes [vote2b, vote3b]
  putStrLn "After vote changes:"
  let changedPercentages = calculateVotes stateWithChangedVotes
  mapM_ (\(blockId, percentage) -> 
          putStrLn $ "  " ++ T.unpack (let (BlockID id) = blockId in id) ++ 
                    ": " ++ show (percentage * 100) ++ "%") 
        (Map.toList changedPercentages)
  
  -- Check consensus after vote changes
  case getConsensusBlock stateWithChangedVotes of
    Nothing -> putStrLn "No block has reached consensus after vote changes"
    Just blockId -> putStrLn $ "Consensus reached for: " ++ 
                               T.unpack (let (BlockID id) = blockId in id)

-- Main function to run all tests
main :: IO ()
main = do
  putStrLn "Running simplified Consensus tests"
  testBasicConsensus
  testChangingVotes 