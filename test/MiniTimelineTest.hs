{-# LANGUAGE OverloadedStrings #-}

-- A minimal standalone test for Timeline functionality, with minimal dependencies

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sortOn)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Maybe (isJust, fromJust)
import qualified Data.ByteString as BS

-- Simplified Timeline types
newtype TimelineID = TimelineID Text
  deriving (Show, Eq, Ord)

newtype BranchID = BranchID Text
  deriving (Show, Eq, Ord)

data TimelineEvent = TimelineEvent
  { eventId :: Text
  , eventTimestamp :: UTCTime
  , eventType :: Text
  , eventData :: Map Text Text
  } deriving (Show, Eq)

data TimelineBranch = TimelineBranch
  { branchId :: BranchID
  , branchName :: Text
  , branchParent :: Maybe BranchID
  , branchCreatedAt :: UTCTime
  , branchEvents :: [TimelineEvent]
  } deriving (Show, Eq)

data Timeline = Timeline
  { timelineId :: TimelineID
  , timelineName :: Text
  , timelineDescription :: Text
  , timelineBranches :: Map BranchID TimelineBranch
  , timelineMainBranch :: BranchID
  } deriving (Show, Eq)

-- Create a new timeline
createTimeline :: Text -> Text -> IO Timeline
createTimeline name description = do
  now <- getCurrentTime
  let mainBranchId = BranchID "main"
  let mainBranch = TimelineBranch
        { branchId = mainBranchId
        , branchName = "main"
        , branchParent = Nothing
        , branchCreatedAt = now
        , branchEvents = []
        }
  return Timeline
    { timelineId = TimelineID $ "tl_" <> name
    , timelineName = name
    , timelineDescription = description
    , timelineBranches = Map.singleton mainBranchId mainBranch
    , timelineMainBranch = mainBranchId
    }

-- Create a new branch from an existing branch
createBranch :: Timeline -> BranchID -> Text -> IO Timeline
createBranch timeline parentBranchId branchName = do
  -- Verify parent branch exists
  if not (Map.member parentBranchId (timelineBranches timeline))
    then error $ "Parent branch does not exist: " ++ T.unpack (getBranchIdText parentBranchId)
    else do
      now <- getCurrentTime
      let newBranchId = BranchID $ branchName <> "_" <> T.pack (show now)
      let parentBranch = timelineBranches timeline Map.! parentBranchId
      let newBranch = TimelineBranch
            { branchId = newBranchId
            , branchName = branchName
            , branchParent = Just parentBranchId
            , branchCreatedAt = now
            , branchEvents = branchEvents parentBranch
            }
      return timeline
        { timelineBranches = Map.insert newBranchId newBranch (timelineBranches timeline) }

-- Helper to get Text from BranchID
getBranchIdText :: BranchID -> Text
getBranchIdText (BranchID t) = t

-- Add an event to a branch
addEvent :: Timeline -> BranchID -> TimelineEvent -> Timeline
addEvent timeline branchId event =
  if not (Map.member branchId (timelineBranches timeline))
    then error $ "Branch does not exist: " ++ T.unpack (getBranchIdText branchId)
    else
      let branch = timelineBranches timeline Map.! branchId
          updatedBranch = branch { branchEvents = branchEvents branch ++ [event] }
      in timeline { timelineBranches = Map.insert branchId updatedBranch (timelineBranches timeline) }

-- Create a timeline event
createEvent :: Text -> Text -> Map Text Text -> IO TimelineEvent
createEvent eventId eventType eventData = do
  now <- getCurrentTime
  return TimelineEvent
    { eventId = eventId
    , eventTimestamp = now
    , eventType = eventType
    , eventData = eventData
    }

-- Test Timeline creation
testTimelineCreation :: IO ()
testTimelineCreation = do
  putStrLn "Testing Timeline creation..."
  
  timeline <- createTimeline "test-timeline" "A test timeline"
  
  putStrLn $ "Timeline ID: " ++ T.unpack (let (TimelineID id) = timelineId timeline in id)
  putStrLn $ "Timeline name: " ++ T.unpack (timelineName timeline)
  putStrLn $ "Timeline branch count: " ++ show (Map.size $ timelineBranches timeline)
  
  if Map.size (timelineBranches timeline) == 1
    then putStrLn "Timeline created with correct branch count!"
    else putStrLn $ "ERROR: Expected 1 branch, but got " ++ show (Map.size $ timelineBranches timeline)

-- Test Timeline branching
testTimelineBranching :: IO ()
testTimelineBranching = do
  putStrLn "\nTesting Timeline branching..."
  
  -- Create a base timeline
  timeline <- createTimeline "test-timeline" "A test timeline"
  let mainBranchId = timelineMainBranch timeline
  
  -- Create a new branch
  timelineWithBranch <- createBranch timeline mainBranchId "feature-1"
  
  putStrLn $ "Timeline branch count after branching: " ++ show (Map.size $ timelineBranches timelineWithBranch)
  
  if Map.size (timelineBranches timelineWithBranch) == 2
    then putStrLn "Timeline branched successfully!"
    else putStrLn $ "ERROR: Expected 2 branches, but got " ++ show (Map.size $ timelineBranches timelineWithBranch)
  
  -- Check parent-child relationship
  let featureBranch = filter (\b -> branchName b == "feature-1") $ Map.elems (timelineBranches timelineWithBranch)
  if length featureBranch == 1
    then do
      let branch = head featureBranch
      case branchParent branch of
        Just parentId -> 
          if parentId == mainBranchId
            then putStrLn "Branch has correct parent reference!"
            else putStrLn "ERROR: Branch has incorrect parent reference!"
        Nothing -> putStrLn "ERROR: Branch should have a parent reference!"
    else putStrLn "ERROR: Could not find feature branch!"

-- Test Timeline event handling
testTimelineEvents :: IO ()
testTimelineEvents = do
  putStrLn "\nTesting Timeline events..."
  
  -- Create a base timeline
  timeline <- createTimeline "test-timeline" "A test timeline"
  let mainBranchId = timelineMainBranch timeline
  
  -- Create and add events
  event1 <- createEvent "evt1" "update" $ Map.fromList [("key", "value")]
  event2 <- createEvent "evt2" "create" $ Map.fromList [("name", "test")]
  
  let timelineWithEvents = addEvent (addEvent timeline mainBranchId event1) mainBranchId event2
  
  -- Check event count
  let mainBranch = timelineBranches timelineWithEvents Map.! mainBranchId
  putStrLn $ "Event count in main branch: " ++ show (length $ branchEvents mainBranch)
  
  if length (branchEvents mainBranch) == 2
    then putStrLn "Events added successfully!"
    else putStrLn $ "ERROR: Expected 2 events, but got " ++ show (length $ branchEvents mainBranch)
  
  -- Check event order
  if not (null (branchEvents mainBranch)) && eventId (head $ branchEvents mainBranch) == "evt1"
    then putStrLn "Events are in correct order!"
    else putStrLn "ERROR: Events are not in the expected order!"

main :: IO ()
main = do
  putStrLn "Running simplified Timeline tests"
  testTimelineCreation
  testTimelineBranching
  testTimelineEvents 