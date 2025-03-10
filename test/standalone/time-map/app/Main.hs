{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Timeline (Timeline, createTimeline, timelineId, getTimelineName)
import TimeMap
import Types (TimelineId)

main :: IO ()
main = do
  putStrLn "TimeMap Test"
  putStrLn "==========="

  -- Create some timelines
  let timelines = [
        createTimeline "main" "Main timeline",
        createTimeline "feature-a" "Feature A development",
        createTimeline "feature-b" "Feature B development",
        createTimeline "release-1" "Release 1",
        createTimeline "hotfix-1" "Hotfix for Release 1"
        ]
      
      mainTimeline = head timelines
      featureATimeline = timelines !! 1
      featureBTimeline = timelines !! 2
      release1Timeline = timelines !! 3
      hotfixTimeline = timelines !! 4
      
      mainId = timelineId mainTimeline
      featureAId = timelineId featureATimeline
      featureBId = timelineId featureBTimeline
      release1Id = timelineId release1Timeline
      hotfixId = timelineId hotfixTimeline

  -- Create a time map with the timelines
  let timeMap = fromList timelines
  
  putStrLn "\nInitial TimeMap:"
  TIO.putStrLn $ renderTimeMap timeMap
  
  -- Create a development workflow:
  -- 1. Fork feature-a and feature-b from main
  -- 2. Create release-1 by continuing from main
  -- 3. Create a hotfix branching from release-1
  
  -- 1. Fork features from main
  let Just timeMap1 = linkTimelines mainId featureAId ForkEdge timeMap
      Just timeMap2 = linkTimelines mainId featureBId ForkEdge timeMap1
  
  putStrLn "\nAfter forking features:"
  TIO.putStrLn $ renderTimeMap timeMap2
  
  -- 2. Create release-1 continuing from main
  let Just timeMap3 = linkTimelines mainId release1Id ContinueEdge timeMap2
  
  putStrLn "\nAfter creating release-1:"
  TIO.putStrLn $ renderTimeMap timeMap3
  
  -- 3. Create hotfix from release-1
  let Just timeMap4 = linkTimelines release1Id hotfixId ForkEdge timeMap3
  
  putStrLn "\nFinal TimeMap:"
  TIO.putStrLn $ renderTimeMap timeMap4
  
  -- Test some queries
  putStrLn "\nTimeMap Queries:"
  
  -- Check if timelines exist
  putStrLn $ "Main timeline exists: " ++ show (timelineExists mainId timeMap4)
  putStrLn $ "Unknown timeline exists: " ++ show (timelineExists "unknown" timeMap4)
  
  -- Get root timelines
  let roots = getRootTimelines timeMap4
  putStrLn $ "\nRoot timelines: " ++ show (length roots)
  mapM_ (printTimeline timeMap4) roots
  
  -- Get latest timelines
  let leaves = getLatestTimelines timeMap4
  putStrLn $ "\nLatest timelines: " ++ show (length leaves)
  mapM_ (printTimeline timeMap4) leaves
  
  -- Get ancestors and descendants
  putStrLn $ "\nAncestors of " ++ show hotfixId ++ ":"
  mapM_ (printTimeline timeMap4) (getAncestors hotfixId timeMap4)
  
  putStrLn $ "\nDescendants of " ++ show mainId ++ ":"
  mapM_ (printTimeline timeMap4) (getDescendants mainId timeMap4)
  
  -- Find common ancestor
  let commonAncestor = findCommonAncestor featureAId hotfixId timeMap4
  putStrLn $ "\nCommon ancestor of " ++ show featureAId ++ " and " ++ show hotfixId ++ ":"
  case commonAncestor of
    Nothing -> putStrLn "None found"
    Just id -> printTimeline timeMap4 id

-- | Print information about a timeline
printTimeline :: TimeMap -> TimelineId -> IO ()
printTimeline tm id = do
  case getTimeline id tm of
    Nothing -> putStrLn $ "  " ++ show id ++ " (not found)"
    Just timeline -> 
      putStrLn $ "  " ++ show id ++ " - " ++ T.unpack (getTimelineName timeline) 