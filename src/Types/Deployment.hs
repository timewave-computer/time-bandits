{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module: Types.Deployment
Description: Shared types for deployment and scenario configuration

This module provides shared types between CLI.Deployment and Programs.Scenario
to break circular dependencies.
-}
module Types.Deployment (
  -- * Deployment Types
  DeploymentConfig(..),
  DeploymentError(..),
  Deployment(..),
  DeploymentStatus(..),
  DeploymentResult(..)
) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Core.Common (SimulationMode(..))
import Types.Actor (ActorSpec)

-- | Configuration for a deployment
data DeploymentConfig = DeploymentConfig
  { deploymentMode :: SimulationMode     -- ^ Mode of deployment
  , deploymentLogPath :: FilePath        -- ^ Path to log directory
  , deploymentVerbose :: Bool            -- ^ Whether to output verbose logs
  , deploymentActors :: [ActorSpec]      -- ^ Actors to deploy
  , deploymentInitialPrograms :: [Text]  -- ^ Initial programs to deploy
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Errors that can occur during deployment
data DeploymentError
  = InvalidConfiguration Text           -- ^ Configuration error
  | ActorInitializationError Text       -- ^ Error initializing actors
  | ProgramInitializationError Text     -- ^ Error initializing programs
  | NetworkingError Text                -- ^ Error setting up networking
  | InternalError Text                  -- ^ Internal error
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Deployment status
data DeploymentStatus
  = Initializing
  | Running
  | Stopping
  | Stopped
  | Failed Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A deployment represents a running instance of the system
data Deployment = Deployment
  { deploymentConfig :: DeploymentConfig    -- ^ Configuration used
  , deploymentName :: Text                  -- ^ Name of the deployment
  , deploymentId :: Text                    -- ^ Unique ID for the deployment
  , deploymentStartTime :: Text             -- ^ When the deployment started
  , deploymentActorIds :: [Text]            -- ^ IDs of deployed actors
  , deploymentProgramIds :: [Text]          -- ^ IDs of deployed programs
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Result of a deployment operation
data DeploymentResult = DeploymentResult
  { resultDeployment :: Deployment          -- ^ The deployment
  , resultStatus :: DeploymentStatus        -- ^ Current status
  , resultMessage :: Text                   -- ^ Optional message
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize) 