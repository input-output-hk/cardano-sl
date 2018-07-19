
module Pos.Recovery.Types
       ( RecoveryHeaderTag
       , RecoveryHeader
       , MonadRecoveryHeader
       ) where

import           Universum

import           Control.Concurrent.STM (TMVar)

import           Pos.Core.Block (BlockHeader)
import           Pos.Infra.Communication.Types.Protocol (NodeId)
import           Pos.Util.Util (HasLens (..))

data RecoveryHeaderTag

type RecoveryHeader = TMVar (NodeId, BlockHeader)

type MonadRecoveryHeader ctx m
     = (MonadReader ctx m, HasLens RecoveryHeaderTag ctx RecoveryHeader)
