module Pos.Infra.Recovery.Types
       ( RecoveryHeaderTag
       , RecoveryHeader
       ) where

import           Control.Concurrent.STM (TMVar)

import           Pos.Chain.Block (BlockHeader)
import           Pos.Infra.Communication.Types.Protocol (NodeId)

data RecoveryHeaderTag

type RecoveryHeader = TMVar (NodeId, BlockHeader)
