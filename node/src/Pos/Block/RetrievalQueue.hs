module Pos.Block.RetrievalQueue
       ( BlockRetrievalQueueTag
       , BlockRetrievalQueue
       , BlockRetrievalTask(..)
       ) where

import           Universum

import           Control.Concurrent.STM  (TBQueue)

import           Pos.Block.Core          (BlockHeader)
import           Pos.Communication.Types (NodeId)

data BlockRetrievalTask ssc = BlockRetrievalTask
    { brtHeader    :: !(BlockHeader ssc)
    , brtContinues :: !Bool
    }

data BlockRetrievalQueueTag

type BlockRetrievalQueue ssc = TBQueue (NodeId, BlockRetrievalTask ssc)
