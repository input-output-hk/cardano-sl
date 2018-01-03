-- | Block retrieval queue with accompanying datatypes.
module Pos.Block.RetrievalQueue
       ( BlockRetrievalQueueTag
       , BlockRetrievalQueue
       , BlockRetrievalTask(..)
       ) where

import           Universum

import           Control.Concurrent.STM  (TBQueue)

import           Pos.Block.Core          (BlockHeader)
import           Pos.Communication.Types (NodeId)

-- | Task that are put in the block retrieval queue for the retrieval worker to
-- perform.
data BlockRetrievalTask ssc = BlockRetrievalTask
    { brtHeader    :: !(BlockHeader ssc)
    , brtContinues :: !Bool
    }

data BlockRetrievalQueueTag

type BlockRetrievalQueue ssc = TBQueue (NodeId, BlockRetrievalTask ssc)
