-- | Block retrieval queue with accompanying datatypes.
module Pos.Block.RetrievalQueue
       ( BlockRetrievalQueueTag
       , BlockRetrievalQueue
       , BlockRetrievalTask(..)
       ) where

import           Control.Concurrent.STM (TBQueue)

import           Pos.Core.Block (BlockHeader)
import           Pos.Network.Types (NodeId)

-- | Task that is put in the block retrieval queue for the retrieval
-- worker to perform.
newtype BlockRetrievalTask = BlockRetrievalTask
    { brtHeader :: BlockHeader
      -- ^ Header we're insterested in.
    }

data BlockRetrievalQueueTag

-- | Queue types.
type BlockRetrievalQueue = TBQueue (NodeId, BlockRetrievalTask)
