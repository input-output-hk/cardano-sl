-- | Block retrieval queue with accompanied datatypes.
module Pos.Block.RetrievalQueue
       ( BlockRetrievalQueueTag
       , BlockRetrievalQueue
       , BlockRetrievalTask(..)
       ) where

import           Universum

import           Control.Concurrent.STM  (TBQueue)

import           Pos.Communication.Types (NodeId)
import           Pos.Core.Block          (BlockHeader)

-- | Task that block retrieval queue is asked to do.
data BlockRetrievalTask = BlockRetrievalTask
    { brtHeader    :: !BlockHeader
      -- ^ Header we're insterested in.
    , brtContinues :: !Bool
      -- ^ If it was tentatively classified as "direct continuation of
      -- our chain".
    }

data BlockRetrievalQueueTag

-- | Queue types.
type BlockRetrievalQueue = TBQueue (NodeId, BlockRetrievalTask)
