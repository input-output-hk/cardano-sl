-- | Block retrieval queue with accompanied datatypes.
module Pos.Block.RetrievalQueue
       ( BlockRetrievalQueueTag
       , BlockRetrievalQueue
       , BlockRetrievalTask(..)
       ) where

import           Universum

import           Control.Concurrent.STM  (TBQueue)

import           Pos.Block.Core          (BlockHeader)
import           Pos.Communication.Types (NodeId)

-- | Task that block retrieval queue is asked to do.
data BlockRetrievalTask ssc = BlockRetrievalTask
    { brtHeader    :: !(BlockHeader ssc)
      -- ^ Header we're insterested in.
    , brtContinues :: !Bool
      -- ^ If it was tentatively classified as "direct continuation of
      -- our chain".
    }

data BlockRetrievalQueueTag

-- | Queue types.
type BlockRetrievalQueue ssc = TBQueue (NodeId, BlockRetrievalTask ssc)
