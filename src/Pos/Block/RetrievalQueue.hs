module Pos.Block.RetrievalQueue
       ( BlockRetrievalQueueTag
       , BlockRetrievalQueue
       , BlockRetrievalTask(..)
       , MonadBlockRetrievalQueue
       ) where

import           Universum

import           Control.Concurrent.STM  (TBQueue)
import qualified Ether

import           Pos.Block.Core          (BlockHeader)
import           Pos.Communication.Types (NodeId)

data BlockRetrievalTask ssc = BlockRetrievalTask
    { brtHeader    :: !(BlockHeader ssc)
    , brtContinues :: !Bool
    }

data BlockRetrievalQueueTag

type BlockRetrievalQueue ssc = TBQueue (NodeId, BlockRetrievalTask ssc)

type MonadBlockRetrievalQueue ssc =
    Ether.MonadReader BlockRetrievalQueueTag (BlockRetrievalQueue ssc)
