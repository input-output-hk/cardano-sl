module Pos.Block.RetrievalQueue
       ( BlockRetrievalQueueTag
       , BlockRetrievalQueue
       , BlockRetrievalTask(..)
       ) where

import           Control.Concurrent.STM  (TBQueue)

import           Pos.Block.Core          (BlockHeader)
import           Pos.Communication.Types (NodeId)
import           Pos.Util.Chrono         (NE, NewestFirst)

data BlockRetrievalTask ssc
    = RetrieveBlocksByHeaders (NewestFirst NE (BlockHeader ssc))
    | RetrieveHeadersByTip (BlockHeader ssc)

data BlockRetrievalQueueTag

type BlockRetrievalQueue ssc = TBQueue (NodeId, BlockRetrievalTask ssc)
