module Pos.Block.RetrievalQueue
       ( BlockRetrievalQueueTag
       , BlockRetrievalQueue
       , BlockRetrievalTask(..)
       , MonadBlockRetrievalQueue
       ) where

import           Control.Concurrent.STM  (TBQueue)
import qualified Ether

import           Pos.Block.Core          (BlockHeader)
import           Pos.Communication.Types (NodeId)
import           Pos.Util.Chrono         (NE, NewestFirst)

data BlockRetrievalTask ssc
    = RetrieveBlocksByHeaders (NewestFirst NE (BlockHeader ssc))
    | RetrieveHeadersByTip (BlockHeader ssc)

data BlockRetrievalQueueTag

type BlockRetrievalQueue ssc = TBQueue (NodeId, BlockRetrievalTask ssc)

type MonadBlockRetrievalQueue ssc =
    Ether.MonadReader BlockRetrievalQueueTag (BlockRetrievalQueue ssc)
