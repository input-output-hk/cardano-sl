module Pos.Modern.Txp.RocksDB
       ( createPutTx
       , createDelTx
       ) where
import           Database.RocksDB (BatchOp (..))
import           Universum

import           Pos.Types        (TxId, TxOut)

----------------------------------------------------------------------------
-- RocksDB Tx
----------------------------------------------------------------------------
createPutTx :: ((TxId, Word32), TxOut) -> BatchOp
createPutTx = notImplemented

createDelTx :: (TxId, Word32) -> BatchOp
createDelTx = notImplemented
