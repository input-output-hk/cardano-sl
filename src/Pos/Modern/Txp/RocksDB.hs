module Pos.Modern.Txp.RocksDB
       (
         createPutTx
       , createDelTx
       ) where
import           Database.RocksDB (BatchOp, BatchOp (..))
import           Universum

import           Pos.Types        (TxId, TxOut)
import           Pos.Util         (binaryToBS)

----------------------------------------------------------------------------
-- RocksDB Tx
----------------------------------------------------------------------------
createPutTx :: ((TxId, Word32), TxOut) -> BatchOp
createPutTx (key, val) = Put (binaryToBS key) (binaryToBS val)

createDelTx :: (TxId, Word32) -> BatchOp
createDelTx key = Del (binaryToBS key)
