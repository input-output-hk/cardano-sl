-- | Module containing explorer-specific logic and data

module Pos.Explorer.DB
       ( ExplorerOp (..)
       , getTxExtra
       , getAddrHistory
       , getAddrBalance
       ) where

import qualified Database.RocksDB     as Rocks
import           Universum

import           Pos.Binary.Class     (encodeStrict)
import           Pos.Core.Types       (Address, Coin)
import           Pos.DB.Class         (MonadDB)
import           Pos.DB.Functions     (RocksBatchOp (..))
import           Pos.DB.GState.Common (gsGetBi)
import           Pos.Explorer.Core    (AddrHistory, TxExtra (..))
import           Pos.Txp.Core         (TxId)
import           Pos.Util.Chrono      (NewestFirst (..))

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getTxExtra :: MonadDB m => TxId -> m (Maybe TxExtra)
getTxExtra = gsGetBi . txExtraPrefix

getAddrHistory :: MonadDB m => Address -> m AddrHistory
getAddrHistory = fmap (NewestFirst . concat . maybeToList) .
                 gsGetBi . addrHistoryPrefix

getAddrBalance :: MonadDB m => Address -> m (Maybe Coin)
getAddrBalance = gsGetBi . addrBalancePrefix

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data ExplorerOp
    = AddTxExtra !TxId !TxExtra
    | DelTxExtra !TxId
    | UpdateAddrHistory !Address !AddrHistory
    | PutAddrBalance !Address !Coin
    | DelAddrBalance !Address

instance RocksBatchOp ExplorerOp where
    toBatchOp (AddTxExtra id extra) =
        [Rocks.Put (txExtraPrefix id) (encodeStrict extra)]
    toBatchOp (DelTxExtra id) =
        [Rocks.Del $ txExtraPrefix id]
    toBatchOp (UpdateAddrHistory addr txs) =
        [Rocks.Put (addrHistoryPrefix addr) (encodeStrict txs)]
    toBatchOp (PutAddrBalance addr coin) =
        [Rocks.Put (addrBalancePrefix addr) (encodeStrict coin)]
    toBatchOp (DelAddrBalance addr) =
        [Rocks.Del $ addrBalancePrefix addr]

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

txExtraPrefix :: TxId -> ByteString
txExtraPrefix h = "e/tx/" <> encodeStrict h

addrHistoryPrefix :: Address -> ByteString
addrHistoryPrefix addr = "e/ah/" <> encodeStrict addr

addrBalancePrefix :: Address -> ByteString
addrBalancePrefix addr = "e/ab/" <> encodeStrict addr
