-- | Module containing explorer-specific logic and data

module Pos.DB.GState.Explorer
       ( ExplorerOp (..)
       , getTxExtra
       ) where

import qualified Data.List.NonEmpty   as NE
import qualified Database.RocksDB     as Rocks
import           Universum

import           Pos.Binary.Class     (encodeStrict)
import           Pos.Binary.Explorer  ()
import           Pos.Core.Types       (Address)
import           Pos.DB.Class         (MonadDB)
import           Pos.DB.Functions     (RocksBatchOp (..))
import           Pos.DB.GState.Common (gsGetBi)
import           Pos.Txp.Core.Types   (TxId)
import           Pos.Types.Explorer   (TxExtra (..))

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getTxExtra :: MonadDB m => TxId -> m (Maybe TxExtra)
getTxExtra = gsGetBi . txExtraPrefix

getAddrHistory :: MonadDB m => Address -> m [TxId]
getAddrHistory = fmap (concat . maybeToList) . gsGetBi . addrHistoryPrefix

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data ExplorerOp
    = AddTxExtra !TxId !TxExtra
    | DelTxExtra !TxId
    | UpdateAddrHistory !Address ![TxId]

instance RocksBatchOp ExplorerOp where
    toBatchOp (AddTxExtra id extra) =
        [Rocks.Put (txExtraPrefix id) (encodeStrict extra)]
    toBatchOp (DelTxExtra id) =
        [Rocks.Del $ txExtraPrefix id]
    toBatchOp (UpdateAddrHistory addr txs) =
        [Rocks.Put (addrHistoryPrefix addr) (encodeStrict txs)]

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

txExtraPrefix :: TxId -> ByteString
txExtraPrefix h = "e/tx/" <> encodeStrict h

addrHistoryPrefix :: Address -> ByteString
addrHistoryPrefix addr = "e/addr/" <> encodeStrict addr
