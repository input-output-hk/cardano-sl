-- | Type class to work with list of pending transactions.

module Pos.Txp.Pending.Class
    ( MonadPendingTxs (..)
    ) where

import           Universum

import           Pos.Core.Types        (SlotId)
import           Pos.Txp.Core.Types    (TxId)
import           Pos.Txp.Pending.Types (PendingTx, TxPendingState)

-- | Allows to work with list of pending transactions.
-- /Pending transactions/ are ones which are not guaranteed to be persisted in
-- blockchain yet.
-- This constraint is used in transaction resubmition.
class MonadPendingTxs m where
    -- | Get list of pending transactions, filtered by 'TxPendingState'.
    getPendingTxs :: TxPendingState -> m [PendingTx]

    -- | Modify state of pending tx, and add it if it was absent.
    setPendingTx :: PendingTx -> TxPendingState -> m ()

    -- | Remove tx from list of pending ones.
    -- Also, 'isPendingTxInBlock' have to return @False@ after the call.
    removePendingTx :: PendingTx -> m ()

    -- | Whether given transaction got into blockchain.
    -- Expired transactions are removed from here on nearest check for
    -- resubmition.
    isPendingTxInBlock :: TxId -> m (Maybe SlotId)

    -- | Invoked when transaction got into blockchain.
    markPendingTxIsInBlock :: SlotId -> TxId -> m ()
