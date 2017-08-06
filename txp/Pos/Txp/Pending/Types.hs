-- | Types related to pending transactions.

module Pos.Txp.Pending.Types
    ( PendingTx (..)
    , PtxCondition (..)
    ) where

import           Data.Hashable      (Hashable (..))
import           Universum

import           Pos.Txp.Core.Types (TxAux, TxId)

data PendingTx = PendingTx
    { ptxTxAux :: TxAux
    , ptxTxId  :: TxId
    }

instance Eq PendingTx where
    ptx1 == ptx2 = ptxTxId ptx1 == ptxTxId ptx2

instance Hashable PendingTx where
    hashWithSalt s ptx = hashWithSalt s (ptxTxId ptx)

-- | Persistance assessment for given pending transaction.
data PtxCondition
    = PtxApplying        -- ^ Is waiting to be applyed
    | PtxInUpperBlocks   -- ^ Recently appeared in blocks
    | PtxPersisted       -- ^ Transaction is ~guaranteed to remain in blockchain
                         --   (with up to *high* assurance level)
    | PtxWon'tApply Text -- ^ Can't be applyed and requires user's input to
                         --   reform tx
    deriving (Eq, Show)
