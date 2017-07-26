-- | Types related to pending transactions.

module Pos.Txp.Pending.Types
    ( PendingTx (..)
    , TxPendingState (..)
    ) where

import           Pos.Txp.Core.Types (TxAux)
import           Universum

data PendingTx = PendingTx
    { ptTxAux :: TxAux
    } deriving (Eq)

-- | Persistance assessment for given pending transaction.
data TxPendingState
    = TxApplying   -- ^ Still can be applyed
    | TxWon'tSend  -- ^ Can't be applyed and requires user's input to reform tx
