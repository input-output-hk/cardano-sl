-- | Types related to pending transactions.

module Pos.Wallet.Web.Pending.Types
    ( PendingTx (..)
    , PtxCondition (..)
    ) where

import           Universum

import           Pos.Core.Types     (SlotCount, SlotId)
import           Pos.Txp.Core.Types (TxAux, TxId)

-- | Current state of pending transaction.
data PtxCondition
    = PtxApplying              -- ^ Is waiting to be applyed
    | PtxInUpperBlocks SlotId  -- ^ Recently appeared in block of given slot
    | PtxPersisted             -- ^ Transaction is ~guaranteed to remain in
                               --   blockchain (with up to *high* assurance
                               --   level)
    | PtxWon'tApply Text       -- ^ Can't be applyed and requires user's input
                               --   to reform tx
    deriving (Eq, Show)

data PendingTx = PendingTx
    { ptxTxId         :: TxId  -- for the sake of optimization
    , ptxTxAux        :: TxAux
    , ptxCreationSlot :: SlotId  -- when tx was formed, for scheduling purposes.
                                 -- this in NOT when tx appears into blockchain
    , ptxCond         :: PtxCondition
    , ptxAssuredDepth :: SlotCount
    } deriving (Eq)
