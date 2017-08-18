-- | Types related to pending transactions.

module Pos.Wallet.Web.Pending.Types
    ( PendingTx (..)
    , ptxTxId
    , ptxTxAux
    , ptxCond
    , ptxCreationSlot
    , ptxAssuredDepth
    , ptxAttemptsRem

    , PtxCondition (..)
    , PtxBlockInfo
    ) where

import           Universum

import           Control.Lens       (makeLenses)
import           Pos.Core.Types     (HeaderHash, SlotCount, SlotId)
import           Pos.Txp.Core.Types (TxAux, TxId)

-- | Information about block where given pending transaction is sited
type PtxBlockInfo = (SlotId, HeaderHash)

-- | Current state of pending transaction.
--
-- Once transaction is created, it should be assigned 'PtxApplying' condition
-- in order to be tracked by resubmitter.
--
-- When wallet tracker notices a block with given transaction, it switches
-- condition to 'PtxInUpperBlocks' providing needed information about that block.
-- 'HeaderHash' is required for atomic modifications of db by resubmitter
-- (i.e. not to accidentially overwrite possible changes made by tracker),
-- see 'Pos.Wallet.Web.State.State.casPtxCondition' function.
-- Transactions in this state are periodically checked by resubmitter for
-- the possibility (and need) to submit them again.
--
-- Resubmitter also checks whether transaction is deep enough in blockchain to
-- be moved to 'PtxPersisted' state.
--
-- If transaction is ever noticed to conflict with transactions existing in
-- blockchain / mempool, it is assigned 'PtxWontApply' state and is stopped
-- being tracked further (effect can be canceled by BListener on rollback
-- though).
-- This behaviour is to be improved in CSM-390.
data PtxCondition
    = PtxApplying                    -- ^ Is waiting to be applyed
    | PtxInUpperBlocks PtxBlockInfo  -- ^ Recently appeared in block.
    | PtxPersisted                   -- ^ Transaction is ~guaranteed to remain in blockchain
                                     --   (with up to *high* assurance level)
    | PtxWontApply Text              -- ^ Can't be applyed and requires user's
                                     --   input to reform tx
    deriving (Eq, Show)

-- | All info kept about pending transaction
data PendingTx = PendingTx
    { _ptxTxId         :: TxId  -- for the sake of optimization
    , _ptxTxAux        :: TxAux
    , _ptxCreationSlot :: SlotId  -- when tx was formed, for scheduling purposes.
                                  -- this in NOT when tx appeared in blockchain
    , _ptxCond         :: PtxCondition
    , _ptxAssuredDepth :: SlotCount  -- ^ at which depth tx is considered persistent
    , _ptxAttemptsRem  :: Int  -- ^ remaining number of resubmission attempts
    } deriving (Eq)

makeLenses ''PendingTx
