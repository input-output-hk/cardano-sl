-- | Types related to pending transactions.

module Pos.Wallet.Web.Pending.Types
    ( PendingTx (..)
    , ptxTxId
    , ptxTxAux
    , ptxCond
    , ptxCreationSlot
    , ptxWallet

    , PtxCondition (..)
    , _PtxApplying
    , _PtxInNewestBlocks
    , _PtxPersisted
    , _PtxWontApply

    , PtxBlockInfo
    , PtxPoolInfo
    ) where

import           Universum

import           Control.Lens                     (makeLenses, makePrisms)
import qualified Data.Text.Buildable
import           Formatting                       (bprint, build, (%))

import           Pos.Client.Txp.History           (TxHistoryEntry)
import           Pos.Core.Types                   (ChainDifficulty, SlotId)
import           Pos.Txp.Core.Types               (TxAux, TxId)
import           Pos.Wallet.Web.ClientTypes.Types (CId, Wal)

-- | Required information about block where given pending transaction is sited
type PtxBlockInfo = ChainDifficulty

-- | Information which is stored in pending tx pool when correspondent
-- transaction is not in blockchain
type PtxPoolInfo = TxHistoryEntry

-- | Current state of pending transaction.
--
-- Once transaction is created, it should be assigned 'PtxApplying' condition
-- in order to be tracked by resubmitter.
--
-- When wallet tracker notices a block with given transaction, it switches
-- condition to 'PtxInNewestBlocks' providing needed information about that
-- block. Transactions in this state are periodically tried to be submitted
-- again by special wallet worker.
--
-- Resubmitter also checks whether transaction is deep enough in blockchain to
-- be moved to 'PtxPersisted' state.
-- Transactions which are marked as persistent can lose their status if block
-- which contains this transactions gets rollbacked. However, if at some moment
-- transaction becomes just "not deep enough" due to rollbacks, it doesn't cause
-- the transaction to become 'PtxInNewestBlocks' again, because such small
-- "tides" of chain difficulty likely occur and we do not want to confuse users
-- switching status back and forth.
--
-- If transaction is ever noticed to be impossible to apply to current utxo,
-- it is assigned 'PtxWontApply' state and is stopped being tracked further
-- (effect can be canceled by BListener on rollback though).
-- This behaviour is to be improved in CSM-390.
data PtxCondition
    = PtxApplying PtxPoolInfo         -- ^ Is waiting to be applyed
    | PtxInNewestBlocks PtxBlockInfo  -- ^ Recently appeared in block.
    | PtxPersisted                    -- ^ Transaction is ~guaranteed to remain
                                      --   in blockchain
                                      --   (with up to *high* assurance level)
    | PtxWontApply Text PtxPoolInfo   -- ^ Can't be applyed and requires user's
                                      --   input to reform tx
    deriving (Eq, Show)

makePrisms ''PtxCondition

instance Buildable PtxCondition where
    build = \case
        PtxApplying{} ->
            "applying"
        PtxInNewestBlocks cd ->
            bprint ("in newest blocks (since "%build%" difficulty)") cd
        PtxPersisted ->
            "persisted"
        PtxWontApply reason _ ->
            bprint ("wont apply ("%build%")") reason

-- | All info kept about pending transaction
data PendingTx = PendingTx
    { _ptxTxId         :: !TxId  -- for the sake of optimization
    , _ptxTxAux        :: !TxAux
    , _ptxCreationSlot :: !SlotId  -- when tx was formed, for scheduling purposes.
                                   -- this in NOT when tx appeared in blockchain
    , _ptxCond         :: !PtxCondition
    , _ptxWallet       :: !(CId Wal)
    } deriving (Eq, Show)

makeLenses ''PendingTx
