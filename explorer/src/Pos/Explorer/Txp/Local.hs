{-# LANGUAGE TypeFamilies #-}

-- | Explorer's local Txp.

module Pos.Explorer.Txp.Local
       ( eTxProcessTransaction
       , eTxProcessTransactionNoLock
       , eTxNormalize
       ) where

import           JsonLog (CanJsonLog (..))
import           Nub (ordNub)
import           Universum

import qualified Data.HashMap.Strict as HM

import           Pos.Core (BlockVersionData, EpochIndex, Timestamp)
import           Pos.Core.Txp (TxAux (..), TxId)
import           Pos.Slotting (MonadSlots (getCurrentSlot), getSlotStart)
import           Pos.StateLock (Priority (..), StateLock, StateLockMetrics, withStateLock)
import           Pos.Txp.Logic.Local (ProcessTxContext (..), buildProccessTxContext, ptcExtra,
                                      txNormalizeAbstract, txProcessTransactionAbstract)
import           Pos.Txp.MemState (MempoolExt, MemPoolModifyReason (..), MonadTxpMem,
                                   TxpLocalWorkMode, getTxpExtra)
import           Pos.Txp.Toil (ToilVerFailure (..))
import           Pos.Util.Chrono (NewestFirst (..))
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Util (HasLens')

import           Pos.Explorer.Core (TxExtra (..))
import           Pos.Explorer.Txp.Common (buildExplorerExtraLookup)
import           Pos.Explorer.Txp.Toil (ELocalToilM, ExplorerExtraLookup (..),
                                        ExplorerExtraModifier, eNormalizeToil, eProcessTx,
                                        eemLocalTxsExtra)


type ETxpLocalWorkMode ctx m =
    ( TxpLocalWorkMode ctx m
    , MempoolExt m ~ ExplorerExtraModifier
    )

-- Base context for tx processing in explorer.
type EProcessTxContext = ProcessTxContext ExplorerExtraTxp

-- Base monad for tx processing in explorer.
type EProcessTxMode = ReaderT EProcessTxContext (NamedPureLogger Identity)

instance MonadTxExtraRead EProcessTxMode where
    getTxExtra txId = HM.lookup txId . eetTxExtra <$> view ptcExtra
    getAddrHistory addr =
        HM.lookupDefault (NewestFirst []) addr . eetAddrHistories <$>
        view ptcExtra
    getAddrBalance addr =
        HM.lookup addr . eetAddrBalances <$> view ptcExtra
    getUtxoSum =
        eetUtxoSum <$> view ptcExtra

eTxProcessTransaction
    :: (ETxpLocalWorkMode ctx m, MonadMask m,
        HasLens' ctx StateLock, HasLens' ctx (StateLockMetrics MemPoolModifyReason),
        CanJsonLog m)
    => (TxId, TxAux) -> m (Either ToilVerFailure ())
eTxProcessTransaction itw =
    withStateLock LowPriority ProcessTransaction $ \__tip -> eTxProcessTransactionNoLock itw

eTxProcessTransactionNoLock ::
       forall ctx m. (ETxpLocalWorkMode ctx m)
    => (TxId, TxAux)
    -> m (Either ToilVerFailure ())
eTxProcessTransactionNoLock itw = getCurrentSlot >>= \case
    Nothing   -> pure $ Left ToilSlotUnknown
    Just slot -> do
        -- First get the current @SlotId@ so we can calculate the time.
        -- Then get when that @SlotId@ started and use that as a time for @Tx@.
        mTxTimestamp <- getSlotStart slot
        txProcessTransactionAbstract buildContext (processTx' mTxTimestamp) itw
  where
    buildContext :: Utxo -> TxAux -> m ExplorerExtraLookup
    buildContext utxo = buildExplorerExtraLookup utxo . one

    processTx' ::
           Maybe Timestamp
        -> BlockVersionData
        -> EpochIndex
        -> (TxId, TxAux)
        -> ExceptT ToilVerFailure ELocalToilM ()
    processTx' mTxTimestamp bvd epoch tx =
        eProcessTx bvd epoch tx (TxExtra Nothing mTxTimestamp)

-- | 1. Recompute UtxoView by current MemPool
--   2. Remove invalid transactions from MemPool
--   3. Set new tip to txp local data
eTxNormalize ::
       forall ctx m. (ETxpLocalWorkMode ctx m)
    => m ()
eTxNormalize = do
    extras <- MM.insertionsMap . view eemLocalTxsExtra <$> withTxpLocalData getTxpExtra
    txNormalizeAbstract buildExplorerExtraLookup (normalizeToil' extras)
  where
    normalizeToil' ::
           HashMap TxId TxExtra
        -> BlockVersionData
        -> EpochIndex
        -> HashMap TxId TxAux
        -> ELocalToilM ()
    normalizeToil' extras bvd epoch txs =
        let toNormalize = HM.toList $ HM.intersectionWith (,) txs extras
        in eNormalizeToil bvd epoch toNormalize
