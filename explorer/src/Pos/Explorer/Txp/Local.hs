{-# LANGUAGE TypeFamilies #-}

-- | Explorer's local Txp.

module Pos.Explorer.Txp.Local
       ( eTxProcessTransaction
       , eTxProcessTransactionNoLock
       , eTxNormalize
       ) where

import           Universum

import           Control.Monad.Morph (generalize)
import qualified Data.HashMap.Strict as HM

import           Pos.Chain.Txp (ToilVerFailure (..), TxpConfiguration, Utxo)
import           Pos.Core (EpochIndex, Timestamp)
import           Pos.Core.Txp (TxAux (..), TxId)
import           Pos.Core.Update (BlockVersionData)
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB.GState.Lock (Priority (..), StateLock, StateLockMetrics,
                     withStateLock)
import           Pos.DB.Txp.Logic (txNormalizeAbstract,
                     txProcessTransactionAbstract)
import           Pos.DB.Txp.MemState (MempoolExt, TxpLocalWorkMode, getTxpExtra,
                     withTxpLocalData)
import           Pos.Infra.Slotting (MonadSlots (getCurrentSlot), getSlotStart)
import           Pos.Infra.Util.JsonLog.Events (MemPoolModifyReason (..))
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Trace (noTrace)
import           Pos.Util.Trace.Named (TraceNamed, natTrace)
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

eTxProcessTransaction ::
       ( ETxpLocalWorkMode ctx m
       , HasLens' ctx StateLock
       , HasLens' ctx (StateLockMetrics MemPoolModifyReason)
       )
    => TraceNamed m
    -> ProtocolMagic
    -> TxpConfiguration
    -> (TxId, TxAux)
    -> m (Either ToilVerFailure ())
eTxProcessTransaction _ pm txpConfig itw =
    withStateLock noTrace LowPriority ProcessTransaction $
        \__tip -> eTxProcessTransactionNoLock noTrace pm txpConfig itw

eTxProcessTransactionNoLock ::
       forall ctx m. (ETxpLocalWorkMode ctx m)
    => TraceNamed Identity
    -> ProtocolMagic
    -> TxpConfiguration
    -> (TxId, TxAux)
    -> m (Either ToilVerFailure ())
eTxProcessTransactionNoLock logTrace pm txpConfig itw = getCurrentSlot >>= \case
    Nothing   -> pure $ Left ToilSlotUnknown
    Just slot -> do
        -- First get the current @SlotId@ so we can calculate the time.
        -- Then get when that @SlotId@ started and use that as a time for @Tx@.
        mTxTimestamp <- getSlotStart slot
        txProcessTransactionAbstract (natTrace generalize logTrace) buildContext (processTx' mTxTimestamp) itw
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
        eProcessTx noTrace pm txpConfig bvd epoch tx (TxExtra Nothing mTxTimestamp)

-- | 1. Recompute UtxoView by current MemPool
--   2. Remove invalid transactions from MemPool
--   3. Set new tip to txp local data
eTxNormalize
    :: forall ctx m . (ETxpLocalWorkMode ctx m)
    => TraceNamed m
    -> ProtocolMagic
    -> TxpConfiguration
    -> m ()
eTxNormalize _ pm txpConfig = do
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
        in eNormalizeToil noTrace pm txpConfig bvd epoch toNormalize
