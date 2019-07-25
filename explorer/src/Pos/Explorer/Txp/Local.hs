{-# LANGUAGE TypeFamilies #-}

-- | Explorer's local Txp.

module Pos.Explorer.Txp.Local
       ( eTxProcessTransaction
       , eTxNormalize
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM

import           Pos.Chain.Genesis as Genesis (Config (..), configEpochSlots)
import           Pos.Chain.Txp (ToilVerFailure (..), TxAux (..), TxId,
                     TxValidationRules, TxpConfiguration, Utxo)
import           Pos.Chain.Update (BlockVersionData)
import           Pos.Core (EpochIndex, Timestamp)
import           Pos.Core.JsonLog (CanJsonLog (..))
import           Pos.DB.Txp.Logic (txNormalizeAbstract,
                     txProcessTransactionAbstract)
import           Pos.DB.Txp.MemState (MempoolExt, TxpLocalWorkMode, getTxpExtra,
                     withTxpLocalData)
import           Pos.Infra.Slotting (MonadSlots (getCurrentSlot), getSlotStart)
import           Pos.Infra.StateLock (Priority (..), StateLock,
                     StateLockMetrics, withStateLock)
import           Pos.Infra.Util.JsonLog.Events (MemPoolModifyReason (..))
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

eTxProcessTransaction ::
       ( ETxpLocalWorkMode ctx m
       , HasLens' ctx StateLock
       , HasLens' ctx (StateLockMetrics MemPoolModifyReason)
       , CanJsonLog m
       )
    => Genesis.Config
    -> TxpConfiguration
    -> (TxId, TxAux)
    -> m (Either ToilVerFailure ())
eTxProcessTransaction genesisConfig txpConfig itw =
    withStateLock LowPriority ProcessTransaction
        $ \__tip -> eTxProcessTransactionNoLock genesisConfig txpConfig itw

eTxProcessTransactionNoLock ::
       forall ctx m. (ETxpLocalWorkMode ctx m)
    => Genesis.Config
    -> TxpConfiguration
    -> (TxId, TxAux)
    -> m (Either ToilVerFailure ())
eTxProcessTransactionNoLock genesisConfig txpConfig itw = getCurrentSlot epochSlots >>= \case
    Nothing   -> pure $ Left ToilSlotUnknown
    Just slot -> do
        -- First get the current @SlotId@ so we can calculate the time.
        -- Then get when that @SlotId@ started and use that as a time for @Tx@.
        mTxTimestamp <- getSlotStart slot
        txProcessTransactionAbstract epochSlots
                                     genesisConfig
                                     buildContext
                                     (processTx' mTxTimestamp)
                                     itw
  where
    epochSlots = configEpochSlots genesisConfig
    buildContext :: Utxo -> TxAux -> m ExplorerExtraLookup
    buildContext utxo = buildExplorerExtraLookup utxo . one

    processTx' ::
           Maybe Timestamp
        -> BlockVersionData
        -> TxValidationRules
        -> EpochIndex
        -> (TxId, TxAux)
        -> ExceptT ToilVerFailure ELocalToilM ()
    processTx' mTxTimestamp bvd txValRules epoch tx = eProcessTx
        (configProtocolMagic genesisConfig)
        txValRules
        txpConfig
        bvd
        epoch
        tx
        (TxExtra Nothing mTxTimestamp)

-- | 1. Recompute UtxoView by current MemPool
--   2. Remove invalid transactions from MemPool
--   3. Set new tip to txp local data
eTxNormalize
    :: forall ctx m . (ETxpLocalWorkMode ctx m)
    => Genesis.Config
    -> TxValidationRules
    -> TxpConfiguration
    -> m ()
eTxNormalize genesisConfig txValRules txpConfig = do
    extras <- MM.insertionsMap . view eemLocalTxsExtra <$> withTxpLocalData getTxpExtra
    txNormalizeAbstract (configEpochSlots genesisConfig)
                        buildExplorerExtraLookup
                        (normalizeToil' extras txValRules)
  where
    normalizeToil' ::
           HashMap TxId TxExtra
        -> TxValidationRules
        -> BlockVersionData
        -> EpochIndex
        -> HashMap TxId TxAux
        -> ELocalToilM ()
    normalizeToil' extras txValRules' bvd epoch txs =
        let toNormalize = HM.toList $ HM.intersectionWith (,) txs extras
        in eNormalizeToil (configProtocolMagic genesisConfig)
                          txValRules'
                          txpConfig
                          bvd
                          epoch
                          toNormalize
