{-# LANGUAGE TypeFamilies #-}

-- | Explorer's local Txp.

module Pos.Explorer.Txp.Local
       ( eTxProcessTransaction
       , eTxProcessTransactionNoLock
       , eTxNormalize
       ) where

import           Nub (ordNub)
import           Universum

import qualified Data.HashMap.Strict as HM
import           System.Wlog (NamedPureLogger)

import           Pos.Core.Txp (Tx (..), TxAux (..), TxId, toaOut, txOutAddress)
import           Pos.DB.Class (MonadDBRead, MonadGState (..))
import qualified Pos.Explorer.DB as ExDB
import           Pos.Slotting (MonadSlots (getCurrentSlot), getSlotStart)
import           Pos.StateLock (Priority (..), StateLock, StateLockMetrics, withStateLock)
import           Pos.Txp.Logic.Local (ProcessTxContext (..), buildProccessTxContext, ptcExtra,
                                      txNormalizeAbstract, txProcessTransactionAbstract)
import           Pos.Txp.MemState (MempoolExt, MonadTxpMem, TxpLocalWorkMode, getTxpExtra)
import           Pos.Txp.Toil (ToilVerFailure (..))
import           Pos.Util.Chrono (NewestFirst (..))
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Util (HasLens')

import           Pos.Explorer.Core (TxExtra (..))
import           Pos.Explorer.Txp.Toil (ExplorerExtra, ExplorerExtraTxp (..), MonadTxExtraRead (..),
                                        eNormalizeToil, eProcessTx, eeLocalTxsExtra)


type ETxpLocalWorkMode ctx m =
    ( TxpLocalWorkMode ctx m
    , MempoolExt m ~ ExplorerExtra
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
        HasLens' ctx StateLock, HasLens' ctx StateLockMetrics)
    => (TxId, TxAux) -> m (Either ToilVerFailure ())
eTxProcessTransaction itw =
    withStateLock LowPriority "eTxProcessTransaction" $ \__tip -> eTxProcessTransactionNoLock itw

eTxProcessTransactionNoLock
    :: ETxpLocalWorkMode ctx m
    => (TxId, TxAux) -> m (Either ToilVerFailure ())
eTxProcessTransactionNoLock itw = getCurrentSlot >>= \case
    Nothing   -> pure $ Left ToilSlotUnknown
    Just slot -> do
        -- First get the current @SlotId@ so we can calculate the time.
        -- Then get when that @SlotId@ started and use that as a time for @Tx@.
        mTxTimestamp <- getSlotStart slot
        txProcessTransactionAbstract
            buildEProcessTxContext
            (\e tx -> eProcessTx e tx (TxExtra Nothing mTxTimestamp))
            itw

buildEProcessTxContext
    :: forall m ctx.
       ( MonadIO m
       , MonadDBRead m
       , MonadGState m
       , MonadTxpMem (MempoolExt m) ctx m
       )
    => TxAux -> m EProcessTxContext
buildEProcessTxContext txAux = do
    ProcessTxContext{..} <- buildProccessTxContext txAux
    let UncheckedTx {..} = taTx txAux
    let txInAddrs = map (txOutAddress . toaOut) $ toList _ptcUtxoBase
        txOutAddrs = toList $ map txOutAddress _txOutputs
        allAddrs = ordNub $ txInAddrs <> txOutAddrs
    hmHistories <-
        buildMap allAddrs <$> mapM (fmap Just . ExDB.getAddrHistory) allAddrs
    hmBalances <- buildMap allAddrs <$> mapM ExDB.getAddrBalance allAddrs
    utxoSum <- ExDB.getUtxoSum
    -- `eet` is passed to `txProcessTransactionAbstract` where it is used in
    -- a ReaderT environment to provide underlying functions (`modifyAddrHistory`
    -- and `modifyAddrBalance`) with data to update. In case of `TxExtra` data
    -- is only added, but never updated, hence `mempty` here.
    let eet = ExplorerExtraTxp mempty hmHistories hmBalances utxoSum
    pure $ ProcessTxContext _ptcAdoptedBVData _ptcUtxoBase eet
  where
    buildMap :: (Eq a, Hashable a) => [a] -> [Maybe b] -> HM.HashMap a b
    buildMap keys maybeValues =
        HM.fromList $
        catMaybes $ toList $ zipWith (liftM2 (,) . Just) keys maybeValues

-- | 1. Recompute UtxoView by current MemPool
--   2. Remove invalid transactions from MemPool
--   3. Set new tip to txp local data
eTxNormalize ::
       ( ETxpLocalWorkMode ctx m
       , MonadSlots ctx m
       )
    => m ()
eTxNormalize = do
    extra <- getTxpExtra
    let extras = MM.insertionsMap $ extra ^. eeLocalTxsExtra
    txNormalizeAbstract $ \e localTxs -> do
        let toNormalize = HM.toList $ HM.intersectionWith (,) localTxs extras
        eNormalizeToil e toNormalize
