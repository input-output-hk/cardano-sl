{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Logic for local processing of transactions.
-- Local transaction is a transaction which has not yet been added to the blockchain.

module Pos.Txp.Logic.Local
       ( TxpProcessTransactionMode
       , txProcessTransaction
       , txProcessTransactionNoLock
       , txNormalize
       , txGetPayload

       -- * Utils for transaction processing and mempool normalization
       , txProcessTransactionAbstract
       , txNormalizeAbstract
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Control.Monad.Except (mapExceptT, runExceptT, throwError)
import           Control.Monad.Morph (generalize, hoist)
import           Data.Default (Default (def))
import qualified Data.HashMap.Strict as HM
import           Formatting (build, sformat, (%))
import           JsonLog (CanJsonLog (..))
import           System.Wlog (NamedPureLogger, WithLogger, launchNamedPureLog, logDebug, logError,
                              logWarning)

import           Pos.Core (BlockVersionData, EpochIndex, HeaderHash, HasProtocolMagic,
                           siEpoch)
import           Pos.Core.Txp (TxAux (..), TxId, TxUndo)
import           Pos.Crypto (WithHash (..))
import           Pos.DB.Class (MonadGState (..))
import qualified Pos.DB.GState.Common as GS
import           Pos.Reporting (reportError)
import           Pos.Slotting (MonadSlots (..))
import           Pos.StateLock (Priority (..), StateLock, StateLockMetrics, withStateLock)
import           Pos.Txp.Logic.Common (buildUtxo)
import           Pos.Txp.MemState (GenericTxpLocalData (..), MempoolExt, MonadTxpMem,
                                   TxpLocalWorkMode, getLocalTxsMap, getLocalUndos, getMemPool,
                                   getTxpExtra, getUtxoModifier, setTxpLocalData, withTxpLocalData,
                                   withTxpLocalDataLog)
import           Pos.Txp.Toil (ExtendedLocalToilM, LocalToilState (..), MemPool,
                               ToilVerFailure (..), UndoMap, Utxo, UtxoLookup, UtxoModifier,
                               extendLocalToilM, mpLocalTxs, normalizeToil, processTx, utxoToLookup)
import           Pos.Txp.Topsort (topsortTxs)
import           Pos.Util.JsonLog.Events (MemPoolModifyReason (..))
import           Pos.Util.Util (HasLens')

type TxpProcessTransactionMode ctx m =
    ( TxpLocalWorkMode ctx m
    , HasLens' ctx StateLock
    , HasLens' ctx (StateLockMetrics MemPoolModifyReason)
    , MempoolExt m ~ ()
    , CanJsonLog m
    )

-- | Process transaction. 'TxId' is expected to be the hash of
-- transaction in 'TxAux'. Separation is supported for optimization
-- only.
txProcessTransaction
    :: ( TxpProcessTransactionMode ctx m, HasProtocolMagic )
    => (TxId, TxAux) -> m (Either ToilVerFailure ())
txProcessTransaction itw =
    withStateLock LowPriority ProcessTransaction $ \__tip -> txProcessTransactionNoLock itw

-- | Unsafe version of 'txProcessTransaction' which doesn't take a
-- lock. Can be used in tests.
txProcessTransactionNoLock
    :: forall ctx m.
       ( TxpLocalWorkMode ctx m
       , MempoolExt m ~ ()
       , HasProtocolMagic
       )
    => (TxId, TxAux)
    -> m (Either ToilVerFailure ())
txProcessTransactionNoLock =
    txProcessTransactionAbstract buildContext processTxHoisted
  where
    buildContext :: Utxo -> TxAux -> m ()
    buildContext _ _ = pure ()

    processTxHoisted ::
           BlockVersionData
        -> EpochIndex
        -> (TxId, TxAux)
        -> ExceptT ToilVerFailure (ExtendedLocalToilM () ()) TxUndo
    processTxHoisted = mapExceptT extendLocalToilM ... processTx

txProcessTransactionAbstract ::
       forall extraEnv extraState ctx m a.
       (TxpLocalWorkMode ctx m, MempoolExt m ~ extraState)
    => (Utxo -> TxAux -> m extraEnv)
    -> (BlockVersionData -> EpochIndex -> (TxId, TxAux) -> ExceptT ToilVerFailure (ExtendedLocalToilM extraEnv extraState) a)
    -> (TxId, TxAux)
    -> m (Either ToilVerFailure ())
txProcessTransactionAbstract buildEnv txAction itw@(txId, txAux) = reportTipMismatch $ runExceptT $ do
    -- Note: we need to read tip from the DB and check that it's the
    -- same as the one in mempool. That's because mempool state is
    -- valid only with respect to the tip stored there. Normally tips
    -- will match, because whenever we apply/rollback blocks we
    -- normalize mempool. However, there is a corner case when we
    -- receive an unexpected exception after modifying GState and
    -- before normalization. In this case normalization can fail and
    -- tips will differ. Rejecting transactions in this case should be
    -- fine, because the fact that we receive exceptions likely
    -- indicates that something is bad and we have more serious issues.
    --
    -- Also note that we don't need to use a snapshot here and can be
    -- sure that GState won't change, because changing it requires
    -- 'StateLock' which we own inside this function.
    tipDB <- lift GS.getTip
    epoch <- siEpoch <$> (note ToilSlotUnknown =<< getCurrentSlot)
    utxoModifier <- withTxpLocalData getUtxoModifier
    utxo <- buildUtxo utxoModifier [txAux]
    extraEnv <- lift $ buildEnv utxo txAux
    bvd <- gsAdoptedBVData
    let env = (utxoToLookup utxo, extraEnv)

    pRes <- lift . withTxpLocalDataLog $ \txpData -> do
        mp <- lift $ getMemPool txpData
        undo <- lift $ getLocalUndos txpData
        tip <- lift $ STM.readTVar (txpTip txpData)
        extra <- lift $ getTxpExtra txpData
        tm <- hoist generalize $ processTransactionPure bvd epoch env tipDB itw (utxoModifier, mp, undo, tip, extra)
        forM tm $ lift . setTxpLocalData txpData
    -- We report 'ToilTipsMismatch' as an error, because usually it
    -- should't happen. If it happens, it's better to look at logs.
    case pRes of
        Left er -> do
            logDebug $ sformat ("Transaction processing failed: " %build) txId
            throwError er
        Right _ ->
            logDebug
                (sformat ("Transaction is processed successfully: " %build) txId)
  where
    processTransactionPure
        :: BlockVersionData
        -> EpochIndex
        -> (UtxoLookup, extraEnv)
        -> HeaderHash
        -> (TxId, TxAux)
        -> (UtxoModifier, MemPool, UndoMap, HeaderHash, extraState)
        -> NamedPureLogger Identity (Either ToilVerFailure (UtxoModifier, MemPool, UndoMap, HeaderHash, extraState))
    processTransactionPure bvd curEpoch env tipDB tx (um, mp, undo, tip, extraState)
        | tipDB /= tip = pure . Left $ ToilTipsMismatch tipDB tip
        | otherwise = do
            let initialState = LocalToilState { _ltsMemPool = mp
                                              , _ltsUtxoModifier = um
                                              , _ltsUndos = undo
                                              }
            res :: (Either ToilVerFailure a, (LocalToilState, extraState)) <-
                    usingStateT (initialState, extraState) $
                    usingReaderT env $
                    runExceptT $
                    txAction bvd curEpoch tx
            case res of
                (Left er, _) -> pure $ Left er
                (Right _, (LocalToilState {..}, newExtraState)) -> pure $ Right
                    (_ltsUtxoModifier, _ltsMemPool, _ltsUndos, tip, newExtraState)
    -- REPORT:ERROR Tips mismatch in txp.
    reportTipMismatch action = do
        res <- action
        res <$ case res of
            (Left err@(ToilTipsMismatch {})) -> reportError (pretty err)
            _                                -> pass

-- | 1. Recompute UtxoView by current MemPool
-- | 2. Remove invalid transactions from MemPool
-- | 3. Set new tip to txp local data
txNormalize
    :: forall ctx m.
       ( TxpLocalWorkMode ctx m
       , MempoolExt m ~ ()
       , HasProtocolMagic
       )
    => m ()
txNormalize =
    txNormalizeAbstract buildContext normalizeToilHoisted
  where
    buildContext :: Utxo -> [TxAux] -> m ()
    buildContext _ _ = pure ()

    normalizeToilHoisted ::
           BlockVersionData
        -> EpochIndex
        -> HashMap TxId TxAux
        -> ExtendedLocalToilM () () ()
    normalizeToilHoisted bvd epoch txs =
        extendLocalToilM $ normalizeToil bvd epoch $ HM.toList txs

txNormalizeAbstract ::
       (TxpLocalWorkMode ctx m, MempoolExt m ~ extraState, Default extraState)
    => (Utxo -> [TxAux] -> m extraEnv)
    -> (BlockVersionData -> EpochIndex -> HashMap TxId TxAux -> ExtendedLocalToilM extraEnv extraState ())
    -> m ()
txNormalizeAbstract buildEnv normalizeAction =
    getCurrentSlot >>= \case
        Nothing -> do
            tip <- GS.getTip
            -- Clear and update tip
            withTxpLocalData $ flip setTxpLocalData (mempty, def, mempty, tip, def)
        Just (siEpoch -> epoch) -> do
            globalTip <- GS.getTip
            localTxs <- withTxpLocalData getLocalTxsMap
            let txAuxes = toList localTxs
            utxo <- buildUtxo mempty txAuxes
            extraEnv <- buildEnv utxo txAuxes
            bvd <- gsAdoptedBVData
            let initialState =
                    LocalToilState
                        { _ltsMemPool = def
                        , _ltsUtxoModifier = mempty
                        , _ltsUndos = mempty
                        }
            (LocalToilState {..}, newExtraState) <-
                launchNamedPureLog generalize $
                execStateT
                    (runReaderT
                         (normalizeAction bvd epoch localTxs)
                         (utxoToLookup utxo, extraEnv))
                    (initialState, def)
            withTxpLocalData $ flip setTxpLocalData
                ( _ltsUtxoModifier
                , _ltsMemPool
                , _ltsUndos
                , globalTip
                , newExtraState)

-- | Get 'TxPayload' from mempool to include into a new block which
-- will be based on the given tip. In something goes wrong, empty
-- payload is returned. That's because we would sooner create an empty
-- block to maintain decent chain quality than skip block creation.
--
-- We need to explicitly check that tip matches, even though we do
-- mempool normalization whenever we apply/rollback a block. That's
-- because we can't make them both atomically, i. e. can't guarantee
-- that either none or both of them will be done.
txGetPayload :: (MonadIO m, MonadTxpMem ext ctx m, WithLogger m) => HeaderHash -> m [TxAux]
txGetPayload neededTip = do
    (view mpLocalTxs -> memPool, memPoolTip) <- withTxpLocalData $ \(TxpLocalData{..}) ->
        (,) <$> readTVar txpMemPool <*> readTVar txpTip
    let tipMismatchMsg =
            sformat
                ("txGetPayload: tip mismatch (in DB: )"%build%
                 ", (in mempool: "%build%")")
                neededTip memPoolTip
    let topsortFailMsg = "txGetPayload: topsort failed!"
    let convertTx (txId, txAux) = WithHash (taTx txAux) txId
    case (memPoolTip == neededTip, topsortTxs convertTx $ HM.toList memPool) of
        (False, _)       -> [] <$ logWarning tipMismatchMsg
        (True, Nothing)  -> [] <$ logError topsortFailMsg
        (True, Just res) -> return $ map snd res
