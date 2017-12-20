{-# LANGUAGE RankNTypes #-}

-- | Logic for local processing of transactions.
-- Local transaction is transaction which hasn't been added in the blockchain yet.

module Pos.Txp.Logic.Local
       ( txProcessTransaction
       , txProcessTransactionNoLock
       , txNormalize
       , txGetPayload
       ) where

import           Universum

import           Control.Lens         (makeLenses)
import           Control.Monad.Except (MonadError (..), runExceptT)
import           Data.Default         (Default (def))
import qualified Data.HashMap.Strict  as HM
import qualified Data.List.NonEmpty   as NE
import qualified Data.Map             as M (fromList)
import           Formatting           (build, sformat, (%))
import           Mockable             (CurrentTime, Mockable)
import           System.Wlog          (WithLogger, logDebug, logError, logWarning)

import           Pos.Core             (BlockVersionData, EpochIndex, HasConfiguration,
                                       HeaderHash, siEpoch)
import           Pos.Crypto           (WithHash (..))
import           Pos.DB.Class         (MonadDBRead, MonadGState (..))
import qualified Pos.DB.GState.Common as GS
import           Pos.Reporting        (MonadReporting, reportError)
import           Pos.Slotting         (MonadSlots (..))
import           Pos.StateLock        (Priority (..), StateLock, StateLockMetrics,
                                       withStateLock)
import           Pos.Txp.Core         (Tx (..), TxAux (..), TxId, TxUndo, topsortTxs)
import           Pos.Txp.MemState     (GenericTxpLocalData (..), MemPoolSnapshot,
                                       MonadTxpMem, TxpLocalDataPure, askTxpMem,
                                       getLocalTxs, modifyTxpLocalData, setTxpLocalData)
import           Pos.Txp.Toil         (GenericToilModifier (..), MonadUtxoRead (..),
                                       ToilModifier, ToilT, ToilVerFailure (..), Utxo,
                                       UtxoModifier, execToilTLocal, mpLocalTxs,
                                       normalizeToil, processTx, runDBToil, runToilTLocal,
                                       utxoGetReader)
import           Pos.Util.Util        (HasLens (..), HasLens')

type TxpLocalWorkMode ctx m =
    ( MonadIO m
    , MonadDBRead m
    , MonadGState m
    , MonadSlots ctx m
    , MonadTxpMem () ctx m
    , WithLogger m
    , Mockable CurrentTime m
    , MonadMask m
    , MonadReporting ctx m
    , HasConfiguration
    )

-- Base context for tx processing in.
data ProcessTxContext = ProcessTxContext
    { _ptcAdoptedBVData :: !BlockVersionData
    , _ptcUtxoBase      :: !Utxo
    }

makeLenses ''ProcessTxContext

instance HasLens Utxo ProcessTxContext Utxo where
    lensOf = ptcUtxoBase

-- Base monad for tx processing in.
type ProcessTxMode = Reader ProcessTxContext

instance HasConfiguration => MonadUtxoRead ProcessTxMode where
    utxoGet = utxoGetReader

instance MonadGState ProcessTxMode where
    gsAdoptedBVData = view ptcAdoptedBVData

-- | Process transaction. 'TxId' is expected to be the hash of
-- transaction in 'TxAux'. Separation is supported for optimization
-- only.
txProcessTransaction
    :: (TxpLocalWorkMode ctx m, HasLens' ctx StateLock,
        HasLens' ctx StateLockMetrics, MonadMask m)
    => UtxoModifier -> (TxId, TxAux) -> m (Either ToilVerFailure ())
txProcessTransaction localUM itw =
    withStateLock LowPriority "txProcessTransaction" $ \__tip -> txProcessTransactionNoLock localUM itw

-- | Unsafe version of 'txProcessTransaction' which doesn't take a
-- lock. Can be used in tests.
txProcessTransactionNoLock
    :: (TxpLocalWorkMode ctx m)
    => UtxoModifier
    -> (TxId, TxAux)
    -> m (Either ToilVerFailure ())
txProcessTransactionNoLock localUM itw@(txId, txAux) = reportTipMismatch $ runExceptT $ do
    let UnsafeTx {..} = taTx txAux
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
    tipDB <- GS.getTip
    bvd <- gsAdoptedBVData
    epoch <- siEpoch <$> (note ToilSlotUnknown =<< getCurrentSlot)
    let runUM um = runToilTLocal um def mempty
    (resolvedOuts, _) <- runDBToil $ runUM localUM $ mapM utxoGet _txInputs
    -- Resolved are unspent transaction outputs corresponding to input
    -- of given transaction.
    let resolved =
            M.fromList $
            catMaybes $
            toList $ NE.zipWith (liftM2 (,) . Just) _txInputs resolvedOuts
    let ctx =
            ProcessTxContext
            { _ptcAdoptedBVData = bvd
            , _ptcUtxoBase = resolved
            }
    pRes <-
        lift $
        modifyTxpLocalData $
        processTxDo epoch ctx tipDB itw
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
    processTxDo ::
           EpochIndex
        -> ProcessTxContext
        -> HeaderHash
        -> (TxId, TxAux)
        -> TxpLocalDataPure
        -> (Either ToilVerFailure (), TxpLocalDataPure)
    processTxDo curEpoch ctx tipDB tx txld@(uv, mp, undo, tip, ())
        | tipDB /= tip = (Left $ ToilTipsMismatch tipDB tip, txld)
        | otherwise =
            let action :: ExceptT ToilVerFailure (ToilT () ProcessTxMode) TxUndo
                action = processTx curEpoch tx
                res :: (Either ToilVerFailure TxUndo, ToilModifier)
                res =
                    usingReader ctx $
                    runToilTLocal uv mp undo $ runExceptT action
            in case res of
                   (Left er, _) -> (Left er, txld)
                   (Right _, ToilModifier {..}) ->
                       ( Right ()
                       , (_tmUtxo, _tmMemPool, _tmUndos, tip, _tmExtra))
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
    :: ( TxpLocalWorkMode ctx m
       , MonadSlots ctx m)
    => MemPoolSnapshot -> m ()
txNormalize memPoolSnapshot = getCurrentSlot >>= \case
    Nothing -> do
        tip <- GS.getTip
        -- Clear and update tip
        setTxpLocalData (mempty, def, mempty, tip, def)
    Just (siEpoch -> epoch) -> do
        utxoTip <- GS.getTip
        let localTxs = getLocalTxs memPoolSnapshot
        ToilModifier {..} <-
            runDBToil $ execToilTLocal mempty def mempty $ normalizeToil epoch localTxs
        setTxpLocalData (_tmUtxo, _tmMemPool, _tmUndos, utxoTip, _tmExtra)

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
    TxpLocalData {..} <- askTxpMem
    (view mpLocalTxs -> memPool, memPoolTip) <-
        atomically $ (,) <$> readTVar txpMemPool <*> readTVar txpTip
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
