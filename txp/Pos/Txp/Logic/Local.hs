{-# LANGUAGE RankNTypes #-}

-- | Logic for local processing of transactions.
-- Local transaction is transaction which hasn't been added in the blockchain yet.

module Pos.Txp.Logic.Local
       ( txProcessTransaction
       , txProcessTransactionNoLock
       , txNormalize
       ) where

import           Universum

import           Control.Lens                (makeLenses)
import           Control.Monad.Except        (MonadError (..), runExceptT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Default                (Default (def))
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as M (fromList)
import           Formatting                  (build, sformat, (%))
import           System.Wlog                 (WithLogger, logDebug)

import           Pos.Core                    (BlockVersionData, EpochIndex,
                                              GenesisWStakeholders, HeaderHash, siEpoch)
import           Pos.DB.Class                (MonadDBRead, MonadGState (..))
import qualified Pos.DB.GState.Common        as GS
import           Pos.Infra.Semaphore         (BlkSemaphore, withBlkSemaphoreIgnoreTip)
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Txp.Core                (Tx (..), TxAux (..), TxId, TxUndo)
import           Pos.Txp.MemState            (MonadTxpMem, TxpLocalDataPure, getLocalTxs,
                                              getUtxoModifier, modifyTxpLocalData,
                                              setTxpLocalData)
import           Pos.Txp.Toil                (GenericToilModifier (..),
                                              MonadUtxoRead (..), ToilModifier, ToilT,
                                              ToilVerFailure (..), Utxo, execToilTLocal,
                                              normalizeToil, processTx, runDBToil,
                                              runToilTLocal, utxoGetReader)
import           Pos.Util.Util               (HasLens (..), HasLens')

type TxpLocalWorkMode ctx m =
    ( MonadIO m
    , MonadBaseControl IO m
    , MonadDBRead m
    , MonadGState m
    , MonadSlots ctx m
    , MonadTxpMem () ctx m
    , WithLogger m
    , HasLens' ctx GenesisWStakeholders
    )

-- Base context for tx processing in.
data ProcessTxContext = ProcessTxContext
    { _ptcGenStakeholders :: !GenesisWStakeholders
    , _ptcAdoptedBVData   :: !BlockVersionData
    , _ptcUtxoBase        :: !Utxo
    }

makeLenses ''ProcessTxContext

instance HasLens GenesisWStakeholders ProcessTxContext GenesisWStakeholders where
    lensOf = ptcGenStakeholders

instance HasLens Utxo ProcessTxContext Utxo where
    lensOf = ptcUtxoBase

-- Base monad for tx processing in.
type ProcessTxMode = Reader ProcessTxContext

instance MonadUtxoRead ProcessTxMode where
    utxoGet = utxoGetReader

instance MonadGState ProcessTxMode where
    gsAdoptedBVData = view ptcAdoptedBVData

-- | Process transaction. 'TxId' is expected to be the hash of
-- transaction in 'TxAux'. Separation is supported for optimization
-- only.
txProcessTransaction
    :: (TxpLocalWorkMode ctx m, HasLens' ctx BlkSemaphore, MonadMask m)
    => (TxId, TxAux) -> m (Either ToilVerFailure ())
txProcessTransaction itw = withBlkSemaphoreIgnoreTip $ txProcessTransactionNoLock itw

-- | Unsafe version of 'txProcessTransaction' which doesn't take a
-- lock. Can be used in tests.
txProcessTransactionNoLock
    :: (TxpLocalWorkMode ctx m)
    => (TxId, TxAux) -> m (Either ToilVerFailure ())
txProcessTransactionNoLock itw@(txId, txAux) = runExceptT $ do
    let UnsafeTx {..} = taTx txAux
    -- Note: we don't need to check tip, but it's for proof of concept.
    tipDB <- GS.getTip
    bvd <- gsAdoptedBVData
    epoch <- siEpoch <$> (note ToilSlotUnknown =<< getCurrentSlot)
    bootHolders <- view (lensOf @GenesisWStakeholders)
    localUM <- lift $ getUtxoModifier @()
    -- Note: snapshot isn't used here, because it's not necessary.  If
    -- tip changes after 'getTip' and before resolving all inputs, it's
    -- possible that invalid transaction will appear in
    -- mempool. However, in this case it will be removed by
    -- normalization before releasing lock on block application.
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
            { _ptcGenStakeholders = bootHolders
            , _ptcAdoptedBVData = bvd
            , _ptcUtxoBase = resolved
            }
    pRes <-
        lift $
        modifyTxpLocalData "txProcessTransaction" $
        processTxDo epoch ctx tipDB itw
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

-- | 1. Recompute UtxoView by current MemPool
-- | 2. Remove invalid transactions from MemPool
-- | 3. Set new tip to txp local data
txNormalize
    :: ( TxpLocalWorkMode ctx m
       , MonadSlots ctx m)
    => m ()
txNormalize = getCurrentSlot >>= \case
    Nothing -> do
        tip <- GS.getTip
        -- Clear and update tip
        setTxpLocalData "txNormalize" (mempty, def, mempty, tip, def)
    Just (siEpoch -> epoch) -> do
        utxoTip <- GS.getTip
        localTxs <- getLocalTxs
        ToilModifier {..} <-
            runDBToil $ execToilTLocal mempty def mempty $ normalizeToil epoch localTxs
        setTxpLocalData "txNormalize" (_tmUtxo, _tmMemPool, _tmUndos, utxoTip, _tmExtra)
