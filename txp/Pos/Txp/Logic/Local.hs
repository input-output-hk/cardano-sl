-- | Logic for local processing of transactions.
-- Local transaction is transaction which hasn't been added in the blockchain yet.

module Pos.Txp.Logic.Local
       ( txProcessTransaction
       , txNormalize
       ) where

import           Universum
import           Unsafe                      (unsafeHead)

import           Control.Lens                (views)
import           Control.Monad.Except        (MonadError (..))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Default                (Default (def))
import qualified Data.HashSet                as HS
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as M (fromList)
import           Ether.Internal              (HasLens (..))
import           Formatting                  (build, sformat, (%))
import           System.Wlog                 (WithLogger, logDebug)

import           Pos.Core                    (BlockVersionData, Coin, EpochIndex,
                                              HeaderHash, StakeholderId, siEpoch)
import           Pos.DB.Class                (MonadDBRead, MonadGState (..),
                                              gsIsBootstrapEra)
import qualified Pos.DB.GState.Common        as GS
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Txp.Core                (Tx (..), TxAux (..), TxId,
                                              getTxDistribution)
import           Pos.Txp.MemState            (MonadTxpMem, TxpLocalDataPure, getLocalTxs,
                                              getUtxoModifier, modifyTxpLocalData,
                                              setTxpLocalData)
import           Pos.Txp.Toil                (GenericToilModifier (..), GenesisUtxo (..),
                                              MonadUtxoRead (..), ToilEnv,
                                              ToilVerFailure (..), Utxo, evalUtxoStateT,
                                              execToilTLocal, getToilEnv, normalizeToil,
                                              processTx, runDBToil, runToilTLocal,
                                              utxoGet, utxoToStakes)
import           Pos.Util.Util               (getKeys)

type TxpLocalWorkMode ctx m =
    ( MonadIO m
    , MonadBaseControl IO m
    , MonadDBRead m
    , MonadGState m
    , MonadSlots m
    , MonadTxpMem () ctx m
    , WithLogger m
    , HasLens GenesisUtxo ctx GenesisUtxo
    )

-- | Process transaction. 'TxId' is expected to be the hash of
-- transaction in 'TxAux'. Separation is supported for optimization
-- only.
txProcessTransaction
    :: TxpLocalWorkMode ctx m
    => (TxId, TxAux) -> ExceptT ToilVerFailure m ()
txProcessTransaction itw@(txId, txAux) = do
    let UnsafeTx {..} = taTx txAux
    tipDB <- GS.getTip
    bvd <- gsAdoptedBVData
    genUtxo <- view (lensOf @GenesisUtxo)
    epoch <- siEpoch <$> (note ToilUnknownCurEpoch =<< getCurrentSlot)
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
    let resolved = M.fromList $
                   catMaybes $
                   toList $
                   NE.zipWith (liftM2 (,) . Just) _txInputs resolvedOuts
    pRes <- lift $
            modifyTxpLocalData "txProcessTransaction" $
            processTxDo epoch bvd genUtxo resolved tipDB itw
    case pRes of
        Left er -> do
            logDebug $ sformat ("Transaction processing failed: "%build) txId
            throwError er
        Right _   ->
            logDebug (sformat ("Transaction is processed successfully: "%build) txId)
  where
    processTxDo
        :: EpochIndex
        -> BlockVersionData
        -> GenesisUtxo
        -> Utxo
        -> HeaderHash
        -> (TxId, TxAux)
        -> TxpLocalDataPure
        -> (Either ToilVerFailure (), TxpLocalDataPure)
    processTxDo curEpoch bvd genUtxo resolved tipDB tx txld@(uv, mp, undo, tip, ())
        | tipDB /= tip =
            (Left $ ToilTipsMismatch tipDB tip, txld)
        | otherwise =
            let res = (runExceptT $
                    flip runReaderT genUtxo $
                    flip evalUtxoStateT resolved $
                    execToilTLocal uv mp undo $
                    processTx curEpoch tx) bvd
            in case res of
                Left er  -> (Left er, txld)
                Right ToilModifier{..} ->
                    (Right (), (_tmUtxo, _tmMemPool, _tmUndos, tip, ()))

-- | 1. Recompute UtxoView by current MemPool
-- | 2. Remove invalid transactions from MemPool
-- | 3. Set new tip to txp local data
txNormalize
    :: ( TxpLocalWorkMode ctx m
       , MonadSlots m)
    => m ()
txNormalize = do
    utxoTip <- GS.getTip
    epoch <- maybe (throwM ToilUnknownCurEpoch) (pure . siEpoch) =<< getCurrentSlot
    localTxs <- getLocalTxs
    ToilModifier {..} <-
        runDBToil $ execToilTLocal mempty def mempty $ normalizeToil epoch localTxs
    setTxpLocalData "txNormalize" (_tmUtxo, _tmMemPool, _tmUndos, utxoTip, _tmExtra)
