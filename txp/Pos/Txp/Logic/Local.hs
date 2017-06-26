-- | Logic for local processing of transactions.
-- Local transaction is transaction which hasn't been added in the blockchain yet.

module Pos.Txp.Logic.Local
       ( txProcessTransaction
       , txNormalize
       ) where

import           Control.Monad.Except        (MonadError (..))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Default                (Default (def))
import qualified Data.HashSet                as HS
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as M (fromList)
import           Formatting                  (build, sformat, (%))
import           System.Wlog                 (WithLogger, logDebug)
import           Universum
import           Unsafe                      (unsafeHead)

import           Pos.Core                    (HeaderHash, genesisBootProdStakeholders)
import           Pos.Core.Constants          (isDevelopment)
import           Pos.DB.Class                (MonadDBRead, MonadGState, gsIsBootstrapEra)
import qualified Pos.DB.GState.Common        as GS
import           Pos.Txp.Core                (Tx (..), TxAux (..), TxId,
                                              getTxDistribution)
import           Pos.Txp.MemState            (MonadTxpMem, TxpLocalDataPure, getLocalTxs,
                                              getUtxoModifier, modifyTxpLocalData,
                                              setTxpLocalData)
import           Pos.Txp.Toil                (GenericToilModifier (..),
                                              MonadUtxoRead (..), ToilEnv,
                                              ToilVerFailure (..), Utxo, execToilTLocal,
                                              getToilEnv, normalizeToil, processTx,
                                              runDBToil, runToilTLocal, runUtxoReaderT,
                                              utxoGet)

type TxpLocalWorkMode m =
    ( MonadIO m
    , MonadBaseControl IO m
    , MonadDBRead m
    , MonadGState m
    , MonadTxpMem () m
    , WithLogger m
    , MonadError ToilVerFailure m
    )

-- CHECK: @processTx
-- #processTxDo
txProcessTransaction
    :: TxpLocalWorkMode m
    => (TxId, TxAux) -> m ()
txProcessTransaction itw@(txId, txAux) = do
    let UnsafeTx {..} = taTx txAux
    tipDB <- GS.getTip
    bootEra <- gsIsBootstrapEra
    localUM <- getUtxoModifier @()
    -- Note: snapshot isn't used here, because it's not necessary.  If
    -- tip changes after 'getTip' and before resolving all inputs, it's
    -- possible that invalid transaction will appear in
    -- mempool. However, in this case it will be removed by
    -- normalization before releasing lock on block application.
    (resolvedOuts, _) <- runDBToil $ runUM localUM $ mapM utxoGet _txInputs
    toilEnv <- runDBToil getToilEnv
    -- Resolved are unspent transaction outputs corresponding to input
    -- of given transaction.
    let resolved = M.fromList $
                   catMaybes $
                   toList $
                   NE.zipWith (liftM2 (,) . Just) _txInputs resolvedOuts
    pRes <- modifyTxpLocalData $
            processTxDo resolved toilEnv tipDB itw bootEra
    case pRes of
        Left er -> do
            logDebug $ sformat ("Transaction processing failed: "%build) txId
            throwError er
        Right _   ->
            logDebug (sformat ("Transaction is processed successfully: "%build) txId)
  where
    notBootRelated =
        let txDistr = getTxDistribution $ taDistribution txAux
            inBoot (s,_) = s `HS.member` genesisBootProdStakeholders
        in NE.filter (\txOutDistr -> any (not . inBoot) txOutDistr) txDistr

    processTxDo
        :: Utxo
        -> ToilEnv
        -> HeaderHash
        -> (TxId, TxAux)
        -> Bool
        -> TxpLocalDataPure
        -> (Either ToilVerFailure (), TxpLocalDataPure)
    processTxDo resolved toilEnv tipDB tx bootEra txld@(uv, mp, undo, tip, ())
        | tipDB /= tip =
            (Left $ ToilTipsMismatch tipDB tip, txld)
        | not isDevelopment && bootEra && not (null notBootRelated) =
            -- We do not allow txs with non-boot-addr stake distr in boot era.
            (Left $ ToilBootDifferentStake $ unsafeHead notBootRelated, txld)
        | otherwise =
            let res = (runExceptT $
                       flip runUtxoReaderT resolved $
                       execToilTLocal uv mp undo $
                       processTx tx) toilEnv
            in case res of
                Left er  -> (Left er, txld)
                Right ToilModifier{..} ->
                    (Right (), (_tmUtxo, _tmMemPool, _tmUndos, tip, ()))
    runUM um = runToilTLocal um def mempty

-- | 1. Recompute UtxoView by current MemPool
-- | 2. Remove invalid transactions from MemPool
-- | 3. Set new tip to txp local data
txNormalize ::
       ( MonadIO m
       , MonadBaseControl IO m
       , MonadDBRead m
       , MonadGState m
       , MonadTxpMem () m
       )
    => m ()
txNormalize = do
    utxoTip <- GS.getTip
    localTxs <- getLocalTxs
    ToilModifier {..} <-
        runDBToil $ execToilTLocal mempty def mempty $ normalizeToil localTxs
    setTxpLocalData (_tmUtxo, _tmMemPool, _tmUndos, utxoTip, _tmExtra)
