-- | Logic for local processing of transactions.
-- Local transaction is transaction which hasn't been added in the blockchain yet.

module Pos.Txp.Logic.Local
       ( txProcessTransaction
       , txNormalize
       ) where

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
import           Universum
import           Unsafe                      (unsafeHead)

import           Pos.Core                    (Coin, HeaderHash, StakeholderId,
                                              Stakeholders)
import           Pos.DB.Class                (MonadDBRead, MonadGState, gsIsBootstrapEra)
import qualified Pos.DB.GState.Common        as GS
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
    , MonadTxpMem () ctx m
    , WithLogger m
    , HasLens GenesisUtxo ctx GenesisUtxo
    , MonadError ToilVerFailure m
    )

-- CHECK: @processTx
-- #processTxDo
txProcessTransaction
    :: TxpLocalWorkMode ctx m
    => (TxId, TxAux) -> m ()
txProcessTransaction itw@(txId, txAux) = do
    let UnsafeTx {..} = taTx txAux
    tipDB <- GS.getTip
    bootEra <- gsIsBootstrapEra
    bootHolders <- views (lensOf @GenesisUtxo) $ getKeys . utxoToStakes . unGenesisUtxo
    localUM <- getUtxoModifier @()
    -- Note: snapshot isn't used here, because it's not necessary.  If
    -- tip changes after 'getTip' and before resolving all inputs, it's
    -- possible that invalid transaction will appear in
    -- mempool. However, in this case it will be removed by
    -- normalization before releasing lock on block application.
    let runUM um = runToilTLocal um def mempty
    (resolvedOuts, _) <- runDBToil $ runUM localUM $ mapM utxoGet _txInputs
    toilEnv <- runDBToil getToilEnv
    -- Resolved are unspent transaction outputs corresponding to input
    -- of given transaction.
    let resolved = M.fromList $
                   catMaybes $
                   toList $
                   NE.zipWith (liftM2 (,) . Just) _txInputs resolvedOuts
    pRes <- modifyTxpLocalData $
            processTxDo resolved bootHolders toilEnv tipDB itw bootEra
    case pRes of
        Left er -> do
            logDebug $ sformat ("Transaction processing failed: "%build) txId
            throwError er
        Right _   ->
            logDebug (sformat ("Transaction is processed successfully: "%build) txId)
  where
    notBootRelated bootHolders =
        let txDistr = getTxDistribution $ taDistribution txAux
            inBoot s = s `HS.member` bootHolders
            mentioned pool addr = addr `elem` pool
            bad :: [(StakeholderId, Coin)] -> Bool
            bad (map fst -> txOutDistr) =
                -- Has unrelated address
                any (not . inBoot) txOutDistr ||
                -- Not all genesis boot addrs are mentioned
                any (not . mentioned txOutDistr) (HS.toList bootHolders)
        in NE.filter bad txDistr

    processTxDo
        :: Utxo
        -> Stakeholders
        -> ToilEnv
        -> HeaderHash
        -> (TxId, TxAux)
        -> Bool
        -> TxpLocalDataPure
        -> (Either ToilVerFailure (), TxpLocalDataPure)
    processTxDo resolved bootHolders toilEnv tipDB tx bootEra txld@(uv, mp, undo, tip, ()) = do
        let tipMismatch = tipDB /= tip
            bootRel = notBootRelated bootHolders
        if | tipMismatch ->
             (Left $ ToilTipsMismatch tipDB tip, txld)
           | bootEra && not (null bootRel) ->
            -- We do not allow txs with non-boot-addr stake distr in boot era.
             (Left $ ToilBootDifferentStake $ unsafeHead bootRel, txld)
           | otherwise ->
             let res = (runExceptT $
                        flip evalUtxoStateT resolved $
                        execToilTLocal uv mp undo $
                        processTx tx) toilEnv
             in case res of
                 Left er  -> (Left er, txld)
                 Right ToilModifier{..} ->
                     (Right (), (_tmUtxo, _tmMemPool, _tmUndos, tip, ()))

-- | 1. Recompute UtxoView by current MemPool
-- | 2. Remove invalid transactions from MemPool
-- | 3. Set new tip to txp local data
txNormalize ::
       ( MonadIO m
       , MonadBaseControl IO m
       , MonadDBRead m
       , MonadGState m
       , MonadTxpMem () ctx m
       )
    => m ()
txNormalize = do
    utxoTip <- GS.getTip
    localTxs <- getLocalTxs
    ToilModifier {..} <-
        runDBToil $ execToilTLocal mempty def mempty $ normalizeToil localTxs
    setTxpLocalData (_tmUtxo, _tmMemPool, _tmUndos, utxoTip, _tmExtra)
