{-# LANGUAGE TypeFamilies #-}

-- | All logic of Toil.  It operates in terms of MonadUtxo,
-- MonadToilEnv, MonadBalances and MonadTxPool.

module Pos.Txp.Toil.Logic
       ( GlobalToilMode
       , verifyToil
       , applyToil
       , rollbackToil

       , LocalToilMode
       , normalizeToil
       , processTx

       , verifyAndApplyTx
       ) where

import           Universum

import           Control.Monad.Except       (MonadError (..))
import           Serokell.Data.Memory.Units (Byte)
import           System.Wlog                (WithLogger)

import           Pos.Binary.Class           (biSize)
import           Pos.Core.Coin              (integerToCoin)
import           Pos.Core.Constants         (memPoolLimitRatio)
import qualified Pos.Core.Fee               as Fee
import           Pos.Crypto                 (WithHash (..), hash)
import           Pos.Txp.Core               (TxAux (..), TxId, TxUndo, TxpUndo,
                                             topsortTxs)
import           Pos.Txp.Toil.Balances      (applyTxsToBalances, rollbackTxsBalances)
import           Pos.Txp.Toil.Class         (MonadBalances (..), MonadToilEnv (..),
                                             MonadTxPool (..), MonadUtxo (..))
import           Pos.Txp.Toil.Failure       (ToilVerFailure (..))
import           Pos.Txp.Toil.Types         (ToilEnv (..), TxFee (..))
import qualified Pos.Txp.Toil.Utxo          as Utxo

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

type GlobalToilMode m = ( MonadUtxo m
                        , MonadBalances m
                        , MonadToilEnv m
                        , WithLogger m)

-- CHECK: @verifyToil
-- | Verify transactions correctness with respect to Utxo applying
-- them one-by-one.
-- Note: transactions must be topsorted to pass check.
-- Warning: this function may apply some transactions and fail
-- eventually. Use it only on temporary data.
--
-- If the first argument is 'True', all data (script versions,
-- witnesses, addresses, attributes) must be known. Otherwise unknown
-- data is just ignored.
verifyToil
    :: (GlobalToilMode m, MonadError ToilVerFailure m)
    => Bool -> [TxAux] -> m TxpUndo
verifyToil verifyAllIsKnown =
    mapM (verifyAndApplyTx verifyAllIsKnown . withTxId)

-- | Apply transactions from one block. They must be valid (for
-- example, it implies topological sort).
applyToil
    :: GlobalToilMode m
    => [(TxAux, TxUndo)]
    -> m ()
applyToil txun = do
    applyTxsToBalances txun
    mapM_ (applyTxToUtxo' . withTxId . fst) txun

-- | Rollback transactions from one block.
rollbackToil :: GlobalToilMode m => [(TxAux, TxUndo)] -> m ()
rollbackToil txun = do
    rollbackTxsBalances txun
    mapM_ Utxo.rollbackTxUtxo $ reverse txun

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

type LocalToilMode m = ( MonadUtxo m
                       , MonadToilEnv m
                       , MonadTxPool m
                       )

-- CHECK: @processTx
-- | Verify one transaction and also add it to mem pool and apply to utxo
-- if transaction is valid.
processTx
    :: (LocalToilMode m, MonadError ToilVerFailure m)
    => (TxId, TxAux) -> m TxUndo
processTx tx@(id, aux) = do
    whenM (hasTx id) $ throwError ToilKnown
    maxBlockSize <- teMaxBlockSize <$> getToilEnv
    let maxPoolSize = memPoolLimitRatio * maxBlockSize
    whenM ((>= maxPoolSize) <$> poolSize) $
        throwError (ToilOverwhelmed maxPoolSize)
    undo <- verifyAndApplyTx True tx
    undo <$ putTxWithUndo id aux undo

-- | Get rid of invalid transactions.
-- All valid transactions will be added to mem pool and applied to utxo.
normalizeToil
    :: (LocalToilMode m)
    => [(TxId, TxAux)]
    -> m ()
normalizeToil txs = mapM_ normalize ordered
  where
    ordered = fromMaybe txs $ topsortTxs wHash txs
    wHash (i, txAux) = WithHash (taTx txAux) i
    normalize = runExceptT . processTx

----------------------------------------------------------------------------
-- ToilEnv logic
----------------------------------------------------------------------------

verifyToilEnv
    :: (MonadToilEnv m, MonadError ToilVerFailure m)
    => TxAux -> TxFee -> m ()
verifyToilEnv txAux txFee = do
    toilEnv <- getToilEnv
    let
        limit = teMaxTxSize toilEnv
        mtxFeePolicy = teTxFeePolicy toilEnv
        txSize = biSize txAux
    case mtxFeePolicy of
        Nothing ->
            -- There's no adopted minimal transaction fee policy. Allow
            -- arbitrary fees (including no fee).
            return ()
        Just txFeePolicy -> do
            verifyTxFeePolicy txFee txFeePolicy txSize
    when (txSize > limit) $
        throwError ToilTooLargeTx {ttltSize = txSize, ttltLimit = limit}

verifyTxFeePolicy
    :: (MonadToilEnv m, MonadError ToilVerFailure m)
    => TxFee
    -> Fee.TxFeePolicy
    -> Byte
    -> m ()
verifyTxFeePolicy (TxFee txFee) policy txSize = case policy of
    Fee.TxFeePolicyTxSizeLinear txSizeLinear -> do
        let
            -- We use 'ceiling' to convert from a fixed-precision fractional
            -- to coin amount. The actual fee is always a non-negative integer
            -- amount of coins, so if @min_fee <= fee@ holds (the ideal check),
            -- then @ceiling min_fee <= fee@ holds too.
            -- The reason we can't compare fractionals directly is that the
            -- minimal fee may need to appear in an error message (as a reason
            -- for rejecting the transaction).
            mTxMinFee = integerToCoin . ceiling $
                Fee.calculateTxSizeLinear txSizeLinear txSize
        -- The policy must be designed in a way that makes this impossible,
        -- but in case the result of its evaluation is negative or exceeds
        -- maximum coin value, we throw an error.
        txMinFee <- case mTxMinFee of
            Nothing -> throwError ToilInvalidMinFee
                { timfPolicy = policy
                , timfSize = txSize }
            Just a -> return a
        unless (txMinFee <= txFee) $
            throwError ToilInsufficientFee
                { tifSize = txSize
                , tifFee = TxFee txFee
                , tifMinFee = TxFee txMinFee
                , tifPolicy = policy }
    Fee.TxFeePolicyUnknown _ _ ->
        -- The minimal transaction fee policy exists, but the current
        -- version of the node doesn't know how to handle it. There are
        -- three possible options mentioned in [CSLREQ-157]:
        -- 1. Reject all new-coming transactions (b/c we can't calculate
        --    fee for them)
        -- 2. Use latest policy of known type
        -- 3. Discard the check
        -- Implementation-wise, the 1st option corresponds to throwing an
        -- error here (reject), the 3rd option -- doing nothing (accept), and
        -- the 2nd option would require some engineering feats to
        -- retrieve previous 'TxFeePolicy' and check against it.
        return ()

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

verifyAndApplyTx
    :: (MonadUtxo m, MonadToilEnv m, MonadError ToilVerFailure m)
    => Bool -> (TxId, TxAux) -> m TxUndo
verifyAndApplyTx verifyVersions tx@(_, txAux) = do
    (txUndo, txFee) <- Utxo.verifyTxUtxo ctx txAux
    verifyToilEnv txAux txFee
    applyTxToUtxo' tx
    return txUndo
  where
    ctx = Utxo.VTxContext verifyVersions

withTxId :: TxAux -> (TxId, TxAux)
withTxId aux = (hash (taTx aux), aux)

applyTxToUtxo' :: MonadUtxo m => (TxId, TxAux) -> m ()
applyTxToUtxo' (i, TxAux tx _ distr) = Utxo.applyTxToUtxo (WithHash tx i) distr
