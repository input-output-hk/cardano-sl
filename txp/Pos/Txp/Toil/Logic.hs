{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}

-- | All logic of Toil.  It operates in terms of MonadUtxo,
-- MonadBalances and MonadTxPool.

module Pos.Txp.Toil.Logic
       ( GlobalApplyToilMode
       , GlobalVerifyToilMode
       , verifyToil
       , applyToil
       , rollbackToil

       , LocalToilMode
       , normalizeToil
       , processTx

       , verifyAndApplyTx
       ) where

import           Universum
import           Unsafe                     (unsafeHead)

import           Control.Monad.Except       (MonadError (..))
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import qualified Data.List.NonEmpty         as NE
import           Formatting                 (sformat, (%))
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util.Text         (mapJson)
import           System.Wlog                (WithLogger)

import           Pos.Binary.Class           (biSize)
import           Pos.Core.Address           (isRedeemAddress)
import           Pos.Core.Coin              (integerToCoin, mkCoin, unsafeGetCoin)
import           Pos.Core.Constants         (memPoolLimitRatio)
import qualified Pos.Core.Fee               as Fee
import           Pos.Core.Genesis           (GenesisWStakeholders (..), bootDustThreshold)
import           Pos.Core.Slotting          (isBootstrapEra)
import           Pos.Core.Types             (BlockVersionData (..), Coin, EpochIndex,
                                             StakeholderId)
import           Pos.Crypto                 (WithHash (..), hash)
import           Pos.DB.Class               (MonadGState (..))
import           Pos.Txp.Core               (TxAux (..), TxId, TxOutDistribution, TxUndo,
                                             TxpUndo, getTxDistribution, toaOut,
                                             topsortTxs, txInputs, txOutAddress)
import           Pos.Txp.Toil.Balances      (applyTxsToBalances, rollbackTxsBalances)
import           Pos.Txp.Toil.Class         (MonadBalances (..), MonadTxPool (..),
                                             MonadUtxo (..), MonadUtxoRead (..))
import           Pos.Txp.Toil.Failure       (ToilVerFailure (..))
import           Pos.Txp.Toil.Types         (TxFee (..))
import qualified Pos.Txp.Toil.Utxo          as Utxo
import           Pos.Util.Util              (HasLens', getKeys, lensOf')

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

type GlobalApplyToilMode m =
    ( MonadUtxo m
    , MonadBalances m
    , MonadGState m
    , WithLogger m)

type GlobalVerifyToilMode ctx m =
    ( MonadUtxo m
    , MonadBalances m
    , MonadGState m
    , WithLogger m
    , HasLens' ctx GenesisWStakeholders
    , MonadReader ctx m)

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
    :: forall ctx m . (GlobalVerifyToilMode ctx m, MonadError ToilVerFailure m)
    => EpochIndex -> Bool -> [TxAux] -> m TxpUndo
verifyToil curEpoch verifyAllIsKnown =
    mapM (verifyAndApplyTx @ctx curEpoch verifyAllIsKnown . withTxId)

-- | Apply transactions from one block. They must be valid (for
-- example, it implies topological sort).
applyToil
    :: GlobalApplyToilMode m
    => [(TxAux, TxUndo)]
    -> m ()
applyToil txun = do
    applyTxsToBalances txun
    mapM_ (applyTxToUtxo' . withTxId . fst) txun

-- | Rollback transactions from one block.
rollbackToil :: GlobalApplyToilMode m => [(TxAux, TxUndo)] -> m ()
rollbackToil txun = do
    rollbackTxsBalances txun
    mapM_ Utxo.rollbackTxUtxo $ reverse txun

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

type LocalToilMode ctx m =
    ( MonadUtxo m
    , MonadGState m
    , MonadTxPool m
    , HasLens' ctx GenesisWStakeholders
    , MonadReader ctx m
    -- The war which we lost.
    )

-- CHECK: @processTx
-- | Verify one transaction and also add it to mem pool and apply to utxo
-- if transaction is valid.
processTx
    :: forall ctx m . (LocalToilMode ctx m, MonadError ToilVerFailure m)
    => EpochIndex -> (TxId, TxAux) -> m TxUndo
processTx curEpoch tx@(id, aux) = do
    whenM (hasTx id) $ throwError ToilKnown
    maxBlockSize <- bvdMaxBlockSize <$> gsAdoptedBVData
    let maxPoolSize = memPoolLimitRatio * maxBlockSize
    whenM ((>= maxPoolSize) <$> poolSize) $
        throwError (ToilOverwhelmed maxPoolSize)
    undo <- verifyAndApplyTx @ctx curEpoch True tx
    undo <$ putTxWithUndo id aux undo

-- | Get rid of invalid transactions.
-- All valid transactions will be added to mem pool and applied to utxo.
normalizeToil
    :: forall ctx m . LocalToilMode ctx m
    => EpochIndex -> [(TxId, TxAux)] -> m ()
normalizeToil curEpoch txs = mapM_ normalize ordered
  where
    ordered = fromMaybe txs $ topsortTxs wHash txs
    wHash (i, txAux) = WithHash (taTx txAux) i
    normalize = runExceptT . processTx @ctx curEpoch

----------------------------------------------------------------------------
-- Verify and Apply logic
----------------------------------------------------------------------------

verifyAndApplyTx
    :: forall ctx m .
       ( MonadUtxo m
       , MonadGState m
       , MonadError ToilVerFailure m
       , HasLens' ctx GenesisWStakeholders
       , MonadReader ctx m)
    => EpochIndex -> Bool -> (TxId, TxAux) -> m TxUndo
verifyAndApplyTx curEpoch verifyVersions tx@(_, txAux) = do
    (txUndo, txFeeMB) <- Utxo.verifyTxUtxo ctx txAux
    verifyGState @ctx curEpoch txAux txFeeMB
    applyTxToUtxo' tx
    pure txUndo
  where
    ctx = Utxo.VTxContext verifyVersions

isRedeemTx :: MonadUtxoRead m => TxAux -> m Bool
isRedeemTx txAux = do
    resolvedOuts <- traverse utxoGet $ (view txInputs . taTx) txAux
    let inputAddresses = fmap (txOutAddress . toaOut) . catMaybes . toList $ resolvedOuts
    return $ all isRedeemAddress inputAddresses

verifyGState
    :: forall ctx m .
       ( MonadGState m
       , MonadUtxoRead m
       , MonadError ToilVerFailure m
       , HasLens' ctx GenesisWStakeholders
       , MonadReader ctx m)
    => EpochIndex -> TxAux -> Maybe TxFee -> m ()
verifyGState curEpoch txAux txFeeMB = do
    BlockVersionData {..} <- gsAdoptedBVData
    verifyBootEra @ctx curEpoch bvdUnlockStakeEpoch txAux
    let txSize = biSize txAux
    let limit = bvdMaxTxSize
    unlessM (isRedeemTx txAux) $ whenJust txFeeMB $ \txFee ->
        verifyTxFeePolicy txFee bvdTxFeePolicy txSize
    when (txSize > limit) $
        throwError ToilTooLargeTx {ttltSize = txSize, ttltLimit = limit}

-- | Checks whether txOutDistribution matches the set of weighted boot
-- stakeholders. Notice: it doesn't use actual txdistr type because
-- it's defined in txp module above.
bootRelatedDistr :: GenesisWStakeholders -> [(StakeholderId, Coin)] -> Bool
bootRelatedDistr g@(GenesisWStakeholders bootHolders) txOutDistr
    | coinSum < unsafeGetCoin (bootDustThreshold g) =
        -- We allow small outputs to attribute stake in any proportion
        -- user wants because it's not likely they will shift overall
        -- stake distribution much. Anyway we require that all
        -- addresses in txDistr are keys of boot stakeholders.
        let bootHoldersKeys = getKeys bootHolders
        in all (`HS.member` bootHoldersKeys) stakeholders
    | otherwise =
        -- All addresses in txDistr are from bootHolders and every boot
        -- stakeholder is mentioned in txOutDistr
        getKeys bootHolders == HS.fromList stakeholders &&
        -- Every stakeholder gets his divisor. It's safe to use ! here
        -- because we're already sure that bootHolders ~ addrs is
        -- bijective.
        all (\(a,c) -> c >= (minimumPerStakeholder HM.! a)) txOutDistr
  where
    stakeholders = map fst txOutDistr
    coins = map snd txOutDistr
    -- It's safe to sum here since txOutDistr is a distribution of
    -- coins such that their sum is a coin itself.
    coinSum = sum $ map unsafeGetCoin coins
    weightSum :: Word64
    weightSum = sum $ map fromIntegral $ HM.elems bootHolders
    coinItem = coinSum `div` weightSum
    -- For each stakeholder the minimum amount of coins the tx should
    -- send to him (weighted). The multiplication can't overflow
    -- Word64 because sum of values of minimumPerStakeholder is less
    -- than coinSum.
    minimumPerStakeholder :: HashMap StakeholderId Coin
    minimumPerStakeholder = HM.map (mkCoin . (* coinItem) . fromIntegral) bootHolders

verifyBootEra
    :: forall ctx m .
       ( MonadError ToilVerFailure m
       , HasLens' ctx GenesisWStakeholders
       , MonadReader ctx m)
    => EpochIndex -> EpochIndex -> TxAux -> m ()
verifyBootEra curEpoch unlockEpoch txAux = do
    let bootEra = isBootstrapEra unlockEpoch curEpoch
    bootHolders <- view (lensOf' @GenesisWStakeholders)
    let bootRel = notBootRelated bootHolders
    when (bootEra && not (null bootRel)) $
        throwError $ ToilBootInappropriate $
        sformat ("transaction has non-boot stake distr in boot era: "%mapJson)
                (unsafeHead bootRel)
  where
    notBootRelated :: GenesisWStakeholders -> [TxOutDistribution]
    notBootRelated bootHolders =
        NE.filter (not . bootRelatedDistr bootHolders)
                  (getTxDistribution $ taDistribution txAux)

verifyTxFeePolicy
    :: MonadError ToilVerFailure m
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
            Left reason -> throwError ToilInvalidMinFee
                { timfPolicy = policy
                , timfReason = reason
                , timfSize = txSize }
            Right a -> return a
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

withTxId :: TxAux -> (TxId, TxAux)
withTxId aux = (hash (taTx aux), aux)

applyTxToUtxo' :: MonadUtxo m => (TxId, TxAux) -> m ()
applyTxToUtxo' (i, TxAux tx _ distr) = Utxo.applyTxToUtxo (WithHash tx i) distr
