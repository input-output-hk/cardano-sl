{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}

-- | All high-level logic of Toil.  It operates in 'LocalToilM' and
-- 'GlobalToilM'.

module Pos.Txp.Toil.Logic
       ( verifyToil
       , applyToil
       , rollbackToil

       , normalizeToil
       , processTx
       ) where

import           Universum hiding (id)

import           Control.Monad.Except (ExceptT, mapExceptT, throwError)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class (biSize)
import           Pos.Core (AddrAttributes (..), AddrStakeDistribution (..), Address,
                           BlockVersionData (..), EpochIndex, addrAttributesUnwrapped,
                           isBootstrapEraBVD, isRedeemAddress, HasProtocolMagic, HasGenesisData)
import           Pos.Core.Common (integerToCoin)
import qualified Pos.Core.Common as Fee (TxFeePolicy (..), calculateTxSizeLinear)
import           Pos.Core.Txp (Tx (..), TxAux (..), TxId, TxOut (..), TxUndo, TxpUndo, checkTxAux,
                               toaOut, txOutAddress)
import           Pos.Crypto (WithHash (..), hash)
import           Pos.Txp.Configuration (HasTxpConfiguration, memPoolLimitTx)
import           Pos.Txp.Toil.Failure (ToilVerFailure (..))
import           Pos.Txp.Toil.Monad (GlobalToilM, LocalToilM, UtxoM, hasTx, memPoolSize,
                                     putTxWithUndo, utxoMToGlobalToilM, utxoMToLocalToilM)
import           Pos.Txp.Toil.Stakes (applyTxsToStakes, rollbackTxsStakes)
import           Pos.Txp.Toil.Types (TxFee (..))
import           Pos.Txp.Toil.Utxo (VerifyTxUtxoRes (..))
import qualified Pos.Txp.Toil.Utxo as Utxo
import           Pos.Txp.Topsort (topsortTxs)
import           Pos.Util (liftEither)

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

-- CHECK: @verifyToil
-- | Verify transactions correctness with respect to Utxo applying
-- them one-by-one.
-- Note: transactions must be topsorted to pass check.
-- Warning: this function may apply some transactions and fail
-- eventually.
--
-- If the 'Bool' argument is 'True', all data (script versions,
-- witnesses, addresses, attributes) must be known. Otherwise unknown
-- data is just ignored.
verifyToil ::
       (HasProtocolMagic)
    => BlockVersionData
    -> EpochIndex
    -> Bool
    -> [TxAux]
    -> ExceptT ToilVerFailure UtxoM TxpUndo
verifyToil bvd curEpoch verifyAllIsKnown =
    mapM (verifyAndApplyTx bvd curEpoch verifyAllIsKnown . withTxId)

-- | Apply transactions from one block. They must be valid (for
-- example, it implies topological sort).
applyToil :: HasGenesisData => [(TxAux, TxUndo)] -> GlobalToilM ()
applyToil [] = pass
applyToil txun = do
    applyTxsToStakes txun
    utxoMToGlobalToilM $ mapM_ (applyTxToUtxo' . withTxId . fst) txun

-- | Rollback transactions from one block.
rollbackToil :: HasGenesisData => [(TxAux, TxUndo)] -> GlobalToilM ()
rollbackToil txun = do
    rollbackTxsStakes txun
    utxoMToGlobalToilM $ mapM_ Utxo.rollbackTxUtxo $ reverse txun

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

-- | Verify one transaction and also add it to mem pool and apply to utxo
-- if transaction is valid.
processTx ::
       (HasTxpConfiguration, HasProtocolMagic)
    => BlockVersionData
    -> EpochIndex
    -> (TxId, TxAux)
    -> ExceptT ToilVerFailure LocalToilM TxUndo
processTx bvd curEpoch tx@(id, aux) = do
    whenM (lift $ hasTx id) $ throwError ToilKnown
    whenM ((>= memPoolLimitTx) <$> lift memPoolSize) $
        throwError (ToilOverwhelmed memPoolLimitTx)
    undo <- mapExceptT utxoMToLocalToilM $ verifyAndApplyTx bvd curEpoch True tx
    undo <$ lift (putTxWithUndo id aux undo)

-- | Get rid of invalid transactions.
-- All valid transactions will be added to mem pool and applied to utxo.
normalizeToil ::
       (HasTxpConfiguration, HasProtocolMagic)
    => BlockVersionData
    -> EpochIndex
    -> [(TxId, TxAux)]
    -> LocalToilM ()
normalizeToil bvd curEpoch txs = mapM_ normalize ordered
  where
    ordered = fromMaybe txs $ topsortTxs wHash txs
    wHash (i, txAux) = WithHash (taTx txAux) i
    normalize ::
           (HasTxpConfiguration)
        => (TxId, TxAux)
        -> LocalToilM ()
    normalize = void . runExceptT . processTx bvd curEpoch

----------------------------------------------------------------------------
-- Verify and Apply logic
----------------------------------------------------------------------------

-- Note: it doesn't consider/affect stakes! That's because we don't
-- care about stakes for local txp.
verifyAndApplyTx ::
       (HasProtocolMagic)
    => BlockVersionData
    -> EpochIndex
    -> Bool
    -> (TxId, TxAux)
    -> ExceptT ToilVerFailure UtxoM TxUndo
verifyAndApplyTx adoptedBVD curEpoch verifyVersions tx@(_, txAux) = do
    whenLeft (checkTxAux txAux) (throwError . ToilInconsistentTxAux)
    vtur@VerifyTxUtxoRes {..} <- Utxo.verifyTxUtxo ctx txAux
    liftEither $ verifyGState adoptedBVD curEpoch txAux vtur
    lift $ applyTxToUtxo' tx
    pure vturUndo
  where
    ctx = Utxo.VTxContext verifyVersions

isRedeemTx :: TxUndo -> Bool
isRedeemTx resolvedOuts = all isRedeemAddress inputAddresses
  where
    inputAddresses =
        fmap (txOutAddress . toaOut) . catMaybes . toList $ resolvedOuts

verifyGState ::
       BlockVersionData
    -> EpochIndex
    -> TxAux
    -> VerifyTxUtxoRes
    -> Either ToilVerFailure ()
verifyGState bvd@BlockVersionData {..} curEpoch txAux vtur = do
    verifyBootEra bvd curEpoch txAux
    let txFeeMB = vturFee vtur
    let txSize = biSize txAux
    let limit = bvdMaxTxSize
    unless (isRedeemTx $ vturUndo vtur) $ whenJust txFeeMB $ \txFee ->
        verifyTxFeePolicy txFee bvdTxFeePolicy txSize
    when (txSize > limit) $
        throwError $ ToilTooLargeTx txSize limit

verifyBootEra ::
       BlockVersionData -> EpochIndex -> TxAux -> Either ToilVerFailure ()
verifyBootEra bvd curEpoch TxAux {..} = do
    when (isBootstrapEraBVD bvd curEpoch) $
        whenNotNull notBootstrapDistrAddresses $
        throwError . ToilNonBootstrapDistr
  where
    notBootstrapDistrAddresses :: [Address]
    notBootstrapDistrAddresses =
        filter (not . isBootstrapEraDistr) $
        map txOutAddress $ toList $ _txOutputs taTx
    isBootstrapEraDistr :: Address -> Bool
    isBootstrapEraDistr (addrAttributesUnwrapped -> AddrAttributes {..}) =
        case aaStakeDistribution of
            BootstrapEraDistr -> True
            _                 -> False

verifyTxFeePolicy ::
       TxFee -> Fee.TxFeePolicy -> Byte -> Either ToilVerFailure ()
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
            Left reason -> throwError $
                ToilInvalidMinFee policy reason txSize
            Right a -> return a
        unless (txMinFee <= txFee) $
            throwError $
                ToilInsufficientFee policy (TxFee txFee) (TxFee txMinFee) txSize
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

applyTxToUtxo' :: (TxId, TxAux) -> UtxoM ()
applyTxToUtxo' (i, TxAux tx _) = Utxo.applyTxToUtxo (WithHash tx i)
