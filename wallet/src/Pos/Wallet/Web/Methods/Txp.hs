{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Utils for payments and redeeming.

module Pos.Wallet.Web.Methods.Txp
    ( rewrapTxError
    , coinDistrToOutputs
    , submitAndSaveTxWithPending
    ) where

import           Universum

import           Control.Monad.Catch        (Handler (..), catches)
import qualified Data.List.NonEmpty         as NE
import           Formatting                 (build, sformat, stext, (%))

import           Pos.Client.Txp.Util        (TxError (..))
import           Pos.Communication          (SendActions (..), submitAndSaveTx)
import           Pos.Core.Types             (Coin)
import           Pos.Crypto                 (hash)
import           Pos.Txp                    (TxAux (..), TxOut (..), TxOutAux (..))
import           Pos.Wallet.Web.ClientTypes (Addr, CId, Wal)
import           Pos.Wallet.Web.Error       (WalletError (..), rewrapToWalletError)
import           Pos.Wallet.Web.Pending     (MonadPendings, mkPendingTx,
                                             processPtxFailure)
import           Pos.Wallet.Web.State       (addOnlyNewPendingTx)
import           Pos.Wallet.Web.Util        (decodeCTypeOrFail)

rewrapTxError
    :: forall m a. MonadCatch m
    => Text -> m a -> m a
rewrapTxError prefix =
    rewrapToWalletError (const True) (InternalError . sbuild) .
    rewrapToWalletError (\TxError{} -> True) (RequestError . sbuild)
  where
    sbuild = sformat (stext%": "%build) prefix

coinDistrToOutputs
    :: MonadThrow m
    => NonEmpty (CId Addr, Coin)
    -> m (NonEmpty TxOutAux)
coinDistrToOutputs distr = do
    addrs <- mapM decodeCTypeOrFail cAddrs
    pure $ NE.zipWith mkTxOut addrs coins
  where
    (cAddrs, coins) = NE.unzip distr
    mkTxOut addr coin = TxOutAux (TxOut addr coin) []

-- | Like 'submitAndSaveTx', but also remembers given transaction as pending
-- in case when submition succeeded, or failed with reclaimable error.
submitAndSaveTxWithPending
    :: MonadPendings m
    => SendActions m -> CId Wal -> TxAux -> m ()
submitAndSaveTxWithPending SendActions{..} wid txAux = do
    let txHash = hash (taTx txAux)
    ptx <- mkPendingTx wid txHash txAux
    (submitAndSaveTx enqueueMsg txAux >> addOnlyNewPendingTx ptx)
        `catches` handlers ptx
  where
    handlers ptx =
        [ Handler $ \e -> do
            processPtxFailure False ptx e
            throwM e

        , Handler $ \e@TxError{} ->
            throwM e

        , Handler $ \(SomeException e) -> do
            addOnlyNewPendingTx ptx
            throwM e
        ]

