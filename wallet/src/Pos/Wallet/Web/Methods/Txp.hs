{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Utils for payments and redeeming.

module Pos.Wallet.Web.Methods.Txp
    ( rewrapTxError
    , coinDistrToOutputs
    , submitAndSaveNewPtx
    , getPendingAddresses
    ) where

import           Universum

import qualified Data.List.NonEmpty         as NE
import           Formatting                 (build, sformat, stext, (%))
import           Pos.Communication          (EnqueueMsg)

import           Pos.Client.Txp.Util        (InputSelectionPolicy (..),
                                             PendingAddresses (..), isCheckedTxError)
import           Pos.Core.Types             (Coin)
import           Pos.Txp                    (TxOut (..), TxOutAux (..))
import           Pos.Wallet.Web.ClientTypes (Addr, CId)
import           Pos.Wallet.Web.Error       (WalletError (..), rewrapToWalletError)
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending     (PendingTx, allPendingAddresses,
                                             ptxFirstSubmissionHandler, submitAndSavePtx)
import           Pos.Wallet.Web.State       (WalletSnapshot, getPendingTxs)
import           Pos.Wallet.Web.Util        (decodeCTypeOrFail)


rewrapTxError
    :: forall m a. MonadCatch m
    => Text -> m a -> m a
rewrapTxError prefix =
    rewrapToWalletError (\SomeException{} -> True) (InternalError . sbuild) .
    rewrapToWalletError isCheckedTxError (RequestError . sbuild)
  where
    sbuild :: Buildable e => e -> Text
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
    mkTxOut addr coin = TxOutAux (TxOut addr coin)

-- | Like 'submitAndSaveTx', but suppresses errors which can get gone
-- by the time of resubmission.
submitAndSaveNewPtx
    :: MonadWalletWebMode m
    => EnqueueMsg m -> PendingTx -> m ()
submitAndSaveNewPtx = submitAndSavePtx ptxFirstSubmissionHandler

-- | With regard to tx creation policy which is going to be used,
-- get addresses which are refered by some yet unconfirmed transaction outputs.
getPendingAddresses :: WalletSnapshot -> InputSelectionPolicy -> PendingAddresses
getPendingAddresses ws = \case
    OptimizeForSecurity ->
        -- NOTE (int-index) The pending transactions are ignored when we optimize
        -- for security, so it is faster to not get them. In case they start being
        -- used for other purposes, this shortcut must be removed.
        mempty
    OptimizeForHighThroughput ->
        allPendingAddresses (getPendingTxs ws)

