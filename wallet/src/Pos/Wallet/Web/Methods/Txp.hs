{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Utils for payments and redeeming.

module Pos.Wallet.Web.Methods.Txp
    ( rewrapTxError
    , coinDistrToOutputs
    , submitAndSaveNewPtx
    ) where

import           Universum

import qualified Data.List.NonEmpty         as NE
import           Formatting                 (build, sformat, stext, (%))
import           Pos.Communication          (EnqueueMsg)

import           Pos.Client.Txp.Util        (TxError (..))
import           Pos.Core.Types             (Coin)
import           Pos.Txp                    (TxOut (..), TxOutAux (..))
import           Pos.Wallet.Web.ClientTypes (Addr, CId)
import           Pos.Wallet.Web.Error       (WalletError (..), rewrapToWalletError)
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending     (PendingTx, ptxFirstSubmissionHandler,
                                             submitAndSavePtx)
import           Pos.Wallet.Web.Util        (decodeCTypeOrFail)


rewrapTxError
    :: forall m a. MonadCatch m
    => Text -> m a -> m a
rewrapTxError prefix =
    rewrapToWalletError (\SomeException{} -> True) (InternalError . sbuild) .
    rewrapToWalletError (\TxError{} -> True) (RequestError . sbuild)
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

