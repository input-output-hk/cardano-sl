{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Utils for payments and redeeming.

module Pos.Wallet.Web.Methods.Txp
    ( MonadWalletTxFull
    , rewrapTxError
    , coinDistrToOutputs
    , submitAndSaveNewPtx
    ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import           Formatting (build, sformat, stext, (%))

import           Pos.Client.KeyStorage (MonadKeys)
import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Util (isCheckedTxError)
import           Pos.Core.Common (Coin)
import           Pos.Core.Txp (TxOut (..), TxOutAux (..), TxAux)
import           Pos.Crypto (PassPhrase)
import           Pos.Wallet.Web.ClientTypes (AccountId, Addr, CId)
import           Pos.Wallet.Web.Error (WalletError (..), rewrapToWalletError)
import           Pos.Wallet.Web.Methods.History (MonadWalletHistory)
import           Pos.Wallet.Web.Pending (PendingTx, TxSubmissionMode, ptxFirstSubmissionHandler,
                                         submitAndSavePtx)
import           Pos.Wallet.Web.Util (decodeCTypeOrFail)

type MonadWalletTxFull ctx m =
    ( TxSubmissionMode ctx m
    , MonadWalletHistory ctx m
    , MonadKeys m
    , AddrData m ~ (AccountId, PassPhrase)
    )

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
    :: TxSubmissionMode ctx m
    => (TxAux -> m Bool)
    -> PendingTx
    -> m ()
submitAndSaveNewPtx submit = submitAndSavePtx submit ptxFirstSubmissionHandler
