{-# LANGUAGE ScopedTypeVariables #-}

-- | Utils for payments and redeeming.

module Pos.Wallet.Web.Methods.Txp
    ( rewrapTxError
    , coinDistrToOutputs
    ) where

import           Universum

import qualified Data.List.NonEmpty         as NE
import           Formatting                 (build, sformat, stext, (%))

import           Pos.Client.Txp.Util        (TxError (..))
import           Pos.Core.Types             (Coin)
import           Pos.Txp                    (TxOut (..), TxOutAux (..))
import           Pos.Wallet.Web.ClientTypes (Addr, CId)
import           Pos.Wallet.Web.Error       (WalletError (..), rewrapToWalletError)
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
