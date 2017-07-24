{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Different utils for wallets

module Pos.Wallet.Web.Util
    ( getWalletAccountIds
    , rewrapTxError
    ) where

import           Universum

import           Formatting                 (build, sformat, stext, (%))

import           Pos.Client.Txp.Util        (TxError (..))
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CId, Wal)
import           Pos.Wallet.Web.Error       (WalletError (..), rewrapToWalletError)
import           Pos.Wallet.Web.State       (WebWalletModeDB, getAccountIds)

getWalletAccountIds :: WebWalletModeDB ctx m => CId Wal -> m [AccountId]
getWalletAccountIds cWalId = filter ((== cWalId) . aiWId) <$> getAccountIds

rewrapTxError
    :: forall m a. MonadCatch m
    => Text -> m a -> m a
rewrapTxError prefix =
    rewrapToWalletError (const True) (InternalError . sbuild) .
    rewrapToWalletError (\TxError{} -> True) (RequestError . sbuild)
  where
    sbuild = sformat (stext%": "%build) prefix
