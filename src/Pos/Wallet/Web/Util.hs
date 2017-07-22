{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Different utils for wallets

module Pos.Wallet.Web.Util
    ( getWalletAccountIds
    , getAccountAddrsOrThrow
    , getWalletAddrMetas
    , rewrapTxError
    ) where

import           Universum

import           Formatting                 (build, sformat, stext, (%))

import           Pos.Client.Txp.Util        (TxError (..))
import           Pos.Util                   (maybeThrow)

import           Pos.Wallet.Web.ClientTypes (AccountId (..), CId, CWAddressMeta, Wal)
import           Pos.Wallet.Web.Error       (WalletError (..), rewrapToWalletError)
import           Pos.Wallet.Web.State       (AddressLookupMode, WebWalletModeDB,
                                             getAccountIds, getAccountWAddresses)

getWalletAccountIds :: WebWalletModeDB ctx m => CId Wal -> m [AccountId]
getWalletAccountIds cWalId = filter ((== cWalId) . aiWId) <$> getAccountIds

getAccountAddrsOrThrow
    :: (WebWalletModeDB ctx m, MonadThrow m)
    => AddressLookupMode -> AccountId -> m [CWAddressMeta]
getAccountAddrsOrThrow mode accId =
    getAccountWAddresses mode accId >>= maybeThrow noWallet
  where
    noWallet =
        RequestError $
        sformat ("No account with id "%build%" found") accId

getWalletAddrMetas
    :: (WebWalletModeDB ctx m, MonadThrow m)
    => AddressLookupMode -> CId Wal -> m [CWAddressMeta]
getWalletAddrMetas lookupMode cWalId =
    concatMapM (getAccountAddrsOrThrow lookupMode) =<<
    getWalletAccountIds cWalId

rewrapTxError
    :: forall m a. MonadCatch m
    => Text -> m a -> m a
rewrapTxError prefix =
    rewrapToWalletError (const True) (InternalError . sbuild) .
    rewrapToWalletError (\TxError{} -> True) (RequestError . sbuild)
  where
    sbuild = sformat (stext%": "%build) prefix
