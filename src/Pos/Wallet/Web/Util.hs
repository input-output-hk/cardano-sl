{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Different utils for wallets

module Pos.Wallet.Web.Util
    ( getWalletAccountIds
    , getAccountAddrsOrThrow
    , getWalletAddrMetas
    , getWalletAddrs
    , rewrapTxError
    , decodeCTypeOrFail
    , coinDistrToOutputs
    ) where

import           Universum

import qualified Data.List.NonEmpty         as NE
import           Formatting                 (build, sformat, stext, (%))

import           Pos.Client.Txp.Util        (TxError (..))
import           Pos.Core.Types             (Coin)
import           Pos.Txp                    (TxOut (..), TxOutAux (..))
import           Pos.Util.Servant           (FromCType (..), OriginType)
import           Pos.Util.Util              (maybeThrow)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CId,
                                             CWAddressMeta (..), Wal)
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

getWalletAddrs
    :: (WebWalletModeDB ctx m, MonadThrow m)
    => AddressLookupMode -> CId Wal -> m [CId Addr]
getWalletAddrs = (cwamId <<$>>) ... getWalletAddrMetas

rewrapTxError
    :: forall m a. MonadCatch m
    => Text -> m a -> m a
rewrapTxError prefix =
    rewrapToWalletError (const True) (InternalError . sbuild) .
    rewrapToWalletError (\TxError{} -> True) (RequestError . sbuild)
  where
    sbuild = sformat (stext%": "%build) prefix

decodeCTypeOrFail :: (MonadThrow m, FromCType c) => c -> m (OriginType c)
decodeCTypeOrFail = either wrongAddress pure . decodeCType
  where wrongAddress err = throwM . DecodeError $
            sformat ("Error while decoding CId: "%stext) err

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
