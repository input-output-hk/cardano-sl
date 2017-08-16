{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Different utils for wallets

module Pos.Wallet.Web.Util
    ( getWalletAccountIds
    , getAccountAddrsOrThrow
    , getWalletAddrMetas
    , rewrapTxError
    , decodeCIdOrFail
    , decodeCAccountIdOrFail
    , decodeCCoinOrFail
    , coinDistrToOutputs
    ) where

import           Universum

import qualified Data.List.NonEmpty         as NE
import           Formatting                 (build, sformat, stext, (%))

import           Pos.Client.Txp.Util        (TxError (..))
import           Pos.Txp                    (TxOut (..), TxOutAux (..))
import           Pos.Types                  (Address, Coin)
import           Pos.Util                   (maybeThrow)
import           Pos.Util.Servant           (decodeCType)

import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CAccountId, CCoin, CId,
                                             CWAddressMeta, Wal, cIdToAddress,
                                             coinFromCCoin)
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

-- TODO: probably poor naming
decodeCIdOrFail :: MonadThrow m => CId w -> m Address
decodeCIdOrFail = either wrongAddress pure . cIdToAddress
  where wrongAddress err = throwM . DecodeError $
            sformat ("Error while decoding CId: "%stext) err

-- TODO: these two could be removed if we decide to encode endpoint result
-- to CType automatically
decodeCAccountIdOrFail :: MonadThrow m => CAccountId -> m AccountId
decodeCAccountIdOrFail = either wrongAddress pure . decodeCType
  where wrongAddress err = throwM . DecodeError $
            sformat ("Error while decoding CAccountId: "%stext) err

decodeCCoinOrFail :: MonadThrow m => CCoin -> m Coin
decodeCCoinOrFail c =
    coinFromCCoin c `whenNothing` throwM (DecodeError "Wrong coin format")

coinDistrToOutputs
    :: MonadThrow m
    => NonEmpty (CId Addr, Coin)
    -> m (NonEmpty TxOutAux)
coinDistrToOutputs distr = do
    addrs <- mapM decodeCIdOrFail cAddrs
    pure $ NE.zipWith mkTxOut addrs coins
  where
    (cAddrs, coins) = NE.unzip distr
    mkTxOut addr coin = TxOutAux (TxOut addr coin) []
