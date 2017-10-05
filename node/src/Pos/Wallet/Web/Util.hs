{-# LANGUAGE Rank2Types #-}

-- | Different utils for wallets

module Pos.Wallet.Web.Util
    ( getWalletAccountIds
    , getAccountAddrsOrThrow
    , getWalletAddrMetas
    , getWalletAddrs
    , decodeCTypeOrFail
    , getWalletAssuredDepth
    ) where

import           Universum

import           Formatting                 (build, sformat, (%))

import           Pos.Core                   (BlockCount)
import           Pos.Util.Servant           (FromCType (..), OriginType)
import           Pos.Util.Util              (maybeThrow)
import           Pos.Wallet.Web.Assurance   (AssuranceLevel (HighAssurance),
                                             assuredBlockDepth)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CId,
                                             CWAddressMeta (..), Wal,
                                             cwAssurance)


import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.State       (AddressLookupMode, WebWalletModeDB,
                                             getAccountIds, getAccountWAddresses,
                                             getWalletMeta)

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

decodeCTypeOrFail :: (MonadThrow m, FromCType c) => c -> m (OriginType c)
decodeCTypeOrFail = either (throwM . DecodeError) pure . decodeCType

getWalletAssuredDepth
    :: (WebWalletModeDB ctx m)
    => CId Wal -> m (Maybe BlockCount)
getWalletAssuredDepth wid =
    assuredBlockDepth HighAssurance . cwAssurance <<$>>
    getWalletMeta wid
