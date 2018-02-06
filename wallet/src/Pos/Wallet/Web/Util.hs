{-# LANGUAGE Rank2Types #-}

-- | Various utilities for Wallet.

module Pos.Wallet.Web.Util
    ( getAccountMetaOrThrow
    , getWalletAccountIds
    , getAccountAddrsOrThrow
    , getWalletAddrMetas
    , getWalletAddrs
    , getWalletAddrsDetector
    , decodeCTypeOrFail
    , getWalletAssuredDepth
    , testOnlyEndpoint
    ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Formatting (build, sformat, (%))
import           Servant.Server (err405, errReasonPhrase)

import           Pos.Configuration (HasNodeConfiguration, walletProductionApi)
import           Pos.Core (BlockCount)
import           Pos.Util.Servant (FromCType (..), OriginType)
import           Pos.Util.Util (maybeThrow)
import           Pos.Wallet.Web.Assurance (AssuranceLevel (HighAssurance), assuredBlockDepth)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CAccountMeta, CId,
                                             CWAddressMeta (..), Wal, cwAssurance)

import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.State (AddressInfo (..), AddressLookupMode (..),
                                       CurrentAndRemoved (..), MonadWalletDBRead,
                                       getAccountAddrMaps, getAccountIds, getAccountMeta,
                                       getAccountWAddresses, getWalletMeta)

getAccountMetaOrThrow ::
       (MonadWalletDBRead ctx m, MonadThrow m) => AccountId -> m CAccountMeta
getAccountMetaOrThrow accId = getAccountMeta accId >>= maybeThrow noAccount
  where
    noAccount =
        RequestError $ sformat ("No account with id "%build%" found") accId

getWalletAccountIds :: MonadWalletDBRead ctx m => CId Wal -> m [AccountId]
getWalletAccountIds cWalId = filter ((== cWalId) . aiWId) <$> getAccountIds

getAccountAddrsOrThrow
    :: (MonadWalletDBRead ctx m, MonadThrow m)
    => AddressLookupMode -> AccountId -> m [AddressInfo]
getAccountAddrsOrThrow mode accId =
    getAccountWAddresses mode accId >>= maybeThrow noWallet
  where
    noWallet =
        RequestError $
        sformat ("No account with id "%build%" found") accId

getWalletAddrMetas
    :: (MonadWalletDBRead ctx m, MonadThrow m)
    => AddressLookupMode -> CId Wal -> m [CWAddressMeta]
getWalletAddrMetas lookupMode cWalId = do
    accountIds <- getWalletAccountIds cWalId
    map adiCWAddressMeta <$> concatMapM (getAccountAddrsOrThrow lookupMode) accountIds

getWalletAddrs
    :: (MonadWalletDBRead ctx m, MonadThrow m)
    => AddressLookupMode -> CId Wal -> m [CId Addr]
getWalletAddrs mode wid = cwamId <<$>> getWalletAddrMetas mode wid

getWalletAddrsDetector
    :: (MonadWalletDBRead ctx m, MonadThrow m)
    => AddressLookupMode -> CId Wal -> m (CId Addr -> Bool)
getWalletAddrsDetector lookupMode cWalId = do
    accIds <- getWalletAccountIds cWalId
    accAddrMaps <- mapM getAccountAddrMaps accIds
    let lookupExisting addr = any (HM.member addr . getCurrent) accAddrMaps
        lookupDeleted  addr = any (HM.member addr . getRemoved) accAddrMaps
        lookupEver     addr = lookupExisting addr
                           || lookupDeleted  addr
    return $ case lookupMode of
        Existing -> lookupExisting
        Deleted  -> lookupDeleted
        Ever     -> lookupEver

decodeCTypeOrFail :: (MonadThrow m, FromCType c) => c -> m (OriginType c)
decodeCTypeOrFail = either (throwM . DecodeError) pure . decodeCType

getWalletAssuredDepth
    :: (MonadWalletDBRead ctx m)
    => CId Wal -> m (Maybe BlockCount)
getWalletAssuredDepth wid =
    assuredBlockDepth HighAssurance . cwAssurance <<$>>
    getWalletMeta wid

testOnlyEndpoint :: (HasNodeConfiguration, MonadThrow m) => m a -> m a
testOnlyEndpoint action
    | walletProductionApi = throwM err405{ errReasonPhrase = errReason }
    | otherwise = action
  where
    errReason = "Disabled in production, switch 'walletProductionApi' \
                \parameter in config if you want to use this endpoint"
