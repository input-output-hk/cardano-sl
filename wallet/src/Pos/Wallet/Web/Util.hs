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
import           Pos.Core (Address, BlockCount)
import           Pos.Util.Servant (FromCType (..), OriginType)
import           Pos.Util.Util (maybeThrow)
import           Pos.Wallet.Web.Assurance (AssuranceLevel (HighAssurance), assuredBlockDepth)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccountMeta, CId, Wal,
                                             cwAssurance)


import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.State (AddressInfo (..), AddressLookupMode (..),
                                       CurrentAndRemoved (getCurrent, getRemoved),
                                       WAddressMeta (..), WalletSnapshot, getAccountAddrMaps,
                                       getAccountIds, getAccountMeta, getAccountWAddresses,
                                       getWAddresses, getWalletMeta)

getAccountMetaOrThrow :: MonadThrow m => WalletSnapshot -> AccountId -> m CAccountMeta
getAccountMetaOrThrow ws accId = maybeThrow noAccount (getAccountMeta ws accId)
  where
    noAccount =
        RequestError $ sformat ("No account with id "%build%" found") accId

getWalletAccountIds :: WalletSnapshot -> CId Wal -> [AccountId]
getWalletAccountIds ws cWalId = filter ((== cWalId) . aiWId) (getAccountIds ws)

getAccountAddrsOrThrow :: MonadThrow m
                       => WalletSnapshot
                       -> AddressLookupMode
                       -> AccountId
                       -> m [AddressInfo]
getAccountAddrsOrThrow ws mode accId = maybeThrow noWallet (getAccountWAddresses ws mode accId)
  where
    noWallet =
        RequestError $
        sformat ("No account with id "%build%" found") accId

getWalletAddrMetas
    :: WalletSnapshot
    -> AddressLookupMode
    -> CId Wal
    -> [WAddressMeta]
getWalletAddrMetas ws lookupMode cWalId =
  map adiWAddressMeta $ getWAddresses ws lookupMode cWalId

getWalletAddrs :: WalletSnapshot -> AddressLookupMode -> CId Wal -> [Address]
getWalletAddrs ws mode wid = _wamAddress <$> getWalletAddrMetas ws mode wid

getWalletAddrsDetector :: WalletSnapshot -> AddressLookupMode -> CId Wal -> (Address -> Bool)
getWalletAddrsDetector ws lookupMode cWalId = do
    let accIds = getWalletAccountIds ws cWalId
    let accAddrMaps = map (getAccountAddrMaps ws) accIds
    let lookupExisting addr = any (HM.member addr . getCurrent) accAddrMaps
        lookupDeleted  addr = any (HM.member addr . getRemoved) accAddrMaps
        lookupEver     addr = lookupExisting addr
                           || lookupDeleted  addr
    case lookupMode of
        Existing -> lookupExisting
        Deleted  -> lookupDeleted
        Ever     -> lookupEver

decodeCTypeOrFail :: (MonadThrow m, FromCType c) => c -> m (OriginType c)
decodeCTypeOrFail = either (throwM . DecodeError) pure . decodeCType

getWalletAssuredDepth :: WalletSnapshot -> CId Wal -> Maybe BlockCount
getWalletAssuredDepth ws wid =
    assuredBlockDepth HighAssurance . cwAssurance <$>
    getWalletMeta ws wid

testOnlyEndpoint :: (HasNodeConfiguration, MonadThrow m) => m a -> m a
testOnlyEndpoint action
    | walletProductionApi = throwM err405{ errReasonPhrase = errReason }
    | otherwise = action
  where
    errReason = "Disabled in production, switch 'walletProductionApi' \
                \parameter in config if you want to use this endpoint"
