{-# LANGUAGE Rank2Types #-}

-- | Different utils for wallets

module Pos.Wallet.Web.Util
    ( getAccountAddrsOrThrow
    , getWalletAddrMetas
    , getWalletAddrs
    , getWalletAddrsSet
    , decodeCTypeOrFail
    , getWalletAssuredDepth
    ) where

import           Universum

import qualified Data.Set                   as S
import           Formatting                 (build, sformat, (%))

import           Pos.Core                   (BlockCount)
import           Pos.Util.Servant           (FromCType (..), OriginType)
import           Pos.Util.Util              (maybeThrow)

import           Pos.Wallet.Web.Assurance   (AssuranceLevel (HighAssurance),
                                             assuredBlockDepth)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CId,
                                             CWAddressMeta (..), Wal, cwAssurance)

import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.State       (AddressLookupMode, MonadWalletDBMempoolRead,
                                             MonadWalletDBRead, getAccountWAddresses,
                                             getWalletMeta, getWalletWAddresses)

getAccountAddrsOrThrow
    :: (MonadWalletDBMempoolRead m, MonadThrow m)
    => AddressLookupMode -> AccountId -> m [CWAddressMeta]
getAccountAddrsOrThrow mode accId =
    getAccountWAddresses mode accId >>= maybeThrow noAccount
  where
    noAccount =
        RequestError $
        sformat ("No account with id "%build%" found") accId

getWalletAddrMetas
    :: MonadWalletDBMempoolRead m
    => AddressLookupMode -> CId Wal -> m [CWAddressMeta]
getWalletAddrMetas mode wid = fmap (fromMaybe []) $ getWalletWAddresses mode wid

getWalletAddrs
    :: MonadWalletDBMempoolRead m
    => AddressLookupMode -> CId Wal -> m [CId Addr]
getWalletAddrs mode wid = (cwamId <<$>>) . fmap (fromMaybe []) $ getWalletWAddresses mode wid

getWalletAddrsSet
    :: MonadWalletDBMempoolRead m
    => AddressLookupMode -> CId Wal -> m (Set (CId Addr))
getWalletAddrsSet = fmap S.fromList ... getWalletAddrs

decodeCTypeOrFail :: (MonadThrow m, FromCType c) => c -> m (OriginType c)
decodeCTypeOrFail = either (throwM . DecodeError) pure . decodeCType

getWalletAssuredDepth
    :: MonadWalletDBRead m
    => CId Wal -> m (Maybe BlockCount)
getWalletAssuredDepth wid =
    assuredBlockDepth HighAssurance . cwAssurance <<$>>
    getWalletMeta wid
