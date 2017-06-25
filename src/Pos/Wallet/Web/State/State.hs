{-# LANGUAGE TypeFamilies #-}

module Pos.Wallet.Web.State.State
       ( WalletState
       , MonadWalletWebDB
       , getWalletWebState
       , WebWalletModeDB
       , openState
       , openMemState
       , closeState

       -- * Getters
       , AddressLookupMode (..)
       , getProfile
       , getWAddressIds
       , getAccountMetas
       , getAccountMeta
       , getAccountWAddresses
       , getWalletMetas
       , getWalletMeta
       , getWalletPassLU
       , getWalletSyncTip
       , getWalletAddresses
       , doesWAddressExist
       , getTxMeta
       , getWalletTxHistory
       , getUpdates
       , getNextUpdate
       , getHistoryCache

       -- * Setters
       , testReset
       , createAccount
       , createWallet
       , addRemovedAccount
       , addWAddress
       , setProfile
       , setAccountMeta
       , setWalletMeta
       , setWalletPassLU
       , setWalletSyncTip
       , setWalletTxMeta
       , setWalletTxHistory
       , addOnlyNewTxMeta
       , removeWallet
       , removeAccount
       , removeWAddress
       , totallyRemoveWAddress
       , addUpdate
       , removeNextUpdate
       , updateHistoryCache
       ) where

import           Data.Acid                    (EventResult, EventState, QueryEvent,
                                               UpdateEvent)
import           EtherCompat
import           Mockable                     (MonadMockable)
import           Universum

import           Pos.Client.Txp.History       (TxHistoryEntry)
import           Pos.Txp                      (Utxo)
import           Pos.Types                    (HeaderHash)
import           Pos.Wallet.Web.ClientTypes   (AccountId, CAccountMeta, CId, CProfile,
                                               CTxId, CTxMeta, CUpdateInfo, CWAddressMeta,
                                               CWalletMeta, PassPhraseLU, Wal)
import           Pos.Wallet.Web.State.Acidic  (WalletState, closeState, openMemState,
                                               openState)
import           Pos.Wallet.Web.State.Acidic  as A
import           Pos.Wallet.Web.State.Storage (AddressLookupMode (..), WalletStorage)

-- | MonadWalletWebDB stands for monad which is able to get web wallet state
type MonadWalletWebDB ctx m = MonadCtx ctx WalletState WalletState m

getWalletWebState :: MonadWalletWebDB ctx m => m WalletState
getWalletWebState = askCtx @WalletState

-- | Constraint for working with web wallet DB
type WebWalletModeDB ctx m = (MonadWalletWebDB ctx m, MonadIO m, MonadMockable m)

queryDisk
    :: (EventState event ~ WalletStorage, QueryEvent event, WebWalletModeDB ctx m)
    => event -> m (EventResult event)
queryDisk e = getWalletWebState >>= flip A.query e

updateDisk
    :: (EventState event ~ WalletStorage, UpdateEvent event, WebWalletModeDB ctx m)
    => event -> m (EventResult event)
updateDisk e = getWalletWebState >>= flip A.update e

getWAddressIds :: WebWalletModeDB ctx m => m [AccountId]
getWAddressIds = queryDisk A.GetWAddressIds

getAccountMetas :: WebWalletModeDB ctx m => m [CAccountMeta]
getAccountMetas = queryDisk A.GetAccountMetas

getAccountMeta :: WebWalletModeDB ctx m => AccountId -> m (Maybe CAccountMeta)
getAccountMeta = queryDisk . A.GetAccountMeta

getWalletAddresses :: WebWalletModeDB ctx m => m [CId Wal]
getWalletAddresses = queryDisk A.GetWalletAddresses

getWalletMeta :: WebWalletModeDB ctx m => CId Wal -> m (Maybe CWalletMeta)
getWalletMeta = queryDisk . A.GetWalletMeta

getWalletMetas :: WebWalletModeDB ctx m => m ([CWalletMeta])
getWalletMetas = queryDisk A.GetWalletMetas

getWalletPassLU :: WebWalletModeDB ctx m => CId Wal -> m (Maybe PassPhraseLU)
getWalletPassLU = queryDisk . A.GetWalletPassLU

getWalletSyncTip :: WebWalletModeDB ctx m => CId Wal -> m (Maybe HeaderHash)
getWalletSyncTip = queryDisk . A.GetWalletSyncTip

getAccountWAddresses
    :: WebWalletModeDB ctx m
    => AddressLookupMode -> AccountId -> m (Maybe [CWAddressMeta])
getAccountWAddresses mode = queryDisk . A.GetAccountWAddresses mode

doesWAddressExist
    :: WebWalletModeDB ctx m
    => AddressLookupMode -> CWAddressMeta -> m Bool
doesWAddressExist mode = queryDisk . A.DoesWAddressExist mode

getProfile :: WebWalletModeDB ctx m => m CProfile
getProfile = queryDisk A.GetProfile

getTxMeta :: WebWalletModeDB ctx m => CId Wal -> CTxId -> m (Maybe CTxMeta)
getTxMeta cWalId = queryDisk . A.GetTxMeta cWalId

getWalletTxHistory :: WebWalletModeDB ctx m => CId Wal -> m (Maybe [CTxMeta])
getWalletTxHistory = queryDisk . A.GetWalletTxHistory

getUpdates :: WebWalletModeDB ctx m => m [CUpdateInfo]
getUpdates = queryDisk A.GetUpdates

getNextUpdate :: WebWalletModeDB ctx m => m (Maybe CUpdateInfo)
getNextUpdate = queryDisk A.GetNextUpdate

getHistoryCache :: WebWalletModeDB ctx m => CId Wal -> m (Maybe (HeaderHash, Utxo, [TxHistoryEntry]))
getHistoryCache = queryDisk . A.GetHistoryCache

createAccount :: WebWalletModeDB ctx m => AccountId -> CAccountMeta -> m ()
createAccount accId = updateDisk . A.CreateAccount accId

createWallet :: WebWalletModeDB ctx m => CId Wal -> CWalletMeta -> PassPhraseLU -> m ()
createWallet cWalId passLU = updateDisk . A.CreateWallet cWalId passLU

addWAddress :: WebWalletModeDB ctx m => CWAddressMeta -> m ()
addWAddress addr = updateDisk $ A.AddWAddress addr

addRemovedAccount :: WebWalletModeDB ctx m => CWAddressMeta -> m ()
addRemovedAccount addr = updateDisk $ A.AddRemovedAccount addr

setAccountMeta :: WebWalletModeDB ctx m => AccountId -> CAccountMeta -> m ()
setAccountMeta accId = updateDisk . A.SetAccountMeta accId

setWalletMeta :: WebWalletModeDB ctx m => CId Wal -> CWalletMeta -> m ()
setWalletMeta cWalId = updateDisk . A.SetWalletMeta cWalId

setWalletPassLU :: WebWalletModeDB ctx m => CId Wal -> PassPhraseLU -> m ()
setWalletPassLU cWalId = updateDisk . A.SetWalletPassLU cWalId

setWalletSyncTip :: WebWalletModeDB ctx m => CId Wal -> HeaderHash -> m ()
setWalletSyncTip cWalId = updateDisk . A.SetWalletSyncTip cWalId

setProfile :: WebWalletModeDB ctx m => CProfile -> m ()
setProfile = updateDisk . A.SetProfile

setWalletTxMeta :: WebWalletModeDB ctx m => CId Wal -> CTxId -> CTxMeta -> m ()
setWalletTxMeta cWalId cTxId = updateDisk . A.SetWalletTxMeta cWalId cTxId

setWalletTxHistory :: WebWalletModeDB ctx m => CId Wal -> [(CTxId, CTxMeta)] -> m ()
setWalletTxHistory cWalId = updateDisk . A.SetWalletTxHistory cWalId

addOnlyNewTxMeta :: WebWalletModeDB ctx m => CId Wal -> CTxId -> CTxMeta -> m ()
addOnlyNewTxMeta cWalId cTxId = updateDisk . A.AddOnlyNewTxMeta cWalId cTxId

removeWallet :: WebWalletModeDB ctx m => CId Wal -> m ()
removeWallet = updateDisk . A.RemoveWallet

removeAccount :: WebWalletModeDB ctx m => AccountId -> m ()
removeAccount = updateDisk . A.RemoveAccount

removeWAddress :: WebWalletModeDB ctx m => CWAddressMeta -> m ()
removeWAddress = updateDisk . A.RemoveWAddress

totallyRemoveWAddress :: WebWalletModeDB ctx m => CWAddressMeta -> m ()
totallyRemoveWAddress = updateDisk . A.TotallyRemoveWAddress

addUpdate :: WebWalletModeDB ctx m => CUpdateInfo -> m ()
addUpdate = updateDisk . A.AddUpdate

removeNextUpdate :: WebWalletModeDB ctx m => m ()
removeNextUpdate = updateDisk A.RemoveNextUpdate

testReset :: WebWalletModeDB ctx m => m ()
testReset = updateDisk A.TestReset

updateHistoryCache :: WebWalletModeDB ctx m => CId Wal -> HeaderHash -> Utxo -> [TxHistoryEntry] -> m ()
updateHistoryCache cWalId hh utxo = updateDisk . A.UpdateHistoryCache cWalId hh utxo
