{-# LANGUAGE TypeFamilies #-}

module Pos.Wallet.Web.State.State
       ( WalletState
       , MonadWalletWebDB
       , WalletWebDBTag
       , getWalletWebState
       , WebWalletModeDB
       , openState
       , openMemState
       , closeState

       , AddressLookupMode (..)
       , CustomAddressType (..)

       -- * Getters
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
       , getCustomAddresses
       , getCustomAddress
       , isCustomAddress
       , getWalletUtxo

       -- * Setters
       , testReset
       , createAccount
       , createWallet
       , addRemovedAccount
       , addWAddress
       , addCustomAddress
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
       , removeCustomAddress
       , totallyRemoveWAddress
       , addUpdate
       , removeNextUpdate
       , updateHistoryCache
       , setWalletUtxo
       ) where

import           Data.Acid                    (EventResult, EventState, QueryEvent,
                                               UpdateEvent)
import qualified Ether
import           Mockable                     (MonadMockable)
import           Universum

import           Pos.Client.Txp.History       (TxHistoryEntry)
import           Pos.Txp                      (Utxo)
import           Pos.Types                    (HeaderHash)
import           Pos.Wallet.Web.ClientTypes   (AccountId, Addr, CAccountMeta, CId,
                                               CProfile, CTxId, CTxMeta, CUpdateInfo,
                                               CWAddressMeta, CWalletMeta, PassPhraseLU,
                                               Wal)
import           Pos.Wallet.Web.State.Acidic  (WalletState, closeState, openMemState,
                                               openState)
import           Pos.Wallet.Web.State.Acidic  as A
import           Pos.Wallet.Web.State.Storage (AddressLookupMode (..),
                                               CustomAddressType (..), WalletStorage)

data WalletWebDBTag

-- | MonadWalletWebDB stands for monad which is able to get web wallet state
type MonadWalletWebDB = Ether.MonadReader' WalletState

getWalletWebState :: MonadWalletWebDB m => m WalletState
getWalletWebState = Ether.ask'

-- | Constraint for working with web wallet DB
type WebWalletModeDB m = (MonadWalletWebDB m, MonadIO m, MonadMockable m)

queryDisk
    :: (EventState event ~ WalletStorage, QueryEvent event, WebWalletModeDB m)
    => event -> m (EventResult event)
queryDisk e = getWalletWebState >>= flip A.query e

updateDisk
    :: (EventState event ~ WalletStorage, UpdateEvent event, WebWalletModeDB m)
    => event -> m (EventResult event)
updateDisk e = getWalletWebState >>= flip A.update e

getWAddressIds :: WebWalletModeDB m => m [AccountId]
getWAddressIds = queryDisk A.GetWAddressIds

getAccountMetas :: WebWalletModeDB m => m [CAccountMeta]
getAccountMetas = queryDisk A.GetAccountMetas

getAccountMeta :: WebWalletModeDB m => AccountId -> m (Maybe CAccountMeta)
getAccountMeta = queryDisk . A.GetAccountMeta

getWalletAddresses :: WebWalletModeDB m => m [CId Wal]
getWalletAddresses = queryDisk A.GetWalletAddresses

getWalletMeta :: WebWalletModeDB m => CId Wal -> m (Maybe CWalletMeta)
getWalletMeta = queryDisk . A.GetWalletMeta

getWalletMetas :: WebWalletModeDB m => m ([CWalletMeta])
getWalletMetas = queryDisk A.GetWalletMetas

getWalletPassLU :: WebWalletModeDB m => CId Wal -> m (Maybe PassPhraseLU)
getWalletPassLU = queryDisk . A.GetWalletPassLU

getWalletSyncTip :: WebWalletModeDB m => CId Wal -> m (Maybe HeaderHash)
getWalletSyncTip = queryDisk . A.GetWalletSyncTip

getAccountWAddresses
    :: WebWalletModeDB m
    => AddressLookupMode -> AccountId -> m (Maybe [CWAddressMeta])
getAccountWAddresses mode = queryDisk . A.GetAccountWAddresses mode

doesWAddressExist
    :: WebWalletModeDB m
    => AddressLookupMode -> CWAddressMeta -> m Bool
doesWAddressExist mode = queryDisk . A.DoesWAddressExist mode

getProfile :: WebWalletModeDB m => m CProfile
getProfile = queryDisk A.GetProfile

getTxMeta :: WebWalletModeDB m => CId Wal -> CTxId -> m (Maybe CTxMeta)
getTxMeta cWalId = queryDisk . A.GetTxMeta cWalId

getWalletTxHistory :: WebWalletModeDB m => CId Wal -> m (Maybe [CTxMeta])
getWalletTxHistory = queryDisk . A.GetWalletTxHistory

getUpdates :: WebWalletModeDB m => m [CUpdateInfo]
getUpdates = queryDisk A.GetUpdates

getNextUpdate :: WebWalletModeDB m => m (Maybe CUpdateInfo)
getNextUpdate = queryDisk A.GetNextUpdate

getHistoryCache :: WebWalletModeDB m => CId Wal -> m (Maybe [TxHistoryEntry])
getHistoryCache = queryDisk . A.GetHistoryCache

getCustomAddresses :: WebWalletModeDB m => CustomAddressType -> m [CId Addr]
getCustomAddresses = queryDisk ... A.GetCustomAddresses

getCustomAddress :: WebWalletModeDB m => CustomAddressType -> CId Addr -> m (Maybe HeaderHash)
getCustomAddress = queryDisk ... A.GetCustomAddress

isCustomAddress :: WebWalletModeDB m => CustomAddressType -> CId Addr -> m Bool
isCustomAddress = fmap isJust . queryDisk ... A.GetCustomAddress

createAccount :: WebWalletModeDB m => AccountId -> CAccountMeta -> m ()
createAccount accId = updateDisk . A.CreateAccount accId

createWallet :: WebWalletModeDB m => CId Wal -> CWalletMeta -> PassPhraseLU -> m ()
createWallet cWalId passLU = updateDisk . A.CreateWallet cWalId passLU

addWAddress :: WebWalletModeDB m => CWAddressMeta -> m ()
addWAddress addr = updateDisk $ A.AddWAddress addr

addCustomAddress :: WebWalletModeDB m => CustomAddressType -> (CId Addr, HeaderHash) -> m Bool
addCustomAddress = updateDisk ... A.AddCustomAddress

addRemovedAccount :: WebWalletModeDB m => CWAddressMeta -> m ()
addRemovedAccount addr = updateDisk $ A.AddRemovedAccount addr

setAccountMeta :: WebWalletModeDB m => AccountId -> CAccountMeta -> m ()
setAccountMeta accId = updateDisk . A.SetAccountMeta accId

setWalletMeta :: WebWalletModeDB m => CId Wal -> CWalletMeta -> m ()
setWalletMeta cWalId = updateDisk . A.SetWalletMeta cWalId

setWalletPassLU :: WebWalletModeDB m => CId Wal -> PassPhraseLU -> m ()
setWalletPassLU cWalId = updateDisk . A.SetWalletPassLU cWalId

setWalletSyncTip :: WebWalletModeDB m => CId Wal -> HeaderHash -> m ()
setWalletSyncTip cWalId = updateDisk . A.SetWalletSyncTip cWalId

setProfile :: WebWalletModeDB m => CProfile -> m ()
setProfile = updateDisk . A.SetProfile

setWalletTxMeta :: WebWalletModeDB m => CId Wal -> CTxId -> CTxMeta -> m ()
setWalletTxMeta cWalId cTxId = updateDisk . A.SetWalletTxMeta cWalId cTxId

setWalletTxHistory :: WebWalletModeDB m => CId Wal -> [(CTxId, CTxMeta)] -> m ()
setWalletTxHistory cWalId = updateDisk . A.SetWalletTxHistory cWalId

getWalletUtxo :: WebWalletModeDB m => m Utxo
getWalletUtxo = queryDisk A.GetWalletUtxo

setWalletUtxo :: WebWalletModeDB m => Utxo -> m ()
setWalletUtxo = updateDisk . A.SetWalletUtxo

addOnlyNewTxMeta :: WebWalletModeDB m => CId Wal -> CTxId -> CTxMeta -> m ()
addOnlyNewTxMeta cWalId cTxId = updateDisk . A.AddOnlyNewTxMeta cWalId cTxId

removeWallet :: WebWalletModeDB m => CId Wal -> m ()
removeWallet = updateDisk . A.RemoveWallet

removeAccount :: WebWalletModeDB m => AccountId -> m ()
removeAccount = updateDisk . A.RemoveAccount

removeWAddress :: WebWalletModeDB m => CWAddressMeta -> m ()
removeWAddress = updateDisk . A.RemoveWAddress

totallyRemoveWAddress :: WebWalletModeDB m => CWAddressMeta -> m ()
totallyRemoveWAddress = updateDisk . A.TotallyRemoveWAddress

removeCustomAddress
    :: WebWalletModeDB m
    => CustomAddressType -> (CId Addr, HeaderHash) -> m Bool
removeCustomAddress = updateDisk ... A.RemoveCustomAddress

addUpdate :: WebWalletModeDB m => CUpdateInfo -> m ()
addUpdate = updateDisk . A.AddUpdate

removeNextUpdate :: WebWalletModeDB m => m ()
removeNextUpdate = updateDisk A.RemoveNextUpdate

testReset :: WebWalletModeDB m => m ()
testReset = updateDisk A.TestReset

updateHistoryCache :: WebWalletModeDB m => CId Wal -> [TxHistoryEntry] -> m ()
updateHistoryCache cWalId = updateDisk . A.UpdateHistoryCache cWalId
