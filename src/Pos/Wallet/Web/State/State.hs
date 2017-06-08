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
       , getAccountHistory
       , getUpdates
       , getNextUpdate
       , getHistoryCache
       , getChangeAddresses
       , isChangeAddress

       -- * Setters
       , testReset
       , createAccount
       , createWallet
       , addRemovedAccount
       , addWAddress
       , addChangeAddress
       , setProfile
       , setAccountMeta
       , setWalletMeta
       , setWalletPassLU
       , setWalletSyncTip
       , setAccountTransactionMeta
       , setAccountHistory
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
import qualified Ether
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

getAccountHistory :: WebWalletModeDB m => AccountId -> m (Maybe [CTxMeta])
getAccountHistory = queryDisk . A.GetAccountHistory

getUpdates :: WebWalletModeDB m => m [CUpdateInfo]
getUpdates = queryDisk A.GetUpdates

getNextUpdate :: WebWalletModeDB m => m (Maybe CUpdateInfo)
getNextUpdate = queryDisk A.GetNextUpdate

getHistoryCache :: WebWalletModeDB m => CId Wal -> m (Maybe (HeaderHash, Utxo, [TxHistoryEntry]))
getHistoryCache = queryDisk . A.GetHistoryCache

getChangeAddresses :: WebWalletModeDB m => m (HashSet CWAddressMeta)
getChangeAddresses = queryDisk A.GetChangeAddresses

isChangeAddress :: WebWalletModeDB m => CWAddressMeta -> m Bool
isChangeAddress = queryDisk . A.IsChangeAddress

createAccount :: WebWalletModeDB m => AccountId -> CAccountMeta -> m ()
createAccount addr = updateDisk . A.CreateAccount addr

createWallet :: WebWalletModeDB m => CId Wal -> CWalletMeta -> PassPhraseLU -> m ()
createWallet addr passLU = updateDisk . A.CreateWallet addr passLU

addWAddress :: WebWalletModeDB m => CWAddressMeta -> m ()
addWAddress addr = updateDisk $ A.AddWAddress addr

addChangeAddress :: WebWalletModeDB m => CWAddressMeta -> m ()
addChangeAddress addr = updateDisk $ A.AddChangeAddress addr

addRemovedAccount :: WebWalletModeDB m => CWAddressMeta -> m ()
addRemovedAccount addr = updateDisk $ A.AddRemovedAccount addr

setAccountMeta :: WebWalletModeDB m => AccountId -> CAccountMeta -> m ()
setAccountMeta addr = updateDisk . A.SetAccountMeta addr

setWalletMeta :: WebWalletModeDB m => CId Wal -> CWalletMeta -> m ()
setWalletMeta addr = updateDisk . A.SetWalletMeta addr

setWalletPassLU :: WebWalletModeDB m => CId Wal -> PassPhraseLU -> m ()
setWalletPassLU addr = updateDisk . A.SetWalletPassLU addr

setWalletSyncTip :: WebWalletModeDB m => CId Wal -> HeaderHash -> m ()
setWalletSyncTip addr = updateDisk . A.SetWalletSyncTip addr

setProfile :: WebWalletModeDB m => CProfile -> m ()
setProfile = updateDisk . A.SetProfile

setAccountTransactionMeta :: WebWalletModeDB m => AccountId -> CTxId -> CTxMeta -> m ()
setAccountTransactionMeta addr ctxId = updateDisk . A.SetAccountTransactionMeta addr ctxId

setAccountHistory :: WebWalletModeDB m => AccountId -> [(CTxId, CTxMeta)] -> m ()
setAccountHistory addr = updateDisk . A.SetAccountHistory addr

addOnlyNewTxMeta :: WebWalletModeDB m => CId Wal -> CTxId -> CTxMeta -> m ()
addOnlyNewTxMeta cWalId ctxId = updateDisk . A.AddOnlyNewTxMeta cWalId ctxId

removeWallet :: WebWalletModeDB m => CId Wal -> m ()
removeWallet = updateDisk . A.RemoveWallet

removeAccount :: WebWalletModeDB m => AccountId -> m ()
removeAccount = updateDisk . A.RemoveAccount

removeWAddress :: WebWalletModeDB m => CWAddressMeta -> m ()
removeWAddress = updateDisk . A.RemoveWAddress

totallyRemoveWAddress :: WebWalletModeDB m => CWAddressMeta -> m ()
totallyRemoveWAddress = updateDisk . A.TotallyRemoveWAddress

addUpdate :: WebWalletModeDB m => CUpdateInfo -> m ()
addUpdate = updateDisk . A.AddUpdate

removeNextUpdate :: WebWalletModeDB m => m ()
removeNextUpdate = updateDisk A.RemoveNextUpdate

testReset :: WebWalletModeDB m => m ()
testReset = updateDisk A.TestReset

updateHistoryCache :: WebWalletModeDB m => CId Wal -> HeaderHash -> Utxo -> [TxHistoryEntry] -> m ()
updateHistoryCache cWalId h utxo = updateDisk . A.UpdateHistoryCache cWalId h utxo
