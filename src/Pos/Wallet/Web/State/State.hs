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
       , AccountLookupMode (..)
       , getProfile
       , getWalletAddresses
       , getWalletMetas
       , getWalletMeta
       , getWalletAccounts
       , getWSetMetas
       , getWSetMeta
       , getWSetPassLU
       , getWSetSyncTip
       , getWSetAddresses
       , doesAccountExist
       , getTxMeta
       , getWalletHistory
       , getUpdates
       , getNextUpdate
       , getHistoryCache

       -- * Setters
       , testReset
       , createWallet
       , createWSet
       , addRemovedAccount
       , addAccount
       , setProfile
       , setWalletMeta
       , setWSetMeta
       , setWSetPassLU
       , setWSetSyncTip
       , setWalletTransactionMeta
       , setWalletHistory
       , addOnlyNewTxMeta
       , removeWSet
       , removeWallet
       , removeAccount
       , totallyRemoveAccount
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
import           Pos.Wallet.Web.ClientTypes   (CAccountAddress, CAddress, CProfile, CTxId,
                                               CTxMeta, CUpdateInfo, CWalletMeta,
                                               CWalletSetMeta, PassPhraseLU, WS,
                                               WalletAddress)
import           Pos.Wallet.Web.State.Acidic  (WalletState, closeState, openMemState,
                                               openState)
import           Pos.Wallet.Web.State.Acidic  as A
import           Pos.Wallet.Web.State.Storage (AccountLookupMode (..), WalletStorage)

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

getWalletAddresses :: WebWalletModeDB m => m [WalletAddress]
getWalletAddresses = queryDisk A.GetWalletAddresses

getWalletMetas :: WebWalletModeDB m => m [CWalletMeta]
getWalletMetas = queryDisk A.GetWalletMetas

getWalletMeta :: WebWalletModeDB m => WalletAddress -> m (Maybe CWalletMeta)
getWalletMeta = queryDisk . A.GetWalletMeta

getWSetAddresses :: WebWalletModeDB m => m [CAddress WS]
getWSetAddresses = queryDisk A.GetWSetAddresses

getWSetMeta :: WebWalletModeDB m => CAddress WS -> m (Maybe CWalletSetMeta)
getWSetMeta = queryDisk . A.GetWSetMeta

getWSetMetas :: WebWalletModeDB m => m ([CWalletSetMeta])
getWSetMetas = queryDisk A.GetWSetMetas

getWSetPassLU :: WebWalletModeDB m => CAddress WS -> m (Maybe PassPhraseLU)
getWSetPassLU = queryDisk . A.GetWSetPassLU

getWSetSyncTip :: WebWalletModeDB m => CAddress WS -> m (Maybe HeaderHash)
getWSetSyncTip = queryDisk . A.GetWSetSyncTip

getWalletAccounts
    :: WebWalletModeDB m
    => AccountLookupMode -> WalletAddress -> m (Maybe [CAccountAddress])
getWalletAccounts mode = queryDisk . A.GetWalletAccounts mode

doesAccountExist
    :: WebWalletModeDB m
    => AccountLookupMode -> CAccountAddress -> m Bool
doesAccountExist mode = queryDisk . A.DoesAccountExist mode

getProfile :: WebWalletModeDB m => m CProfile
getProfile = queryDisk A.GetProfile

getTxMeta :: WebWalletModeDB m => WalletAddress -> CTxId -> m (Maybe CTxMeta)
getTxMeta addr = queryDisk . A.GetTxMeta addr

getWalletHistory :: WebWalletModeDB m => WalletAddress -> m (Maybe [CTxMeta])
getWalletHistory = queryDisk . A.GetWalletHistory

getUpdates :: WebWalletModeDB m => m [CUpdateInfo]
getUpdates = queryDisk A.GetUpdates

getNextUpdate :: WebWalletModeDB m => m (Maybe CUpdateInfo)
getNextUpdate = queryDisk A.GetNextUpdate

getHistoryCache :: WebWalletModeDB m => WalletAddress -> m (Maybe (HeaderHash, Utxo, [TxHistoryEntry]))
getHistoryCache = queryDisk . A.GetHistoryCache

createWallet :: WebWalletModeDB m => WalletAddress -> CWalletMeta -> m ()
createWallet addr = updateDisk . A.CreateWallet addr

createWSet :: WebWalletModeDB m => CAddress WS -> CWalletSetMeta -> PassPhraseLU -> m ()
createWSet addr passLU = updateDisk . A.CreateWSet addr passLU

addAccount :: WebWalletModeDB m => CAccountAddress -> m ()
addAccount addr = updateDisk $ A.AddAccount addr

addRemovedAccount :: WebWalletModeDB m => CAccountAddress -> m ()
addRemovedAccount addr = updateDisk $ A.AddRemovedAccount addr

setWalletMeta :: WebWalletModeDB m => WalletAddress -> CWalletMeta -> m ()
setWalletMeta addr = updateDisk . A.SetWalletMeta addr

setWSetMeta :: WebWalletModeDB m => CAddress WS -> CWalletSetMeta -> m ()
setWSetMeta addr = updateDisk . A.SetWSetMeta addr

setWSetPassLU :: WebWalletModeDB m => CAddress WS -> PassPhraseLU -> m ()
setWSetPassLU addr = updateDisk . A.SetWSetPassLU addr

setWSetSyncTip :: WebWalletModeDB m => CAddress WS -> HeaderHash -> m ()
setWSetSyncTip addr = updateDisk . A.SetWSetSyncTip addr

setProfile :: WebWalletModeDB m => CProfile -> m ()
setProfile = updateDisk . A.SetProfile

setWalletTransactionMeta :: WebWalletModeDB m => WalletAddress -> CTxId -> CTxMeta -> m ()
setWalletTransactionMeta addr ctxId = updateDisk . A.SetWalletTransactionMeta addr ctxId

setWalletHistory :: WebWalletModeDB m => WalletAddress -> [(CTxId, CTxMeta)] -> m ()
setWalletHistory addr = updateDisk . A.SetWalletHistory addr

addOnlyNewTxMeta :: WebWalletModeDB m => WalletAddress -> CTxId -> CTxMeta -> m ()
addOnlyNewTxMeta addr ctxId = updateDisk . A.AddOnlyNewTxMeta addr ctxId

removeWSet :: WebWalletModeDB m => CAddress WS -> m ()
removeWSet = updateDisk . A.RemoveWSet

removeWallet :: WebWalletModeDB m => WalletAddress -> m ()
removeWallet = updateDisk . A.RemoveWallet

removeAccount :: WebWalletModeDB m => CAccountAddress -> m ()
removeAccount = updateDisk . A.RemoveAccount

totallyRemoveAccount :: WebWalletModeDB m => CAccountAddress -> m ()
totallyRemoveAccount = updateDisk . A.TotallyRemoveAccount

addUpdate :: WebWalletModeDB m => CUpdateInfo -> m ()
addUpdate = updateDisk . A.AddUpdate

removeNextUpdate :: WebWalletModeDB m => m ()
removeNextUpdate = updateDisk A.RemoveNextUpdate

testReset :: WebWalletModeDB m => m ()
testReset = updateDisk A.TestReset

updateHistoryCache :: WebWalletModeDB m => WalletAddress -> HeaderHash -> Utxo -> [TxHistoryEntry] -> m ()
updateHistoryCache cAddr h utxo = updateDisk . A.UpdateHistoryCache cAddr h utxo
