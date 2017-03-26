{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

module Pos.Wallet.Web.State.State
       ( WalletState
       , MonadWalletWebDB (..)
       , WebWalletModeDB
       , openState
       , openMemState
       , closeState

       -- * Getters
       , getProfile
       , getWalletAddresses
       , getWalletMetas
       , getWalletMeta
       , getWalletAccounts
       , getAddressPath
       , getWSetMetas
       , getWSetMeta
       , getTxMeta
       , getWalletHistory
       , getUpdates
       , getNextUpdate
       , getHistoryCache

       -- * Setters
       , testReset
       , createWallet
       , createWSet
       , addWalletAccount
       , addAddressPath
       , setProfile
       , setWalletMeta
       , setWSetMeta
       , setWalletTransactionMeta
       , setWalletHistory
       , addOnlyNewTxMeta
       , removeWallet
       , removeWalletAccount
       , addUpdate
       , removeNextUpdate
       , updateHistoryCache
       ) where

import           Data.Acid                    (EventResult, EventState, QueryEvent,
                                               UpdateEvent)
import           Mockable                     (MonadMockable)
import           Universum

import           Pos.Client.Txp.History       (TxHistoryEntry)
import           Pos.Slotting                 (NtpSlotting)
import           Pos.Txp                      (Utxo)
import           Pos.Types                    (HeaderHash)
import           Pos.Wallet.Web.ClientTypes   (CAddress, CProfile, CTxId, CTxMeta,
                                               CUpdateInfo, CWalletMeta, CWalletSetMeta)
import           Pos.Wallet.Web.State.Acidic  (WalletState, closeState, openMemState,
                                               openState)
import           Pos.Wallet.Web.State.Acidic  as A
import           Pos.Wallet.Web.State.Storage (WalletStorage)

-- | MonadWalletWebDB stands for monad which is able to get web wallet state
class Monad m => MonadWalletWebDB m where
    getWalletWebState :: m WalletState

-- | Instances for common transformers
instance MonadWalletWebDB m => MonadWalletWebDB (ReaderT r m) where
    getWalletWebState = lift getWalletWebState

instance MonadWalletWebDB m => MonadWalletWebDB (StateT s m) where
    getWalletWebState = lift getWalletWebState

instance MonadWalletWebDB m => MonadWalletWebDB (NtpSlotting m) where
    getWalletWebState = lift getWalletWebState

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

getWalletAddresses :: WebWalletModeDB m => m [CAddress]
getWalletAddresses = queryDisk A.GetWalletAddresses

getWalletMetas :: WebWalletModeDB m => m [CWalletMeta]
getWalletMetas = queryDisk A.GetWalletMetas

getWalletMeta :: WebWalletModeDB m => CAddress -> m (Maybe CWalletMeta)
getWalletMeta = queryDisk . A.GetWalletMeta

getWSetMetas :: WebWalletModeDB m => m [CWalletSetMeta]
getWSetMetas = queryDisk A.GetWSetMetas

getWSetMeta :: WebWalletModeDB m => CAddress -> m (Maybe CWalletSetMeta)
getWSetMeta = queryDisk . A.GetWSetMeta

getWalletAccounts :: WebWalletModeDB m => CAddress -> m (Maybe [CAddress])
getWalletAccounts = queryDisk . A.GetWalletAccounts

getAddressPath :: WebWalletModeDB m => CAddress -> m (Maybe [Word32])
getAddressPath = queryDisk . A.GetAddressPath

getProfile :: WebWalletModeDB m => m (Maybe CProfile)
getProfile = queryDisk A.GetProfile

getTxMeta :: WebWalletModeDB m => CAddress -> CTxId -> m (Maybe CTxMeta)
getTxMeta addr = queryDisk . A.GetTxMeta addr

getWalletHistory :: WebWalletModeDB m => CAddress -> m (Maybe [CTxMeta])
getWalletHistory = queryDisk . A.GetWalletHistory

getUpdates :: WebWalletModeDB m => m [CUpdateInfo]
getUpdates = queryDisk A.GetUpdates

getNextUpdate :: WebWalletModeDB m => m (Maybe CUpdateInfo)
getNextUpdate = queryDisk A.GetNextUpdate

getHistoryCache :: WebWalletModeDB m => CAddress -> m (Maybe (HeaderHash, Utxo, [TxHistoryEntry]))
getHistoryCache = queryDisk . A.GetHistoryCache

createWallet :: WebWalletModeDB m => CAddress -> CWalletMeta -> m ()
createWallet addr = updateDisk . A.CreateWallet addr

createWSet :: WebWalletModeDB m => CAddress -> CWalletSetMeta -> m ()
createWSet addr = updateDisk . A.CreateWSet addr

addWalletAccount :: WebWalletModeDB m => CAddress -> CAddress -> m ()
addWalletAccount waddr aaddr = updateDisk $ A.AddWalletAccount waddr aaddr

addAddressPath :: WebWalletModeDB m => CAddress -> [Word32] -> m ()
addAddressPath addr = updateDisk . A.AddAddressPath addr

setWalletMeta :: WebWalletModeDB m => CAddress -> CWalletMeta -> m ()
setWalletMeta addr = updateDisk . A.SetWalletMeta addr

setWSetMeta :: WebWalletModeDB m => CAddress -> CWalletSetMeta -> m ()
setWSetMeta addr = updateDisk . A.SetWSetMeta addr

setProfile :: WebWalletModeDB m => CProfile -> m ()
setProfile = updateDisk . A.SetProfile

setWalletTransactionMeta :: WebWalletModeDB m => CAddress -> CTxId -> CTxMeta -> m ()
setWalletTransactionMeta addr ctxId = updateDisk . A.SetWalletTransactionMeta addr ctxId

setWalletHistory :: WebWalletModeDB m => CAddress -> [(CTxId, CTxMeta)] -> m ()
setWalletHistory addr = updateDisk . A.SetWalletHistory addr

addOnlyNewTxMeta :: WebWalletModeDB m => CAddress -> CTxId -> CTxMeta -> m ()
addOnlyNewTxMeta addr ctxId = updateDisk . A.AddOnlyNewTxMeta addr ctxId

removeWallet :: WebWalletModeDB m => CAddress -> m ()
removeWallet = updateDisk . A.RemoveWallet

removeWalletAccount :: WebWalletModeDB m => CAddress -> CAddress -> m ()
removeWalletAccount aaddr = updateDisk . A.RemoveWalletAccount aaddr

addUpdate :: WebWalletModeDB m => CUpdateInfo -> m ()
addUpdate = updateDisk . A.AddUpdate

removeNextUpdate :: WebWalletModeDB m => m ()
removeNextUpdate = updateDisk A.RemoveNextUpdate

testReset :: WebWalletModeDB m => m ()
testReset = updateDisk A.TestReset

updateHistoryCache :: WebWalletModeDB m => CAddress -> HeaderHash -> Utxo -> [TxHistoryEntry] -> m ()
updateHistoryCache cAddr h utxo = updateDisk . A.UpdateHistoryCache cAddr h utxo
