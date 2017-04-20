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
       , getWSetMetas
       , getWSetMeta
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
       , addAccount
       , setProfile
       , setWalletMeta
       , setWSetMeta
       , setWalletTransactionMeta
       , setWalletHistory
       , addOnlyNewTxMeta
       , removeWSet
       , removeWallet
       , removeAccount
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
import           Pos.Wallet.Web.ClientTypes   (CAccountAddress, CAddress, CProfile, CTxId,
                                               CTxMeta, CUpdateInfo, CWalletAddress,
                                               CWalletMeta, CWalletSetMeta, WS)
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

getWalletAddresses :: WebWalletModeDB m => m [CWalletAddress]
getWalletAddresses = queryDisk A.GetWalletAddresses

getWalletMetas :: WebWalletModeDB m => m [CWalletMeta]
getWalletMetas = queryDisk A.GetWalletMetas

getWalletMeta :: WebWalletModeDB m => CWalletAddress -> m (Maybe CWalletMeta)
getWalletMeta = queryDisk . A.GetWalletMeta

getWSetAddresses :: WebWalletModeDB m => m [CAddress WS]
getWSetAddresses = queryDisk A.GetWSetAddresses

getWSetMeta :: WebWalletModeDB m => CAddress WS -> m (Maybe CWalletSetMeta)
getWSetMeta = queryDisk . A.GetWSetMeta

getWSetMetas :: WebWalletModeDB m => m ([CWalletSetMeta])
getWSetMetas = queryDisk A.GetWSetMetas

getWalletAccounts :: WebWalletModeDB m => CWalletAddress -> m (Maybe [CAccountAddress])
getWalletAccounts = queryDisk . A.GetWalletAccounts

doesAccountExist :: WebWalletModeDB m => CAccountAddress -> m Bool
doesAccountExist = queryDisk . A.DoesAccountExist

getProfile :: WebWalletModeDB m => m (Maybe CProfile)
getProfile = queryDisk A.GetProfile

getTxMeta :: WebWalletModeDB m => CWalletAddress -> CTxId -> m (Maybe CTxMeta)
getTxMeta addr = queryDisk . A.GetTxMeta addr

getWalletHistory :: WebWalletModeDB m => CWalletAddress -> m (Maybe [CTxMeta])
getWalletHistory = queryDisk . A.GetWalletHistory

getUpdates :: WebWalletModeDB m => m [CUpdateInfo]
getUpdates = queryDisk A.GetUpdates

getNextUpdate :: WebWalletModeDB m => m (Maybe CUpdateInfo)
getNextUpdate = queryDisk A.GetNextUpdate

getHistoryCache :: WebWalletModeDB m => CWalletAddress -> m (Maybe (HeaderHash, Utxo, [TxHistoryEntry]))
getHistoryCache = queryDisk . A.GetHistoryCache

createWallet :: WebWalletModeDB m => CWalletAddress -> CWalletMeta -> m ()
createWallet addr = updateDisk . A.CreateWallet addr

createWSet :: WebWalletModeDB m => CAddress WS -> CWalletSetMeta -> m ()
createWSet addr = updateDisk . A.CreateWSet addr

addAccount :: WebWalletModeDB m => CAccountAddress -> m ()
addAccount addr = updateDisk $ A.AddAccount addr

setWalletMeta :: WebWalletModeDB m => CWalletAddress -> CWalletMeta -> m ()
setWalletMeta addr = updateDisk . A.SetWalletMeta addr

setWSetMeta :: WebWalletModeDB m => CAddress WS -> CWalletSetMeta -> m ()
setWSetMeta addr = updateDisk . A.SetWSetMeta addr

setProfile :: WebWalletModeDB m => CProfile -> m ()
setProfile = updateDisk . A.SetProfile

setWalletTransactionMeta :: WebWalletModeDB m => CWalletAddress -> CTxId -> CTxMeta -> m ()
setWalletTransactionMeta addr ctxId = updateDisk . A.SetWalletTransactionMeta addr ctxId

setWalletHistory :: WebWalletModeDB m => CWalletAddress -> [(CTxId, CTxMeta)] -> m ()
setWalletHistory addr = updateDisk . A.SetWalletHistory addr

addOnlyNewTxMeta :: WebWalletModeDB m => CWalletAddress -> CTxId -> CTxMeta -> m ()
addOnlyNewTxMeta addr ctxId = updateDisk . A.AddOnlyNewTxMeta addr ctxId

removeWSet :: WebWalletModeDB m => CAddress WS -> m ()
removeWSet = updateDisk . A.RemoveWSet

removeWallet :: WebWalletModeDB m => CWalletAddress -> m ()
removeWallet = updateDisk . A.RemoveWallet

removeAccount :: WebWalletModeDB m => CAccountAddress -> m ()
removeAccount = updateDisk . A.RemoveAccount

addUpdate :: WebWalletModeDB m => CUpdateInfo -> m ()
addUpdate = updateDisk . A.AddUpdate

removeNextUpdate :: WebWalletModeDB m => m ()
removeNextUpdate = updateDisk A.RemoveNextUpdate

testReset :: WebWalletModeDB m => m ()
testReset = updateDisk A.TestReset

updateHistoryCache :: WebWalletModeDB m => CWalletAddress -> HeaderHash -> Utxo -> [TxHistoryEntry] -> m ()
updateHistoryCache cAddr h utxo = updateDisk . A.UpdateHistoryCache cAddr h utxo
