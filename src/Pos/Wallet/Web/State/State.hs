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
       , getWalletMetas
       , getWalletMeta
       , getWalletHistory

       -- * Setters
       , createWallet
       , setWalletMeta
       , setWalletHistory
       , addOnlyNewHistory
       , removeWallet
       ) where

import           Data.Acid                    (EventResult, EventState, QueryEvent,
                                               UpdateEvent)
import           Universum

import           Pos.Wallet.Web.ClientTypes   (CAddress, CTx, CWalletMeta)
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

-- | Constraint for working with web wallet DB
type WebWalletModeDB m = (MonadWalletWebDB m, MonadIO m)

queryDisk
    :: (EventState event ~ WalletStorage, QueryEvent event, WebWalletModeDB m)
    => event -> m (EventResult event)
queryDisk e = getWalletWebState >>= flip A.query e

updateDisk
    :: (EventState event ~ WalletStorage, UpdateEvent event, WebWalletModeDB m)
    => event -> m (EventResult event)
updateDisk e = getWalletWebState >>= flip A.update e

getWalletMetas :: WebWalletModeDB m => m [CWalletMeta]
getWalletMetas = queryDisk A.GetWalletMetas

getWalletMeta :: WebWalletModeDB m => CAddress -> m (Maybe CWalletMeta)
getWalletMeta = queryDisk . A.GetWalletMeta

getWalletHistory :: WebWalletModeDB m => CAddress -> m (Maybe [CTx])
getWalletHistory = queryDisk . A.GetWalletHistory

createWallet :: WebWalletModeDB m => CAddress -> CWalletMeta -> m ()
createWallet addr = updateDisk . A.CreateWallet addr

setWalletMeta :: WebWalletModeDB m => CAddress -> CWalletMeta -> m ()
setWalletMeta addr = updateDisk . A.SetWalletMeta addr

setWalletHistory :: WebWalletModeDB m => CAddress -> [CTx] -> m ()
setWalletHistory addr = updateDisk . A.SetWalletHistory addr

addOnlyNewHistory :: WebWalletModeDB m => CAddress -> [CTx] -> m ()
addOnlyNewHistory addr = updateDisk . A.AddOnlyNewHistory addr

removeWallet :: WebWalletModeDB m => CAddress -> m ()
removeWallet = updateDisk . A.RemoveWallet
