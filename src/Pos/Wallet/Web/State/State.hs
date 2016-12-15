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
       , getDummyAttribute

       -- * Setters
       , setDummyAttribute
       ) where

import           Data.Acid                    (EventResult, EventState, QueryEvent,
                                               UpdateEvent)
import           Universum

import           Pos.Wallet.Web.State.Acidic  (WalletState, closeState, openMemState,
                                               openState)
import           Pos.Wallet.Web.State.Acidic  as A
import           Pos.Wallet.Web.State.Storage (Storage)

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
    :: (EventState event ~ Storage, QueryEvent event, WebWalletModeDB m)
    => event -> m (EventResult event)
queryDisk e = getWalletWebState >>= flip A.query e

updateDisk
    :: (EventState event ~ Storage, UpdateEvent event, WebWalletModeDB m)
    => event -> m (EventResult event)
updateDisk e = getWalletWebState >>= flip A.update e

getDummyAttribute :: WebWalletModeDB m => m Int
getDummyAttribute = queryDisk A.GetDummyAttribute

setDummyAttribute :: WebWalletModeDB m => Int -> m ()
setDummyAttribute = updateDisk . A.SetDummyAttribute
