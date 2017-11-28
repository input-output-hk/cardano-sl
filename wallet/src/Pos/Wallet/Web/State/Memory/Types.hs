-- | WalletStorage modifier

module Pos.Wallet.Web.State.Memory.Types
       ( ExtStorageModifier (..)
       , esmTipL
       , esmMemStorageModifierL
       , ExtStorageModifierVar
       , HasExtStorageModifier

       , StorageModifier (..)
       , applyWalModifier
       , applyToWalletStorage
       , applyToWalletStorageForOneWallet
       , removeWalModifiers
       ) where

import           Universum

import qualified Control.Concurrent.STM           as STM
import           Control.Lens                     (makeLensesWith)
import qualified Data.HashMap.Strict              as HM

import           Pos.Core                         (HeaderHash)
import           Pos.Core.Configuration           (HasConfiguration)
import           Pos.Util.Util                    (HasLens, postfixLFields2)

import           Pos.Wallet.Web.ClientTypes       (CId, Wal)
import           Pos.Wallet.Web.State.Storage     (WalletInfo (..), WalletStorage (..),
                                                   WalletTip (..), applyModifierToWallet)
import           Pos.Wallet.Web.Tracking.Modifier (WalletModifier)

----------------------------------------------------------------------------
-- WalletsModifier
----------------------------------------------------------------------------

data ExtStorageModifier = ExtStorageModifier
    { esmTip                :: !HeaderHash
    , esmMemStorageModifier :: !StorageModifier
    }

-- TMVar is used here to handle synchronization of wallet-db and ExtStorageModifier.
-- When a value from TMVar is available then everything is in consistent state,
-- otherwise chaning tip for wallet-db and ExtStorageModifier
-- is going atm in a thread taken value from TMVar.
type ExtStorageModifierVar = STM.TMVar ExtStorageModifier
type HasExtStorageModifier ctx = HasLens ExtStorageModifierVar ctx ExtStorageModifierVar

----------------------------------------------------------------------------
-- SModifier
----------------------------------------------------------------------------

-- | Simple WalletStorage modifier.
-- It stores CWalletModifier in reverse order.
newtype StorageModifier = StorageModifier
    { getStorageModifier :: HashMap (CId Wal) WalletModifier
    } deriving (Monoid)

applyWalModifier :: (CId Wal, WalletModifier) -> StorageModifier -> StorageModifier
applyWalModifier (wid, md) =
    StorageModifier . HM.insertWith mappend wid md . getStorageModifier

removeWalModifiers :: CId Wal -> StorageModifier -> StorageModifier
removeWalModifiers id (StorageModifier modifiers) = StorageModifier (HM.delete id modifiers)

applyToWalletStorage :: HasConfiguration => StorageModifier -> WalletStorage -> WalletStorage
applyToWalletStorage = applyModifiers . HM.toList . getStorageModifier

applyToWalletStorageForOneWallet :: HasConfiguration => CId Wal -> StorageModifier -> WalletStorage -> WalletStorage
applyToWalletStorageForOneWallet id =
    applyModifiers . one . (id, ) . HM.lookupDefault mempty id . getStorageModifier

applyModifiers :: HasConfiguration => [(CId Wal, WalletModifier)] -> WalletStorage -> WalletStorage
applyModifiers modifiers s = foldr f s modifiers
  where
    f :: (CId Wal, WalletModifier) -> WalletStorage -> WalletStorage
    f (id, modif) storage@(WalletStorage {..})
        | Just wi       <- HM.lookup id _wsWalletInfos
        , SyncedWith hh <- _wiSyncTip wi =
            execState (applyModifierToWallet id hh modif) storage
        | otherwise = storage

makeLensesWith postfixLFields2 ''ExtStorageModifier
