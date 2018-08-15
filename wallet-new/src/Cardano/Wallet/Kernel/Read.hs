-- | Read-only access to the DB
module Cardano.Wallet.Kernel.Read (
    -- * Read-only access to the DB
    DB -- opaque
    -- ** The only effectful getter you will ever need
  , getWalletSnapshot
    -- ** Pure getters acting on a DB snapshot
  , module Getters
  ) where

import           Universum hiding (State)

import           Data.Acid.Advanced (query')

import           Cardano.Wallet.Kernel.DB.AcidState (DB, Snapshot (..))
import           Cardano.Wallet.Kernel.DB.Read as Getters
import           Cardano.Wallet.Kernel.Internal

-- | The only effectful query on this 'PassiveWallet'.
getWalletSnapshot :: PassiveWallet -> IO DB
getWalletSnapshot pw = query' (pw ^. wallets) Snapshot
