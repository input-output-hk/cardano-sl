-- | DELETE operatiosn on HD wallets
module Cardano.Wallet.Kernel.DB.HdWallet.Delete (
    deleteHdRoot
  , deleteHdAccount
  , DeleteHdAccountError(..)
  ) where

import           Universum

import           Control.Lens (at, (.=))
import           Data.SafeCopy (base, deriveSafeCopy)

import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.Util.AcidState

{-------------------------------------------------------------------------------
  DELETE
-------------------------------------------------------------------------------}

-- | Errors thrown by 'deleteHdAccount'
data DeleteHdAccountError =
    -- | Parent root not found
    DeleteHdAccountUnknownRoot UnknownHdRoot
  deriving Eq

deriveSafeCopy 1 'base ''DeleteHdAccountError

-- | Delete a wallet
deleteHdRoot :: HdRootId -> Update' HdWallets e ()
deleteHdRoot rootId = zoom hdWalletsRoots $ at rootId .= Nothing

-- | Delete an account
deleteHdAccount :: HdAccountId -> Update' HdWallets DeleteHdAccountError ()
deleteHdAccount accId = do
    -- Check that the parent root does exist.
    zoomHdRootId DeleteHdAccountUnknownRoot (accId ^. hdAccountIdParent) $
      return ()
    zoom hdWalletsAccounts $ at accId .= Nothing
