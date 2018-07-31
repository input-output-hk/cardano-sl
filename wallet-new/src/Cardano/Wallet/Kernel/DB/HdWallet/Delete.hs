-- | DELETE operatiosn on HD wallets
module Cardano.Wallet.Kernel.DB.HdWallet.Delete (
    deleteHdRoot
  , deleteHdAccount
  ) where

import           Universum

import           Control.Lens (at, (.=))

import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.Util.AcidState

{-------------------------------------------------------------------------------
  DELETE
-------------------------------------------------------------------------------}

-- | Delete a wallet
deleteHdRoot :: HdRootId -> Update' HdWallets e ()
deleteHdRoot rootId = zoom hdWalletsRoots $ at rootId .= Nothing

-- | Delete an account
deleteHdAccount :: HdAccountId -> Update' HdWallets UnknownHdAccount ()
deleteHdAccount accId = do
    -- Check that the account & its parent root do exist before deleting anything.
    zoomHdAccountId identity accId $
      return ()
    zoom hdWalletsAccounts $ at accId .= Nothing
