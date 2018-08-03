-- | DELETE operatiosn on HD wallets
module Cardano.Wallet.Kernel.DB.HdWallet.Delete (
    deleteHdRoot
  , deleteHdAccount
  , deleteHdAddress
  ) where

import           Universum

import           Control.Lens (at, (.=))

import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.Util.AcidState

{-------------------------------------------------------------------------------
  DELETE
-------------------------------------------------------------------------------}

-- | Delete a wallet
deleteHdRoot :: HdRootId -> Update' HdWallets UnknownHdRoot ()
deleteHdRoot rootId = do
    -- Check that the root existed to begin with
    zoomHdRootId identity rootId $
      return ()
    zoom hdWalletsRoots $ at rootId .= Nothing

-- | Delete an account
deleteHdAccount :: HdAccountId -> Update' HdWallets UnknownHdAccount ()
deleteHdAccount accId = do
    -- Check that the account & its parent root do exist before deleting anything.
    zoomHdAccountId identity accId $
      return ()
    zoom hdWalletsAccounts $ at accId .= Nothing

-- | Delete an address.
deleteHdAddress :: HdAddressId -> Update' HdWallets UnknownHdAccount ()
deleteHdAddress addrId = do
    -- Check that the account & its parent root do exist before deleting anything.
    zoomHdAccountId identity (addrId ^. hdAddressIdParent) $
      return ()
    zoom hdWalletsAddresses $ at addrId .= Nothing
