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
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet

{-------------------------------------------------------------------------------
  DELETE
-------------------------------------------------------------------------------}

-- | Delete a wallet in a cascade fashion, i.e. including all its accounts
-- and transitively all its addresses.
deleteHdRoot :: HdRootId -> Update' HdWallets UnknownHdRoot ()
deleteHdRoot rootId = do
    -- Check that the root exists to begin with
    zoomHdRootId identity rootId $
        return ()

    -- Deletes all the addresses related this wallet, first.
    zoom hdWalletsAddresses $ do
        rootAddresses <- gets (IxSet.toList . IxSet.getEQ rootId)
        forM_ rootAddresses (\addr -> at (addr ^. hdAddressId) .= Nothing)

    -- Deletes all the accounts for this wallet.
    zoom hdWalletsAccounts $ do
        rootAccounts <- gets (IxSet.toList . IxSet.getEQ rootId)
        forM_ rootAccounts (\account -> at (account ^. hdAccountId) .= Nothing)

    -- Finally, delete the wallet.
    zoom hdWalletsRoots $ do
        at rootId .= Nothing


-- | Delete an account
deleteHdAccount :: HdAccountId -> Update' HdWallets UnknownHdAccount ()
deleteHdAccount accId = do
    -- Check that the account & its parent root do exist before deleting anything.
    zoomHdAccountId identity accId $
        return ()

    -- Cascade-delete all the addresses associated with this account
    zoom hdWalletsAddresses $ do
        rootAddresses <- gets (IxSet.toList . IxSet.getEQ accId)
        forM_ rootAddresses (\addr -> at (addr ^. hdAddressId) .= Nothing)

    -- Finally, delete the account.
    zoom hdWalletsAccounts $
        at accId .= Nothing

-- | Delete an address.
deleteHdAddress :: HdAddressId -> Update' HdWallets UnknownHdAccount ()
deleteHdAddress addrId = do
    -- Check that the account & its parent root do exist before deleting anything.
    zoomHdAccountId identity (addrId ^. hdAddressIdParent) $
      return ()
    zoom hdWalletsAddresses $ at addrId .= Nothing
