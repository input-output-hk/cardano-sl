-- | DELETE operatiosn on HD wallets
module Cardano.Wallet.Kernel.DB.HdWallet.Delete (
    deleteHdRoot
  , deleteHdAccount
  ) where

import           Universum

import           Control.Lens (at, (.=))

import           Cardano.Wallet.Kernel.DB.AcidStateUtil
import           Cardano.Wallet.Kernel.DB.HdWallet

{-------------------------------------------------------------------------------
  DELETE

  NOTE:

  * There is no 'deleteAddress'.
-------------------------------------------------------------------------------}

-- | Delete a wallet
deleteHdRoot :: HdRootId -> Update' HdRoots Void ()
deleteHdRoot rootId = at rootId .= Nothing

-- | Delete an account
deleteHdAccount :: HdAccountId -> Update' HdRoots UnknownHdRoot ()
deleteHdAccount (HdAccountId rootId accIx) =
    zoomHdRootId identity rootId $
    zoom hdRootAccounts $
      at accIx .= Nothing
