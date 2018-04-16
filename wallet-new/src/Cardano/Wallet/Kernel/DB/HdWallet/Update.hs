-- | UPDATE operations on HD wallets
module Cardano.Wallet.Kernel.DB.HdWallet.Update (
    updateHdRootAssurance
  , updateHdRootName
  , updateHdAccountName
  ) where

import           Universum

import           Control.Lens ((.=))

import           Cardano.Wallet.Kernel.DB.AcidStateUtil
import           Cardano.Wallet.Kernel.DB.HdWallet

{-------------------------------------------------------------------------------
  UPDATE
-------------------------------------------------------------------------------}

updateHdRootAssurance :: HdRootId
                      -> AssuranceLevel
                      -> Update' HdRoots UnknownHdRoot ()
updateHdRootAssurance rootId assurance =
    zoomHdRootId identity rootId $
      hdRootAssurance .= assurance

updateHdRootName :: HdRootId
                 -> WalletName
                 -> Update' HdRoots UnknownHdRoot ()
updateHdRootName rootId name =
    zoomHdRootId identity rootId $
      hdRootName .= name

updateHdAccountName :: HdAccountId
                    -> AccountName
                    -> Update' HdRoots UnknownHdAccount ()
updateHdAccountName accId name =
    zoomHdAccountId identity accId $
      hdAccountName .= name
