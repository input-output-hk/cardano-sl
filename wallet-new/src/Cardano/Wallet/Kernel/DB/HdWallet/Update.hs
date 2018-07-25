-- | UPDATE operations on HD wallets
module Cardano.Wallet.Kernel.DB.HdWallet.Update (
    updateHdRootAssurance
  , updateHdRootName
  , updateHdAccountName
  -- * Errors
  , UpdateHdAccountError (..)
  ) where

import           Universum

import           Control.Lens ((.=))
import           Data.SafeCopy (base, deriveSafeCopy)

import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.Util.AcidState

{-------------------------------------------------------------------------------
  UPDATE
-------------------------------------------------------------------------------}

-- | Errors thrown by 'updateHdAccountName'
data UpdateHdAccountError =
    -- | Parent root not found
    UpdateHdAccountUnknownRoot UnknownHdRoot
    -- | Account not found
  | UpdateHdAccountUnknownAccount UnknownHdAccount
  deriving Eq

deriveSafeCopy 1 'base ''UpdateHdAccountError

updateHdRootAssurance :: HdRootId
                      -> AssuranceLevel
                      -> Update' HdWallets UnknownHdRoot ()
updateHdRootAssurance rootId assurance =
    zoomHdRootId identity rootId $
      hdRootAssurance .= assurance

updateHdRootName :: HdRootId
                 -> WalletName
                 -> Update' HdWallets UnknownHdRoot ()
updateHdRootName rootId name =
    zoomHdRootId identity rootId $
      hdRootName .= name

updateHdAccountName :: HdAccountId
                    -> AccountName
                    -> Update' HdWallets UpdateHdAccountError HdAccount
updateHdAccountName accId name = do
    -- Check that the parent root & account does exist.
    zoomHdRootId UpdateHdAccountUnknownRoot (accId ^. hdAccountIdParent) $
      return ()
    zoomHdAccountId UpdateHdAccountUnknownAccount accId $ do
        oldAccount <- get
        let newAccount = oldAccount & hdAccountName .~ name
        put newAccount
        return newAccount
