-- | UPDATE operations on HD wallets
module Cardano.Wallet.Kernel.DB.HdWallet.Update (
    updateHdRootAssurance
  , updateHdRootName
  , updateHdRootPassword
  , updateHdAccountName
  ) where

import           Universum

import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.Util.AcidState

{-------------------------------------------------------------------------------
  UPDATE
-------------------------------------------------------------------------------}

updateHdRootAssurance :: HdRootId
                      -> AssuranceLevel
                      -> Update' HdWallets UnknownHdRoot HdRoot
updateHdRootAssurance rootId assurance =
    zoomHdRootId identity rootId $ do
        oldHdRoot <- get
        let newHdRoot = oldHdRoot & hdRootAssurance .~ assurance
        put newHdRoot
        return newHdRoot

updateHdRootPassword :: HdRootId
                     -> HasSpendingPassword
                     -> Update' HdWallets UnknownHdRoot HdRoot
updateHdRootPassword rootId hasSpendingPassword =
    zoomHdRootId identity rootId $ do
        oldHdRoot <- get
        let newHdRoot = oldHdRoot & hdRootHasPassword .~ hasSpendingPassword
        put newHdRoot
        return newHdRoot

updateHdRootName :: HdRootId
                 -> WalletName
                 -> Update' HdWallets UnknownHdRoot HdRoot
updateHdRootName rootId name =
    zoomHdRootId identity rootId $ do
      oldHdRoot <- get
      let newHdRoot = oldHdRoot & hdRootName .~ name
      put newHdRoot
      return newHdRoot

updateHdAccountName :: HdAccountId
                    -> AccountName
                    -> Update' HdWallets UnknownHdAccount HdAccount
updateHdAccountName accId name = do
    zoomHdAccountId identity accId $ do
        oldAccount <- get
        let newAccount = oldAccount & hdAccountName .~ name
        put newAccount
        return newAccount
