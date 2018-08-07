-- | UPDATE operations on HD wallets
module Cardano.Wallet.Kernel.DB.HdWallet.Update (
    updateHdRoot
  , updateHdRootPassword
  , updateHdAccountName
  ) where

import           Universum

import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.Util.AcidState

{-------------------------------------------------------------------------------
  UPDATE
-------------------------------------------------------------------------------}

-- | Updates in one gulp the Hd Wallet name and assurance level.
updateHdRoot :: HdRootId
             -> AssuranceLevel
             -> WalletName
             -> Update' HdWallets UnknownHdRoot HdRoot
updateHdRoot rootId assurance name =
    zoomHdRootId identity rootId $ do
        oldHdRoot <- get
        let newHdRoot = oldHdRoot & set hdRootAssurance assurance
                                  . set hdRootName name
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

updateHdAccountName :: HdAccountId
                    -> AccountName
                    -> Update' HdWallets UnknownHdAccount HdAccount
updateHdAccountName accId name = do
    zoomHdAccountId identity accId $ do
        oldAccount <- get
        let newAccount = oldAccount & hdAccountName .~ name
        put newAccount
        return newAccount
