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

-- | Modifies the 'State' @st@ with the supplied 'Setter' and returns it.
modifyAndGet :: MonadState s m => (s -> s) -> m s
modifyAndGet f = do
    st <- get
    let st' = f st
    put st'
    return st'

-- | Updates in one gulp the Hd Wallet name and assurance level.
updateHdRoot :: HdRootId
             -> AssuranceLevel
             -> WalletName
             -> Update' HdWallets UnknownHdRoot HdRoot
updateHdRoot rootId assurance name =
    zoomHdRootId identity rootId $ do
        modifyAndGet $ set hdRootAssurance assurance . set hdRootName name

updateHdRootPassword :: HdRootId
                     -> HasSpendingPassword
                     -> Update' HdWallets UnknownHdRoot HdRoot
updateHdRootPassword rootId hasSpendingPassword =
    zoomHdRootId identity rootId $ do
        modifyAndGet $ hdRootHasPassword .~ hasSpendingPassword

updateHdAccountName :: HdAccountId
                    -> AccountName
                    -> Update' HdWallets UnknownHdAccount HdAccount
updateHdAccountName accId name = do
    zoomHdAccountId identity accId $ do
        modifyAndGet $ hdAccountName .~ name
