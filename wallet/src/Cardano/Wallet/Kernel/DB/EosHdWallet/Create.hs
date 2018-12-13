{-# LANGUAGE RankNTypes #-}

-- | CREATE operations on externally-owned sequential (EOS) HD wallets
module Cardano.Wallet.Kernel.DB.EosHdWallet.Create (
    -- * Errors
    CreateEosHdRootError(..)
  , CreateEosHdAccountError(..)
    -- * Functions
  , createEosHdRoot
  , createEosHdAccount
  ) where

import           Universum

import           Control.Lens (at, (.=))
import           Data.SafeCopy (base, deriveSafeCopy)

import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable

import           Pos.Crypto (PublicKey)

import           Cardano.Wallet.Kernel.DB.EosHdWallet
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.EosWalletId (EosWalletId)

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Errors thrown by 'createEosHdWallet'
data CreateEosHdRootError =
    -- | We already have an EOS-wallet with the specified ID
    CreateEosHdRootExists EosWalletId

-- | Errors thrown by 'createEosHdAccount'
data CreateEosHdAccountError =
    -- | The specified EOS-wallet could not be found
    CreateEosHdAccountUnknownRoot UnknownEosHdRoot
    -- | Account already exists
  | CreateEosHdAccountExists PublicKey

deriveSafeCopy 1 'base ''CreateEosHdRootError
deriveSafeCopy 1 'base ''CreateEosHdAccountError

{-------------------------------------------------------------------------------
  CREATE
-------------------------------------------------------------------------------}

-- | Create a new EOS-wallet.
createEosHdRoot :: EosHdRoot -> Update' CreateEosHdRootError EosHdWallets ()
createEosHdRoot eosHdRoot = do
    zoom eosHdWalletsRoots $ do
      exists <- gets $ IxSet.member rootId
      when exists $ throwError $ CreateEosHdRootExists rootId
      at rootId .= Just eosHdRoot
  where
    rootId = eosHdRoot ^. eosHdRootId

-- | Create a new account in EOS-wallet
createEosHdAccount :: EosHdAccount -> Update' CreateEosHdAccountError EosHdWallets ()
createEosHdAccount eosHdAccount = do
    -- Check that the root ID exists
    zoomEosWalletId CreateEosHdAccountUnknownRoot rootId $
      return ()

    zoom eosHdWalletsAccounts $ do
      exists <- gets $ IxSet.member accountPK
      when exists $ throwError $ CreateEosHdAccountExists accountPK
      at accountPK .= Just eosHdAccount
  where
    accountPK = eosHdAccount ^. eosHdAccountPK
    rootId    = eosHdAccount ^. eosHdAccountRootId

{-------------------------------------------------------------------------------
  Pretty printing
-------------------------------------------------------------------------------}

instance Buildable CreateEosHdRootError where
    build (CreateEosHdRootExists rootId)
        = bprint ("CreateEosHdRootError::CreateEosHdRootExists "%build) rootId

instance Buildable CreateEosHdAccountError where
    build (CreateEosHdAccountUnknownRoot (UnknownEosHdRoot rootId))
        = bprint ("CreateHdAccountError::CreateHdAccountUnknownRoot "%build) rootId
    build (CreateEosHdAccountExists accountId)
        = bprint ("CreateEosHdAccountError::CreateEosHdAccountExists "%build) accountId
