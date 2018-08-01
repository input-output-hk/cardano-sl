{-# LANGUAGE RankNTypes #-}

-- | CREATE operations on HD wallets
module Cardano.Wallet.Kernel.DB.HdWallet.Create (
    -- * Errors
    CreateHdRootError(..)
  , CreateHdAccountError(..)
  , CreateHdAddressError(..)
    -- * Functions
  , createHdRoot
  , createHdAccount
  , createHdAddress
    -- * Initial values
  , initHdRoot
  , initHdAccount
  , initHdAddress
  ) where

import           Universum

import           Control.Lens (at, (.=))
import           Data.SafeCopy (base, deriveSafeCopy)

import           Formatting (bprint, build, sformat, (%))
import qualified Formatting.Buildable

import qualified Pos.Core as Core

import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Errors thrown by 'createHdWallet'
data CreateHdRootError =
    -- | We already have a wallet with the specified ID
    CreateHdRootExists HdRootId

-- | Errors thrown by 'createHdAccount'
data CreateHdAccountError =
    -- | The specified wallet could not be found
    CreateHdAccountUnknownRoot UnknownHdRoot

    -- | Account already exists
  | CreateHdAccountExists HdAccountId

-- | Errors thrown by 'createHdAddress'
data CreateHdAddressError =
    -- | Account not found
    CreateHdAddressUnknown UnknownHdAccount

    -- | Address already used
  | CreateHdAddressExists HdAddressId
  deriving Eq

deriveSafeCopy 1 'base ''CreateHdRootError
deriveSafeCopy 1 'base ''CreateHdAccountError
deriveSafeCopy 1 'base ''CreateHdAddressError

{-------------------------------------------------------------------------------
  CREATE
-------------------------------------------------------------------------------}

-- | Create a new wallet
createHdRoot :: HdRoot -> Update' HdWallets CreateHdRootError ()
createHdRoot hdRoot =
    zoom hdWalletsRoots $ do
      exists <- gets $ IxSet.member rootId
      when exists $ throwError $ CreateHdRootExists rootId
      at rootId .= Just hdRoot
  where
    rootId = hdRoot ^. hdRootId

-- | Create a new account
createHdAccount :: HdAccount -> Update' HdWallets CreateHdAccountError ()
createHdAccount hdAccount = do
    -- Check that the root ID exists
    zoomHdRootId CreateHdAccountUnknownRoot rootId $
      return ()

    zoom hdWalletsAccounts $ do
      exists <- gets $ IxSet.member accountId
      when exists $ throwError $ CreateHdAccountExists accountId
      at accountId .= Just hdAccount
  where
    accountId = hdAccount ^. hdAccountId
    rootId    = accountId ^. hdAccountIdParent

-- | Create a new address
createHdAddress :: HdAddress -> Update' HdWallets CreateHdAddressError ()
createHdAddress hdAddress = do
    -- Check that the account ID exists
    zoomHdAccountId CreateHdAddressUnknown (addrId ^. hdAddressIdParent) $
      return ()
    -- Create the new address
    zoom hdWalletsAddresses $ do
      exists <- gets $ IxSet.member addrId
      when exists $ throwError $ CreateHdAddressExists addrId
      at addrId .= Just hdAddress
  where
    addrId = hdAddress ^. hdAddressId

{-------------------------------------------------------------------------------
  Initial values
-------------------------------------------------------------------------------}

-- | New wallet
--
-- The encrypted secret key of the wallet is assumed to be stored elsewhere in
-- some kind of secure key storage; here we ask for the hash of the public key
-- only (i.e., a 'HdRootId'). It is the responsibility of the caller to use the
-- 'BackupPhrase' and (optionally) the 'SpendingPassword' to create a new key
-- add it to the key storage. This is important, because these are secret
-- bits of information that should never end up in the DB log.
initHdRoot :: HdRootId
           -> WalletName
           -> HasSpendingPassword
           -> AssuranceLevel
           -> InDb Core.Timestamp
           -> HdRoot
initHdRoot rootId name hasPass assurance created = HdRoot {
      _hdRootId          = rootId
    , _hdRootName        = name
    , _hdRootHasPassword = hasPass
    , _hdRootAssurance   = assurance
    , _hdRootCreatedAt   = created
    }

-- | New account
--
-- It is the responsibility of the caller to check the wallet's spending
-- password.
--
-- TODO: If any key derivation is happening when creating accounts, should we
-- store a public key or an address or something?
initHdAccount :: HdAccountId
              -> Checkpoint
              -> HdAccount
initHdAccount accountId checkpoint = HdAccount {
      _hdAccountId          = accountId
    , _hdAccountName        = defName
    , _hdAccountCheckpoints = checkpoint :| []
    }
  where
    defName = AccountName $ sformat ("Account: " % build)
                                    (accountId ^. hdAccountIdIx)

-- | New address in the specified account
--
-- Since the DB does not contain the private key of the wallet, we cannot
-- do the actual address derivation here; this will be the responsibility of
-- the caller (which will require the use of the spending password, if
-- one exists).
--
-- Similarly, it will be the responsibility of the caller to pick a random
-- address index, as we do not have access to a random number generator here.
initHdAddress :: HdAddressId
              -> InDb Core.Address
              -> HdAddress
initHdAddress addrId address = HdAddress {
      _hdAddressId       = addrId
    , _hdAddressAddress  = address
    , _hdAddressIsUsed   = error "TODO: _hdAddressIsUsed"
    , _hdAddressIsChange = error "TODO: _hdAddressIsChange"
    }

{-------------------------------------------------------------------------------
  Pretty printing
-------------------------------------------------------------------------------}

instance Buildable CreateHdRootError where
    build (CreateHdRootExists rootId)
        = bprint ("CreateHdRootError::CreateHdRootExists "%build) rootId

instance Buildable CreateHdAccountError where
    build (CreateHdAccountUnknownRoot (UnknownHdRoot rootId))
        = bprint ("CreateHdAccountError::CreateHdAccountUnknownRoot "%build) rootId
    build (CreateHdAccountExists accountId)
        = bprint ("CreateHdAccountError::CreateHdAccountExists "%build) accountId

instance Buildable CreateHdAddressError where
  build (CreateHdAddressUnknown unknownRoot)
      = bprint ("CreateHdAddressUnknown: "%build) unknownRoot
  build (CreateHdAddressExists addressId)
      = bprint ("CreateHdAddressExists: "%build) addressId
