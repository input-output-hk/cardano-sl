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
  ) where

import           Universum

import           Control.Lens (at, (.=))
import           Data.SafeCopy (base, deriveSafeCopy)

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
    CreateHdAccountUnknown UnknownHdRoot

-- | Errors thrown by 'createHdAddress'
data CreateHdAddressError =
    -- | Account not found
    CreateHdAddressUnknown UnknownHdAccount

    -- | Address already used
  | CreateHdAddressExists HdAddressId

deriveSafeCopy 1 'base ''CreateHdRootError
deriveSafeCopy 1 'base ''CreateHdAccountError
deriveSafeCopy 1 'base ''CreateHdAddressError

{-------------------------------------------------------------------------------
  CREATE
-------------------------------------------------------------------------------}

-- | Create a new wallet
--
-- The encrypted secret key of the wallet is assumed to be stored elsewhere in
-- some kind of secure key storage; here we ask for the hash of the public key
-- only (i.e., a 'HdRootId'). It is the responsibility of the caller to use the
-- 'BackupPhrase' and (optionally) the 'SpendingPassword' to create a new key
-- add it to the key storage. This is important, beacuse these are secret
-- bits of information that should never end up in the DB log.
--
-- NOTE: The wallet initially has no accounts (see 'createHdAccount').
createHdRoot :: HdRootId
             -> WalletName
             -> HasSpendingPassword
             -> AssuranceLevel
             -> InDb Core.Timestamp
             -> Update' HdWallets CreateHdRootError ()
createHdRoot rootId name hasPass assurance created =
    zoom hdWalletsRoots $ do
      exists <- gets $ IxSet.member rootId
      when exists $ throwError $ CreateHdRootExists rootId
      at rootId .= Just hdRoot
  where
    hdRoot :: HdRoot
    hdRoot = HdRoot {
          _hdRootId          = rootId
        , _hdRootName        = name
        , _hdRootHasPassword = hasPass
        , _hdRootAssurance   = assurance
        , _hdRootCreatedAt   = created
        }

-- | Create a new account in the specified wallet
--
-- It is the responsibility of the caller to check the wallet's spending
-- password.
--
-- TODO: If any key derivation is happening when creating accounts, should we
-- store a public key or an address or something?
createHdAccount :: HdRootId
                -> AccountName
                -> Checkpoint
                -> Update' HdWallets CreateHdAccountError HdAccountId
createHdAccount rootId name checkpoint = do
    -- Check that the root ID exists
    zoomHdRootId CreateHdAccountUnknown rootId $
      return ()
    -- Create the new account
    zoom hdWalletsAccounts $ do
      numAccounts <- gets $ IxSet.size . IxSet.getEQ rootId
      let accIx = HdAccountIx (fromIntegral numAccounts)
          accId = HdAccountId rootId accIx
      at accId .= Just (hdAccount accId)
      return accId
  where
    hdAccount :: HdAccountId -> HdAccount
    hdAccount accId = HdAccount {
          _hdAccountId          = accId
        , _hdAccountName        = name
        , _hdAccountCheckpoints = checkpoint :| []
        }

-- | Create a new address in the specified account
--
-- Since the DB does not contain the private key of the wallet, we cannot
-- do the actual address derivation here; this will be the responsibility of
-- the caller (which will require the use of the spending password, if
-- one exists).
--
-- Similarly, it will be the responsibility of the caller to pick a random
-- address index, as we do not have access to a random number generator here.
createHdAddress :: HdAddressId
                -> InDb Core.Address
                -> Update' HdWallets CreateHdAddressError ()
createHdAddress addrId address = do
    -- Check that the account ID exists
    zoomHdAccountId CreateHdAddressUnknown (addrId ^. hdAddressIdParent) $
      return ()
    -- Create the new address
    zoom hdWalletsAddresses $ do
      exists <- gets $ IxSet.member addrId
      when exists $ throwError $ CreateHdAddressExists addrId
      at addrId .= Just hdAddress
  where
    hdAddress :: HdAddress
    hdAddress = HdAddress {
          _hdAddressId       = addrId
        , _hdAddressAddress  = address
        , _hdAddressIsUsed   = error "TODO: _hdAddressIsUsed"
        , _hdAddressIsChange = error "TODO: _hdAddressIsChange"
        }
