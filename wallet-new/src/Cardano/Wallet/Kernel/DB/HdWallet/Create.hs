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
import qualified Data.Map.Strict as Map
import           Data.SafeCopy (base, deriveSafeCopy)

import qualified Pos.Core as Core

import           Cardano.Wallet.Kernel.DB.AcidStateUtil
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec
import           Cardano.Wallet.Kernel.DB.HdWallet

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
             -> Update' HdRoots CreateHdRootError ()
createHdRoot rootId name hasPass assurance created = do
    exists <- gets $ Map.member rootId
    when exists $ throwError $ CreateHdRootExists rootId
    at rootId .= Just hdRoot
  where
    hdRoot :: HdRoot
    hdRoot = HdRoot {
          _hdRootName        = name
        , _hdRootHasPassword = hasPass
        , _hdRootAssurance   = assurance
        , _hdRootCreatedAt   = created
        , _hdRootAccounts    = Map.empty
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
                -> Update' HdRoots CreateHdAccountError AccountIx
createHdAccount rootId name checkpoint =
    zoomHdRootId CreateHdAccountUnknown rootId $
    zoom hdRootAccounts $ do
      numAccounts <- gets Map.size
      let accIx = AccountIx (fromIntegral numAccounts)
      at accIx .= Just hdAccount
      return accIx
  where
    hdAccount :: HdAccount
    hdAccount = HdAccount {
          _hdAccountName        = name
        , _hdAccountAddresses   = Map.empty
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
                -> Update' HdRoots CreateHdAddressError ()
createHdAddress addrId@(HdAddressId accId addrIx) address =
    zoomHdAccountId CreateHdAddressUnknown accId $
    zoom hdAccountAddresses $ do
      exists <- gets $ Map.member addrIx
      when exists $ throwError $ CreateHdAddressExists addrId
      at addrIx .= Just hdAddress
  where
    hdAddress :: HdAddress
    hdAddress = HdAddress {
          _hdAddressAddress  = address
        , _hdAddressIsUsed   = error "TODO"
        , _hdAddressIsChange = error "TODO"
        }
