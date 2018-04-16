{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Acid-state database for the wallet kernel
module Cardano.Wallet.Kernel.DB.AcidState (
    -- * Top-level database
    DB(..)
  , hdRoots
    -- * Acid-state operations
    -- ** Snapshot
  , Snapshot(..)
    -- ** Spec mandated updates
  , NewPending(..)
  , ApplyBlock(..)
  , SwitchToFork(..)
    -- ** Updates on HD wallets
    -- *** CREATE
  , CreateHdRoot(..)
  , CreateHdAccount(..)
  , CreateHdAddress(..)
    -- *** UPDATE
  , UpdateHdRootAssurance
  , UpdateHdRootName(..)
  , UpdateHdAccountName(..)
    -- *** DELETE
  , DeleteHdRoot(..)
  , DeleteHdAccount(..)
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import           Data.Acid (Query, Update, makeAcidic)
import           Data.SafeCopy (base, deriveSafeCopy)

import qualified Pos.Core as Core

import           Cardano.Wallet.Kernel.DB.AcidStateUtil
import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.HdWallet
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Create as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Delete as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Update as HD
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Resolved
import           Cardano.Wallet.Kernel.DB.Spec
import qualified Cardano.Wallet.Kernel.DB.Spec.Update as Spec

{-------------------------------------------------------------------------------
  Top-level database
-------------------------------------------------------------------------------}

-- | Full state of the wallet, with the exception of transaction metadata
--
-- We store the different kinds of wallets in different maps for increased
-- type safety. Moreover, since we currently only have a single type of wallet,
-- trying to factor our common parts would be premature at this point.
--
--  References:
--
--  * The acid-state DB for the legacy wallet is defined in module
--    "Pos.Wallet.Web.State.Storage".
--  * V1 API defined in "Cardano.Wallet.API.V1.*" (in @src/@)
data DB = DB {
      _hdRoots :: HdRoots
    }

makeLenses ''DB
deriveSafeCopy 1 'base ''DB

{-------------------------------------------------------------------------------
  Specialized lenses
-------------------------------------------------------------------------------}

-- | All list of checkpoints across the entire DB
dbCheckpoints :: Traversal' DB Checkpoints
dbCheckpoints = hdRoots        . traverse
              . hdRootAccounts . traverse
              . hdAccountCheckpoints

{-------------------------------------------------------------------------------
  Wrap wallet spec
-------------------------------------------------------------------------------}

-- | Errors thrown by 'newPending'
data NewPendingError =
    -- | Unknown account
    NewPendingUnknown UnknownHdAccount

    -- | Some inputs are not in the wallet utxo
  | NewPendingFailed Spec.NewPendingFailed

deriveSafeCopy 1 'base ''NewPendingError

newPending :: HdAccountId
           -> InDb (Core.TxAux)
           -> Update DB (Either NewPendingError ())
newPending accountId tx = runUpdate' . zoom hdRoots $
    zoomHdAccountId NewPendingUnknown accountId $
    zoom hdAccountCheckpoints $
      mapUpdateErrors NewPendingFailed $ Spec.newPending tx

-- | Apply a block
--
-- The block should be prefiltered to contain only inputs and outputs relevant
-- to /any/ of the wallets and accounts.
--
-- NOTE: Calls to 'applyBlock' must be sequentialized by the caller
-- (although concurrent calls to 'applyBlock' cannot interfere with each
-- other, 'applyBlock' must be called in the right order.)
applyBlock :: (ResolvedBlock, BlockMeta) -> Update DB ()
applyBlock block = runUpdateNoErrors . zoomAll dbCheckpoints $
    Spec.applyBlock block

switchToFork :: Int
             -> [(ResolvedBlock, BlockMeta)]
             -> Update DB ()
switchToFork n blocks = runUpdateNoErrors . zoomAll dbCheckpoints $
    Spec.switchToFork n blocks

{-------------------------------------------------------------------------------
  Wrap HD C(R)UD operations
-------------------------------------------------------------------------------}

createHdRoot :: HdRootId
             -> WalletName
             -> HasSpendingPassword
             -> AssuranceLevel
             -> InDb Core.Timestamp
             -> Update DB (Either HD.CreateHdRootError ())
createHdRoot rootId name hasPass assurance created = runUpdate' . zoom hdRoots $
    HD.createHdRoot rootId name hasPass assurance created

createHdAccount :: HdRootId
                -> AccountName
                -> Checkpoint
                -> Update DB (Either HD.CreateHdAccountError AccountIx)
createHdAccount rootId name checkpoint = runUpdate' . zoom hdRoots $
    HD.createHdAccount rootId name checkpoint

createHdAddress :: HdAddressId
                -> InDb Core.Address
                -> Update DB (Either HD.CreateHdAddressError ())
createHdAddress addrId address = runUpdate' . zoom hdRoots $
    HD.createHdAddress addrId address

updateHdRootAssurance :: HdRootId
                      -> AssuranceLevel
                      -> Update DB (Either UnknownHdRoot ())
updateHdRootAssurance rootId assurance = runUpdate' . zoom hdRoots $
    HD.updateHdRootAssurance rootId assurance

updateHdRootName :: HdRootId
                 -> WalletName
                 -> Update DB (Either UnknownHdRoot ())
updateHdRootName rootId name = runUpdate' . zoom hdRoots $
    HD.updateHdRootName rootId name

updateHdAccountName :: HdAccountId
                    -> AccountName
                    -> Update DB (Either UnknownHdAccount ())
updateHdAccountName accId name = runUpdate' . zoom hdRoots $
    HD.updateHdAccountName accId name

deleteHdRoot :: HdRootId -> Update DB ()
deleteHdRoot rootId = runUpdateNoErrors . zoom hdRoots $
    HD.deleteHdRoot rootId

deleteHdAccount :: HdAccountId -> Update DB (Either UnknownHdRoot ())
deleteHdAccount accId = runUpdate' . zoom hdRoots $
    HD.deleteHdAccount accId

{-------------------------------------------------------------------------------
  Acid-state magic
-------------------------------------------------------------------------------}

snapshot :: Query DB DB
snapshot = ask

makeAcidic ''DB [
      -- Database snapshot
      'snapshot
      -- Updates on the "spec state"
    , 'newPending
    , 'applyBlock
    , 'switchToFork
      -- Updates on HD wallets
    , 'createHdRoot
    , 'createHdAccount
    , 'createHdAddress
    , 'updateHdRootAssurance
    , 'updateHdRootName
    , 'updateHdAccountName
    , 'deleteHdRoot
    , 'deleteHdAccount
    ]
