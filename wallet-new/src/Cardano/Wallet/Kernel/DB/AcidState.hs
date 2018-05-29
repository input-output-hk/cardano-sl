{-# OPTIONS_GHC -fno-warn-orphans #-} -- to enable... deriveSafeCopy 1 'base ''EncryptedSecretKey
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Acid-state database for the wallet kernel
module Cardano.Wallet.Kernel.DB.AcidState (
    -- * Top-level database
    DB(..)
  , dbHdWallets
  , defDB
    -- * Acid-state operations
    -- ** Snapshot
  , Snapshot(..)
    -- ** Spec mandated updates
  , NewPending(..)
  , ApplyBlock(..)
  , SwitchToFork(..)
    -- ** Updates on HD wallets
    -- *** CREATE
  , CreateHdWallet(..)
  , CreateHdAddress(..)
    -- *** UPDATE
  , UpdateHdRootAssurance
  , UpdateHdRootName(..)
  , UpdateHdAccountName(..)
    -- *** DELETE
  , DeleteHdRoot(..)
  , DeleteHdAccount(..)
    -- * errors
  , NewPendingError
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)

import           Data.Acid (Query, Update, makeAcidic)
import qualified Data.Map.Strict as Map
import           Data.SafeCopy (base, deriveSafeCopy)

import qualified Pos.Core as Core
import           Pos.Txp (Utxo)
import           Pos.Core.Chrono (OldestFirst(..))

import           Cardano.Wallet.Kernel.PrefilterTx (AddrWithId, PrefilteredBlock (..), PrefilteredUtxo)

import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.HdWallet
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Create as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Delete as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Update as HD
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec
import qualified Cardano.Wallet.Kernel.DB.Spec.Util as Spec
import qualified Cardano.Wallet.Kernel.DB.Spec.Update as Spec
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet

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
      _dbHdWallets :: HdWallets
    }

makeLenses ''DB
deriveSafeCopy 1 'base ''DB

-- | Default DB
defDB :: DB
defDB = DB initHdWallets

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
           -> InDb Core.TxAux
           -> Update DB (Either NewPendingError ())
newPending accountId tx = runUpdate' . zoom dbHdWallets $
    zoomHdAccountId NewPendingUnknown accountId $
    zoom hdAccountCheckpoints $
      mapUpdateErrors NewPendingFailed $ Spec.newPending tx

-- | Apply prefiltered block (indexed by HdAccountId) to the matching accounts.
--
-- The prefiltered block should be indexed by AccountId, with each prefiltered block
-- containing only inputs and outputs relevant to the account. Since HdAccountId embeds HdRootId,
-- it unambiguously places an Account in the Wallet/Account hierarchy. The AccountIds here could
-- therefor refer to an Account in /any/ Wallet (not only sibling accounts in a single wallet).

-- NOTE:
-- * Calls to 'applyBlock' must be sequentialized by the caller
-- (although concurrent calls to 'applyBlock' cannot interfere with each
-- other, 'applyBlock' must be called in the right order.)
--
-- * Since a block may reference wallet accounts that do not exist yet locally,
-- we need to create such 'missing' accounts. (An Account might not exist locally
-- if it was created on another node instance of this wallet).
--
-- * For every address encountered in the block outputs, create an HdAddress if it
-- does not already exist.
applyBlock :: (Map HdAccountId PrefilteredBlock, BlockMeta) -> Update DB ()
applyBlock (blocksByAccount,meta) = runUpdateNoErrors $
    zoom dbHdWallets $
        forM_ (Map.toList blocksByAccount) $ \(accountId,prefBlock) ->
            zoomHdAccountIdWithCreate accountId (pfbAddrs prefBlock) initAccountUtxo $
                zoom hdAccountCheckpoints $
                    modify $ Spec.applyBlock (prefBlock,meta)
     where
         -- Accounts are discovered during wallet creation (if the account was given
         -- a balance in the genesis block) or otherwise, during ApplyBlock. For accounts
         -- discovered during ApplyBlock, we can assume that there was no genesis utxo,
         -- hence we use empty initial utxo for such new accounts.
         initAccountUtxo = Map.empty

-- | Switch to a fork
--
-- See comments about prefiltering for 'applyBlock'.
--
-- TODO: We use a plain list here rather than 'OldestFirst' since the latter
-- does not have a 'SafeCopy' instance.
switchToFork :: Int
             -> [(PrefilteredBlock, BlockMeta)]
             -> Update DB ()
switchToFork n blocks = runUpdateNoErrors $
    zoomAll (dbHdWallets . hdWalletsAccounts) $
      hdAccountCheckpoints %~ Spec.switchToFork n (OldestFirst blocks)

{-------------------------------------------------------------------------------
Wallet creation
-------------------------------------------------------------------------------}

-- | Create an HdWallet with HdRoot, possibly with HdAccounts and HdAddresses.
--
--  Given prefiltered utxo's, by account, create an HdAccount for each account,
--  along with HdAddresses for all utxo outputs.
createHdWallet :: HdRootId
               -> WalletName
               -> HasSpendingPassword
               -> AssuranceLevel
               -> InDb Core.Timestamp
               -> Map HdAccountId PrefilteredUtxo
               -> Update DB (Either HD.CreateHdWalletError ())
createHdWallet rootId name spendingPassword assuranceLevel created utxoByAccount
    = runUpdate' . zoom dbHdWallets $ do
          mapUpdateErrors HD.CreateHdWalletRootFailed
              $ void
              $ HD.createHdRoot rootId name spendingPassword assuranceLevel created
          cannotFail
              $ mapM_ createHdAccountWithAddrs utxoByAccountL

    where
        utxoByAccountL = Map.toList utxoByAccount

        -- | Cannot fail with domain-level errors (we avoid any failure due
        --   to already existing accounts or addresses, or missing parent entities)
        createHdAccountWithAddrs (accId,(utxo,addrs)) = do
            cannotFail $
                createHdAccount accId utxo
            cannotFail $
                createHdAddresses addrs

-- | Create an HdAccount for the given HdAccountId and Utxo.
--
--   Create a new checkpoint with Utxo and UtxoBalance.
--   NOTE: The given Utxo must be prefiltered.
createHdAccount :: HdAccountId
                -> Utxo
                -> Update' HdWallets HD.CreateHdAccountError ()
createHdAccount accountId utxo = HD.createHdAccount accountId newCheckpoint
    where
        newCheckpoint
            = Checkpoint {
               _checkpointUtxo        = InDb utxo
             , _checkpointUtxoBalance = InDb $ Spec.balance utxo
             , _checkpointExpected    = InDb Map.empty
             , _checkpointPending     = Pending . InDb $ Map.empty
             -- TODO proper BlockMeta initialisation
             , _checkpointBlockMeta   = BlockMeta . InDb $ Map.empty
             }

-- | Create a collection of HdAddresses (only create an address if it does not
--   already exist)
createHdAddresses :: [AddrWithId]
                  -> Update' HdWallets Void ()
createHdAddresses = mapM_ (uncurry createHdAddressIfNotExists)

createHdAddressIfNotExists :: HdAddressId
                           -> Core.Address
                           -> Update' HdWallets Void ()
createHdAddressIfNotExists addressId address = do
   exists <- zoom hdWalletsAddresses $ gets (IxSet.member addressId)
   unless exists $
       cannotFail $
           HD.createHdAddress addressId (InDb address)

{-------------------------------------------------------------------------------
  Wrap HD C(R)UD operations
-------------------------------------------------------------------------------}

createHdRoot :: HdRootId
             -> WalletName
             -> HasSpendingPassword
             -> AssuranceLevel
             -> InDb Core.Timestamp
             -> Update DB (Either HD.CreateHdRootError ())
createHdRoot rootId name hasPass assurance created = runUpdate' . zoom dbHdWallets $
    HD.createHdRoot rootId name hasPass assurance created

createHdAddress :: HdAddressId
                -> InDb Core.Address
                -> Update DB (Either HD.CreateHdAddressError ())
createHdAddress addrId address = runUpdate' . zoom dbHdWallets $
    HD.createHdAddress addrId address

updateHdRootAssurance :: HdRootId
                      -> AssuranceLevel
                      -> Update DB (Either UnknownHdRoot ())
updateHdRootAssurance rootId assurance = runUpdate' . zoom dbHdWallets $
    HD.updateHdRootAssurance rootId assurance

updateHdRootName :: HdRootId
                 -> WalletName
                 -> Update DB (Either UnknownHdRoot ())
updateHdRootName rootId name = runUpdate' . zoom dbHdWallets $
    HD.updateHdRootName rootId name

updateHdAccountName :: HdAccountId
                    -> AccountName
                    -> Update DB (Either UnknownHdAccount ())
updateHdAccountName accId name = runUpdate' . zoom dbHdWallets $
    HD.updateHdAccountName accId name

deleteHdRoot :: HdRootId -> Update DB ()
deleteHdRoot rootId = runUpdateNoErrors . zoom dbHdWallets $
    HD.deleteHdRoot rootId

deleteHdAccount :: HdAccountId -> Update DB (Either UnknownHdRoot ())
deleteHdAccount accId = runUpdate' . zoom dbHdWallets $
    HD.deleteHdAccount accId

{-------------------------------------------------------------------------------
  Error handling utils and safe account zooming
-------------------------------------------------------------------------------}

-- | Handles a domain error we consider to be an impossible occurence
--   (at the call site of this function)
impossibleErr :: forall e e'.
                 e
              -> e'
impossibleErr _ = error "undefined" -- TODO do nothing

-- | `mapUpdateErrors` for "impossible" domain errors, used to assert that
--   the domain error `e` is not a possibility in the calling context
cannotFail :: forall st e e' a.
              Update' st e a
           -> Update' st e' a
cannotFail = mapUpdateErrors impossibleErr

-- | Create an account and any addresses that do not exist, then zoom safely
--   to the accountId
zoomHdAccountIdWithCreate :: HdAccountId
                          -> [AddrWithId]
                          -> Utxo
                          -> Update' HdAccount Void ()
                          -> Update' HdWallets Void ()
zoomHdAccountIdWithCreate accId addrs initUtxo updateAction = do
    exists <- zoom hdWalletsAccounts $ gets (IxSet.member accId)
    unless exists $
        cannotFail $
            createHdAccount accId initUtxo

    createHdAddresses addrs

    zoomHdAccountId impossibleErr accId updateAction

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
    , 'createHdAddress
    , 'createHdWallet
    , 'updateHdRootAssurance
    , 'updateHdRootName
    , 'updateHdAccountName
    , 'deleteHdRoot
    , 'deleteHdAccount
    ]
