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
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Txp (Utxo)

import           Cardano.Wallet.Kernel.PrefilterTx (AddrWithId, PrefilteredBlock (..),
                                                    PrefilteredUtxo)

import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.HdWallet
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Create as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Delete as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Update as HD
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec
import qualified Cardano.Wallet.Kernel.DB.Spec.Update as Spec
import qualified Cardano.Wallet.Kernel.DB.Spec.Util as Spec
import           Cardano.Wallet.Kernel.DB.Util.AcidState

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
--
-- TODO(@uroboros/ryan) Move BlockMeta inside PrefilteredBlock (as part of CBR-239: Support history tracking and queries)
applyBlock :: (Map HdAccountId PrefilteredBlock, BlockMeta) -> Update DB ()
applyBlock (blocksByAccount,meta) = runUpdateNoErrors $ zoom dbHdWallets $
    createPrefiltered
        initUtxoAndAddrs
        (\prefBlock -> zoom hdAccountCheckpoints $
                           modify $ Spec.applyBlock (prefBlock,meta))
        blocksByAccount
  where
    -- Accounts are discovered during wallet creation (if the account was given
    -- a balance in the genesis block) or otherwise, during ApplyBlock. For
    -- accounts discovered during ApplyBlock, we can assume that there was no
    -- genesis utxo, hence we use empty initial utxo for such new accounts.
    initUtxoAndAddrs :: PrefilteredBlock -> (Utxo, [AddrWithId])
    initUtxoAndAddrs pb = (Map.empty, pfbAddrs pb)

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
createHdWallet :: HdRoot
               -> Map HdAccountId PrefilteredUtxo
               -> Update DB (Either HD.CreateHdRootError ())
createHdWallet newRoot utxoByAccount = runUpdate' . zoom dbHdWallets $ do
      HD.createHdRoot newRoot
      createPrefiltered
        identity
        (\_ -> return ()) -- we just want to create the accounts
        utxoByAccount

{-------------------------------------------------------------------------------
  Internal auxiliary: apply a function to a prefiltered block/utxo
-------------------------------------------------------------------------------}

-- | For each of the specified accounts, create them if they do not exist,
-- and apply the specified function.
createPrefiltered :: forall p e.
                     (p -> (Utxo, [AddrWithId]))
                      -- ^ Initial UTxO (when we are creating the account),
                      -- as well as set of addresses the account should have
                  -> (p -> Update' HdAccount e ())
                      -- ^ Function to apply to the account
                  -> Map HdAccountId p -> Update' HdWallets e ()
createPrefiltered initUtxoAndAddrs applyP accs = do
      forM_ (Map.toList accs) $ \(accId, p) -> do
        let utxo  :: Utxo
            addrs :: [AddrWithId]
            (utxo, addrs) = initUtxoAndAddrs p

        -- apply the update to the account
        zoomOrCreateHdAccount
            assumeHdRootExists
            (newAccount accId utxo)
            accId
            (applyP p)

        -- create addresses (if they don't exist)
        forM_ addrs $ \(addressId, address) -> do
            let newAddress :: HdAddress
                newAddress = HD.initHdAddress addressId (InDb address)

            zoomOrCreateHdAddress
                assumeHdAccountExists -- we created it above
                newAddress
                addressId
                (return ())

        where
            newAccount :: HdAccountId -> Utxo -> HdAccount
            newAccount accId' utxo' = HD.initHdAccount accId' (firstCheckpoint utxo')

            firstCheckpoint :: Utxo -> Checkpoint
            firstCheckpoint utxo' = Checkpoint {
                  _checkpointUtxo        = InDb utxo'
                , _checkpointUtxoBalance = InDb $ Spec.balance utxo'
                , _checkpointExpected    = InDb Map.empty
                , _checkpointPending     = Pending . InDb $ Map.empty
                -- TODO(@uroboros/ryan) proper BlockMeta initialisation (as part of CBR-239: Support history tracking and queries)
                , _checkpointBlockMeta   = BlockMeta . InDb $ Map.empty
                }

{-------------------------------------------------------------------------------
  Wrap HD C(R)UD operations
-------------------------------------------------------------------------------}

createHdRoot :: HdRoot -> Update DB (Either HD.CreateHdRootError ())
createHdRoot hdRoot = runUpdate' . zoom dbHdWallets $
    HD.createHdRoot hdRoot

createHdAddress :: HdAddress -> Update DB (Either HD.CreateHdAddressError ())
createHdAddress hdAddress = runUpdate' . zoom dbHdWallets $
    HD.createHdAddress hdAddress

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
