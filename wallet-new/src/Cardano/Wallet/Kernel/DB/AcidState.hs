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
  , CancelPending(..)
  , ApplyBlock(..)
  , SwitchToFork(..)
    -- ** Updates on HD wallets
    -- *** CREATE
  , CreateHdWallet(..)
  , CreateHdAccount(..)
  , CreateHdAddress(..)
    -- *** UPDATE
  , UpdateHdWallet(..)
  , UpdateHdRootPassword(..)
  , UpdateHdAccountName(..)
    -- *** DELETE
  , DeleteHdRoot(..)
  , DeleteHdAccount(..)
    -- *** Testing
  , ObservableRollbackUseInTestsOnly(..)
    -- * Errors
  , NewPendingError(..)
  , RollbackDuringRestoration(..)
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import           Control.Monad.Except (MonadError, catchError)
import           Data.Acid (Query, Update, makeAcidic)
import qualified Data.Map.Merge.Strict as Map.Merge
import qualified Data.Map.Strict as Map
import           Data.SafeCopy (base, deriveSafeCopy)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Test.QuickCheck (Arbitrary (..), oneof)

import           Pos.Chain.Txp (Utxo)
import           Pos.Core.Chrono (OldestFirst (..))
import qualified Pos.Core.Txp as Txp

import           Cardano.Wallet.Kernel.ChainState
import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.HdWallet
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Create as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Delete as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Update as HD
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import qualified Cardano.Wallet.Kernel.DB.Spec.Update as Spec
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.PrefilterTx (AddrWithId,
                     PrefilteredBlock (..), emptyPrefilteredBlock)
import qualified Cardano.Wallet.Kernel.Util.Core as Core

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
  Custom errors
-------------------------------------------------------------------------------}

-- | Errors thrown by 'newPending'
data NewPendingError =
    -- | Unknown account
    NewPendingUnknown UnknownHdAccount

    -- | Some inputs are not in the wallet utxo
  | NewPendingFailed Spec.NewPendingFailed


-- | We cannot roll back  when we don't have full historical data available
data RollbackDuringRestoration = RollbackDuringRestoration

deriveSafeCopy 1 'base ''NewPendingError
deriveSafeCopy 1 'base ''RollbackDuringRestoration

{-------------------------------------------------------------------------------
  Wrap wallet spec
-------------------------------------------------------------------------------}

newPending :: HdAccountId
           -> InDb Txp.TxAux
           -> Update DB (Either NewPendingError ())
newPending accountId tx = runUpdateDiscardSnapshot . zoom dbHdWallets $
    zoomHdAccountId NewPendingUnknown accountId $
    zoomHdAccountCheckpoints $
      mapUpdateErrors NewPendingFailed $ Spec.newPending tx

-- | Cancels the input transactions from the 'Checkpoints' of each of
-- the accounts cointained in the 'Cancelled' map.
--
-- The reason why this function doesn't take a 'HdRootId' as an argument
-- is because the submission layer doesn't have the notion of \"which HdWallet
-- is this transaction associated with?\", but it merely dispatch and cancels
-- transactions for all the wallets managed by this edge node.
cancelPending :: Map HdAccountId (InDb (Set Txp.TxId)) -> Update DB ()
cancelPending cancelled = void . runUpdate' . zoom dbHdWallets $
    forM_ (Map.toList cancelled) $ \(accountId, InDb txids) ->
        -- Here we are deliberately swallowing the possible exception
        -- returned by the wrapped 'zoom' as the only reason why this update
        -- might fail is if, in the meantime, the target account was cancelled,
        -- in which case we do want the entire computation to abort, but simply
        -- skip cancelling the transactions for the account that has been removed.
        handleError (\(_e :: UnknownHdAccount) -> return ()) $
            zoomHdAccountId identity accountId $ do
              zoomHdAccountCheckpoints $
                modify' $ Spec.cancelPending txids
    where
        handleError :: MonadError e m => (e -> m a) -> m a -> m a
        handleError = flip catchError

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
applyBlock :: Map HdAccountId PrefilteredBlock -> Update DB ()
applyBlock blocksByAccount = runUpdateNoErrors $ zoom dbHdWallets $ do
    blocksByAccount' <- fillInEmptyBlock blocksByAccount
    createPrefiltered
        initUtxoAndAddrs
        updateAccount
        blocksByAccount'
  where
    updateAccount :: PrefilteredBlock -> Update' HdAccount Void ()
    updateAccount pb =
        matchHdAccountCheckpoints
          (modify $ Spec.applyBlock        pb)
          (modify $ Spec.applyBlockPartial pb)

    -- Initial UTxO and addresses for a new account
    --
    -- NOTE: When we initialize the kernel, we look at the genesis UTxO and create
    -- an initial balance for all accounts that we recognize as ours. This means
    -- that when we later discover a new account that is also ours, it cannot appear
    -- in the genesis UTxO, because if it did, we would already have seen it (the
    -- genesis UTxO is static, after all). Hence we use empty initial utxo for
    -- accounts discovered during 'applyBlock' (and 'switchToFork')
    --
    -- The Addrs need to be created during account initialisation and so we pass
    -- them here.
    initUtxoAndAddrs :: PrefilteredBlock -> (Utxo, [AddrWithId])
    initUtxoAndAddrs pb = (Map.empty, pfbAddrs pb)

-- | Switch to a fork
--
-- See comments about prefiltering for 'applyBlock'.
--
-- TODO: We use a plain list here rather than 'OldestFirst' since the latter
-- does not have a 'SafeCopy' instance.
switchToFork :: Int
             -> [Map HdAccountId PrefilteredBlock]
             -> Update DB (Either RollbackDuringRestoration ())
switchToFork n blocks = runUpdateDiscardSnapshot $ zoom dbHdWallets $ do
    blocks' <- mapM fillInEmptyBlock blocks
    createPrefiltered
        initUtxoAndAddrs
        updateAccount
        (distribute blocks')
  where
    updateAccount :: [PrefilteredBlock]
                  -> Update' HdAccount RollbackDuringRestoration ()
    updateAccount pbs =
        matchHdAccountCheckpoints
          (modify $ Spec.switchToFork n (OldestFirst pbs))
          (throwError RollbackDuringRestoration)

    -- The natural result of prefiltering each block is a list of maps, but
    -- in order to apply them to each account, we want a map of lists
    --
    -- NOTE: We have to be careful to /first/ use 'fillInEmptyBlock' to make
    -- sure that if, say, the first and third slot both contain a block for
    -- account A, but the second does not, we end up with an empty block
    -- inserted for slot 2.
    distribute :: [Map HdAccountId PrefilteredBlock]
               -> Map HdAccountId [PrefilteredBlock]
    distribute = Map.unionsWith (++) . map (Map.map (:[]))

    -- See comments in 'applyBlock'
    initUtxoAndAddrs :: [PrefilteredBlock] -> (Utxo, [AddrWithId])
    initUtxoAndAddrs pbs = (Map.empty, concatMap pfbAddrs pbs)

-- | Observable rollback, used for tests only
--
-- See 'switchToFork' for use in real code.
observableRollbackUseInTestsOnly :: Update DB (Either RollbackDuringRestoration ())
observableRollbackUseInTestsOnly = runUpdateDiscardSnapshot $
    zoomAll (dbHdWallets . hdWalletsAccounts) $
      matchHdAccountCheckpoints
        (modify' Spec.observableRollbackUseInTestsOnly)
        (throwError RollbackDuringRestoration)

{-------------------------------------------------------------------------------
  Wallet creation
-------------------------------------------------------------------------------}

-- | Create an HdWallet with HdRoot, possibly with HdAccounts and HdAddresses.
--
--  Given prefiltered utxo's, by account, create an HdAccount for each account,
--  along with HdAddresses for all utxo outputs.
--
-- NOTE: since the genesis Utxo does not come into being through regular transactions,
--       there is no block metadata to record when we create a wallet
createHdWallet :: HdRoot
               -> Map HdAccountId (Utxo,[AddrWithId])
               -> Update DB (Either HD.CreateHdRootError ())
createHdWallet newRoot utxoByAccount =
    runUpdateDiscardSnapshot . zoom dbHdWallets $ do
          HD.createHdRoot newRoot
          createPrefiltered
              identity
              (\_ -> return ()) -- we just want to create the accounts
              utxoByAccount

{-------------------------------------------------------------------------------
  Internal auxiliary: apply a function to a prefiltered block/utxo
-------------------------------------------------------------------------------}

-- | Given a map from account IDs, add default values for all accounts in
-- the wallet that aren't given a value in the map
fillInDefaults :: forall p e.
                  (HdAccount -> p)   -- ^ Default value
               -> Map HdAccountId p  -- ^ Map with values per account
               -> Update' HdWallets e (Map HdAccountId p)
fillInDefaults def accs =
    aux . IxSet.toMap <$> use hdWalletsAccounts
  where
    aux :: Map HdAccountId HdAccount -> Map HdAccountId p
    aux = Map.Merge.merge newAccount needsDefault valueForExistingAcc accs

    newAccount :: Map.Merge.SimpleWhenMissing HdAccountId p p
    newAccount = Map.Merge.mapMaybeMissing $ \_accId p -> Just p

    needsDefault :: Map.Merge.SimpleWhenMissing HdAccountId HdAccount p
    needsDefault = Map.Merge.mapMaybeMissing $ \_accId acc -> Just (def acc)

    valueForExistingAcc :: Map.Merge.SimpleWhenMatched HdAccountId p HdAccount p
    valueForExistingAcc = Map.Merge.zipWithMatched $ \_accId p _acc -> p

-- | Specialization of 'fillInDefaults' for prefiltered blocks
fillInEmptyBlock :: Map HdAccountId PrefilteredBlock
                 -> Update' HdWallets e (Map HdAccountId PrefilteredBlock)
fillInEmptyBlock = fillInDefaults $ \_ -> emptyPrefilteredBlock dummyChainBrief

-- | For each of the specified accounts, create them if they do not exist,
-- and apply the specified function.
--
-- NOTE: Any accounts that aren't in the map are simply skilled. See
-- 'fillInDefaults'.
createPrefiltered :: forall p e.
                     (p -> (Utxo,[AddrWithId]))
                      -- ^ Initial UTxO (when we are creating the account),
                      -- as well as set of addresses the account should have
                  -> (p -> Update' HdAccount e ())
                      -- ^ Function to apply to the account
                  -> Map HdAccountId p
                  -> Update' HdWallets e ()
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
                , _checkpointUtxoBalance = InDb $ Core.utxoBalance utxo'
                , _checkpointPending     = Pending.empty
                -- Since this is the first checkpoint before we have applied
                -- any blocks, the block metadata is empty
                , _checkpointBlockMeta   = emptyBlockMeta
                , _checkpointChainBrief  = dummyChainBrief
                }

{-------------------------------------------------------------------------------
  Wrap HD C(R)UD operations
-------------------------------------------------------------------------------}

createHdRoot :: HdRoot -> Update DB (Either HD.CreateHdRootError ())
createHdRoot hdRoot = runUpdateDiscardSnapshot . zoom dbHdWallets $
    HD.createHdRoot hdRoot

createHdAccount :: HdAccount -> Update DB (Either HD.CreateHdAccountError ())
createHdAccount hdAccount = runUpdateDiscardSnapshot . zoom dbHdWallets $
    HD.createHdAccount hdAccount

createHdAddress :: HdAddress -> Update DB (Either HD.CreateHdAddressError ())
createHdAddress hdAddress = runUpdateDiscardSnapshot . zoom dbHdWallets $
    HD.createHdAddress hdAddress

updateHdWallet :: HdRootId
               -> AssuranceLevel
               -> WalletName
               -> Update DB (Either UnknownHdRoot (DB, HdRoot))
updateHdWallet rootId assurance name = do
    runUpdate' . zoom dbHdWallets $ do
        HD.updateHdRoot rootId assurance name

updateHdRootPassword :: HdRootId
                     -> HasSpendingPassword
                     -> Update DB (Either UnknownHdRoot (DB, HdRoot))
updateHdRootPassword rootId hasSpendingPassword = do
    runUpdate' . zoom dbHdWallets $
        HD.updateHdRootPassword rootId hasSpendingPassword

updateHdAccountName :: HdAccountId
                    -> AccountName
                    -> Update DB (Either UnknownHdAccount (DB, HdAccount))
updateHdAccountName accId name = do
    runUpdate' . zoom dbHdWallets $ HD.updateHdAccountName accId name

deleteHdRoot :: HdRootId -> Update DB (Either UnknownHdRoot ())
deleteHdRoot rootId = runUpdateDiscardSnapshot . zoom dbHdWallets $
    HD.deleteHdRoot rootId

-- | Deletes the 'HdAccount' identified by the input 'HdAccountId' together
-- with all the linked addresses.
deleteHdAccount :: HdAccountId -> Update DB (Either UnknownHdAccount ())
deleteHdAccount accId = runUpdateDiscardSnapshot . zoom dbHdWallets $
    HD.deleteHdAccount accId

{-------------------------------------------------------------------------------
  Acid-state magic
-------------------------------------------------------------------------------}

-- | Reads the full DB. This is and @must@ be the only 'Query' ever exported
-- by this module. All the getters exposed for the kernel @must@ take a 'DB'
-- as input and be completely pure.
snapshot :: Query DB DB
snapshot = ask

makeAcidic ''DB [
      -- Database snapshot
      'snapshot
      -- Updates on the "spec state"
    , 'newPending
    , 'cancelPending
    , 'applyBlock
    , 'switchToFork
      -- Updates on HD wallets
    , 'createHdRoot
    , 'createHdAddress
    , 'createHdAccount
    , 'createHdWallet
    , 'updateHdWallet
    , 'updateHdRootPassword
    , 'updateHdAccountName
    , 'deleteHdRoot
    , 'deleteHdAccount
      -- Testing
    , 'observableRollbackUseInTestsOnly
    ]

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable NewPendingError where
    build (NewPendingUnknown unknownAccount) =
        bprint ("NewPendingUnknown " % build) unknownAccount
    build (NewPendingFailed npf) =
        bprint ("NewPendingFailed " % build) npf

{-------------------------------------------------------------------------------
  Arbitrary
-------------------------------------------------------------------------------}

instance Arbitrary NewPendingError where
    arbitrary = oneof [ NewPendingUnknown <$> arbitrary
                      , NewPendingFailed  <$> arbitrary
                      ]
