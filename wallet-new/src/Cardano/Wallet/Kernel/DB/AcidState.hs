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
  , NewForeign(..)
  , CancelPending(..)
  , ApplyBlock(..)
  , SwitchToFork(..)
  , ApplyHistoricalBlock(..)
    -- ** Updates on HD wallets
    -- *** CREATE
  , CreateHdWallet(..)
  , RestoreHdWallet(..)
  , CreateHdAccount(..)
  , CreateHdAddress(..)
    -- *** UPDATE
  , UpdateHdWallet(..)
  , UpdateHdRootPassword(..)
  , UpdateHdAccountName(..)
    -- *** DELETE
  , DeleteHdRoot(..)
  , DeleteHdAccount(..)
    -- ** Software updates
  , AddUpdate(..)
  , RemoveNextUpdate(..)
  , GetNextUpdate(..)
    -- *** Testing
  , ObservableRollbackUseInTestsOnly(..)
    -- * Errors
  , NewPendingError(..)
  , NewForeignError(..)
  , RollbackDuringRestoration(..)
  ) where

import           Universum

import           Control.Lens ((.=))
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Except (MonadError, catchError)
import           Data.Acid (Query, Update, makeAcidic)
import qualified Data.Map.Strict as Map
import           Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Set as Set
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Test.QuickCheck (Arbitrary (..), oneof)

import           Pos.Chain.Txp (Utxo)
import           Pos.Core (FlatSlotId, SlotCount, SlotId, flattenSlotIdExplicit)
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Core.Txp (TxAux, TxId)
import           Pos.Core.Update (SoftwareVersion)

import           Cardano.Wallet.Kernel.DB.HdWallet
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Create as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Delete as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Update as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (InDb))
import           Cardano.Wallet.Kernel.DB.Spec
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending)
import qualified Cardano.Wallet.Kernel.DB.Spec.Update as Spec
import           Cardano.Wallet.Kernel.DB.Updates (Updates, noUpdates)
import qualified Cardano.Wallet.Kernel.DB.Updates as Updates
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.NodeStateAdaptor (SecurityParameter (..))
import           Cardano.Wallet.Kernel.PrefilterTx (AddrWithId,
                     PrefilteredBlock (..), emptyPrefilteredBlock)
import           Cardano.Wallet.Kernel.Util (markMissingMapEntries)

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
      -- | HD wallets with randomly assigned account and address indices
      _dbHdWallets :: !HdWallets

      -- | Available updates
    , _dbUpdates   :: !Updates
    }

makeLenses ''DB
deriveSafeCopy 1 'base ''DB

-- | Default DB
defDB :: DB
defDB = DB initHdWallets noUpdates

{-------------------------------------------------------------------------------
  Custom errors
-------------------------------------------------------------------------------}

-- | Errors thrown by 'newPending'
data NewPendingError =
    -- | Unknown account
    NewPendingUnknown UnknownHdAccount

    -- | Some inputs are not in the wallet utxo
  | NewPendingFailed Spec.NewPendingFailed

-- | Errors thrown by 'newForeign'
data NewForeignError =
    -- | Unknown account
    NewForeignUnknown UnknownHdAccount

    -- | Some inputs are not in the wallet utxo
  | NewForeignFailed Spec.NewForeignFailed

-- | We cannot roll back  when we don't have full historical data available
data RollbackDuringRestoration = RollbackDuringRestoration

deriveSafeCopy 1 'base ''NewPendingError
deriveSafeCopy 1 'base ''NewForeignError
deriveSafeCopy 1 'base ''RollbackDuringRestoration

{-------------------------------------------------------------------------------
  Wrap wallet spec
-------------------------------------------------------------------------------}


-- | Create a new pending transaction
--
--  NOTE: also creates all "our" addresses that appear in the transaction outputs,
--        by adding them to the HdAddresses they will be henceforth recognised as "ours"
newPending :: HdAccountId
           -> InDb TxAux
           -> [HdAddress] -- ^ "our" addresses that appear in the transaction outputs
           -> Update DB (Either NewPendingError ())
newPending accountId tx ourAddrs = runUpdateDiscardSnapshot . zoom dbHdWallets $ do
    zoomHdAccountId NewPendingUnknown accountId $
        zoomHdAccountCheckpoints $
            mapUpdateErrors NewPendingFailed $ Spec.newPending tx

    mapM_ ensureExistsHdAddress ourAddrs

newForeign :: HdAccountId
           -> InDb TxAux
           -> [HdAddress] -- ^ "our" addresses that appear in the transaction outputs
           -> Update DB (Either NewForeignError ())
newForeign accountId tx ourAddrs = runUpdateDiscardSnapshot . zoom dbHdWallets $ do
    zoomHdAccountId NewForeignUnknown accountId $
        zoomHdAccountCheckpoints $
            mapUpdateErrors NewForeignFailed $ Spec.newForeign tx

    mapM_ ensureExistsHdAddress ourAddrs

ensureExistsHdAddress :: HdAddress -> Update' HdWallets e ()
ensureExistsHdAddress newAddress = do
    zoomOrCreateHdAddress
        assumeHdAccountExists
        newAddress
        (newAddress ^. hdAddressId)
        (return ())

-- | Cancels the input transactions from the 'Checkpoints' of each of
-- the accounts cointained in the 'Cancelled' map.
--
-- The reason why this function doesn't take a 'HdRootId' as an argument
-- is because the submission layer doesn't have the notion of \"which HdWallet
-- is this transaction associated with?\", but it merely dispatch and cancels
-- transactions for all the wallets managed by this edge node.
cancelPending :: Map HdAccountId (InDb (Set TxId)) -> Update DB ()
cancelPending cancelled = void . runUpdate' . zoom dbHdWallets $
    forM_ (Map.toList cancelled) $ \(accountId, InDb txids) -> do
        -- Here we are deliberately swallowing the possible exception
        -- returned by the wrapped 'zoom' as the only reason why this update
        -- might fail is if, in the meantime, the target account was cancelled,
        -- in which case we do want the entire computation to abort, but simply
        -- skip cancelling the transactions for the account that has been removed.
        handleError (\(_e :: UnknownHdAccount) -> return ()) $
            zoomHdAccountId identity accountId $ do
              zoomHdAccountCheckpoints $
                modify $ Spec.cancelPending txids
    where
        handleError :: MonadError e m => (e -> m a) -> m a -> m a
        handleError = flip catchError

-- | Apply prefiltered block (indexed by HdAccountId) to the matching accounts.
--
-- The prefiltered block should be indexed by AccountId, with each prefiltered
-- block containing only inputs and outputs relevant to the account. Since
-- HdAccountId embeds HdRootId, it unambiguously places an Account in the
-- Wallet/Account hierarchy. The AccountIds here could therefor refer to an
-- Account in /any/ Wallet (not only sibling accounts in a single wallet).
--
-- Returns the set of transaction IDs that got removed from pending.
--
-- NOTE:
--
-- * Calls to 'applyBlock' must be sequentialized by the caller (although
--   concurrent calls to 'applyBlock' cannot interfere with each other,
--   'applyBlock' must be called in the right order.)
--
-- * Since a block may reference wallet accounts that do not exist yet locally,
--   we need to create such 'missing' accounts. (An Account might not exist
--   locally if it was created on another node instance of this wallet).
--
-- * For every address encountered in the block outputs, create an HdAddress if
--   it does not already exist.
--
-- * 'applyBlock' should be called /even if the map of prefiltered blocks is
--   empty/. This is important because for blocks that don't change we still
--   need to push a new checkpoint.
applyBlock :: SecurityParameter
           -> InDb SlotId
           -> Map HdAccountId PrefilteredBlock
           -> Update DB (Map HdAccountId (Set TxId))
applyBlock k (InDb slotId) blocks = runUpdateNoErrors $ zoom dbHdWallets $
    updateAccounts =<< mkUpdates <$> use hdWalletsAccounts
  where
    mkUpdates :: IxSet HdAccount -> [AccountUpdate Void (Set TxId)]
    mkUpdates existingAccounts =
          map mkUpdate
        . Map.toList
        . markMissingMapEntries (IxSet.toMap existingAccounts)
        $ blocks

    -- The account update
    --
    -- NOTE: When we initialize the kernel, we look at the genesis UTxO and
    -- create an initial balance for all accounts that we recognize as ours.
    -- This means that when we later discover a new account that is also ours,
    -- it cannot appear in the genesis UTxO, because if it did, we would already
    -- have seen it (the genesis UTxO is static, after all). Hence we use empty
    -- initial utxo for accounts discovered during 'applyBlock' (and
    -- 'switchToFork')
    mkUpdate :: (HdAccountId, Maybe PrefilteredBlock)
             -> AccountUpdate Void (Set TxId)
    mkUpdate (accId, mPB) = AccountUpdate {
          accountUpdateId    = accId
        , accountUpdateAddrs = pfbAddrs pb
        , accountUpdateNew   = AccountUpdateNewUpToDate Map.empty
        , accountUpdate      =
            matchHdAccountCheckpoints
              (state $ swap . Spec.applyBlock        k slotId pb)
              (state $ swap . Spec.applyBlockPartial k slotId pb)
        }
      where
        pb :: PrefilteredBlock
        pb = fromMaybe emptyPrefilteredBlock mPB


-- | Apply a block, as in 'applyBlock', but on the historical
-- checkpoints of an account rather than the current checkpoints.
--
-- Applying a historical block can cause an account's state to change
-- from 'HdAccountOutsideK' to 'HdAccountWithinK', or from
-- 'HdAccountWithinK' to 'HdAccountUpToDate'.
applyHistoricalBlock :: SecurityParameter
                     -> InDb SlotId
                     -> SlotCount
                     -> Map HdAccountId PrefilteredBlock
                     -> Update DB ()
applyHistoricalBlock k (InDb slotId) slotCount blocks =
  void $ runUpdateNoErrors $ zoom dbHdWallets $
    updateAccounts =<< mkUpdates <$> use hdWalletsAccounts
  where
    mkUpdates :: IxSet HdAccount -> [AccountUpdate Void (Set TxId)]
    mkUpdates existingAccounts =
          map mkUpdate
        . Map.toList
        . markMissingMapEntries (IxSet.toMap existingAccounts)
        $ blocks

    -- The account update
    mkUpdate :: (HdAccountId, Maybe PrefilteredBlock)
             -> AccountUpdate Void (Set TxId)
    mkUpdate (accId, mPB) = AccountUpdate {
          accountUpdateId    = accId
        , accountUpdateAddrs = pfbAddrs pb
        , accountUpdateNew   = AccountMustExist -- TODO: Not true
        , accountUpdate      = do
            -- Apply the block to the historical checkpoints
            txIds <- matchHdAccountState
              (return Set.empty)
              (do history <- use hdWithinKHistorical
                  let (history', txIds) = Spec.applyBlock k slotId pb history
                  hdWithinKHistorical .= history'
                  return txIds)
              (do history <- one <$> use hdOutsideKHistorical
                  let (history', txIds) = Spec.applyBlock k slotId pb history
                  hdOutsideKHistorical .= (history' ^. currentCheckpoint)
                  return txIds)
            -- Update the account state, if needed.
            withZoom $ \acc _zoomTo -> do
                case acc ^. hdAccountState of
                    HdAccountStateWithinK  st | isUpToDate st ->
                        hdAccountState .= HdAccountStateUpToDate (finishRestoration k st)
                    HdAccountStateOutsideK st | isUpToDate st ->
                        -- This should not happen normally, but it can happen if
                        -- slots are skipped in the blockchain.
                        hdAccountState .= HdAccountStateUpToDate (finishRestoration k st)
                    HdAccountStateOutsideK st | isWithinK  st ->
                        hdAccountState .= HdAccountStateWithinK (reachedWithinK st)
                    _ -> return ()
            -- Return the TxIds from applyBlock.
            return txIds
        }
      where
        pb :: PrefilteredBlock
        pb = fromMaybe emptyPrefilteredBlock mPB

        checkpointDistance :: Checkpoint -> PartialCheckpoint -> FlatSlotId
        checkpointDistance cp pcp = slot1 - slot0
          where
            slot1 = flattenSlotIdExplicit slotCount (pcp ^. cpSlotId)
            slot0 = flattenSlotIdExplicit slotCount ( cp ^. cpSlotId)

        isUpToDate :: HasHistoricalCheckpoints state => state -> Bool
        isUpToDate st = let cur = st ^. historicalCheckpoints . currentCheckpoint
                            tgt = st ^. partialCheckpoints    . oldestCheckpoint
                        in checkpointDistance cur tgt == 0

        isWithinK :: HdAccountOutsideK -> Bool
        isWithinK st =  let cur = st ^. hdOutsideKHistorical
                            tgt = st ^. hdOutsideKCurrent . oldestCheckpoint
                            SecurityParameter k0 = k
                        in checkpointDistance cur tgt <= fromIntegral k0

-- | Switch to a fork
--
-- See comments about prefiltering for 'applyBlock'.
--
-- Returns the set of transactions that were reintroduced into pending by the
-- rollback and the transactions that were removed from pending by the new
-- blocks.
--
-- TODO: We use a plain list here rather than 'OldestFirst' since the latter
-- does not have a 'SafeCopy' instance.
switchToFork :: SecurityParameter
             -> Int
             -> [(SlotId, Map HdAccountId PrefilteredBlock)]
             -> Update DB (Either RollbackDuringRestoration
                                  (Map HdAccountId (Pending, Set TxId)))
switchToFork k n blocks = runUpdateDiscardSnapshot $ zoom dbHdWallets $
    updateAccounts =<< mkUpdates <$> use hdWalletsAccounts
  where
    mkUpdates :: IxSet HdAccount
              -> [AccountUpdate RollbackDuringRestoration (Pending, Set TxId)]
    mkUpdates existingAccounts =
          map mkUpdate
        . Map.toList
        . redistribute
        . map (second (markMissingMapEntries (IxSet.toMap existingAccounts)))
        $ blocks

    mkUpdate :: (HdAccountId, [(SlotId, Maybe PrefilteredBlock)])
             -> AccountUpdate RollbackDuringRestoration (Pending, Set TxId)
    mkUpdate (accId, mPBs) = AccountUpdate {
          accountUpdateId    = accId
        , accountUpdateAddrs = concatMap (pfbAddrs . snd) pbs
        , accountUpdateNew   = AccountUpdateNewUpToDate Map.empty
        , accountUpdate      =
            matchHdAccountCheckpoints
              (state $ swap . Spec.switchToFork k n (OldestFirst pbs))
              (throwError RollbackDuringRestoration)
        }
      where
        pbs :: [(SlotId, PrefilteredBlock)]
        pbs = map (second (fromMaybe emptyPrefilteredBlock)) mPBs

    -- The natural result of prefiltering each block is a list of maps, but
    -- in order to apply them to each account, we want a map of lists
    --
    -- NOTE: We have to be careful to /first/ use 'markMissingMapEntries' to
    -- make sure that if, say, the first and third slot both contain a block for
    -- account A, but the second does not, we end up with an empty block
    -- inserted for slot 2.
    redistribute :: [(SlotId, Map HdAccountId (Maybe PrefilteredBlock))]
                 -> Map HdAccountId [(SlotId, Maybe PrefilteredBlock)]
    redistribute = Map.map (sortBy (comparing fst))
                 . Map.unionsWith (++)
                 . map (\(slotId, pbs) -> Map.map (\pb -> [(slotId, pb)]) pbs)

-- | Observable rollback, used for tests only
--
-- Returns the set of pending transactions that have become pending again,
-- for each account.
-- See 'switchToFork' for use in real code.
observableRollbackUseInTestsOnly :: Update DB (Either RollbackDuringRestoration
                                                      (Map HdAccountId Pending))
observableRollbackUseInTestsOnly = runUpdateDiscardSnapshot $
    zoomAll (dbHdWallets . hdWalletsAccounts) $
      matchHdAccountCheckpoints
        (state $ swap . Spec.observableRollbackUseInTestsOnly)
        (throwError RollbackDuringRestoration)

{-------------------------------------------------------------------------------
  Wallet creation
-------------------------------------------------------------------------------}

-- | Create an HdWallet with HdRoot
--
-- NOTE: We allow an initial set of accounts with associated addresses and
-- balances /ONLY/ for testing purpose. Normally this should be empty; see
-- 'createHdWallet'/'createWalletHdRnd' in "Cardano.Wallet.Kernel.Wallets".
createHdWallet :: HdRoot
               -> Map HdAccountId (Utxo,[AddrWithId])
               -> Update DB (Either HD.CreateHdRootError ())
createHdWallet newRoot utxoByAccount =
    runUpdateDiscardSnapshot . zoom dbHdWallets $ do
      HD.createHdRoot newRoot
      updateAccounts_ $ map mkUpdate (Map.toList utxoByAccount)
  where
    mkUpdate :: (HdAccountId, (Utxo, [AddrWithId]))
             -> AccountUpdate HD.CreateHdRootError ()
    mkUpdate (accId, (utxo, addrs)) = AccountUpdate {
          accountUpdateId    = accId
        , accountUpdateNew   = AccountUpdateNewUpToDate utxo
        , accountUpdateAddrs = addrs
        , accountUpdate      = return () -- just need to create it, no more
        }

-- | Begin restoration by creating an HdWallet with the given HdRoot,
-- starting from the 'HdAccountOutsideK' state.
restoreHdWallet :: HdRoot
                -> SlotId
                -> Map HdAccountId (Utxo, Utxo, [AddrWithId])
                -- ^ Current and genesis UTxO per account
                -> Update DB (Either HD.CreateHdRootError ())
restoreHdWallet newRoot slotId utxoByAccount =
    runUpdateDiscardSnapshot . zoom dbHdWallets $ do
      HD.createHdRoot newRoot
      updateAccounts_ $ map mkUpdate (Map.toList utxoByAccount)
  where
    mkUpdate :: (HdAccountId, (Utxo, Utxo, [AddrWithId]))
             -> AccountUpdate HD.CreateHdRootError ()
    mkUpdate (accId, (curUtxo, genUtxo, addrs)) = AccountUpdate {
          accountUpdateId    = accId
        , accountUpdateNew   = AccountUpdateNewOutsideK curUtxo genUtxo slotId
        , accountUpdateAddrs = addrs
        , accountUpdate      = return () -- Create it only
        }

{-------------------------------------------------------------------------------
  Internal: support for updating accounts
-------------------------------------------------------------------------------}

-- | All the information we need to update an account
--
-- See 'updateAccount' or 'updateAccounts'.
data AccountUpdate e a = AccountUpdate {
      -- | Account to update
      accountUpdateId    :: !HdAccountId

      -- | Information needed when we need to create the account from scratch
    , accountUpdateNew   :: !AccountUpdateNew

      -- | Set of addresses that should exist in this account
      --
      -- We make sure that these addresses exist even if the account itself
      -- does not need to be created.
      --
      -- NOTE: At the moment these addresses are created /after/ the
      -- update has been run.
    , accountUpdateAddrs :: ![AddrWithId]

      -- | The update to run
    , accountUpdate      :: !(Update' HdAccount e a)
    }

-- | Information we need to create new accounts
--
-- NOTE: Conceptually new accounts are always created in slot 0 of epoch 0,
-- so this is what we use for the 'SlotId' of the first account.
--
-- See 'AccountUpdate'.
data AccountUpdateNew =
     -- | Create new account which is up to date with the blockchain
     --
     -- Conceptually the first checkpoint of new account is always created in
     -- slot 0 of epoch 0, at the dawn of time, and this is what we use for
     -- the 'SlotId'. We nonetheless allow to specify a 'Utxo' for this
     -- checkpoint, since some accounts are assigned an initial balance
     -- in the Cardano genesis block.
     --
     -- NOTE: The /ONLY/ reason that we allow for an initial UTxO is here
     -- is that for testing purposes it is convenient to be able to create
     -- a wallet with an initial non-empty UTxO.
     AccountUpdateNewUpToDate !Utxo

    -- | Create a new account which will be in restoration state
    --
    -- We specify
    --
    -- * The current UTxO (obtained by filtering the full node's current UTxO)
    -- * The genesis UTxO (obtained by filtering 'genesisUtxo')
    -- * The slot ID of the tip at the time of restoration
  | AccountUpdateNewOutsideK !Utxo !Utxo !SlotId

    -- | Never create a new account
    --
    -- This is used when we know externally that the account must exist
  | AccountMustExist

-- | Brand new account (if one needs to be created)
accountUpdateCreate :: HdAccountId -> AccountUpdateNew -> HdAccount
accountUpdateCreate accId (AccountUpdateNewUpToDate utxo) =
    HD.initHdAccount accId initState
  where
    initState :: HdAccountState
    initState = HdAccountStateUpToDate HdAccountUpToDate {
          _hdUpToDateCheckpoints = one $ initCheckpoint utxo
        }
accountUpdateCreate accId (AccountUpdateNewOutsideK curUtxo genUtxo slotId) =
    HD.initHdAccount accId initState
  where
    initState :: HdAccountState
    initState = HdAccountStateOutsideK HdAccountOutsideK {
          _hdOutsideKCurrent    = one $ initPartialCheckpoint curUtxo slotId
        , _hdOutsideKHistorical = initCheckpoint genUtxo
        }
accountUpdateCreate _accId AccountMustExist =
    error "accountUpdateCreate: assertion failed"

updateAccount :: AccountUpdate e a -> Update' HdWallets e (HdAccountId, a)
updateAccount AccountUpdate{..} = do
    res <- zoomOrCreateHdAccount
             assumeHdRootExists
             (accountUpdateCreate accountUpdateId accountUpdateNew)
             accountUpdateId
             ((accountUpdateId, ) <$> accountUpdate)
    mapM_ createAddress accountUpdateAddrs
    return res
  where
    -- Create address (if needed)
    createAddress :: AddrWithId -> Update' HdWallets e ()
    createAddress (addressId, address) =
        zoomOrCreateHdAddress
            assumeHdAccountExists -- we just created it
            (HD.initHdAddress addressId address)
            addressId
            (return ())

updateAccounts :: [AccountUpdate e a] -> Update' HdWallets e (Map HdAccountId a)
updateAccounts = fmap Map.fromList . mapM updateAccount

updateAccounts_ :: [AccountUpdate e ()] -> Update' HdWallets e ()
updateAccounts_ = mapM_ updateAccount

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
  Software updates
-------------------------------------------------------------------------------}

addUpdate :: InDb SoftwareVersion -> Update DB ()
addUpdate upd = runUpdateNoErrors . zoom dbUpdates $
    Updates.addUpdate upd

removeNextUpdate :: Update DB ()
removeNextUpdate = runUpdateNoErrors . zoom dbUpdates $
    Updates.removeNextUpdate

getNextUpdate :: InDb SoftwareVersion -> Update DB (Maybe (InDb SoftwareVersion))
getNextUpdate current = runUpdateNoErrors . zoom dbUpdates $
    Updates.getNextUpdate current

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
    , 'newForeign
    , 'cancelPending
    , 'applyBlock
    , 'switchToFork
    , 'applyHistoricalBlock
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
    , 'restoreHdWallet
      -- Software updates
    , 'addUpdate
    , 'removeNextUpdate
    , 'getNextUpdate
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

instance Buildable NewForeignError where
    build (NewForeignUnknown unknownAccount) =
        bprint ("NewForeignUnknown " % build) unknownAccount
    build (NewForeignFailed npf) =
        bprint ("NewForeignFailed " % build) npf

{-------------------------------------------------------------------------------
  Arbitrary
-------------------------------------------------------------------------------}

instance Arbitrary NewPendingError where
    arbitrary = oneof [ NewPendingUnknown <$> arbitrary
                      , NewPendingFailed  <$> arbitrary
                      ]
