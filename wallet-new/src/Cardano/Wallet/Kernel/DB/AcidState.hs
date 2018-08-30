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
    -- ** Restoration
  , ApplyHistoricalBlock(..)
  , RestorationComplete(..)
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
  , DeleteAllHdAccounts(..)
    -- *** CLEARING
  , ClearDB (..)
    -- ** Software updates
  , AddUpdate(..)
  , RemoveNextUpdate(..)
  , GetNextUpdate(..)
    -- *** Testing
  , ObservableRollbackUseInTestsOnly(..)
    -- * Errors
  , NewPendingError(..)
  , NewForeignError(..)
  , SwitchToForkInternalError(..)
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import           Control.Monad.Except (MonadError, catchError)
import           Data.Acid (Query, Update, makeAcidic)
import qualified Data.Map.Strict as Map
import           Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Set as Set
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable

import           Pos.Chain.Block (HeaderHash)
import           Pos.Chain.Txp (TxAux, TxId, Utxo)
import           Pos.Chain.Update (SoftwareVersion)
import           Pos.Core.Chrono (OldestFirst (..))

import           Cardano.Wallet.Kernel.DB.BlockContext
import           Cardano.Wallet.Kernel.DB.HdWallet
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Create as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Delete as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Update as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (InDb), fromDb)
import           Cardano.Wallet.Kernel.DB.Spec
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending)
import qualified Cardano.Wallet.Kernel.DB.Spec.Update as Spec
import           Cardano.Wallet.Kernel.DB.Updates (Updates, noUpdates)
import qualified Cardano.Wallet.Kernel.DB.Updates as Updates
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import qualified Cardano.Wallet.Kernel.DB.Util.Zoomable as Z
import           Cardano.Wallet.Kernel.NodeStateAdaptor (SecurityParameter (..))
import           Cardano.Wallet.Kernel.PrefilterTx (AddrWithId,
                     PrefilteredBlock (..), emptyPrefilteredBlock)
import           Cardano.Wallet.Kernel.Util (markMissingMapEntries)
import           Cardano.Wallet.Kernel.Util.NonEmptyMap (NonEmptyMap)
import qualified Cardano.Wallet.Kernel.Util.NonEmptyMap as NEM
import           Test.QuickCheck (Arbitrary (..), oneof)

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

-- | Errors thrown by 'SwitchToFork'
data SwitchToForkInternalError =
    -- | We cannot roll back  when we don't have full historical data available
    RollbackDuringRestoration

    -- | Apply block failed
  | ApplyBlockFailedInternal Spec.ApplyBlockFailed

deriveSafeCopy 1 'base ''NewPendingError
deriveSafeCopy 1 'base ''NewForeignError
deriveSafeCopy 1 'base ''SwitchToForkInternalError

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

ensureExistsHdAddress :: HdAddress -> Update' e HdWallets ()
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
           -> BlockContext
           -> Maybe (Set HdAccountId)
           -> Map HdAccountId PrefilteredBlock
           -> Update DB (Either (NonEmptyMap HdAccountId Spec.ApplyBlockFailed) (Map HdAccountId (Set TxId)))
applyBlock k context restriction blocks = runUpdateDiscardSnapshot $ do
    -- Try to apply the block to each account in each wallet. If *any* have failed, throw the
    -- list of *all* failures; otherwise, run the update.
    let applyAll :: Update' Void DB (Map HdAccountId (Either Spec.ApplyBlockFailed (Set TxId)))
        applyAll = zoom dbHdWallets $ updateAccounts =<< mkUpdates <$> use hdWalletsAccounts
    (problems, successes) <- fmap (Map.mapEither id) (mapUpdateErrors absurd applyAll)
    maybe (return successes) throwError (NEM.fromMap problems)

  where
    acctFilter :: HdAccountId -> Bool
    acctFilter = maybe (const True) (flip elem) restriction

    mkUpdates :: IxSet HdAccount
              -> [AccountUpdate Void (Either Spec.ApplyBlockFailed (Set TxId))]
    mkUpdates existingAccounts =
          map mkUpdate
        . Map.toList
        . Map.filterWithKey (const . acctFilter)
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
             -> AccountUpdate Void (Either Spec.ApplyBlockFailed (Set TxId))
    mkUpdate (accId, mPB) = AccountUpdate {
          accountUpdateId    = accId
        , accountUpdateAddrs = pfbAddrs pb
        , accountUpdateNew   = AccountUpdateNewUpToDate Map.empty
        , accountUpdate      =
            matchHdAccountCheckpoints
              (tryUpdate' $ Spec.applyBlock k      pb)
              (tryUpdate' $ Spec.applyBlockPartial pb)
        }
      where
        pb :: PrefilteredBlock
        pb = fromMaybe (emptyPrefilteredBlock context) mPB

-- | Apply a block, as in 'applyBlock', but on the historical
-- checkpoints of an account rather than the current checkpoints.
applyHistoricalBlock :: SecurityParameter
                     -> BlockContext
                     -> Map HdAccountId PrefilteredBlock
                     -> Update DB (Either Spec.ApplyBlockFailed ())
applyHistoricalBlock k context blocks =
    runUpdateDiscardSnapshot $ zoom dbHdWallets $
      updateAccounts_ =<< mkUpdates <$> use hdWalletsAccounts
  where
    mkUpdates :: IxSet HdAccount -> [AccountUpdate Spec.ApplyBlockFailed ()]
    mkUpdates existingAccounts =
          map mkUpdate
        . Map.toList
        . markMissingMapEntries (IxSet.toMap existingAccounts)
        $ blocks

    -- The account update
    --
    -- /If/ we discover an account while we apply the block, that account
    -- must definitely be in incomplete state; its initial checkpoint will
    -- have an empty genesis UTxO and an empty current UTxO. (It can't have
    -- a non-empty genesis UTxO because if it did we would already have
    -- known about this account).
    mkUpdate :: (HdAccountId, Maybe PrefilteredBlock)
             -> AccountUpdate Spec.ApplyBlockFailed ()
    mkUpdate (accId, mPB) = AccountUpdate {
          accountUpdateId    = accId
        , accountUpdateAddrs = pfbAddrs pb
        , accountUpdateNew   = AccountUpdateNewIncomplete mempty mempty context
        , accountUpdate      = void $ Z.wrap $ \acc -> do
            -- Under normal circumstances we should not encounter an account
            -- that is in UpToDate state during restoration. There is only one
            -- circumstance under which this can happen: we start restoration,
            -- and now during a regular call to 'applyBlock' (not
            -- 'applyBlockHistorical') we discover a new account. Since
            -- 'applyBlock' is not aware that we are restoring, it will create a
            -- new account in up-to-date state. If this happens, we rectify the
            -- situation here.
            let updateHistory :: Checkpoints PartialCheckpoint
                              -> Checkpoints Checkpoint
                              -> HdAccount
                updateHistory current history' =
                  acc & hdAccountState .~ HdAccountStateIncomplete
                    (HdAccountIncomplete {
                        _hdIncompleteCurrent    = current
                      , _hdIncompleteHistorical = history'
                      })
            case acc ^. hdAccountState of
              HdAccountStateUpToDate (HdAccountUpToDate upToDate) -> do
                let current = liftCheckpoints (fmap (fromFullCheckpoint context)) upToDate
                    history = Checkpoints $ one $ initCheckpoint mempty
                second (updateHistory current) $
                  Z.unwrap (Spec.applyBlock k pb) history
              HdAccountStateIncomplete (HdAccountIncomplete current history) ->
                second (updateHistory current) $
                  Z.unwrap (Spec.applyBlock k pb) history
        }
      where
        pb :: PrefilteredBlock
        pb = fromMaybe (emptyPrefilteredBlock context) mPB

-- | Finish restoration of a wallet
--
-- When the restoration thread has completed its work, it should call this
-- function to mark all accounts as up to date
restorationComplete :: SecurityParameter -> HdRootId -> Update DB ()
restorationComplete k rootId = runUpdateNoErrors $ zoom dbHdWallets $
    zoomIxSet_ hdWalletsAccounts $
      modify $ \acc -> go acc (acc ^. hdAccountState)
  where
    go :: HdAccount -> HdAccountState -> HdAccount
    go acc (HdAccountStateUpToDate   _)  = acc
    go acc (HdAccountStateIncomplete st)
               | accRootId acc /= rootId = acc
               | otherwise               =
                   let st' = finishRestoration k st
                   in acc & hdAccountState .~ (HdAccountStateUpToDate st')

    accRootId :: HdAccount -> HdRootId
    accRootId = view (hdAccountId . hdAccountIdParent)

{-| Switch to a fork

    See comments about prefiltering for 'applyBlock'.

    Returns the set of transactions that were reintroduced into pending by the
    rollback and the transactions that were removed from pending by the new
    blocks.

    TODO: We use a plain list here rather than 'OldestFirst' since the latter
    does not have a 'SafeCopy' instance.

 genesis
 block         `oldest`                   tip before
  |               |                           |
  V               V                           V   tip after
  o---o---o---o---o---o---o---o---o---o---o---o       |
                   \                                  V
                    `-o---o---o---o---o---o---o---o---o
                      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                     |
                                  `blocks`
-}
switchToFork :: SecurityParameter
             -> Maybe HeaderHash
                -- ^ 'Nothing' for genesis block, or else @Just hh@ where @hh@
                -- is the hash of some main block.
             -> [(BlockContext, Map HdAccountId PrefilteredBlock)]
             -> Set HdRootId
             -> Update DB (Either [HdAccountId]
                                  (Map HdAccountId (Pending, Set TxId)))
switchToFork k oldest blocks toSkip =
    runUpdateDiscardSnapshot $ zoom dbHdWallets $
        updateAccountsWithErrors =<< mkUpdates <$> use hdWalletsAccounts
  where
    mkUpdates :: IxSet HdAccount
              -> [AccountUpdate SwitchToForkInternalError (Pending, Set TxId)]
    mkUpdates existingAccounts =
          map mkUpdate
        . Map.toList
        . (Map.filterWithKey $ \acctId _ -> not ((acctId ^. hdAccountIdParent) `Set.member` toSkip))
        . redistribute
        . map (second (markMissingMapEntries (IxSet.toMap existingAccounts)))
        $ blocks

    mkUpdate :: (HdAccountId, OldestFirst [] PrefilteredBlock)
             -> AccountUpdate SwitchToForkInternalError (Pending, Set TxId)
    mkUpdate (accId, pbs) = AccountUpdate {
          accountUpdateId    = accId
        , accountUpdateAddrs = concatMap pfbAddrs pbs
        , accountUpdateNew   = AccountUpdateNewUpToDate Map.empty
        , accountUpdate      =
            matchHdAccountCheckpoints
              (mapUpdateErrors ApplyBlockFailedInternal $ Spec.switchToFork k oldest pbs)
              (throwError RollbackDuringRestoration)
        }

    -- The natural result of prefiltering each block is a list of maps, but
    -- in order to apply them to each account, we want a map of lists
    --
    -- NOTE: We have to be careful to /first/ use 'markMissingMapEntries' to
    -- make sure that if, say, the first and third slot both contain a block for
    -- account A, but the second does not, we end up with an empty block
    -- inserted for slot 2.
    redistribute :: [(BlockContext, Map HdAccountId (Maybe PrefilteredBlock))]
                 -> Map HdAccountId (OldestFirst [] PrefilteredBlock)
    redistribute = Map.map mkPBS
                 . Map.unionsWith (++)
                 . map (\(slotId, pbs) -> Map.map (\pb -> [(slotId, pb)]) pbs)

    mkPBS :: [(BlockContext, Maybe PrefilteredBlock)]
          -> OldestFirst [] PrefilteredBlock
    mkPBS = OldestFirst
          . map (\(bc, mPB) -> fromMaybe (emptyPrefilteredBlock bc) mPB)
          . sortOn (view bcSlotId . fst)

-- | Observable rollback, used for tests only
--
-- Returns the set of pending transactions that have become pending again,
-- for each account.
-- See 'switchToFork' for use in real code.
observableRollbackUseInTestsOnly :: Update DB (Either SwitchToForkInternalError
                                                      (Map HdAccountId Pending))
observableRollbackUseInTestsOnly = runUpdateDiscardSnapshot $
    zoomIxSet (dbHdWallets . hdWalletsAccounts) $
      matchHdAccountCheckpoints
        (Spec.observableRollbackUseInTestsOnly)
        (throwError RollbackDuringRestoration)

{-------------------------------------------------------------------------------
  Wallet creation
-------------------------------------------------------------------------------}

-- | Create an HdWallet with HdRoot
--
-- NOTE: We allow an initial set of accounts with associated addresses and
-- balances /ONLY/ for testing purpose. Normally this should be empty; see
-- 'createHdWallet'/'createWalletHdRnd' in "Cardano.Wallet.Kernel.Wallets".
--
-- INVARIANT: Creating a new wallet always come with a fresh HdAccount and
-- a fresh 'HdAddress' attached to it, so we have to pass these two extra
-- piece of into to the update function. We do @not@ build these inside the
-- update function because derivation requires an 'EncryptedSecretKey' and
-- definitely we do not want it to show up in our acid-state logs.
--
createHdWallet :: HdRoot
               -> HdAccountId
               -- ^ The default HdAccountId to go with this HdRoot. This
               -- function will take responsibility of creating the associated
               -- 'HdAccount'.
               -> HdAddress
               -- ^ The default HdAddress to go with this HdRoot.
               -> Map HdAccountId (Utxo,[AddrWithId])
               -> Update DB (Either HD.CreateHdRootError ())
createHdWallet newRoot defaultHdAccountId defaultHdAddress utxoByAccount =
    runUpdateDiscardSnapshot . zoom dbHdWallets $ do
      HD.createHdRoot newRoot
      updateAccounts_ $ map mkUpdate (Map.toList (insertDefault utxoByAccount))
  where
    mkUpdate :: (HdAccountId, (Utxo, [AddrWithId]))
             -> AccountUpdate HD.CreateHdRootError ()
    mkUpdate (accId, (utxo, addrs)) = AccountUpdate {
          accountUpdateId    = accId
        , accountUpdateNew   = AccountUpdateNewUpToDate utxo
        , accountUpdateAddrs = addrs
        , accountUpdate      = return () -- just need to create it, no more
        }

    insertDefault :: Map HdAccountId (Utxo, [AddrWithId])
                  -> Map HdAccountId (Utxo, [AddrWithId])
    insertDefault m =
        let defaultAddr = ( defaultHdAddress ^. hdAddressId
                          , defaultHdAddress ^. hdAddressAddress . fromDb
                          )
        in case Map.lookup defaultHdAccountId m of
               Just (utxo, addrs) ->
                   Map.insert defaultHdAccountId (utxo, defaultAddr : addrs) m
               Nothing ->
                   Map.insert defaultHdAccountId (mempty, [defaultAddr]) m

-- | Begin restoration by creating an HdWallet with the given HdRoot,
-- starting from the 'HdAccountOutsideK' state.
--
-- INVARIANT: Creating a new wallet always come with a fresh HdAccount and
-- a fresh 'HdAddress' attached to it, so we have to pass these two extra
-- piece of into to the update function. We do @not@ build these inside the
-- update function because derivation requires an 'EncryptedSecretKey' and
-- definitely we do not want it to show up in our acid-state logs.
--
restoreHdWallet :: HdRoot
                -> HdAccountId
                -- ^ The default HdAccountId to go with this HdRoot. This
                -- function will take responsibility of creating the associated
                -- 'HdAccount'.
                -> HdAddress
                -- ^ The default HdAddress to go with this HdRoot
                -> BlockContext
                -- ^ The initial block context for restorations
                -> Map HdAccountId (Utxo, Utxo, [AddrWithId])
                -- ^ Current and genesis UTxO per account
                -> Update DB (Either HD.CreateHdRootError ())
restoreHdWallet newRoot defaultHdAccountId defaultHdAddress ctx utxoByAccount =
    runUpdateDiscardSnapshot $ do
      zoom dbHdWallets $ do
          recreateHdRoot newRoot
          updateAccounts_ $ map mkUpdate (Map.toList (insertDefault utxoByAccount))
  where
    mkUpdate :: (HdAccountId, (Utxo, Utxo, [AddrWithId]))
             -> AccountUpdate HD.CreateHdRootError ()
    mkUpdate (accId, (curUtxo, genUtxo, addrs)) = AccountUpdate {
          accountUpdateId    = accId
        , accountUpdateNew   = AccountUpdateNewIncomplete curUtxo genUtxo ctx
        , accountUpdateAddrs = addrs
        , accountUpdate      = return () -- Create it only
        }

    insertDefault :: Map HdAccountId (Utxo, Utxo, [AddrWithId])
                  -> Map HdAccountId (Utxo, Utxo, [AddrWithId])
    insertDefault m =
        let defaultAddr = ( defaultHdAddress ^. hdAddressId
                          , defaultHdAddress ^. hdAddressAddress . fromDb
                          )
        in case Map.lookup defaultHdAccountId m of
               Just (utxo, utxo', addrs) ->
                   Map.insert defaultHdAccountId (utxo, utxo', defaultAddr : addrs) m
               Nothing ->
                   Map.insert defaultHdAccountId (mempty, mempty, [defaultAddr]) m

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
    , accountUpdate      :: !(Update' e HdAccount a)
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
    -- * The block context to use for the first partial checkpoint.
  | AccountUpdateNewIncomplete !Utxo !Utxo !BlockContext

-- | Brand new account (if one needs to be created)
accountUpdateCreate :: HdAccountId -> AccountUpdateNew -> HdAccount
accountUpdateCreate accId (AccountUpdateNewUpToDate utxo) =
    HD.initHdAccount accId initState
  where
    initState :: HdAccountState
    initState = HdAccountStateUpToDate HdAccountUpToDate {
          _hdUpToDateCheckpoints = Checkpoints $ one $ initCheckpoint utxo
        }
accountUpdateCreate accId (AccountUpdateNewIncomplete curUtxo genUtxo ctx) =
    HD.initHdAccount accId initState
  where
    initState :: HdAccountState
    initState = HdAccountStateIncomplete HdAccountIncomplete {
          _hdIncompleteCurrent    = Checkpoints $ one $ initPartialCheckpoint ctx curUtxo
        , _hdIncompleteHistorical = Checkpoints $ one $ initCheckpoint        genUtxo
        }

updateAccount :: AccountUpdate e a -> Update' e HdWallets (HdAccountId, a)
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
    createAddress :: AddrWithId -> Update' e HdWallets ()
    createAddress (addressId, address) =
        zoomOrCreateHdAddress
            assumeHdAccountExists -- we just created it
            (HD.initHdAddress addressId address)
            addressId
            (return ())

updateAccounts :: [AccountUpdate e a] -> Update' e HdWallets (Map HdAccountId a)
updateAccounts = fmap Map.fromList . mapM updateAccount

updateAccounts_ :: [AccountUpdate e ()] -> Update' e HdWallets ()
updateAccounts_ = mapM_ updateAccount

-- | Run each update, collecting all errors. Then, if there were any errors for any
-- accounts, throw them all at once without updating the state.
-- Otherwise, update the state as usual.
updateAccountsWithErrors :: [AccountUpdate e a]
                         -> Update' [HdAccountId] HdWallets (Map HdAccountId a)
updateAccountsWithErrors updates = do
    results <- forM updates $ \upd ->
        tryUpdate (mapUpdateErrors (const $ accountUpdateId upd) (updateAccount upd))
    let (errors, successes) = partitionEithers results
    case errors of
      []   -> return (Map.fromList successes)
      errs -> throwError errs

{-------------------------------------------------------------------------------
  Wrap HD C(R)UD operations
-------------------------------------------------------------------------------}

createHdAccount :: HdAccount -> Update DB (Either HD.CreateHdAccountError (DB, ()))
createHdAccount hdAccount = runUpdate' . zoom dbHdWallets $
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

recreateHdRoot :: HdRoot -> Update' HD.CreateHdRootError HdWallets ()
recreateHdRoot hdRoot = do
    -- Delete the wallet, if it exists.
    discardUpdateErrors (HD.deleteHdRoot (hdRoot ^. hdRootId))
    -- Now create it again.
    HD.createHdRoot hdRoot

deleteAllHdAccounts :: HdRootId -> Update DB (Either UnknownHdRoot ())
deleteAllHdAccounts rootId = runUpdateDiscardSnapshot . zoom dbHdWallets $
    HD.deleteAllHdAccounts rootId

{-------------------------------------------------------------------------------
  DB cleaning
-------------------------------------------------------------------------------}

-- | caution: Clears everything in the DB.
clearDB :: Update DB ()
clearDB = do
  runUpdateNoErrors . zoom dbHdWallets $ put initHdWallets
  runUpdateNoErrors . zoom dbUpdates $ Updates.clearUpdates

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
    , 'restorationComplete
      -- Updates on HD wallets
    , 'createHdAddress
    , 'createHdAccount
    , 'createHdWallet
    , 'updateHdWallet
    , 'updateHdRootPassword
    , 'updateHdAccountName
    , 'deleteHdRoot
    , 'deleteHdAccount
    , 'deleteAllHdAccounts
    , 'clearDB
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
