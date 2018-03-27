{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Module which defines internal structure of `acid-state` wallet database.
module Pos.Wallet.Web.State.Storage
       (
         WalletStorage (..)
       , HasWalletStorage (..)
       , WalletInfo (..)
       , AccountInfo (..)
       , AddressInfo (..)
       , AddressLookupMode (..)
       , CAddresses
       , CustomAddressType (..)
       , CurrentAndRemoved (..)
       , WalletBalances
       , WalBalancesAndUtxo
       , WalletTip (..)
       , PtxMetaUpdate (..)
       , Query
       , Update
       , getWalletStorage
       , flushWalletStorage
       , getProfile
       , setProfile
       , doesAccountExist
       , getAccountIds
       , getAccountMeta
       , getAccountAddrMaps
       , getWalletMeta
       , getWalletMetaIncludeUnready
       , getWalletPassLU
       , getWalletSyncTip
       , getWalletAddresses
       , getWalletInfos
       , getWalletInfo
       , getAccountWAddresses
       , getWAddresses
       , doesWAddressExist
       , getTxMeta
       , getNextUpdate
       , getHistoryCache
       , getCustomAddresses
       , getCustomAddress
       , getPendingTxs
       , getWalletPendingTxs
       , getPendingTx
       , addCustomAddress
       , removeCustomAddress
       , createAccount
       , createWallet
       , addWAddress
       , setAccountMeta
       , setWalletMeta
       , setWalletReady
       , setWalletPassLU
       , setWalletSyncTip
       , getWalletTxHistory
       , getWalletUtxo
       , getWalletBalancesAndUtxo
       , updateWalletBalancesAndUtxo
       , setWalletUtxo
       , addOnlyNewTxMeta
       , addOnlyNewTxMetas
       , removeWallet
       , removeWalletTxMetas
       , removeTxMetas
       , removeHistoryCache
       , removeAccount
       , removeWAddress
       , addUpdate
       , removeNextUpdate
       , testReset
       , updateHistoryCache
       , insertIntoHistoryCache
       , removeFromHistoryCache
       , setPtxCondition
       , casPtxCondition
       , removeOnlyCreatingPtx
       , ptxUpdateMeta
       , addOnlyNewPendingTx
       , resetFailedPtxs
       , cancelApplyingPtxs
       , cancelSpecificApplyingPtx
       ) where

import           Universum

import           Control.Lens (at, has, ix, makeClassy, makeLenses, non', to, toListOf, traversed,
                               (%=), (+=), (.=), (<<.=), (?=), _Empty, _Just, _head)
import           Control.Monad.State.Class (get, put)
import qualified Control.Monad.State.Lazy as LS
import           Data.Default (Default, def)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import           Data.SafeCopy (Migrate (..), base, deriveSafeCopySimple, extension)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Serokell.Util (zoom')

import           Pos.Client.Txp.History (TxHistoryEntry, txHistoryListToMap)
import           Pos.Core (HeaderHash, SlotId, Timestamp)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Txp (TxAux, TxId)
import           Pos.SafeCopy ()
import           Pos.Txp (AddrCoinMap, Utxo, UtxoModifier, applyUtxoModToAddrCoinMap,
                          utxoToAddressCoinMap)
import           Pos.Util.BackupPhrase (BackupPhrase)
import qualified Pos.Util.Modifier as MM
import           Pos.Wallet.Web.ClientTypes (AccountId, Addr, CAccountMeta, CCoin, CHash, CId,
                                             CProfile, CTxId, CTxMeta, CUpdateInfo,
                                             CWAddressMeta (..), CWalletAssurance, CWalletMeta,
                                             PassPhraseLU, Wal, addrMetaToAccount, aiWId)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition, PtxSubmitTiming (..),
                                               ptxCond, ptxSubmitTiming, _PtxCreating)
import           Pos.Wallet.Web.Pending.Util (cancelApplyingPtx, incPtxSubmitTimingPure,
                                              mkPtxSubmitTiming, ptxMarkAcknowledgedPure,
                                              resetFailedPtx)

-- | Type alias for indices which are used to maintain order
-- in which addresses were created.
type AddressSortingKey = Int

-- | Information about existing wallet address.
data AddressInfo = AddressInfo
    { -- | Address data, including derivation indices and address itself.
      adiCWAddressMeta :: !CWAddressMeta
      -- | An index which determines which position this address has in
      -- list of all account's addresses.
    , adiSortingKey    :: !AddressSortingKey
    } deriving (Eq)

-- | Alias for set of 'AddressInfo' indexed by address IDs (@CId Addr@)
type CAddresses = HashMap (CId Addr) AddressInfo

-- | Information about existing wallet account.
data AccountInfo = AccountInfo
    { -- | Account metadata (e. g. account name).
      _aiMeta             :: !CAccountMeta
      -- | Addresses which currently belong to this account.
    , _aiAddresses        :: !CAddresses
      -- | Addresses which used to belong to this account, but has been
      -- deleted. We don't actually /delete/ such addresses.
      -- This is done historically in order to preserve valid transaction history
      -- derivation if some address has been deleted.
    , _aiRemovedAddresses :: !CAddresses
      -- | Sorting index of last created address.
    , _aiUnusedKey        :: !AddressSortingKey
    } deriving (Eq)

makeLenses ''AccountInfo

-- | Datatype which stores information about whether
-- or not a wallet has been synced with blockchain and if
-- it has, until which block.
-- See "Pos.Wallet.Web.Tracking.Sync" for syncing functionality.
data WalletTip
    = NotSynced
    | SyncedWith !HeaderHash
    deriving (Eq)

data WalletInfo = WalletInfo
    { -- | Wallet metadata (name, assurance, etc.)
      _wiMeta         :: !CWalletMeta
      -- | Last time when wallet passphrase was updated.
    , _wiPassphraseLU :: !PassPhraseLU
      -- | Wallet creation time.
    , _wiCreationTime :: !POSIXTime
      -- | Header hash until which wallet has been synced
      -- (or @NotSynced@ if wallet hasn't beed synced at all)
    , _wiSyncTip      :: !WalletTip
      -- | Pending states for all created transactions (information related
      -- to transaction resubmission).
      -- See "Pos.Wallet.Web.Pending" for resubmission functionality.
    , _wsPendingTxs   :: !(HashMap TxId PendingTx)
      -- | Wallets that are being synced are marked as not ready, and
      -- are excluded from api endpoints. This info should not be leaked
      -- into a client facing data structure (for example 'CWalletMeta')
    , _wiIsReady      :: !Bool
    } deriving (Eq)

makeLenses ''WalletInfo

-- | Maps addresses to their first occurrence in the blockchain.
type CustomAddresses = HashMap (CId Addr) HeaderHash

-- | Alias for 'Pos.Txp.AddrCoinMap' storing balances for wallet's addresses.
type WalletBalances = AddrCoinMap
type WalBalancesAndUtxo = (WalletBalances, Utxo)

-- | Datatype which defines full structure of acid-state.
data WalletStorage = WalletStorage
    { -- | Stores information about all existing wallets.
      _wsWalletInfos     :: !(HashMap (CId Wal) WalletInfo)
      -- | Stores information about all existing accounts.
    , _wsAccountInfos    :: !(HashMap AccountId AccountInfo)
      -- | Non-wallet-specific client metadata.
    , _wsProfile         :: !CProfile
      -- | List of descriptions of approved and downloaded updates, waiting
      -- for user action. See "Pos.Update.Download" and @updateNotifier@ in
      -- "Pos.Wallet.Web.Sockets.Notifier" for more info of how updates work.
    , _wsReadyUpdates    :: [CUpdateInfo]
      -- | For every wallet ID (@CId Wal@) stores metadata for transactions in
      -- transactions history of correponding wallet.
      -- Some transactions might not have associated metadata.
      -- Invariant:
      -- prop> keys (_wsTxHistory ws) == keys (_wsWalletInfos ws)
    , _wsTxHistory       :: !(HashMap (CId Wal) (HashMap CTxId CTxMeta))
      -- | For every wallet ID (@CId Wal@) stores transaction history of corresponding
      -- wallet as map from 'TxId' to 'TxHistoryEntry'.
      -- Invariant:
      -- prop> keys (_wsHistoryCache ws) == keys (_wsWalletInfos ws)
    , _wsHistoryCache    :: !(HashMap (CId Wal) (Map TxId TxHistoryEntry))
      -- | Cache of unspent transaction outputs which belong
      -- to /one of the wallets present in wallet-db/ (i. e. listed in @_wsWalletInfos@).
    , _wsUtxo            :: !Utxo
      -- | Current balances of wallet's addresses
      -- NB: @_wsBalances@ depends on @_wsUtxo@,
      -- it's forbidden to update @_wsBalances@ without @_wsUtxo@.
    , _wsBalances        :: !WalletBalances
      -- | Set of all addresses which has been used as output address
      -- in some transactions which are /already in blockchain/.
      -- Hashes of blocks where address has been encountered first are stored
      -- in order to maintain valid sets of addresses during rollback
      -- (see "Pos.Wallet.Web.Tracking.Sync" and functions
      -- 'addCustomAddress' and 'removeCustomAddress')
    , _wsUsedAddresses   :: !CustomAddresses
      -- | Subset of @_wsUsedAddresses@ which are /change addresses/.
    , _wsChangeAddresses :: !CustomAddresses
    } deriving (Eq)

makeClassy ''WalletStorage

instance Default WalletStorage where
    def =
        WalletStorage
        { _wsWalletInfos     = mempty
        , _wsAccountInfos    = mempty
        , _wsProfile         = def
        , _wsReadyUpdates    = mempty
        , _wsTxHistory       = mempty
        , _wsHistoryCache    = mempty
        , _wsUsedAddresses   = mempty
        , _wsChangeAddresses = mempty
        , _wsUtxo            = mempty
        , _wsBalances        = mempty
        }

type Query a = forall m. (MonadReader WalletStorage m) => m a
type Update a = forall m. (HasConfiguration, MonadState WalletStorage m) => m a

-- | How to lookup addresses of account
data AddressLookupMode
    = Existing  -- ^ fetch only currently existing addresses
    | Deleted   -- ^ fetch only removed addresses
    | Ever      -- ^ fetch both existing and removed addresses

-- | Choose one of two monadic actions or combine results of both based on
-- given 'AddressLookupMode'. @a@ is meant to be set of addresses or something related.
withAccLookupMode :: (Monad m, Monoid a) => AddressLookupMode -> m a -> m a -> m a
withAccLookupMode Existing existing _       = existing
withAccLookupMode Deleted  _        deleted = deleted
withAccLookupMode Ever     existing deleted = mappend <$> existing <*> deleted

-- | Specifies special category of addresses which are stored in base.
data CustomAddressType
    = UsedAddr
    | ChangeAddr

-- | Choose lens for fetching set of addresses of given type.
customAddressL :: CustomAddressType -> Lens' WalletStorage CustomAddresses
customAddressL UsedAddr   = wsUsedAddresses
customAddressL ChangeAddr = wsChangeAddresses

-- | Keeps existing and pseudo-removed entries, e.g. addresses.
data CurrentAndRemoved a = CurrentAndRemoved
    { getCurrent :: a
    , getRemoved :: a
    }

-- | Get user profile metadata.
getProfile :: Query CProfile
getProfile = view wsProfile

-- | Set user profile metadata.
setProfile :: CProfile -> Update ()
setProfile cProfile = wsProfile .= cProfile

-- | Get IDs of all existing accounts.
getAccountIds :: Query [AccountId]
getAccountIds = HM.keys <$> view wsAccountInfos

-- | Get account meta info by given account ID.
getAccountMeta :: AccountId -> Query (Maybe CAccountMeta)
getAccountMeta accId = preview (wsAccountInfos . ix accId . aiMeta)

getAccountAddrMaps :: AccountId -> Query (CurrentAndRemoved CAddresses)
getAccountAddrMaps accId = do
    getCurrent <- getMap aiAddresses
    getRemoved <- getMap aiRemovedAddresses
    return CurrentAndRemoved{..}
  where
    getMap aiLens = fmap (fromMaybe mempty) $ preview $ wsAccountInfos . ix accId . aiLens

-- | Get wallet meta info considering sync status of wallet.
getWalletMetaIncludeUnready ::
       Bool                      -- ^ If set to @False@, then return @Nothing@ if wallet is not ready.
    -> CId Wal                   -- ^ Wallet ID.
    -> Query (Maybe CWalletMeta)
getWalletMetaIncludeUnready includeUnready cWalId = fmap _wiMeta . applyFilter <$> preview (wsWalletInfos . ix cWalId)
  where
    applyFilter xs = if includeUnready then xs else filterMaybe _wiIsReady xs
    filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
    filterMaybe p ma = ma >>= \a -> guard (p a) >> return a

-- | Retrieve the wallet info.
getWalletInfo :: CId Wal -> Query (Maybe WalletInfo)
getWalletInfo cid = view (wsWalletInfos . at cid)

-- | Get wallet meta info regardless of wallet sync status.
getWalletMeta :: CId Wal -> Query (Maybe CWalletMeta)
getWalletMeta = getWalletMetaIncludeUnready False

-- | Get last time when wallet password was changed.
getWalletPassLU :: CId Wal -> Query (Maybe PassPhraseLU)
getWalletPassLU cWalId = preview (wsWalletInfos . ix cWalId . wiPassphraseLU)

-- | Get wallet sync tip.
getWalletSyncTip :: CId Wal -> Query (Maybe WalletTip)
getWalletSyncTip cWalId = preview (wsWalletInfos . ix cWalId . wiSyncTip)

-- | Get IDs of all existing and /ready/ wallets.
getWalletAddresses :: Query [CId Wal]
getWalletAddresses =
    map fst <$> getWalletInfos

-- | Get IDs and information for of all existing and /ready/ wallets.
getWalletInfos :: Query [(CId Wal, WalletInfo)]
getWalletInfos =
    sortOn (view wiCreationTime . snd) . filter (view wiIsReady . snd) . HM.toList <$>
    view wsWalletInfos

-- | Get addresses of given account by account ID in the same order they were created.
getAccountWAddresses ::
       AddressLookupMode             -- ^ Determines which addresses to include: existing, deleted or both
    -> AccountId                     -- ^ Account ID.
    -> Query (Maybe [AddressInfo]) -- ^ Returns @Nothing@ if given account ID does not exist.
getAccountWAddresses mode accId =
    withAccLookupMode mode (fetch aiAddresses) (fetch aiRemovedAddresses)
  where
    fetch :: MonadReader WalletStorage m => Lens' AccountInfo CAddresses -> m (Maybe [AddressInfo])
    fetch which = fmap HM.elems <$> preview (wsAccountInfos . ix accId . which)

getWAddresses :: AddressLookupMode
              -> CId Wal
              -> Query [AddressInfo]
getWAddresses mode wid =
    withAccLookupMode mode (fetch aiAddresses) (fetch aiRemovedAddresses)
  where
    fetch :: MonadReader WalletStorage m => Lens' AccountInfo CAddresses -> m [AddressInfo]
    fetch which = do
      accs <- HM.filterWithKey (\k _ -> aiWId k == wid) <$> view wsAccountInfos
      return $ HM.elems =<< accs ^.. traverse . which

-- | Check if given address exists.
doesWAddressExist ::
       AddressLookupMode -- ^ Determines where to look for address: in set of existing
                         -- addresses, deleted addresses or both
    -> CWAddressMeta     -- ^ Given address
    -> Query Bool
doesWAddressExist mode addrMeta@(addrMetaToAccount -> wAddr) =
    getAny <$>
        withAccLookupMode mode (exists aiAddresses) (exists aiRemovedAddresses)
  where
    exists :: Lens' AccountInfo CAddresses -> Query Any
    exists which =
        Any . isJust <$>
        preview (wsAccountInfos . ix wAddr . which . ix (cwamId addrMeta))

doesAccountExist :: AccountId -> Query Bool
doesAccountExist accId = view $ wsAccountInfos . at accId . to isJust

-- | Get transaction metadata given wallet ID and transaction ID.
getTxMeta :: CId Wal -> CTxId -> Query (Maybe CTxMeta)
getTxMeta cid ctxId = preview $ wsTxHistory . ix cid . ix ctxId

-- | Get metadata for all transactions in given wallet (for which metadata exist).
getWalletTxHistory :: CId Wal -> Query (Maybe [CTxMeta])
getWalletTxHistory cWalId = toList <<$>> preview (wsTxHistory . ix cWalId)

-- | Get wallet DB 'Utxo' cache.
getWalletUtxo :: Query Utxo
getWalletUtxo = view wsUtxo

-- | Get wallet 'Utxo' cache together with balances cache atomically.
getWalletBalancesAndUtxo :: Query WalBalancesAndUtxo
getWalletBalancesAndUtxo = (,) <$> view wsBalances <*> view wsUtxo

-- | Given 'UtxoModifier', update wallet 'Utxo' cache together with
-- addresses' balances cache atomically.
updateWalletBalancesAndUtxo :: UtxoModifier -> Update ()
updateWalletBalancesAndUtxo modifier = do
    balAndUtxo <- (,) <$> use wsBalances <*> use wsUtxo
    wsBalances .= applyUtxoModToAddrCoinMap modifier balAndUtxo
    wsUtxo %= MM.modifyMap modifier

-- | Given new 'Utxo', set current wallet 'Utxo' cache and update
-- addreses' balances cache accordingly in the same time.
setWalletUtxo :: Utxo -> Update ()
setWalletUtxo utxo = do
    wsUtxo .= utxo
    wsBalances .= utxoToAddressCoinMap utxo

-- | Get next pending update if it exists.
getNextUpdate :: Query (Maybe CUpdateInfo)
getNextUpdate = preview (wsReadyUpdates . _head)

-- | Get block history cache for given wallet.
getHistoryCache :: CId Wal -> Query (Map TxId TxHistoryEntry)
getHistoryCache cWalId = view $ wsHistoryCache . at cWalId . non' _Empty

-- | Get set of all used or change addresses with corresponding
-- header hashes of blocks these addresses were firtst encountered
getCustomAddresses :: CustomAddressType -> Query [(CId Addr, HeaderHash)]
getCustomAddresses t = HM.toList <$> view (customAddressL t)

-- | Given used or change address, return hash of first block it has been
-- encountered.
getCustomAddress :: CustomAddressType -> CId Addr -> Query (Maybe HeaderHash)
getCustomAddress t addr = view $ customAddressL t . at addr

-- | Get list of all pending transactions.
getPendingTxs :: Query [PendingTx]
getPendingTxs = asks $ toListOf (wsWalletInfos . traversed . wsPendingTxs . traversed)

-- | Get list of pending transactions related to given wallet.
getWalletPendingTxs :: CId Wal -> Query (Maybe [PendingTx])
getWalletPendingTxs wid =
    preview $ wsWalletInfos . ix wid . wsPendingTxs . to toList

-- | Given wallet ID and transaction ID, return corresponding pending transaction
-- (if it exists)
getPendingTx :: CId Wal -> TxId -> Query (Maybe PendingTx)
getPendingTx wid txId = preview $ wsWalletInfos . ix wid . wsPendingTxs . ix txId

-- | If given address isn't yet present in set of used\/change addresses, then add it
-- with given block header hash.
addCustomAddress :: CustomAddressType -> (CId Addr, HeaderHash) -> Update Bool
addCustomAddress t (addr, hh) = fmap isJust $ customAddressL t . at addr <<.= Just hh

-- | Remove given address from set of used\/change addresses only if provided
-- header hash is equal to one which is stored in database.
removeCustomAddress :: CustomAddressType -> (CId Addr, HeaderHash) -> Update Bool
removeCustomAddress t (addr, hh) = do
    mhh' <- use $ customAddressL t . at addr
    let exists = mhh' == Just hh
    when exists $
        customAddressL t . at addr .= Nothing
    return exists

-- | Add given account to wallet db.
createAccount :: AccountId -> CAccountMeta -> Update ()
createAccount accId cAccMeta =
    wsAccountInfos . at accId %= Just . fromMaybe (AccountInfo cAccMeta mempty mempty 0)

-- | Create a new wallet with given parameters.
-- @isReady@ should be set to @False@ when syncing is still needed.
createWallet :: CId Wal -> CWalletMeta -> Bool -> POSIXTime -> Update ()
createWallet cWalId cWalMeta isReady curTime = do
    let info = WalletInfo cWalMeta curTime curTime NotSynced mempty isReady
    wsWalletInfos . at cWalId %= (<|> Just info)

-- | Add new address given 'CWAddressMeta' (which contains information about
-- target wallet and account too).
addWAddress :: CWAddressMeta -> Update ()
addWAddress addrMeta@CWAddressMeta{..} = do
    let accInfo :: Traversal' WalletStorage AccountInfo
        accInfo = wsAccountInfos . ix (addrMetaToAccount addrMeta)
    whenJustM (preuse accInfo) $ \info -> do
        let mAddr = info ^. aiAddresses . at cwamId
        when (isNothing mAddr) $ do
            -- Here we increment current account's last address index
            -- and assign its value to sorting index of newly created address.
            accInfo . aiUnusedKey += 1
            let key = info ^. aiUnusedKey
            accInfo . aiAddresses . at cwamId ?= AddressInfo addrMeta key

-- | Update account metadata.
setAccountMeta :: AccountId -> CAccountMeta -> Update ()
setAccountMeta accId cAccMeta = wsAccountInfos . ix accId . aiMeta .= cAccMeta

-- | Update wallet metadata.
setWalletMeta :: CId Wal -> CWalletMeta -> Update ()
setWalletMeta cWalId cWalMeta = wsWalletInfos . ix cWalId . wiMeta .= cWalMeta

-- | Change wallet ready status.
setWalletReady :: CId Wal -> Bool -> Update ()
setWalletReady cWalId isReady = wsWalletInfos . ix cWalId . wiIsReady .= isReady

-- | Update wallet's last password change time.
setWalletPassLU :: CId Wal -> PassPhraseLU -> Update ()
setWalletPassLU cWalId passLU = wsWalletInfos . ix cWalId . wiPassphraseLU .= passLU

-- | Declare that given wallet has been synced with blockchain up to block with given
-- header hash.
setWalletSyncTip :: CId Wal -> HeaderHash -> Update ()
setWalletSyncTip cWalId hh = wsWalletInfos . ix cWalId . wiSyncTip .= SyncedWith hh

-- | Set meta data for transaction only if it hasn't been set already.
-- FIXME: this will be removed later (temporary solution) (not really =\)
addOnlyNewTxMeta :: CId Wal -> CTxId -> CTxMeta -> Update ()
addOnlyNewTxMeta cWalId cTxId cTxMeta =
    -- Double nested HashMap update (if either or both of cWalId, cTxId don't exist, they will be created)
    wsTxHistory . at cWalId . non' _Empty . at cTxId %= Just . fromMaybe cTxMeta

-- | Delete all transactions' metadata for given wallet.
removeTxMetas :: CId Wal -> Update ()
removeTxMetas cWalId = wsTxHistory . at cWalId .= Nothing

-- | 'addOnlyNewTxMeta' for several transactions at once.
addOnlyNewTxMetas :: CId Wal -> [(CTxId, CTxMeta)] -> Update ()
addOnlyNewTxMetas = mapM_ . uncurry . addOnlyNewTxMeta

-- | Delete transactions metadata by given wallet id and transactions ids.
removeWalletTxMetas :: CId Wal -> [CTxId] -> Update ()
removeWalletTxMetas cWalId cTxs =
    wsTxHistory . at cWalId . non' _Empty %= flip (foldr HM.delete) cTxs

-- | Delete the wallet info. This doesn't delete wallet's accounts, history cache
-- and transactions metadata.
removeWallet :: CId Wal -> Update ()
removeWallet cWalId = wsWalletInfos . at cWalId .= Nothing

-- | Delete all transaction history cache for given wallet.
removeHistoryCache :: CId Wal -> Update ()
removeHistoryCache cWalId = wsHistoryCache . at cWalId .= Nothing

-- | Delete given account.
removeAccount :: AccountId -> Update ()
removeAccount accId = wsAccountInfos . at accId .= Nothing

-- | Remove given address, not removing it completely, but marking it as `removed` instead.
-- See also 'addRemovedAccount'.
removeWAddress :: CWAddressMeta -> Update ()
removeWAddress addrMeta@(addrMetaToAccount -> accId) = do
    let addrId = cwamId addrMeta
    -- If the address exists, move it to 'addressesRemoved'
    whenJustM (preuse (accAddresses accId . ix addrId)) $ \addressInfo -> do
        accAddresses        accId . at addrId .= Nothing
        accRemovedAddresses accId . at addrId .= Just addressInfo
  where
    accAddresses        accId' = wsAccountInfos . ix accId' . aiAddresses
    accRemovedAddresses accId' = wsAccountInfos . ix accId' . aiRemovedAddresses

-- | Add info about new pending update to the end of update info list.
addUpdate :: CUpdateInfo -> Update ()
addUpdate ui = wsReadyUpdates %= (++ [ui])

-- | Remove next pending update info from update info list.
removeNextUpdate :: Update ()
removeNextUpdate = wsReadyUpdates %= drop 1

-- | Reset the whole database to clean state completely. Used only in testing and debugging.
testReset :: Update ()
testReset = put def

-- | Legacy transaction, no longer used. For existing Db tx logs only. Now use
-- 'removeHistoryCache', 'insertIntoHistoryCache' or 'removeFromHistoryCache'
updateHistoryCache :: CId Wal -> [TxHistoryEntry] -> Update ()
updateHistoryCache cWalId cTxs =
    wsHistoryCache . at cWalId ?= txHistoryListToMap cTxs

-- | Add new history entries to history cache of given wallet.
insertIntoHistoryCache :: CId Wal -> Map TxId TxHistoryEntry -> Update ()
insertIntoHistoryCache cWalId cTxs =
    wsHistoryCache . at cWalId . non' _Empty %= (cTxs `M.union`)

-- | Remove entries about transactions with given IDs from wallet's history cache.
removeFromHistoryCache :: CId Wal -> Map TxId () -> Update ()
removeFromHistoryCache cWalId cTxs =
    wsHistoryCache . at cWalId . non' _Empty %= (`M.difference` cTxs)

-- | Sets a resubmission condition for pending transaction.
-- This shouldn't be able to create new transaction.
-- NOTE: If you're going to use this function, make sure 'casPtxCondition'
-- doesn't fit your purposes better
setPtxCondition :: CId Wal -> TxId -> PtxCondition -> Update ()
setPtxCondition wid txId cond =
    wsWalletInfos . ix wid . wsPendingTxs . ix txId . ptxCond .= cond

-- | Conditional modifier.
-- Returns 'True' if pending transaction existed and modification was applied.
checkAndSmthPtx
    :: CId Wal
    -> TxId
    -> (Maybe PtxCondition -> Bool)
    -> LS.State (Maybe PendingTx) ()
    -> Update Bool
checkAndSmthPtx wid txId whetherModify modifier =
    fmap getAny $ zoom' (wsWalletInfos . ix wid . wsPendingTxs . at txId) $ do
        matches <- whetherModify . fmap _ptxCond <$> get
        when matches modifier
        return (Any matches)

-- | Compare-and-set version of 'setPtxCondition'.
casPtxCondition :: CId Wal -> TxId -> PtxCondition -> PtxCondition -> Update Bool
casPtxCondition wid txId expectedCond newCond =
    checkAndSmthPtx wid txId (== Just expectedCond) (_Just . ptxCond .= newCond)

-- | Removes pending transaction, if its status is 'PtxCreating'.
removeOnlyCreatingPtx :: CId Wal -> TxId -> Update Bool
removeOnlyCreatingPtx wid txId =
    checkAndSmthPtx wid txId (has (_Just . _PtxCreating)) (put Nothing)

-- | Datatype which defines how state of pending transaction should change.
data PtxMetaUpdate
    = PtxIncSubmitTiming          -- ^ Delay resubmission further
    | PtxResetSubmitTiming SlotId -- ^ Set next resubmission slot explicitely
    | PtxMarkAcknowledged         -- ^ Mark tx as acknowledged by some peer

-- | Update meta info of pending transaction atomically.
ptxUpdateMeta :: CId Wal -> TxId -> PtxMetaUpdate -> Update ()
ptxUpdateMeta wid txId updType =
    wsWalletInfos . ix wid . wsPendingTxs . ix txId %=
        case updType of
            PtxIncSubmitTiming ->
                ptxSubmitTiming %~ incPtxSubmitTimingPure
            PtxResetSubmitTiming curSlot ->
                ptxSubmitTiming .~ mkPtxSubmitTiming curSlot
            PtxMarkAcknowledged ->
                ptxMarkAcknowledgedPure

cancelApplyingPtxs :: Update ()
cancelApplyingPtxs =
    wsWalletInfos . traversed .
    wsPendingTxs . traversed %= cancelApplyingPtx

cancelSpecificApplyingPtx :: TxId -> Update ()
cancelSpecificApplyingPtx txId =
    wsWalletInfos . traversed .
    wsPendingTxs . ix txId %= cancelApplyingPtx

-- | Add transaction to set of pending transactions, if it isn't already there.
addOnlyNewPendingTx :: PendingTx -> Update ()
addOnlyNewPendingTx ptx =
    wsWalletInfos . ix (_ptxWallet ptx) .
    wsPendingTxs . at (_ptxTxId ptx) %= (<|> Just ptx)

-- | Move every transaction which is in 'PtxWontApply' state to 'PtxApplying'
-- state, effectively starting resubmission of failed transactions again.
resetFailedPtxs :: SlotId -> Update ()
resetFailedPtxs curSlot =
    wsWalletInfos . traversed .
    wsPendingTxs . traversed %= resetFailedPtx curSlot

-- | Gets whole wallet storage. Used primarily for testing and diagnostics.
getWalletStorage :: Query WalletStorage
getWalletStorage = ask

-- | Flushes data in wallet storage
-- Preserves all metadata, wallets, accounts and addresses
-- Flushes all data that can be rebuild from blockchain (tx history and etc.)
flushWalletStorage :: Update ()
flushWalletStorage = modify flushDo
  where
    flushDo ws = ws
        { _wsWalletInfos = flushWalletInfo <$> _wsWalletInfos ws
        , _wsHistoryCache = HM.empty
        , _wsUtxo = M.empty
        , _wsUsedAddresses = HM.empty
        , _wsChangeAddresses = HM.empty
        }
    flushWalletInfo wi = wi { _wiSyncTip = NotSynced
                            , _wiIsReady = False
                            }

deriveSafeCopySimple 0 'base ''CCoin
deriveSafeCopySimple 0 'base ''CProfile
deriveSafeCopySimple 0 'base ''CHash
deriveSafeCopySimple 0 'base ''CId
deriveSafeCopySimple 0 'base ''Wal
deriveSafeCopySimple 0 'base ''Addr
deriveSafeCopySimple 0 'base ''BackupPhrase
deriveSafeCopySimple 0 'base ''AccountId
deriveSafeCopySimple 0 'base ''CWAddressMeta
deriveSafeCopySimple 0 'base ''CWalletAssurance
deriveSafeCopySimple 0 'base ''CAccountMeta
deriveSafeCopySimple 0 'base ''CWalletMeta
deriveSafeCopySimple 0 'base ''CTxId
deriveSafeCopySimple 0 'base ''Timestamp
deriveSafeCopySimple 0 'base ''TxHistoryEntry
deriveSafeCopySimple 0 'base ''CTxMeta
deriveSafeCopySimple 0 'base ''CUpdateInfo
deriveSafeCopySimple 0 'base ''AddressLookupMode
deriveSafeCopySimple 0 'base ''CustomAddressType
deriveSafeCopySimple 0 'base ''CurrentAndRemoved
deriveSafeCopySimple 0 'base ''TxAux
deriveSafeCopySimple 0 'base ''PtxCondition
deriveSafeCopySimple 0 'base ''PtxSubmitTiming
deriveSafeCopySimple 0 'base ''PtxMetaUpdate
deriveSafeCopySimple 0 'base ''PendingTx
deriveSafeCopySimple 0 'base ''AddressInfo
deriveSafeCopySimple 0 'base ''AccountInfo
deriveSafeCopySimple 0 'base ''WalletTip
deriveSafeCopySimple 0 'base ''WalletInfo

-- Legacy versions, for migrations

data WalletStorage_v0 = WalletStorage_v0
    { _v0_wsWalletInfos     :: !(HashMap (CId Wal) WalletInfo)
    , _v0_wsAccountInfos    :: !(HashMap AccountId AccountInfo)
    , _v0_wsProfile         :: !CProfile
    , _v0_wsReadyUpdates    :: [CUpdateInfo]
    , _v0_wsTxHistory       :: !(HashMap (CId Wal) (HashMap CTxId CTxMeta))
    , _v0_wsHistoryCache    :: !(HashMap (CId Wal) [TxHistoryEntry])
    , _v0_wsUtxo            :: !Utxo
    , _v0_wsUsedAddresses   :: !CustomAddresses
    , _v0_wsChangeAddresses :: !CustomAddresses
    }

data WalletStorage_v1 = WalletStorage_v1
    { _v1_wsWalletInfos     :: !(HashMap (CId Wal) WalletInfo)
    , _v1_wsAccountInfos    :: !(HashMap AccountId AccountInfo)
    , _v1_wsProfile         :: !CProfile
    , _v1_wsReadyUpdates    :: [CUpdateInfo]
    , _v1_wsTxHistory       :: !(HashMap (CId Wal) (HashMap CTxId CTxMeta))
    , _v1_wsHistoryCache    :: !(HashMap (CId Wal) (Map TxId TxHistoryEntry))
    , _v1_wsUtxo            :: !Utxo
    , _v1_wsUsedAddresses   :: !CustomAddresses
    , _v1_wsChangeAddresses :: !CustomAddresses
    }

deriveSafeCopySimple 0 'base ''WalletStorage_v0
deriveSafeCopySimple 1 'extension ''WalletStorage_v1
deriveSafeCopySimple 2 'extension ''WalletStorage

instance Migrate WalletStorage_v1 where
    type MigrateFrom WalletStorage_v1 = WalletStorage_v0
    migrate WalletStorage_v0{..} = WalletStorage_v1
        { _v1_wsWalletInfos     = _v0_wsWalletInfos
        , _v1_wsAccountInfos    = _v0_wsAccountInfos
        , _v1_wsProfile         = _v0_wsProfile
        , _v1_wsReadyUpdates    = _v0_wsReadyUpdates
        , _v1_wsTxHistory       = _v0_wsTxHistory
        , _v1_wsHistoryCache    = HM.map txHistoryListToMap _v0_wsHistoryCache
        , _v1_wsUtxo            = _v0_wsUtxo
        , _v1_wsUsedAddresses   = _v0_wsUsedAddresses
        , _v1_wsChangeAddresses = _v0_wsChangeAddresses
        }

instance Migrate WalletStorage where
    type MigrateFrom WalletStorage = WalletStorage_v1
    migrate WalletStorage_v1{..} = WalletStorage
        { _wsWalletInfos     = _v1_wsWalletInfos
        , _wsAccountInfos    = _v1_wsAccountInfos
        , _wsProfile         = _v1_wsProfile
        , _wsReadyUpdates    = _v1_wsReadyUpdates
        , _wsTxHistory       = _v1_wsTxHistory
        , _wsHistoryCache    = _v1_wsHistoryCache
        , _wsUtxo            = _v1_wsUtxo
        , _wsBalances        = utxoToAddressCoinMap _v1_wsUtxo
        , _wsUsedAddresses   = _v1_wsUsedAddresses
        , _wsChangeAddresses = _v1_wsChangeAddresses
        }
