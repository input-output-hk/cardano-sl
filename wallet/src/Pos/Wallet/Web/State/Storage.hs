{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module which defines internal structure of `acid-state` wallet database.
module Pos.Wallet.Web.State.Storage
       (
         WalletStorage (..)
       , HasWalletStorage (..)
       , WAddressMeta (..)
       , HasWAddressMeta (..)
       , wamAccount
       , WalletInfo (..)
       , AccountInfo (..)
       , AddressInfo (..)
       , AddressLookupMode (..)
       , CAddresses
       , CustomAddressType (..)
       , CurrentAndRemoved (..)
       , WalletBalances
       , WalBalancesAndUtxo
       , WalletSyncState (..)
       , RestorationBlockDepth (..)
       , SyncThroughput (..)
       , SyncStatistics (..)
       , PtxMetaUpdate (..)
       , Query
       , Update
       , noSyncStatistics
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
       , getWalletSyncState
       , getWalletAddresses
       , getWalletInfos
       , getWalletInfo
       , getAccountWAddresses
       , getWAddresses
       , doesWAddressExist
       , doesWAddressExist2
       , isWalletRestoring
       , getTxMeta
       , getNextUpdate
       , getHistoryCache
       , getCustomAddresses
       , getCustomAddress
       , getPendingTxs
       , getWalletPendingTxs
       , getPendingTx
       , addCustomAddress
       , addCustomAddress2
       , removeCustomAddress
       , removeCustomAddress2
       , createAccount
       , createWallet
       , addWAddress
       , addWAddress2
       , setAccountMeta
       , setWalletMeta
       , setWalletReady
       , setWalletPassLU
       , setWalletSyncTip
       , setWalletRestorationSyncTip
       , updateSyncStatistics
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
       , removeWAddress2
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
         -- * Exported only for testing purposes
       , WalletTip_v0 (..)
       , AddressInfo_v0 (..)
       , AccountInfo_v0 (..)
       , WalletInfo_v0(..)
       , WalletStorage_v2(..)
       , WalletStorage_v3(..)
       ) where

import           Universum

import           Control.Arrow ((***))
import           Control.Lens (at, has, ix, lens, makeClassy, makeLenses, non', to, toListOf,
                               traversed, (%=), (+=), (.=), (<<.=), (?=), _Empty, _Just, _head)
import           Control.Monad.State.Class (get, put)
import qualified Control.Monad.State.Lazy as LS
import           Data.Default (Default, def)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import           Data.SafeCopy (Migrate (..), base, deriveSafeCopySimple, extension)
import qualified Data.Text.Buildable
import           Data.Time.Clock.POSIX (POSIXTime)
import           Formatting ((%))
import qualified Formatting as F
import           Pos.Client.Txp.History (TxHistoryEntry, txHistoryListToMap)
import           Pos.Core (Address, BlockCount (..), ChainDifficulty (..), HeaderHash,
                           ProtocolConstants (..), SlotId, Timestamp, VssMaxTTL (..),
                           VssMinTTL (..))
import           Pos.Core.Txp (TxAux, TxId)
import           Pos.SafeCopy ()
import           Pos.Txp (AddrCoinMap, Utxo, UtxoModifier, applyUtxoModToAddrCoinMap,
                          utxoToAddressCoinMap)
import           Pos.Util.BackupPhrase (BackupPhrase)
import qualified Pos.Util.Modifier as MM
import qualified Pos.Wallet.Web.ClientTypes as WebTypes
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition, PtxSubmitTiming (..),
                                               ptxCond, ptxSubmitTiming, _PtxCreating)
import           Pos.Wallet.Web.Pending.Util (cancelApplyingPtx, incPtxSubmitTimingPure,
                                              mkPtxSubmitTiming, ptxMarkAcknowledgedPure,
                                              resetFailedPtx)
import           Serokell.Util (zoom')

-- | Type alias for indices which are used to maintain order
-- in which addresses were created.

-- | Address with associated metadata locating it in an account in a wallet.
data WAddressMeta = WAddressMeta
    { _wamWalletId     :: WebTypes.CId WebTypes.Wal
    , _wamAccountIndex :: Word32
    , _wamAddressIndex :: Word32
    , _wamAddress      :: Address
    } deriving (Eq, Ord, Show, Generic, Typeable)

instance NFData WAddressMeta

makeClassy ''WAddressMeta
instance Hashable WAddressMeta
instance Buildable WAddressMeta where
    build WAddressMeta{..} =
        F.bprint (F.build%"@"%F.build%"@"%F.build%" ("%F.build%")")
        _wamWalletId _wamAccountIndex _wamAddressIndex _wamAddress

-- | Lens to extract the account from an 'AddressMeta'
wamAccount :: Lens' WAddressMeta WebTypes.AccountId
wamAccount = lens
    (WebTypes.AccountId <$> view wamWalletId <*> view wamAccountIndex)
    (\am (WebTypes.AccountId wid accIdx) -> set wamWalletId wid
                                            . set wamAccountIndex accIdx $ am)

type AddressSortingKey = Int

-- | Information about existing wallet address.
data AddressInfo = AddressInfo
    { -- | Address data, including derivation indices and address itself.
      adiWAddressMeta :: !WAddressMeta
      -- | An index which determines which position this address has in
      -- list of all account's addresses.
    , adiSortingKey   :: !AddressSortingKey
    } deriving Eq

instance NFData AddressInfo where
    rnf x = adiWAddressMeta x
        `deepseq` adiSortingKey x
        `deepseq` ()

type CAddresses = HashMap Address AddressInfo

-- | Information about existing wallet account.
data AccountInfo = AccountInfo
    { -- | Account metadata (e. g. account name).
      _aiMeta             :: !WebTypes.CAccountMeta
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

instance NFData AccountInfo where
    rnf ai =  _aiMeta ai
        `deepseq` _aiAddresses ai
        `deepseq` _aiRemovedAddresses ai
        `deepseq` _aiUnusedKey ai
        `deepseq` ()

makeLenses ''AccountInfo

-- | A 'RestorationBlockDepth' is simply a @newtype@ wrapper over a 'ChainDifficulty',
-- More specifically, it stores the simple but crucial information of when, relative to the blockchain,
-- a certain wallet was restored.
newtype RestorationBlockDepth =
    RestorationBlockDepth { getRestorationBlockDepth :: ChainDifficulty }
    deriving (Eq, Show, NFData)

-- | Datatype which stores information about the sync state
-- of this wallet. Syncing here is always relative to the blockchain.
-- See "Pos.Wallet.Web.Tracking.Sync" for syncing functionality.
data WalletSyncState
    = NotSynced
    -- ^ A sync state associated with a brand-new, prestine wallet,
    -- created by the user (or programmatically) to be later used.
    | RestoringFrom !RestorationBlockDepth !HeaderHash
    -- ^ This wallet has been restored from 'RestorationBlockDepth',
    -- and is now tracking the blockchain up to 'HeaderHash'.
    -- Whilst it might seem that storing the 'RestorationBlockDepth' is
    -- redundant, it's a necessary evil to cover the case when the user
    -- closes the wallet whilst the restoration is in progress. When the
    -- wallet resumes its activity, this distinction (restoration vs normal
    -- syncing) is important to correctly compute the balance for this wallet.
    | SyncedWith !HeaderHash
    -- ^ This wallet is tracking the blockchain up to 'HeaderHash'.
    deriving (Eq)

instance NFData WalletSyncState where
    rnf x = case x of
        NotSynced         -> ()
        SyncedWith h      -> rnf h
        RestoringFrom a b -> a `deepseq` b `deepseq` ()

-- The 'SyncThroughput' is computed during the syncing phase in terms of
-- how many blocks we can sync in one second. This information can be
-- used by consumers of the API to construct heuristics on the state of the
-- syncing (e.g. estimated completion time, for example).
-- We also store the 'ChainDifficulty' we have been syncing up until now so
-- API consumers can also compute progress in terms of percentage. We don't
-- store this information in the 'WalletSyncState' directly as we cannot
-- calculate the 'ChainDifficulty' from a 'HeaderHash', and that requires a
-- RocksDB lookup, which would make migration harder to perform without
-- incidents.
data SyncStatistics = SyncStatistics {
      wspThroughput             :: !SyncThroughput
    , wspCurrentBlockchainDepth :: !ChainDifficulty
    } deriving (Eq)

instance NFData SyncStatistics where
    rnf ss = wspThroughput ss
        `deepseq` wspCurrentBlockchainDepth ss
        `deepseq` ()

-- ^ | The 'SyncThroughput', in blocks/sec. This can be roughly computed
-- during the syncing process, to provide better estimate to the frontend
-- on how much time the restoration/syncing progress is going to take.
newtype SyncThroughput = SyncThroughput BlockCount
     deriving (Eq, Ord, Show, NFData)

zeroThroughput :: SyncThroughput
zeroThroughput = SyncThroughput (BlockCount 0)

noSyncStatistics :: SyncStatistics
noSyncStatistics = SyncStatistics zeroThroughput (ChainDifficulty $ BlockCount 0)

data WalletInfo = WalletInfo
    { -- | Wallet metadata (name, assurance, etc.)
      _wiMeta           :: !WebTypes.CWalletMeta
      -- | Last time when wallet passphrase was updated.
    , _wiPassphraseLU   :: !WebTypes.PassPhraseLU
      -- | Wallet creation time.
    , _wiCreationTime   :: !POSIXTime
      -- | Incapsulate the history of this wallet in terms of "lifecycle".
    , _wiSyncState      :: !WalletSyncState
      -- | Syncing statistics for this wallet, if any.
    , _wiSyncStatistics :: !SyncStatistics
      -- | Pending states for all created transactions (information related
      -- to transaction resubmission).
      -- See "Pos.Wallet.Web.Pending" for resubmission functionality.
    , _wsPendingTxs     :: !(HashMap TxId PendingTx)
      -- | Wallets that are being synced are marked as not ready, and
      -- are excluded from api endpoints. This info should not be leaked
      -- into a client facing data structure (for example 'CWalletMeta')
    , _wiIsReady        :: !Bool
    } deriving (Eq)

instance NFData WalletInfo where
    rnf wi = _wiMeta wi
        `deepseq` _wiPassphraseLU wi
        `deepseq` _wiCreationTime wi
        `deepseq` _wiSyncState wi
        `deepseq` _wiSyncStatistics wi
        `deepseq` _wsPendingTxs wi
        `deepseq` _wiIsReady wi
        `deepseq` ()

makeLenses ''WalletInfo

-- | Maps addresses to their first occurrence in the blockchain
type CustomAddresses = HashMap Address HeaderHash

-- | Alias for 'Pos.Txp.AddrCoinMap' storing balances for wallet's addresses.
type WalletBalances = AddrCoinMap
type WalBalancesAndUtxo = (WalletBalances, Utxo)

-- | Datatype which defines full structure of acid-state.
data WalletStorage = WalletStorage
    { -- | Stores information about all existing wallets.
      _wsWalletInfos     :: !(HashMap (WebTypes.CId WebTypes.Wal) WalletInfo)
      -- | Stores information about all existing accounts.
    , _wsAccountInfos    :: !(HashMap WebTypes.AccountId AccountInfo)
      -- | Non-wallet-specific client metadata.
    , _wsProfile         :: !WebTypes.CProfile
      -- | List of descriptions of approved and downloaded updates, waiting
      -- for user action. See "Pos.Update.Download" and @updateNotifier@ in
      -- "Pos.Wallet.Web.Sockets.Notifier" for more info of how updates work.
    , _wsReadyUpdates    :: [WebTypes.CUpdateInfo]
      -- | For every wallet ID (@CId Wal@) stores metadata for transactions in
      -- transactions history of correponding wallet.
      -- Some transactions might not have associated metadata.
      -- Invariant:
      -- prop> keys (_wsTxHistory ws) == keys (_wsWalletInfos ws)
    , _wsTxHistory       :: !(HashMap (WebTypes.CId WebTypes.Wal) (HashMap WebTypes.CTxId WebTypes.CTxMeta))
      -- | For every wallet ID (@CId Wal@) stores transaction history of corresponding
      -- wallet as map from 'TxId' to 'TxHistoryEntry'.
      -- Invariant:
      -- prop> keys (_wsHistoryCache ws) == keys (_wsWalletInfos ws)
    , _wsHistoryCache    :: !(HashMap (WebTypes.CId WebTypes.Wal) (Map TxId TxHistoryEntry))
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

instance NFData WalletStorage where
    rnf ws = _wsWalletInfos ws
        `deepseq` _wsAccountInfos ws
        `deepseq` _wsProfile ws
        `deepseq` _wsReadyUpdates ws
        `deepseq` _wsTxHistory ws
        `deepseq` _wsHistoryCache ws
        `deepseq` _wsUtxo ws
        `deepseq` _wsBalances ws
        `deepseq` _wsUsedAddresses ws
        `deepseq` _wsChangeAddresses ws
        `deepseq` ()

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
type Update a = forall m. (MonadState WalletStorage m) => m a

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
getProfile :: Query WebTypes.CProfile
getProfile = view wsProfile

-- | Set user profile metadata.
setProfile :: WebTypes.CProfile -> Update ()
setProfile cProfile = wsProfile .= cProfile

doesAccountExist :: WebTypes.AccountId -> Query Bool
doesAccountExist accId = view $ wsAccountInfos . at accId . to isJust

-- | Get IDs of all existing accounts.
getAccountIds :: Query [WebTypes.AccountId]
getAccountIds = HM.keys <$> view wsAccountInfos

-- | Get account meta info by given account ID.
getAccountMeta :: WebTypes.AccountId -> Query (Maybe WebTypes.CAccountMeta)
getAccountMeta accId = preview (wsAccountInfos . ix accId . aiMeta)

getAccountAddrMaps :: WebTypes.AccountId -> Query (CurrentAndRemoved CAddresses)
getAccountAddrMaps accId = do
    getCurrent <- getMap aiAddresses
    getRemoved <- getMap aiRemovedAddresses
    return CurrentAndRemoved{..}
  where
    getMap aiLens = fmap (fromMaybe mempty) $ preview $ wsAccountInfos . ix accId . aiLens

-- | Get wallet meta info considering sync status of wallet.
getWalletMetaIncludeUnready ::
       Bool                      -- ^ If set to @False@, then return @Nothing@ if wallet is not ready.
    -> WebTypes.CId WebTypes.Wal                   -- ^ Wallet ID.
    -> Query (Maybe WebTypes.CWalletMeta)
getWalletMetaIncludeUnready includeUnready cWalId = fmap _wiMeta . applyFilter <$> preview (wsWalletInfos . ix cWalId)
  where
    applyFilter xs = if includeUnready then xs else filterMaybe _wiIsReady xs
    filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
    filterMaybe p ma = ma >>= \a -> guard (p a) >> return a

-- | Retrieve the wallet info.
getWalletInfo :: WebTypes.CId WebTypes.Wal -> Query (Maybe WalletInfo)
getWalletInfo cid = view (wsWalletInfos . at cid)

-- | Get wallet meta info regardless of wallet sync status.
getWalletMeta :: WebTypes.CId WebTypes.Wal -> Query (Maybe WebTypes.CWalletMeta)
getWalletMeta = getWalletMetaIncludeUnready False

-- | Get last time when wallet password was changed.
getWalletPassLU :: WebTypes.CId WebTypes.Wal -> Query (Maybe WebTypes.PassPhraseLU)
getWalletPassLU cWalId = preview (wsWalletInfos . ix cWalId . wiPassphraseLU)

-- | Get wallet sync state.
getWalletSyncState :: WebTypes.CId WebTypes.Wal -> Query (Maybe WalletSyncState)
getWalletSyncState cWalId = preview (wsWalletInfos . ix cWalId . wiSyncState)

-- | Get IDs of all existing and /ready/ wallets.
getWalletAddresses :: Query [WebTypes.CId WebTypes.Wal]
getWalletAddresses =
    map fst <$> getWalletInfos

-- | Get IDs and information for of all existing and /ready/ wallets.
getWalletInfos :: Query [(WebTypes.CId WebTypes.Wal, WalletInfo)]
getWalletInfos =
    sortOn (view wiCreationTime . snd) . filter (view wiIsReady . snd) . HM.toList <$>
    view wsWalletInfos

-- | Get addresses of given account by account ID in the same order they were created.
getAccountWAddresses ::
       AddressLookupMode           -- ^ Determines which addresses to include: existing, deleted or both
    -> WebTypes.AccountId          -- ^ Account ID.
    -> Query (Maybe [AddressInfo]) -- ^ Returns @Nothing@ if given account ID does not exist.
getAccountWAddresses mode accId =
    withAccLookupMode mode (fetch aiAddresses) (fetch aiRemovedAddresses)
  where
    fetch :: MonadReader WalletStorage m => Lens' AccountInfo CAddresses -> m (Maybe [AddressInfo])
    fetch which = fmap HM.elems <$> preview (wsAccountInfos . ix accId . which)

getWAddresses :: AddressLookupMode
              -> WebTypes.CId WebTypes.Wal
              -> Query [AddressInfo]
getWAddresses mode wid =
    withAccLookupMode mode (fetch aiAddresses) (fetch aiRemovedAddresses)
  where
    fetch :: MonadReader WalletStorage m => Lens' AccountInfo CAddresses -> m [AddressInfo]
    fetch which = do
      accs <- HM.filterWithKey (\k _ -> WebTypes.aiWId k == wid) <$> view wsAccountInfos
      return $ HM.elems =<< accs ^.. traverse . which

-- | Check if given address exists.
doesWAddressExist2 ::
       AddressLookupMode -- ^ Determines where to look for address: in set of existing
                         -- addresses, deleted addresses or both
    -> WAddressMeta     -- ^ Given address
    -> Query Bool
doesWAddressExist2 mode addrMeta@(view wamAccount -> wAddr) =
    getAny <$>
        withAccLookupMode mode (exists aiAddresses) (exists aiRemovedAddresses)
  where
    exists :: Lens' AccountInfo CAddresses -> Query Any
    exists which =
        Any . isJust <$>
        preview (wsAccountInfos . ix wAddr . which . ix (addrMeta ^. wamAddress))

-- | Legacy version of 'doesWAddressExist2' for backwards compatibility on
--   the event log.
doesWAddressExist :: AddressLookupMode -> WebTypes.CWAddressMeta -> Query Bool
doesWAddressExist mode addrMeta@(WebTypes.addrMetaToAccount -> wAddr) =
    getAny <$>
        withAccLookupMode mode (exists aiAddresses) (exists aiRemovedAddresses)
  where
    exists :: Lens' AccountInfo CAddresses -> Query Any
    exists which =
        Any . isJust <$>
        preview (wsAccountInfos
            . ix wAddr . which
            . ix (unsafeCIdToAddress (WebTypes.cwamId addrMeta)))

-- | Get transaction metadata given wallet ID and transaction ID.
getTxMeta :: WebTypes.CId WebTypes.Wal -> WebTypes.CTxId -> Query (Maybe WebTypes.CTxMeta)
getTxMeta cid ctxId = preview $ wsTxHistory . ix cid . ix ctxId

-- | Get metadata for all transactions in given wallet (for which metadata exist).
getWalletTxHistory :: WebTypes.CId WebTypes.Wal -> Query (Maybe [WebTypes.CTxMeta])
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
getNextUpdate :: Query (Maybe WebTypes.CUpdateInfo)
getNextUpdate = preview (wsReadyUpdates . _head)

-- | Get block history cache for given wallet.
getHistoryCache :: WebTypes.CId WebTypes.Wal -> Query (Map TxId TxHistoryEntry)
getHistoryCache cWalId = view $ wsHistoryCache . at cWalId . non' _Empty

-- | Get set of all used or change addresses with corresponding
-- header hashes of blocks these addresses were firtst encountered
getCustomAddresses :: CustomAddressType -> Query [(Address, HeaderHash)]
getCustomAddresses t = HM.toList <$> view (customAddressL t)

-- | Given used or change address, return hash of first block it has been
-- encountered.
getCustomAddress :: CustomAddressType -> Address -> Query (Maybe HeaderHash)
getCustomAddress t addr = view $ customAddressL t . at addr

-- | Get list of all pending transactions.
getPendingTxs :: Query [PendingTx]
getPendingTxs = asks $ toListOf (wsWalletInfos . traversed . wsPendingTxs . traversed)

-- | Returns 'True' if the input @Wallet@ (as identified by its @CId Wal@) is
-- being restored, i.e. it has its 'WalletSyncState' on @RestoringFrom@.
isWalletRestoring :: WebTypes.CId WebTypes.Wal -> Query Bool
isWalletRestoring walletId = do
    syncState <- getWalletSyncState walletId
    return $ case syncState of
         Nothing                  -> False
         Just (SyncedWith _)      -> False
         Just NotSynced           -> False
         Just (RestoringFrom _ _) -> True

-- | Get list of pending transactions related to given wallet.
getWalletPendingTxs :: WebTypes.CId WebTypes.Wal -> Query (Maybe [PendingTx])
getWalletPendingTxs wid =
    preview $ wsWalletInfos . ix wid . wsPendingTxs . to toList

-- | Given wallet ID and transaction ID, return corresponding pending transaction
-- (if it exists)
getPendingTx :: WebTypes.CId WebTypes.Wal -> TxId -> Query (Maybe PendingTx)
getPendingTx wid txId = preview $ wsWalletInfos . ix wid . wsPendingTxs . ix txId

-- | Legacy version of 'addCustomAddress2' for backwards compatibility on
--   the event log.
addCustomAddress :: CustomAddressType -> (WebTypes.CId WebTypes.Addr, HeaderHash) -> Update Bool
addCustomAddress t (addr, hh) = fmap isJust $ customAddressL t . at (unsafeCIdToAddress addr) <<.= Just hh

-- | If given address isn't yet present in set of used\/change addresses, then add it
-- with given block header hash.
addCustomAddress2 :: CustomAddressType -> (Address, HeaderHash) -> Update Bool
addCustomAddress2 t (addr, hh) = fmap isJust $ customAddressL t . at addr <<.= Just hh

-- | Legacy version of 'removeCustomAddress2' for backwards compatibility on
--   the event log.
removeCustomAddress :: CustomAddressType -> (WebTypes.CId WebTypes.Addr, HeaderHash) -> Update Bool
removeCustomAddress t (cwa, hh) = removeCustomAddress2 t (unsafeCIdToAddress cwa, hh)

-- | Remove given address from set of used\/change addresses only if provided
-- header hash is equal to one which is stored in database.
removeCustomAddress2 :: CustomAddressType -> (Address, HeaderHash) -> Update Bool
removeCustomAddress2 t (addr, hh) = do
    mhh' <- use $ customAddressL t . at addr
    let exists = mhh' == Just hh
    when exists $
        customAddressL t . at addr .= Nothing
    return exists

-- | Add given account to wallet db.
createAccount :: WebTypes.AccountId -> WebTypes.CAccountMeta -> Update ()
createAccount accId cAccMeta =
    wsAccountInfos . at accId %= Just . fromMaybe (AccountInfo cAccMeta mempty mempty 0)

-- | Create a new wallet with given parameters.
-- @isReady@ should be set to @False@ when syncing is still needed.
createWallet :: WebTypes.CId WebTypes.Wal -> WebTypes.CWalletMeta -> Bool -> POSIXTime -> Update ()
createWallet cWalId cWalMeta isReady curTime = do
    let info = WalletInfo cWalMeta curTime curTime NotSynced noSyncStatistics mempty isReady
    wsWalletInfos . at cWalId %= (<|> Just info)

-- | Add new address given 'CWAddressMeta' (which contains information about
-- target wallet and account too).
addWAddress2 :: WAddressMeta -> Update ()
addWAddress2 addrMeta = do
    let accInfo :: Traversal' WalletStorage AccountInfo
        accInfo = wsAccountInfos . ix (addrMeta ^. wamAccount)
        addr = addrMeta ^. wamAddress
    whenJustM (preuse accInfo) $ \info -> do
        let mAddr = info ^. aiAddresses . at addr
        when (isNothing mAddr) $ do
            -- Here we increment current account's last address index
            -- and assign its value to sorting index of newly created address.
            accInfo . aiUnusedKey += 1
            let key = info ^. aiUnusedKey
            accInfo . aiAddresses . at addr ?= AddressInfo addrMeta key

-- | Legacy version of 'addWAddress2' for backwards compatibility on the event
-- log.
addWAddress :: WebTypes.CWAddressMeta -> Update ()
addWAddress = addWAddress2 . cwamToWam

-- | Update account metadata.
setAccountMeta :: WebTypes.AccountId -> WebTypes.CAccountMeta -> Update ()
setAccountMeta accId cAccMeta = wsAccountInfos . ix accId . aiMeta .= cAccMeta

-- | Update wallet metadata.
setWalletMeta :: WebTypes.CId WebTypes.Wal -> WebTypes.CWalletMeta -> Update ()
setWalletMeta cWalId cWalMeta = wsWalletInfos . ix cWalId . wiMeta .= cWalMeta

-- | Change wallet ready status.
setWalletReady :: WebTypes.CId WebTypes.Wal -> Bool -> Update ()
setWalletReady cWalId isReady = wsWalletInfos . ix cWalId . wiIsReady .= isReady

-- | Update wallet's last password change time.
setWalletPassLU :: WebTypes.CId WebTypes.Wal -> WebTypes.PassPhraseLU -> Update ()
setWalletPassLU cWalId passLU = wsWalletInfos . ix cWalId . wiPassphraseLU .= passLU

-- | Declare that given wallet has been synced with blockchain up to block with given
-- header hash.
setWalletSyncTip :: WebTypes.CId WebTypes.Wal -> HeaderHash -> Update ()
setWalletSyncTip cWalId hh = wsWalletInfos . ix cWalId . wiSyncState .= SyncedWith hh

-- | Deliberately-verbose transaction to update the 'SyncState' for this wallet during a
-- restoration.
setWalletRestorationSyncTip :: WebTypes.CId WebTypes.Wal
                            -> RestorationBlockDepth
                            -> HeaderHash
                            -> Update ()
setWalletRestorationSyncTip cWalId rhh hh = wsWalletInfos . ix cWalId . wiSyncState .= RestoringFrom rhh hh

updateSyncStatistics :: WebTypes.CId WebTypes.Wal
                     -> SyncStatistics
                     -> Update ()
updateSyncStatistics cWalId stats = wsWalletInfos . ix cWalId . wiSyncStatistics .= stats

-- | Set meta data for transaction only if it hasn't been set already.
-- FIXME: this will be removed later (temporary solution) (not really =\)
addOnlyNewTxMeta :: WebTypes.CId WebTypes.Wal -> WebTypes.CTxId -> WebTypes.CTxMeta -> Update ()
addOnlyNewTxMeta cWalId cTxId cTxMeta =
    -- Double nested HashMap update (if either or both of cWalId, cTxId don't exist, they will be created)
    wsTxHistory . at cWalId . non' _Empty . at cTxId %= Just . fromMaybe cTxMeta

-- | Delete all transactions' metadata for given wallet.
removeTxMetas :: WebTypes.CId WebTypes.Wal -> Update ()
removeTxMetas cWalId = wsTxHistory . at cWalId .= Nothing

-- | 'addOnlyNewTxMeta' for several transactions at once.
addOnlyNewTxMetas :: WebTypes.CId WebTypes.Wal -> [(WebTypes.CTxId, WebTypes.CTxMeta)] -> Update ()
addOnlyNewTxMetas = mapM_ . uncurry . addOnlyNewTxMeta

-- | Delete transactions metadata by given wallet id and transactions ids.
removeWalletTxMetas :: WebTypes.CId WebTypes.Wal -> [WebTypes.CTxId] -> Update ()
removeWalletTxMetas cWalId cTxs =
    wsTxHistory . at cWalId . non' _Empty %= flip (foldr HM.delete) cTxs

-- | Delete the wallet info. This doesn't delete wallet's accounts, history cache
-- and transactions metadata.
removeWallet :: WebTypes.CId WebTypes.Wal -> Update ()
removeWallet cWalId = wsWalletInfos . at cWalId .= Nothing

-- | Delete all transaction history cache for given wallet.
removeHistoryCache :: WebTypes.CId WebTypes.Wal -> Update ()
removeHistoryCache cWalId = wsHistoryCache . at cWalId .= Nothing

-- | Delete given account.
removeAccount :: WebTypes.AccountId -> Update ()
removeAccount accId = wsAccountInfos . at accId .= Nothing

-- | Legacy version of 'removeWAddress2' for backwards compatibility on
--   the event log.
removeWAddress :: WebTypes.CWAddressMeta -> Update ()
removeWAddress = removeWAddress2 . cwamToWam

-- | Remove given address, not removing it completely, but marking it as `removed` instead.
-- See also 'addRemovedAccount'.
removeWAddress2 :: WAddressMeta -> Update ()
removeWAddress2 addrMeta@(view wamAccount -> accId) = do
    let addrId = addrMeta ^. wamAddress
    -- If the address exists, move it to 'addressesRemoved'
    whenJustM (preuse (accAddresses accId . ix addrId)) $ \addressInfo -> do
        accAddresses        accId . at addrId .= Nothing
        accRemovedAddresses accId . at addrId .= Just addressInfo
  where
    accAddresses        accId' = wsAccountInfos . ix accId' . aiAddresses
    accRemovedAddresses accId' = wsAccountInfos . ix accId' . aiRemovedAddresses

-- | Add info about new pending update to the end of update info list.
addUpdate :: WebTypes.CUpdateInfo -> Update ()
addUpdate ui = wsReadyUpdates %= (++ [ui])

-- | Remove next pending update info from update info list.
removeNextUpdate :: Update ()
removeNextUpdate = wsReadyUpdates %= drop 1

-- | Reset the whole database to clean state completely. Used only in testing and debugging.
testReset :: Update ()
testReset = put def

-- | Legacy transaction, no longer used. For existing Db tx logs only. Now use
-- 'removeHistoryCache', 'insertIntoHistoryCache' or 'removeFromHistoryCache'
updateHistoryCache :: WebTypes.CId WebTypes.Wal -> [TxHistoryEntry] -> Update ()
updateHistoryCache cWalId cTxs =
    wsHistoryCache . at cWalId ?= txHistoryListToMap cTxs

-- | Add new history entries to history cache of given wallet.
insertIntoHistoryCache :: WebTypes.CId WebTypes.Wal -> Map TxId TxHistoryEntry -> Update ()
insertIntoHistoryCache cWalId cTxs =
    wsHistoryCache . at cWalId . non' _Empty %= (cTxs `M.union`)

-- | Remove entries about transactions with given IDs from wallet's history cache.
removeFromHistoryCache :: WebTypes.CId WebTypes.Wal -> Map TxId () -> Update ()
removeFromHistoryCache cWalId cTxs =
    wsHistoryCache . at cWalId . non' _Empty %= (`M.difference` cTxs)

-- | Sets a resubmission condition for pending transaction.
-- This shouldn't be able to create new transaction.
-- NOTE: If you're going to use this function, make sure 'casPtxCondition'
-- doesn't fit your purposes better
setPtxCondition :: WebTypes.CId WebTypes.Wal -> TxId -> PtxCondition -> Update ()
setPtxCondition wid txId cond =
    wsWalletInfos . ix wid . wsPendingTxs . ix txId . ptxCond .= cond

-- | Conditional modifier.
-- Returns 'True' if pending transaction existed and modification was applied.
checkAndSmthPtx
    :: WebTypes.CId WebTypes.Wal
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
-- Returns 'True' if transaction existed and modification was applied.
casPtxCondition :: WebTypes.CId WebTypes.Wal -> TxId -> PtxCondition -> PtxCondition -> Update Bool
casPtxCondition wid txId expectedCond newCond =
    checkAndSmthPtx wid txId (== Just expectedCond) (_Just . ptxCond .= newCond)

-- | Removes pending transaction, if its status is 'PtxCreating'.
removeOnlyCreatingPtx :: WebTypes.CId WebTypes.Wal -> TxId -> Update Bool
removeOnlyCreatingPtx wid txId =
    checkAndSmthPtx wid txId (has (_Just . _PtxCreating)) (put Nothing)

-- | Datatype which defines how state of pending transaction should change.
data PtxMetaUpdate
    = PtxIncSubmitTiming          -- ^ Delay resubmission further
    | PtxResetSubmitTiming SlotId -- ^ Set next resubmission slot explicitely
    | PtxMarkAcknowledged         -- ^ Mark tx as acknowledged by some peer

-- | Update meta info of pending transaction atomically.
ptxUpdateMeta
    :: ProtocolConstants
    -> WebTypes.CId WebTypes.Wal
    -> TxId
    -> PtxMetaUpdate
    -> Update ()
ptxUpdateMeta pc wid txId updType =
    wsWalletInfos . ix wid . wsPendingTxs . ix txId %=
        case updType of
            PtxIncSubmitTiming ->
                ptxSubmitTiming %~ incPtxSubmitTimingPure pc
            PtxResetSubmitTiming curSlot ->
                ptxSubmitTiming .~ mkPtxSubmitTiming pc curSlot
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
resetFailedPtxs :: ProtocolConstants -> SlotId -> Update ()
resetFailedPtxs pc curSlot =
    wsWalletInfos . traversed .
    wsPendingTxs . traversed %= resetFailedPtx pc curSlot

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
    flushWalletInfo wi = wi { _wiSyncState = NotSynced
                            , _wiIsReady   = False
                            }

-- | Unsafe address conversion for use in migration. This will throw an error if
--   the address cannot be migrated.
unsafeCIdToAddress :: WebTypes.CId WebTypes.Addr -> Address
unsafeCIdToAddress cId = case WebTypes.cIdToAddress cId of
    Left err -> error $ "unsafeCIdToAddress: " <> err
    Right x  -> x

cwamToWam :: WebTypes.CWAddressMeta -> WAddressMeta
cwamToWam (WebTypes.CWAddressMeta wid accIdx addrIdx cAddr) =
    WAddressMeta wid accIdx addrIdx $ unsafeCIdToAddress cAddr

deriveSafeCopySimple 0 'base ''WebTypes.CCoin
deriveSafeCopySimple 0 'base ''WebTypes.CProfile
deriveSafeCopySimple 0 'base ''WebTypes.CHash
deriveSafeCopySimple 0 'base ''WebTypes.CId
deriveSafeCopySimple 0 'base ''WebTypes.Wal
deriveSafeCopySimple 0 'base ''WebTypes.Addr
deriveSafeCopySimple 0 'base ''BackupPhrase
deriveSafeCopySimple 0 'base ''WebTypes.AccountId
deriveSafeCopySimple 0 'base ''WebTypes.CWAddressMeta
deriveSafeCopySimple 0 'base ''WebTypes.CWalletAssurance
deriveSafeCopySimple 0 'base ''WebTypes.CAccountMeta
deriveSafeCopySimple 0 'base ''WebTypes.CWalletMeta
deriveSafeCopySimple 0 'base ''WebTypes.CTxId
deriveSafeCopySimple 0 'base ''Timestamp
deriveSafeCopySimple 0 'base ''TxHistoryEntry
deriveSafeCopySimple 0 'base ''WebTypes.CTxMeta
deriveSafeCopySimple 0 'base ''WebTypes.CUpdateInfo
deriveSafeCopySimple 0 'base ''AddressLookupMode
deriveSafeCopySimple 0 'base ''CustomAddressType
deriveSafeCopySimple 0 'base ''CurrentAndRemoved
deriveSafeCopySimple 0 'base ''TxAux
deriveSafeCopySimple 0 'base ''PtxCondition
deriveSafeCopySimple 0 'base ''PtxSubmitTiming
deriveSafeCopySimple 0 'base ''PtxMetaUpdate
deriveSafeCopySimple 0 'base ''PendingTx
deriveSafeCopySimple 0 'base ''WAddressMeta
deriveSafeCopySimple 0 'base ''RestorationBlockDepth
deriveSafeCopySimple 0 'base ''SyncThroughput
deriveSafeCopySimple 0 'base ''SyncStatistics
deriveSafeCopySimple 0 'base ''ProtocolConstants
deriveSafeCopySimple 0 'base ''VssMinTTL
deriveSafeCopySimple 0 'base ''VssMaxTTL

-- Legacy versions, for migrations

data WalletTip_v0
    = V0_NotSynced
    | V0_SyncedWith !HeaderHash
    deriving (Eq)

deriveSafeCopySimple 0 'base      ''WalletTip_v0
deriveSafeCopySimple 1 'extension ''WalletSyncState

instance Migrate WalletSyncState where
    type MigrateFrom WalletSyncState = WalletTip_v0
    migrate  V0_NotSynced     = NotSynced
    migrate (V0_SyncedWith h) = SyncedWith h

data WalletInfo_v0 = WalletInfo_v0
    { _v0_wiMeta         :: !WebTypes.CWalletMeta
    , _v0_wiPassphraseLU :: !WebTypes.PassPhraseLU
    , _v0_wiCreationTime :: !POSIXTime
    , _v0_wiSyncTip      :: !WalletTip_v0
    , _v0_wsPendingTxs   :: !(HashMap TxId PendingTx)
    , _v0_wiIsReady      :: !Bool
    }

instance Migrate WalletInfo where
    type MigrateFrom WalletInfo = WalletInfo_v0
    migrate WalletInfo_v0{..} = WalletInfo
        { _wiMeta           = _v0_wiMeta
        , _wiPassphraseLU   = _v0_wiPassphraseLU
        , _wiCreationTime   = _v0_wiCreationTime
        , _wiSyncState      = migrate _v0_wiSyncTip
        , _wiSyncStatistics = noSyncStatistics
        , _wsPendingTxs     = _v0_wsPendingTxs
        , _wiIsReady        = _v0_wiIsReady
        }

deriveSafeCopySimple 0 'base ''WalletInfo_v0
deriveSafeCopySimple 1 'extension ''WalletInfo

data AddressInfo_v0 = AddressInfo_v0
    { _v0_adiCWAddressMeta :: !WebTypes.CWAddressMeta
    , _v0_adiSortingKey    :: !AddressSortingKey
    }

type CAddresses_v0 = HashMap (WebTypes.CId WebTypes.Addr) AddressInfo_v0
type CustomAddresses_v0 = HashMap (WebTypes.CId WebTypes.Addr) HeaderHash

data AccountInfo_v0 = AccountInfo_v0
    { _v0_aiMeta             :: !WebTypes.CAccountMeta
    , _v0_aiAddresses        :: !CAddresses_v0
    , _v0_aiRemovedAddresses :: !CAddresses_v0
    , _v0_aiUnusedKey        :: !AddressSortingKey
    }

data WalletStorage_v0 = WalletStorage_v0
    { _v0_wsWalletInfos     :: !(HashMap (WebTypes.CId WebTypes.Wal) WalletInfo_v0)
    , _v0_wsAccountInfos    :: !(HashMap WebTypes.AccountId AccountInfo_v0)
    , _v0_wsProfile         :: !WebTypes.CProfile
    , _v0_wsReadyUpdates    :: [WebTypes.CUpdateInfo]
    , _v0_wsTxHistory       :: !(HashMap (WebTypes.CId WebTypes.Wal) (HashMap WebTypes.CTxId WebTypes.CTxMeta))
    , _v0_wsHistoryCache    :: !(HashMap (WebTypes.CId WebTypes.Wal) [TxHistoryEntry])
    , _v0_wsUtxo            :: !Utxo
    , _v0_wsUsedAddresses   :: !CustomAddresses_v0
    , _v0_wsChangeAddresses :: !CustomAddresses_v0
    }

data WalletStorage_v1 = WalletStorage_v1
    { _v1_wsWalletInfos     :: !(HashMap (WebTypes.CId WebTypes.Wal) WalletInfo_v0)
    , _v1_wsAccountInfos    :: !(HashMap WebTypes.AccountId AccountInfo_v0)
    , _v1_wsProfile         :: !WebTypes.CProfile
    , _v1_wsReadyUpdates    :: [WebTypes.CUpdateInfo]
    , _v1_wsTxHistory       :: !(HashMap (WebTypes.CId WebTypes.Wal) (HashMap WebTypes.CTxId WebTypes.CTxMeta))
    , _v1_wsHistoryCache    :: !(HashMap (WebTypes.CId WebTypes.Wal) (Map TxId TxHistoryEntry))
    , _v1_wsUtxo            :: !Utxo
    , _v1_wsUsedAddresses   :: !CustomAddresses_v0
    , _v1_wsChangeAddresses :: !CustomAddresses_v0
    }

data WalletStorage_v2 = WalletStorage_v2
    { _v2_wsWalletInfos     :: !(HashMap (WebTypes.CId WebTypes.Wal) WalletInfo_v0)
    , _v2_wsAccountInfos    :: !(HashMap WebTypes.AccountId AccountInfo_v0)
    , _v2_wsProfile         :: !WebTypes.CProfile
    , _v2_wsReadyUpdates    :: [WebTypes.CUpdateInfo]
    , _v2_wsTxHistory       :: !(HashMap (WebTypes.CId WebTypes.Wal) (HashMap WebTypes.CTxId WebTypes.CTxMeta))
    , _v2_wsHistoryCache    :: !(HashMap (WebTypes.CId WebTypes.Wal) (Map TxId TxHistoryEntry))
    , _v2_wsUtxo            :: !Utxo
    , _v2_wsBalances        :: !WalletBalances
    , _v2_wsUsedAddresses   :: !CustomAddresses_v0
    , _v2_wsChangeAddresses :: !CustomAddresses_v0
    }

data WalletStorage_v3 = WalletStorage_v3
    { _v3_wsWalletInfos     :: !(HashMap (WebTypes.CId WebTypes.Wal) WalletInfo_v0)
    , _v3_wsAccountInfos    :: !(HashMap WebTypes.AccountId AccountInfo)
    , _v3_wsProfile         :: !WebTypes.CProfile
    , _v3_wsReadyUpdates    :: [WebTypes.CUpdateInfo]
    , _v3_wsTxHistory       :: !(HashMap (WebTypes.CId WebTypes.Wal) (HashMap WebTypes.CTxId WebTypes.CTxMeta))
    , _v3_wsHistoryCache    :: !(HashMap (WebTypes.CId WebTypes.Wal) (Map TxId TxHistoryEntry))
    , _v3_wsUtxo            :: !Utxo
    , _v3_wsBalances        :: !WalletBalances
    , _v3_wsUsedAddresses   :: !CustomAddresses
    , _v3_wsChangeAddresses :: !CustomAddresses
    }

deriveSafeCopySimple 0 'base ''AddressInfo_v0
deriveSafeCopySimple 1 'extension ''AddressInfo

deriveSafeCopySimple 0 'base ''AccountInfo_v0
deriveSafeCopySimple 1 'extension ''AccountInfo

deriveSafeCopySimple 0 'base ''WalletStorage_v0
deriveSafeCopySimple 1 'extension ''WalletStorage_v1
deriveSafeCopySimple 2 'extension ''WalletStorage_v2
deriveSafeCopySimple 3 'extension ''WalletStorage_v3
deriveSafeCopySimple 4 'extension ''WalletStorage

instance Migrate AddressInfo where
    type MigrateFrom AddressInfo = AddressInfo_v0
    migrate AddressInfo_v0{..} = AddressInfo
        { adiWAddressMeta = cwamToWam _v0_adiCWAddressMeta
        , adiSortingKey = _v0_adiSortingKey
        }

instance Migrate AccountInfo where
    type MigrateFrom AccountInfo = AccountInfo_v0
    migrate AccountInfo_v0{..} = AccountInfo
        { _aiMeta = _v0_aiMeta
        , _aiAddresses = mapAddrs _v0_aiAddresses
        , _aiRemovedAddresses = mapAddrs _v0_aiRemovedAddresses
        , _aiUnusedKey = _v0_aiUnusedKey
        }
      where
        mapAddrs =
            HM.fromList
          . fmap (unsafeCIdToAddress *** migrate)
          . HM.toList

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

instance Migrate WalletStorage_v2 where
    type MigrateFrom WalletStorage_v2 = WalletStorage_v1
    migrate WalletStorage_v1{..} = WalletStorage_v2
        { _v2_wsWalletInfos     = _v1_wsWalletInfos
        , _v2_wsAccountInfos    = _v1_wsAccountInfos
        , _v2_wsProfile         = _v1_wsProfile
        , _v2_wsReadyUpdates    = _v1_wsReadyUpdates
        , _v2_wsTxHistory       = _v1_wsTxHistory
        , _v2_wsHistoryCache    = _v1_wsHistoryCache
        , _v2_wsUtxo            = _v1_wsUtxo
        , _v2_wsBalances        = utxoToAddressCoinMap _v1_wsUtxo
        , _v2_wsUsedAddresses   = _v1_wsUsedAddresses
        , _v2_wsChangeAddresses = _v1_wsChangeAddresses
        }

instance Migrate WalletStorage_v3 where
    type MigrateFrom WalletStorage_v3 = WalletStorage_v2
    migrate WalletStorage_v2{..} = WalletStorage_v3
        { _v3_wsWalletInfos     = _v2_wsWalletInfos
        , _v3_wsAccountInfos    = fmap migrate _v2_wsAccountInfos
        , _v3_wsProfile         = _v2_wsProfile
        , _v3_wsReadyUpdates    = _v2_wsReadyUpdates
        , _v3_wsTxHistory       = _v2_wsTxHistory
        , _v3_wsHistoryCache    = _v2_wsHistoryCache
        , _v3_wsUtxo            = _v2_wsUtxo
        , _v3_wsBalances        = _v2_wsBalances
        , _v3_wsUsedAddresses   = mapAddrKeys _v2_wsUsedAddresses
        , _v3_wsChangeAddresses = mapAddrKeys _v2_wsChangeAddresses
        }
      where
        mapAddrKeys  = HM.fromList . fmap (first unsafeCIdToAddress) . HM.toList

instance Migrate WalletStorage where
    type MigrateFrom WalletStorage = WalletStorage_v3
    migrate WalletStorage_v3{..} = WalletStorage
        { _wsWalletInfos     = migrateMapElements _v3_wsWalletInfos
        , _wsAccountInfos    = _v3_wsAccountInfos
        , _wsProfile         = _v3_wsProfile
        , _wsReadyUpdates    = _v3_wsReadyUpdates
        , _wsTxHistory       = _v3_wsTxHistory
        , _wsHistoryCache    = _v3_wsHistoryCache
        , _wsUtxo            = _v3_wsUtxo
        , _wsBalances        = _v3_wsBalances
        , _wsUsedAddresses   = _v3_wsUsedAddresses
        , _wsChangeAddresses = _v3_wsChangeAddresses
        }
      where
        migrateMapElements = HM.fromList . fmap (second migrate) . HM.toList
