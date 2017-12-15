{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- @jens: this document is inspired by https://github.com/input-output-hk/rscoin-haskell/blob/master/src/RSCoin/Explorer/Storage.hs
module Pos.Wallet.Web.State.Storage
       (
         WalletStorage (..)
       , WalletInfo (..)
       , AccountInfo (..)
       , AddressInfo (..)
       , AddressLookupMode (..)
       , CustomAddressType (..)
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
       , getAccountMetas
       , getAccountMeta
       , getWalletMetas
       , getWalletMeta
       , getWalletMetaIncludeUnready
       , getWalletPassLU
       , getWalletSyncTip
       , getWalletAddresses
       , getAccountWAddresses
       , doesWAddressExist
       , getTxMeta
       , getUpdates
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
       , addRemovedAccount
       , setAccountMeta
       , setWalletMeta
       , setWalletReady
       , setWalletPassLU
       , setWalletSyncTip
       , setWalletTxHistory
       , getWalletTxHistory
       , getWalletUtxo
       , getWalletBalancesAndUtxo
       , updateWalletBalancesAndUtxo
       , setWalletUtxo
       , addOnlyNewTxMeta
       , setWalletTxMeta
       , addOnlyNewTxMetas
       , removeWallet
       , removeWalletTxMetas
       , removeTxMetas
       , removeHistoryCache
       , removeAccount
       , removeWAddress
       , totallyRemoveWAddress
       , addUpdate
       , removeNextUpdate
       , testReset
       , updateHistoryCache
       , insertIntoHistoryCache
       , removeFromHistoryCache
       , setPtxCondition
       , casPtxCondition
       , ptxUpdateMeta
       , addOnlyNewPendingTx
       ) where

import           Universum

import           Control.Lens                   (at, ix, makeClassy, makeLenses, non', to,
                                                 toListOf, traversed, (%=), (+=), (.=),
                                                 (<<.=), (?=), _Empty, _head)
import           Control.Monad.State.Class      (put)
import           Data.Default                   (Default, def)
import qualified Data.HashMap.Strict            as HM
import qualified Data.Map                       as M
import           Data.SafeCopy                  (Migrate (..), base, deriveSafeCopySimple,
                                                 extension)
import           Data.Time.Clock.POSIX          (POSIXTime)

import           Pos.Client.Txp.History         (TxHistoryEntry, txHistoryListToMap)
import           Pos.Core.Configuration         (HasConfiguration)
import           Pos.Core.Types                 (SlotId, Timestamp)
import           Pos.Txp                        (AddrCoinMap, TxAux, TxId, Utxo,
                                                 UtxoModifier, applyUtxoModToAddrCoinMap,
                                                 utxoToAddressCoinMap)
import           Pos.Types                      (HeaderHash)
import           Pos.Util.BackupPhrase          (BackupPhrase)
import qualified Pos.Util.Modifier              as MM
import           Pos.Wallet.Web.ClientTypes     (AccountId, Addr, CAccountMeta, CCoin,
                                                 CHash, CId, CProfile, CTxId, CTxMeta,
                                                 CUpdateInfo, CWAddressMeta (..),
                                                 CWalletAssurance, CWalletMeta,
                                                 PassPhraseLU, Wal, addrMetaToAccount)
import           Pos.Wallet.Web.Pending.Types   (PendingTx (..), PtxCondition,
                                                 PtxSubmitTiming (..), ptxCond,
                                                 ptxSubmitTiming)
import           Pos.Wallet.Web.Pending.Updates (incPtxSubmitTimingPure,
                                                 mkPtxSubmitTiming,
                                                 ptxMarkAcknowledgedPure)

type AddressSortingKey = Int

data AddressInfo = AddressInfo
    { adiCWAddressMeta :: !CWAddressMeta
    , adiSortingKey    :: !AddressSortingKey
    }

type CAddresses = HashMap (CId Addr) AddressInfo

data AccountInfo = AccountInfo
    { _aiMeta             :: !CAccountMeta
    , _aiAddresses        :: !CAddresses
    , _aiRemovedAddresses :: !CAddresses
    , _aiUnusedKey        :: !AddressSortingKey
    }

makeLenses ''AccountInfo

data WalletTip
    = NotSynced
    | SyncedWith !HeaderHash

data WalletInfo = WalletInfo
    { _wiMeta         :: !CWalletMeta
    , _wiPassphraseLU :: !PassPhraseLU
    , _wiCreationTime :: !POSIXTime
    , _wiSyncTip      :: !WalletTip
    , _wsPendingTxs   :: !(HashMap TxId PendingTx)
    -- Wallets that are being synced are marked as not ready, and
    -- are excluded from api endpoints. This info should not be leaked
    -- into a client facing data structure (for example `CWalletMeta`)
    , _wiIsReady      :: !Bool
    }

makeLenses ''WalletInfo

-- | Maps addresses to their first occurrence in the blockchain
type CustomAddresses = HashMap (CId Addr) HeaderHash
type WalletBalances = AddrCoinMap
type WalBalancesAndUtxo = (WalletBalances, Utxo)

data WalletStorage = WalletStorage
    { _wsWalletInfos     :: !(HashMap (CId Wal) WalletInfo)
    , _wsAccountInfos    :: !(HashMap AccountId AccountInfo)
    , _wsProfile         :: !CProfile
    , _wsReadyUpdates    :: [CUpdateInfo]
    , _wsTxHistory       :: !(HashMap (CId Wal) (HashMap CTxId CTxMeta))
    , _wsHistoryCache    :: !(HashMap (CId Wal) (Map TxId TxHistoryEntry))
    , _wsUtxo            :: !Utxo
    -- @_wsBalances@ depends on @_wsUtxo@,
    -- it's forbidden to update @_wsBalances@ without @_wsUtxo@
    , _wsBalances        :: !WalletBalances
    , _wsUsedAddresses   :: !CustomAddresses
    , _wsChangeAddresses :: !CustomAddresses
    }

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

withAccLookupMode :: (Monad m, Monoid a) => AddressLookupMode -> m a -> m a -> m a
withAccLookupMode Existing existing _       = existing
withAccLookupMode Deleted  _        deleted = deleted
withAccLookupMode Ever     existing deleted = mappend <$> existing <*> deleted

-- | Specifies special category of addresses which are stored in base.
data CustomAddressType
    = UsedAddr
    | ChangeAddr

customAddressL :: CustomAddressType -> Lens' WalletStorage CustomAddresses
customAddressL UsedAddr   = wsUsedAddresses
customAddressL ChangeAddr = wsChangeAddresses

getProfile :: Query CProfile
getProfile = view wsProfile

setProfile :: CProfile -> Update ()
setProfile cProfile = wsProfile .= cProfile

doesAccountExist :: AccountId -> Query Bool
doesAccountExist accId = view $ wsAccountInfos . at accId . to isJust

getAccountIds :: Query [AccountId]
getAccountIds = HM.keys <$> view wsAccountInfos

getAccountMetas :: Query [CAccountMeta]
getAccountMetas = map (view aiMeta) . toList <$> view wsAccountInfos

getAccountMeta :: AccountId -> Query (Maybe CAccountMeta)
getAccountMeta accId = preview (wsAccountInfos . ix accId . aiMeta)

getWalletMetas :: Query [CWalletMeta]
getWalletMetas = toList . fmap _wiMeta . HM.filter _wiIsReady <$> view wsWalletInfos

getWalletMetaIncludeUnready :: Bool -> CId Wal -> Query (Maybe CWalletMeta)
getWalletMetaIncludeUnready includeUnready cWalId = fmap _wiMeta . applyFilter <$> preview (wsWalletInfos . ix cWalId)
  where
    applyFilter xs = if includeUnready then xs else filterMaybe _wiIsReady xs
    filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
    filterMaybe p ma = ma >>= \a -> guard (p a) >> return a

getWalletMeta :: CId Wal -> Query (Maybe CWalletMeta)
getWalletMeta = getWalletMetaIncludeUnready False

getWalletPassLU :: CId Wal -> Query (Maybe PassPhraseLU)
getWalletPassLU cWalId = preview (wsWalletInfos . ix cWalId . wiPassphraseLU)

getWalletSyncTip :: CId Wal -> Query (Maybe WalletTip)
getWalletSyncTip cWalId = preview (wsWalletInfos . ix cWalId . wiSyncTip)

getWalletAddresses :: Query [CId Wal]
getWalletAddresses =
    map fst . sortOn (view wiCreationTime . snd) . filter (view wiIsReady . snd) . HM.toList <$>
    view wsWalletInfos

getAccountWAddresses :: AddressLookupMode
                  -> AccountId
                  -> Query (Maybe [CWAddressMeta])
getAccountWAddresses mode accId =
    withAccLookupMode mode (fetch aiAddresses) (fetch aiRemovedAddresses)
  where
    fetch :: MonadReader WalletStorage m => Lens' AccountInfo CAddresses -> m (Maybe [CWAddressMeta])
    fetch which = do
        cAddresses <- preview (wsAccountInfos . ix accId . which)
        -- here `cAddresses` has type `Maybe CAddresses`
        pure $
            (map adiCWAddressMeta . sortOn adiSortingKey . map snd . HM.toList)
            <$> cAddresses

doesWAddressExist :: AddressLookupMode -> CWAddressMeta -> Query Bool
doesWAddressExist mode addrMeta@(addrMetaToAccount -> wAddr) =
    getAny <$>
        withAccLookupMode mode (exists aiAddresses) (exists aiRemovedAddresses)
  where
    exists :: Lens' AccountInfo CAddresses -> Query Any
    exists which =
        Any . isJust <$>
        preview (wsAccountInfos . ix wAddr . which . ix (cwamId addrMeta))

getTxMeta :: CId Wal -> CTxId -> Query (Maybe CTxMeta)
getTxMeta cid ctxId = preview $ wsTxHistory . ix cid . ix ctxId

getWalletTxHistory :: CId Wal -> Query (Maybe [CTxMeta])
getWalletTxHistory cWalId = toList <<$>> preview (wsTxHistory . ix cWalId)

getWalletUtxo :: Query Utxo
getWalletUtxo = view wsUtxo

getWalletBalancesAndUtxo :: Query WalBalancesAndUtxo
getWalletBalancesAndUtxo = (,) <$> view wsBalances <*> view wsUtxo

updateWalletBalancesAndUtxo :: UtxoModifier -> Update ()
updateWalletBalancesAndUtxo modifier = do
    balAndUtxo <- (,) <$> use wsBalances <*> use wsUtxo
    wsBalances .= applyUtxoModToAddrCoinMap modifier balAndUtxo
    wsUtxo %= MM.modifyMap modifier

setWalletUtxo :: Utxo -> Update ()
setWalletUtxo utxo = do
    wsUtxo .= utxo
    wsBalances .= utxoToAddressCoinMap utxo

getUpdates :: Query [CUpdateInfo]
getUpdates = view wsReadyUpdates

getNextUpdate :: Query (Maybe CUpdateInfo)
getNextUpdate = preview (wsReadyUpdates . _head)

getHistoryCache :: CId Wal -> Query (Maybe (Map TxId TxHistoryEntry))
getHistoryCache cWalId = view $ wsHistoryCache . at cWalId

getCustomAddresses :: CustomAddressType -> Query [CId Addr]
getCustomAddresses t = HM.keys <$> view (customAddressL t)

getCustomAddress :: CustomAddressType -> CId Addr -> Query (Maybe HeaderHash)
getCustomAddress t addr = view $ customAddressL t . at addr

getPendingTxs :: Query [PendingTx]
getPendingTxs = asks $ toListOf (wsWalletInfos . traversed . wsPendingTxs . traversed)

getWalletPendingTxs :: CId Wal -> Query (Maybe [PendingTx])
getWalletPendingTxs wid =
    preview $ wsWalletInfos . ix wid . wsPendingTxs . to toList

getPendingTx :: CId Wal -> TxId -> Query (Maybe PendingTx)
getPendingTx wid txId = preview $ wsWalletInfos . ix wid . wsPendingTxs . ix txId

addCustomAddress :: CustomAddressType -> (CId Addr, HeaderHash) -> Update Bool
addCustomAddress t (addr, hh) = fmap isJust $ customAddressL t . at addr <<.= Just hh

removeCustomAddress :: CustomAddressType -> (CId Addr, HeaderHash) -> Update Bool
removeCustomAddress t (addr, hh) = do
    mhh' <- use $ customAddressL t . at addr
    let exists = mhh' == Just hh
    when exists $
        customAddressL t . at addr .= Nothing
    return exists

createAccount :: AccountId -> CAccountMeta -> Update ()
createAccount accId cAccMeta =
    wsAccountInfos . at accId %= Just . fromMaybe (AccountInfo cAccMeta mempty mempty 0)

-- `isReady` is marked False when an additional step such as syncing is still needed.
createWallet :: CId Wal -> CWalletMeta -> Bool -> POSIXTime -> Update ()
createWallet cWalId cWalMeta isReady curTime = do
    let info = WalletInfo cWalMeta curTime curTime NotSynced mempty isReady
    wsWalletInfos . at cWalId %= (<|> Just info)

addWAddress :: CWAddressMeta -> Update ()
addWAddress addrMeta@CWAddressMeta{..} = do
    let accInfo :: Traversal' WalletStorage AccountInfo
        accInfo = wsAccountInfos . ix (addrMetaToAccount addrMeta)
    whenJustM (preuse accInfo) $ \info -> do
        let mAddr = info ^. aiAddresses . at cwamId
        when (isNothing mAddr) $ do
            accInfo . aiUnusedKey += 1
            let key = info ^. aiUnusedKey
            accInfo . aiAddresses . at cwamId ?= AddressInfo addrMeta key

-- see also 'removeWAddress'
addRemovedAccount :: CWAddressMeta -> Update ()
addRemovedAccount addrMeta@CWAddressMeta{..} = do
    let accInfo :: Traversal' WalletStorage AccountInfo
        accInfo = wsAccountInfos . ix (addrMetaToAccount addrMeta)
    whenJustM (preuse (accInfo . aiUnusedKey)) $ \key -> do
        accInfo . aiUnusedKey += 1
        accInfo . aiAddresses        . at cwamId .= Nothing
        accInfo . aiRemovedAddresses . at cwamId ?= AddressInfo addrMeta key

setAccountMeta :: AccountId -> CAccountMeta -> Update ()
setAccountMeta accId cAccMeta = wsAccountInfos . ix accId . aiMeta .= cAccMeta

setWalletMeta :: CId Wal -> CWalletMeta -> Update ()
setWalletMeta cWalId cWalMeta = wsWalletInfos . ix cWalId . wiMeta .= cWalMeta

setWalletReady :: CId Wal -> Bool -> Update ()
setWalletReady cWalId isReady = wsWalletInfos . ix cWalId . wiIsReady .= isReady

setWalletPassLU :: CId Wal -> PassPhraseLU -> Update ()
setWalletPassLU cWalId passLU = wsWalletInfos . ix cWalId . wiPassphraseLU .= passLU

setWalletSyncTip :: CId Wal -> HeaderHash -> Update ()
setWalletSyncTip cWalId hh = wsWalletInfos . ix cWalId . wiSyncTip .= SyncedWith hh

addWalletTxHistory :: CId Wal -> CTxId -> CTxMeta -> Update ()
addWalletTxHistory cWalId cTxId cTxMeta =
    wsTxHistory . at cWalId . non' _Empty . at cTxId ?= cTxMeta

setWalletTxHistory :: CId Wal -> [(CTxId, CTxMeta)] -> Update ()
setWalletTxHistory cWalId cTxs = mapM_ (uncurry $ addWalletTxHistory cWalId) cTxs

-- FIXME: this will be removed later (temporary solution)
addOnlyNewTxMeta :: CId Wal -> CTxId -> CTxMeta -> Update ()
addOnlyNewTxMeta cWalId cTxId cTxMeta =
    -- Double nested HashMap update (if either or both of cWalId, cTxId don't exist, they will be created)
    wsTxHistory . at cWalId . non' _Empty . at cTxId %= Just . fromMaybe cTxMeta

-- NOTE: sets transaction meta only for transactions ids that are already seen
setWalletTxMeta :: CId Wal -> CTxId -> CTxMeta -> Update ()
setWalletTxMeta cWalId cTxId cTxMeta =
    wsTxHistory . ix cWalId . at cTxId %= ($> cTxMeta)

removeTxMetas :: CId Wal -> Update ()
removeTxMetas cWalId = wsTxHistory . at cWalId .= Nothing

addOnlyNewTxMetas :: CId Wal -> [(CTxId, CTxMeta)] -> Update ()
addOnlyNewTxMetas = mapM_ . uncurry . addOnlyNewTxMeta

removeWalletTxMetas :: CId Wal -> [CTxId] -> Update ()
removeWalletTxMetas cWalId cTxs =
    wsTxHistory . at cWalId . non' _Empty %= flip (foldr HM.delete) cTxs

removeWallet :: CId Wal -> Update ()
removeWallet cWalId = wsWalletInfos . at cWalId .= Nothing

removeHistoryCache :: CId Wal -> Update ()
removeHistoryCache cWalId = wsHistoryCache . at cWalId .= Nothing

removeAccount :: AccountId -> Update ()
removeAccount accId = wsAccountInfos . at accId .= Nothing

-- see also 'addRemovedAccount'
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

totallyRemoveWAddress :: CWAddressMeta -> Update ()
totallyRemoveWAddress addrMeta@(addrMetaToAccount -> accId) = do
    wsAccountInfos . ix accId . aiAddresses        . at (cwamId addrMeta) .= Nothing
    wsAccountInfos . ix accId . aiRemovedAddresses . at (cwamId addrMeta) .= Nothing

addUpdate :: CUpdateInfo -> Update ()
addUpdate ui = wsReadyUpdates %= (++ [ui])

removeNextUpdate :: Update ()
removeNextUpdate = wsReadyUpdates %= drop 1

testReset :: Update ()
testReset = put def

-- Legacy transaction, no longer used. For existing Db tx logs only. Now use
-- 'removeHistoryCache', 'insertIntoHistoryCache' or 'removeFromHistoryCache'
updateHistoryCache :: CId Wal -> [TxHistoryEntry] -> Update ()
updateHistoryCache cWalId cTxs =
    wsHistoryCache . at cWalId ?= txHistoryListToMap cTxs

insertIntoHistoryCache :: CId Wal -> Map TxId TxHistoryEntry -> Update ()
insertIntoHistoryCache cWalId cTxs =
    wsHistoryCache . at cWalId . non' _Empty %= (cTxs `M.union`)

removeFromHistoryCache :: CId Wal -> Map TxId () -> Update ()
removeFromHistoryCache cWalId cTxs =
    wsHistoryCache . at cWalId . non' _Empty %= (`M.difference` cTxs)

-- This shouldn't be able to create new transaction.
-- NOTE: If you're going to use this function, make sure 'casPtxCondition'
-- doesn't fit your purposes better
setPtxCondition :: CId Wal -> TxId -> PtxCondition -> Update ()
setPtxCondition wid txId cond =
    wsWalletInfos . ix wid . wsPendingTxs . ix txId . ptxCond .= cond

-- | Compare-and-set version of 'setPtxCondition'.
-- Returns 'True' if transaction existed and modification was applied.
casPtxCondition :: CId Wal -> TxId -> PtxCondition -> PtxCondition -> Update Bool
casPtxCondition wid txId expectedCond newCond = do
    oldCond <- preuse $ wsWalletInfos . ix wid . wsPendingTxs . ix txId . ptxCond
    let success = oldCond == Just expectedCond
    when success $ setPtxCondition wid txId newCond
    return success

data PtxMetaUpdate
    = PtxIncSubmitTiming
    | PtxResetSubmitTiming SlotId
    | PtxMarkAcknowledged

-- | For simple atomic updates of meta info
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

addOnlyNewPendingTx :: PendingTx -> Update ()
addOnlyNewPendingTx ptx =
    wsWalletInfos . ix (_ptxWallet ptx) .
    wsPendingTxs . at (_ptxTxId ptx) %= (<|> Just ptx)


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
