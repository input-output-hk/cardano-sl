{-# LANGUAGE TypeFamilies #-}

module Pos.Wallet.Web.State.State
       ( WalletState
       , WalletTip (..)
       , PtxMetaUpdate (..)
       , getWalletWebState
       , MonadWalletDBAccess
       , MonadWalletDBRead (..)
       , MonadWalletDBMempoolRead (..)
       , MonadWalletDB
       , DBWalletStorage (..)
       , UnionWalletStorage (..)
       , getDBWalletStorageWebWallet
       , getWalletStoragesWebWallet
       , openState
       , openMemState
       , closeState
       , waitWalletsCons

       , AddressLookupMode (..)
       , CustomAddressType (..)

       -- * Getters
       , getProfile
       , getWalletAccountIds
       , getAccountMetas
       , getAccountMeta
       , getAccountWAddresses
       , getWalletWAddresses
       , getWalletMetas
       , getWalletMeta
       , getWalletMetaIncludeUnready
       , getWalletPassLU
       , getWalletSyncTip
       , getWalletIds
       , doesWAddressExist
       , getTxMeta
       , getWalletTxHistory
       , getUpdates
       , getNextUpdate
       , getHistoryCache
       , isCustomAddress
       , getCustomAddressesDB
       , getWalletUtxo
       , getWalletBalancesAndUtxo
       , updateWalletBalancesAndUtxo
       , getPendingTxs
       , getWalletPendingTxs
       , getPendingTx

       -- * Setters
       , testReset
       , createAccount
       , createWallet
       , addWAddress
       , setProfile
       , setAccountMeta
       , setWalletMeta
       , setWalletReady
       , setWalletPassLU
       , setWalletSyncTip
       , setWalletTxMeta
       , addOnlyNewTxMetas
       , addOnlyNewTxMeta
       , removeWallet
       , removeTxMetas
       , removeHistoryCache
       , removeAccount
       , totallyRemoveWAddress
       , addUpdate
       , removeNextUpdate
       , casPtxCondition
       , removeOnlyCreatingPtx
       , ptxUpdateMeta
       , addOnlyNewPendingTx
       , resetFailedPtxs
       , flushWalletStorage
       , applyModifierToWallet
       , applyModifierToWallets
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Data.Acid (EventResult, EventState, QueryEvent, UpdateEvent)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import           Ether.Internal (lensOf)

import           Pos.Client.Txp.History (TxHistoryEntry)
import           Pos.Core (HeaderHash, SlotId)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Txp (TxId, Utxo, UtxoModifier)
import           Pos.Util.Servant (encodeCType)
import           Pos.Util.Util (HasLens')
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CAccountMeta, CId, CProfile,
                                             CTxId, CTxMeta, CUpdateInfo, CWAddressMeta (..),
                                             CWalletMeta, PassPhraseLU, Wal)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition)
import           Pos.Wallet.Web.State.Acidic (WalletState, closeState, openMemState, openState)
import           Pos.Wallet.Web.State.Acidic as A
import           Pos.Wallet.Web.State.Memory.Types (ExtStorageModifier (..), ExtStorageModifierVar,
                                                    HasExtStorageModifier, StorageModifier (..))
import           Pos.Wallet.Web.State.Storage (AddressLookupMode (..), CustomAddressType (..),
                                               PtxMetaUpdate (..), WalletBalances, WalletInfo (..),
                                               WalletStorage (..), WalletTip (..))
import qualified Pos.Wallet.Web.State.Storage as S
import           Pos.Wallet.Web.Tracking.Modifier (WalletModifier (..))

----------------------------------------------------------------------------
-- MonadWalletDBRead and MonadWalletDBMempoolRead classes
----------------------------------------------------------------------------

-- | Wrapper around persistent WalletStorage
newtype DBWalletStorage = DBWalletStorage
    { unDBWalletStorage :: WalletStorage
    }

-- | Get persistent WalletStorage
class Monad m => MonadWalletDBRead m where
    getDBWalletStorage :: m DBWalletStorage

performDBReadAction
    :: MonadWalletDBRead m
    => Reader WalletStorage a
    -> m a
performDBReadAction act = runReader act . unDBWalletStorage <$> getDBWalletStorage

-- | Wrapper around union of persistent WalletStorage and mempool
newtype UnionWalletStorage = UnionWalletStorage
    { unUnionWalletStorage :: WalletStorage
    }

-- | Typeclass provides ability to get WalletStorage with applied mempool.
-- It's needed to read parts of WalletStorage and
-- perform several read actions in one snapshot.
-- Method returns DBWalletStorage and corresponding UnionWalletStorage.
class MonadWalletDBRead m => MonadWalletDBMempoolRead m where
    getWalletStorages :: m (DBWalletStorage, UnionWalletStorage)

getUnionWalletStorage :: MonadWalletDBMempoolRead m => m UnionWalletStorage
getUnionWalletStorage = snd <$> getWalletStorages

performReadAction
    :: MonadWalletDBMempoolRead m
    => Reader WalletStorage a
    -> m a
performReadAction act = runReader act . unUnionWalletStorage <$> getUnionWalletStorage

----------------------------------------------------------------------------
-- Acidic specific constraints
----------------------------------------------------------------------------

-- | No read or write, just access to state handler
type MonadWalletDBAccess ctx m =
    ( HasLens' ctx WalletState
    , MonadReader ctx m
    )

type MonadWalletDB ctx m =
    ( MonadWalletDBAccess ctx m
    , MonadWalletDBMempoolRead m
    , MonadIO m
    , HasConfiguration
    )

getWalletWebState :: MonadWalletDBAccess ctx m => m WalletState
getWalletWebState = view (lensOf @WalletState)

queryDisk
    :: (EventState event ~ WalletStorage, QueryEvent event, MonadWalletDBAccess ctx m, MonadIO m)
    => event -> m (EventResult event)
queryDisk e = getWalletWebState >>= flip A.query e

updateDisk
    :: (EventState event ~ WalletStorage, UpdateEvent event, MonadWalletDB ctx m)
    => event -> m (EventResult event)
updateDisk e = getWalletWebState >>= flip A.update e

----------------------------------------------------------------------------
-- Primitives for synchronization with wallet-db
----------------------------------------------------------------------------

type MonadWalletDBReadWithMempool ctx m
    = ( MonadWalletDBRead m
      , HasExtStorageModifier ctx
      , MonadReader ctx m
      , MonadIO m
      , HasConfiguration
      )

-- | Waits until wallets' modifiers are consistent with wallet-db.
-- Returns persistent WalletStorage and WalletStorage along with applied mempool.
waitWalletsCons
    :: MonadWalletDBReadWithMempool ctx m
    => m (DBWalletStorage, UnionWalletStorage)
waitWalletsCons = do
    storage <- unDBWalletStorage <$> getDBWalletStorage
    let walletIds = runReader S.getWalletIds storage
    let wTips = mapMaybe (\wid -> _wiSyncTip <$> HM.lookup wid (_wsWalletInfos storage)) walletIds
    modifierVar <- view (lensOf @ExtStorageModifierVar)
    ExtStorageModifier{..} <- atomically $ STM.readTMVar modifierVar
    if all (SyncedWith esmTip ==) wTips then do
        -- Case when modifier's tip and db's tips are the same
        let modifiers = getStorageModifier esmMemStorageModifier
        let modifiersL =
                zip walletIds $
                map (\wal -> HM.lookupDefault mempty wal modifiers) walletIds
        pure $ (DBWalletStorage storage,
                UnionWalletStorage $ execState (S.applyModifierToWallets esmTip modifiersL) storage)
    else
        -- Similar to case from @waitWalletCons@.
        waitWalletsCons

----------------------------------------------------------------------------
-- Default implementations for MonadWalletDBRead and MonadWalletDBMempoolRead
----------------------------------------------------------------------------

getDBWalletStorageWebWallet :: (MonadWalletDBAccess ctx m, MonadIO m) => m DBWalletStorage
getDBWalletStorageWebWallet = DBWalletStorage <$> queryDisk A.GetWalletStorage

getWalletStoragesWebWallet
    :: MonadWalletDBReadWithMempool ctx m => m (DBWalletStorage, UnionWalletStorage)
getWalletStoragesWebWallet = waitWalletsCons

----------------------------------------------------------------------------
-- Query operations affected by mempool
----------------------------------------------------------------------------

getWalletAccountIds
    :: MonadWalletDBMempoolRead m
    => CId Wal -> m [AccountId]
getWalletAccountIds wal = performReadAction (S.getWalletAccountIds wal)

getHistoryCache
    :: MonadWalletDBMempoolRead m
    => CId Wal -> m (Maybe (Map TxId TxHistoryEntry))
getHistoryCache wal = performReadAction (S.getHistoryCache wal)

getAccountWAddresses
    :: MonadWalletDBMempoolRead m
    => AddressLookupMode -> AccountId -> m (Maybe [CWAddressMeta])
getAccountWAddresses mode accId@AccountId{..} =
    performReadAction (S.getAccountWAddresses mode accId)

getWalletWAddresses
    :: MonadWalletDBMempoolRead m
    => AddressLookupMode -> CId Wal -> m (Maybe [CWAddressMeta])
getWalletWAddresses mode wid =
    performReadAction (S.getWalletWAddresses mode wid)

doesWAddressExist
    :: MonadWalletDBMempoolRead m
    => AddressLookupMode -> CWAddressMeta -> m Bool
doesWAddressExist mode addrId@CWAddressMeta{..} =
    performReadAction (S.doesWAddressExist mode addrId)

getAccountMeta :: MonadWalletDBMempoolRead m => AccountId -> m (Maybe CAccountMeta)
getAccountMeta accId@AccountId{..} =
    performReadAction (S.getAccountMeta accId)

getTxMeta :: MonadWalletDBMempoolRead m => CId Wal -> CTxId -> m (Maybe CTxMeta)
getTxMeta walId txId =
    performReadAction (S.getTxHistoryMeta walId txId)

getWalletTxHistory :: MonadWalletDBMempoolRead m => CId Wal -> m (Maybe [CTxMeta])
getWalletTxHistory walId =
    performReadAction (S.getWalletTxHistoryMetas walId)

-- Operations below which wait for synchronization of all wallets.
-- Some of them should depend on CId Wal, but we can't do it without wallet-db migration
-- which requires secret keys (it can't be performed in SafeCopy's Migration typeclass).

getAccountMetas :: MonadWalletDBMempoolRead m => m [CAccountMeta]
getAccountMetas =
    performReadAction S.getAccountMetas

isCustomAddress :: MonadWalletDBMempoolRead m => CustomAddressType -> CId Addr -> m Bool
isCustomAddress cType addrId =
    performReadAction (fmap isJust $ S.getCustomAddress cType addrId)

getWalletUtxo :: MonadWalletDBMempoolRead m => m Utxo
getWalletUtxo = performReadAction S.getWalletUtxo

getWalletBalancesAndUtxo :: MonadWalletDBMempoolRead m => m (WalletBalances, Utxo)
getWalletBalancesAndUtxo = performReadAction S.getWalletBalancesAndUtxo

----------------------------------------------------------------------------
-- Query operations not affected by mempool
----------------------------------------------------------------------------

getWalletIds :: MonadWalletDBRead m => m [CId Wal]
getWalletIds = performDBReadAction S.getWalletIds

getCustomAddressesDB :: MonadWalletDBMempoolRead m => CustomAddressType -> m [(CId Addr, HeaderHash)]
getCustomAddressesDB cType = performDBReadAction (S.getCustomAddresses cType)

getWalletMetaIncludeUnready :: MonadWalletDBRead m => Bool -> CId Wal -> m (Maybe CWalletMeta)
getWalletMetaIncludeUnready includeReady wid =
    performDBReadAction (S.getWalletMetaIncludeUnready includeReady wid)

getWalletMeta :: MonadWalletDBRead m => CId Wal -> m (Maybe CWalletMeta)
getWalletMeta wid = performDBReadAction (S.getWalletMeta wid)

getWalletMetas :: MonadWalletDBRead m => m [CWalletMeta]
getWalletMetas = performDBReadAction S.getWalletMetas

getWalletPassLU :: MonadWalletDBRead m => CId Wal -> m (Maybe PassPhraseLU)
getWalletPassLU wid = performDBReadAction (S.getWalletPassLU wid)

getWalletSyncTip :: MonadWalletDBRead m => CId Wal -> m (Maybe WalletTip)
getWalletSyncTip wid = performDBReadAction (S.getWalletSyncTip wid)

getProfile :: MonadWalletDBRead m => m CProfile
getProfile = performDBReadAction S.getProfile

getUpdates :: MonadWalletDBRead m => m [CUpdateInfo]
getUpdates = performDBReadAction S.getUpdates

getNextUpdate :: MonadWalletDBRead m => m (Maybe CUpdateInfo)
getNextUpdate = performDBReadAction S.getNextUpdate

-- Though CWalletModifier contains pending txs,
-- but they will be empty for mempool so
-- we can access to pending txs directly from wallet-db.
getPendingTxs :: MonadWalletDBRead m => m [PendingTx]
getPendingTxs = performDBReadAction S.getPendingTxs

getWalletPendingTxs :: MonadWalletDBRead m => CId Wal -> m (Maybe [PendingTx])
getWalletPendingTxs wid = performDBReadAction (S.getWalletPendingTxs wid)

getPendingTx :: MonadWalletDBRead m => CId Wal -> TxId -> m (Maybe PendingTx)
getPendingTx wid txid = performDBReadAction (S.getPendingTx wid txid)

----------------------------------------------------------------------------
-- Modification operations
----------------------------------------------------------------------------

createAccount :: MonadWalletDB ctx m => AccountId -> CAccountMeta -> m ()
createAccount accId = updateDisk . A.CreateAccount accId

createWallet :: MonadWalletDB ctx m => CId Wal -> CWalletMeta -> Bool -> PassPhraseLU -> m ()
createWallet cWalId cwMeta isReady = updateDisk . A.CreateWallet cWalId cwMeta isReady

removeTxMetas :: MonadWalletDB ctx m => CId Wal -> m ()
removeTxMetas = updateDisk . A.RemoveTxMetas

addWAddress :: MonadWalletDB ctx m => CWAddressMeta -> m ()
addWAddress addr = updateDisk $ A.AddWAddress addr

setAccountMeta :: MonadWalletDB ctx m => AccountId -> CAccountMeta -> m ()
setAccountMeta accId = updateDisk . A.SetAccountMeta accId

setWalletMeta :: MonadWalletDB ctx m => CId Wal -> CWalletMeta -> m ()
setWalletMeta cWalId = updateDisk . A.SetWalletMeta cWalId

setWalletReady :: MonadWalletDB ctx m => CId Wal -> Bool -> m ()
setWalletReady cWalId = updateDisk . A.SetWalletReady cWalId

setWalletPassLU :: MonadWalletDB ctx m => CId Wal -> PassPhraseLU -> m ()
setWalletPassLU cWalId = updateDisk . A.SetWalletPassLU cWalId

setWalletSyncTip :: MonadWalletDB ctx m => CId Wal -> HeaderHash -> m ()
setWalletSyncTip cWalId = updateDisk . A.SetWalletSyncTip cWalId

setProfile :: MonadWalletDB ctx m => CProfile -> m ()
setProfile = updateDisk . A.SetProfile

setWalletTxMeta :: MonadWalletDB ctx m => CId Wal -> CTxId -> CTxMeta -> m ()
setWalletTxMeta cWalId cTxId = updateDisk . A.SetWalletTxMeta cWalId cTxId

addOnlyNewTxMetas :: MonadWalletDB ctx m => CId Wal -> Map TxId CTxMeta -> m ()
addOnlyNewTxMetas cWalId cTxMetas = updateDisk (A.AddOnlyNewTxMetas cWalId cTxMetaList)
    where
      cTxMetaList = [ (encodeCType txId, cTxMeta) | (txId, cTxMeta) <- Map.toList cTxMetas ]

updateWalletBalancesAndUtxo :: MonadWalletDB ctx m => UtxoModifier -> m ()
updateWalletBalancesAndUtxo = updateDisk . A.UpdateWalletBalancesAndUtxo

addOnlyNewTxMeta :: MonadWalletDB ctx m => CId Wal -> CTxId -> CTxMeta -> m ()
addOnlyNewTxMeta cWalId cTxId = updateDisk . A.AddOnlyNewTxMeta cWalId cTxId

removeWallet :: MonadWalletDB ctx m => CId Wal -> m ()
removeWallet = updateDisk . A.RemoveWallet

removeHistoryCache :: MonadWalletDB ctx m => CId Wal -> m ()
removeHistoryCache = updateDisk . A.RemoveHistoryCache

removeAccount :: MonadWalletDB ctx m => AccountId -> m ()
removeAccount = updateDisk . A.RemoveAccount

totallyRemoveWAddress :: MonadWalletDB ctx m => CWAddressMeta -> m ()
totallyRemoveWAddress = updateDisk . A.TotallyRemoveWAddress

addUpdate :: MonadWalletDB ctx m => CUpdateInfo -> m ()
addUpdate = updateDisk . A.AddUpdate

removeNextUpdate :: MonadWalletDB ctx m => m ()
removeNextUpdate = updateDisk A.RemoveNextUpdate

testReset :: MonadWalletDB ctx m => m ()
testReset = updateDisk A.TestReset

casPtxCondition
    :: MonadWalletDB ctx m
    => CId Wal -> TxId -> PtxCondition -> PtxCondition -> m Bool
casPtxCondition = updateDisk ... A.CasPtxCondition

removeOnlyCreatingPtx
    :: MonadWalletDB ctx m
    => CId Wal -> TxId -> m Bool
removeOnlyCreatingPtx = updateDisk ... A.RemoveOnlyCreatingPtx

ptxUpdateMeta
    :: MonadWalletDB ctx m
    => CId Wal -> TxId -> PtxMetaUpdate -> m ()
ptxUpdateMeta = updateDisk ... A.PtxUpdateMeta

addOnlyNewPendingTx :: MonadWalletDB ctx m => PendingTx -> m ()
addOnlyNewPendingTx = updateDisk ... A.AddOnlyNewPendingTx

resetFailedPtxs :: MonadWalletDB ctx m => SlotId -> m ()
resetFailedPtxs = updateDisk ... A.ResetFailedPtxs

flushWalletStorage :: MonadWalletDB ctx m => m ()
flushWalletStorage = updateDisk A.FlushWalletStorage

applyModifierToWallet :: MonadWalletDB ctx m => CId Wal -> HeaderHash -> WalletModifier -> m ()
applyModifierToWallet = updateDisk ... A.ApplyModifierToWallet

applyModifierToWallets :: MonadWalletDB ctx m => HeaderHash -> [(CId Wal, WalletModifier)] -> m ()
applyModifierToWallets = updateDisk ... A.ApplyModifierToWallets
