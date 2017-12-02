{-# LANGUAGE TypeFamilies #-}

module Pos.Wallet.Web.State.State
       ( WalletState
       , WalletTip (..)
       , PtxMetaUpdate (..)
       , getWalletWebState
       , MonadWalletDBAccess
       , MonadWalletDBRead
       , MonadWalletDB
       , MonadWalletDBReadWithMempool
       , openState
       , openMemState
       , closeState

       , AddressLookupMode (..)
       , CustomAddressType (..)

       -- * Getters
       , getProfile
       , getWalletAccountIds
       , getAccountMetas
       , getAccountMeta
       , getAccountWAddresses
       , getWalletWAddresses
       , getWalletWAddressesDB
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
       , getCustomAddresses
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
       , ptxUpdateMeta
       , addOnlyNewPendingTx
       , getWalletStorage
       , flushWalletStorage
       , applyModifierToWallet
       , applyModifierToWallets
       ) where

import           Universum

import qualified Control.Concurrent.STM            as STM
import           Data.Acid                         (EventResult, EventState, QueryEvent,
                                                    UpdateEvent)
import qualified Data.HashMap.Strict               as HM
import qualified Data.Map                          as Map
import           Ether.Internal                    (lensOf)

import           Pos.Client.Txp.History            (TxHistoryEntry)
import           Pos.Core.Configuration            (HasConfiguration)
import           Pos.Txp                           (TxId, Utxo, UtxoModifier)
import           Pos.Types                         (HeaderHash)
import           Pos.Util.Servant                  (encodeCType)
import           Pos.Util.Util                     (HasLens')
import           Pos.Wallet.Web.ClientTypes        (AccountId (..), Addr, CAccountMeta,
                                                    CId, CProfile, CTxId, CTxMeta,
                                                    CUpdateInfo, CWAddressMeta (..),
                                                    CWalletMeta, PassPhraseLU, Wal)
import           Pos.Wallet.Web.Pending.Types      (PendingTx (..), PtxCondition)
import           Pos.Wallet.Web.State.Acidic       (WalletState, closeState, openMemState,
                                                    openState)
import           Pos.Wallet.Web.State.Acidic       as A
import           Pos.Wallet.Web.State.Memory.Types (ExtStorageModifier (..),
                                                    ExtStorageModifierVar,
                                                    HasExtStorageModifier,
                                                    StorageModifier (..))
import           Pos.Wallet.Web.State.Storage      (AddressLookupMode (..),
                                                    CustomAddressType (..),
                                                    PtxMetaUpdate (..), WalletBalances,
                                                    WalletInfo (..), WalletStorage (..),
                                                    WalletTip (..))
import qualified Pos.Wallet.Web.State.Storage      as S
import           Pos.Wallet.Web.Tracking.Modifier  (WalletModifier (..))

-- | No read or write, just access to state handler
type MonadWalletDBAccess ctx m =
    ( HasLens' ctx WalletState
    , MonadReader ctx m
    )

-- | Wallet state reading
type MonadWalletDBRead ctx m =
    ( MonadIO m
    , MonadWalletDBAccess ctx m
    , HasConfiguration
    )

-- | Writting to wallet state
class MonadWalletDBRead ctx m => MonadWalletDB ctx m

getWalletWebState :: (MonadReader ctx m, HasLens' ctx WalletState) => m WalletState
getWalletWebState = view (lensOf @WalletState)

queryDisk
    :: (EventState event ~ WalletStorage, QueryEvent event, MonadWalletDBRead ctx m)
    => event -> m (EventResult event)
queryDisk e = getWalletWebState >>= flip A.query e

updateDisk
    :: (EventState event ~ WalletStorage, UpdateEvent event, MonadWalletDB ctx m)
    => event -> m (EventResult event)
updateDisk e = getWalletWebState >>= flip A.update e

----------------------------------------------------------------------------
-- Primitives for synchronization with wallet-db
----------------------------------------------------------------------------

-- | Waits until wallet's modifier is consistent with wallet-db.
-- Call action from WalletStorage from disk along with SModifier applied to it.
waitWalletCons
    :: ( MonadWalletDBRead ctx m
       , HasExtStorageModifier ctx
       )
    => CId Wal -> a -> Reader WalletStorage a -> m a
waitWalletCons wal defRes action = do
    storage <- getWalletStorage
    case _wiSyncTip <$> HM.lookup wal (_wsWalletInfos storage) of
        Just (SyncedWith dbTip) -> do
            modifierVar <- view (lensOf @ExtStorageModifierVar)
            ExtStorageModifier{..} <- atomically $ STM.readTMVar modifierVar
            if esmTip == dbTip then do
                -- Case when modifier's tip and db's tip are the same
                -- it's easy case, just apply modifier to db
                let sm = HM.lookupDefault mempty wal (getStorageModifier esmMemStorageModifier)
                pure $
                    runReader action $
                    execState (S.applyModifierToWallet wal esmTip sm) storage
            else
                -- Case when tips are mismatched, hence,
                -- we took db and modifier at the moment when they were updating
                -- the next such moment will when new block comes. So just can retry.
                -- Basically it's lock free: either another thread made some progress or the current thread.
                waitWalletCons wal defRes action
        _                       -> pure defRes

-- | Waits until wallets' modifiers are consistent with wallet-db.
-- Call action from WalletStorage from disk along with SModifier applied to it.
waitWalletsCons
    :: ( MonadWalletDBRead ctx m
       , HasExtStorageModifier ctx
       )
    => a -> Reader WalletStorage a -> m a
waitWalletsCons defRes action = do
    storage <- getWalletStorage
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
        pure $
            runReader action $
            execState (S.applyModifierToWallets esmTip modifiersL) storage
    else
        -- Similar to case from @waitWalletCons@.
        waitWalletsCons defRes action

----------------------------------------------------------------------------
-- Query operations affected by mempool
----------------------------------------------------------------------------

type MonadWalletDBReadWithMempool ctx m = (MonadWalletDBRead ctx m, HasExtStorageModifier ctx)

getWalletAccountIds
    :: MonadWalletDBReadWithMempool ctx m
    => CId Wal -> m [AccountId]
getWalletAccountIds wal = waitWalletCons wal [] (S.getWalletAccountIds wal)

getHistoryCache
    :: MonadWalletDBReadWithMempool ctx m
    => CId Wal -> m (Maybe (Map TxId TxHistoryEntry))
getHistoryCache wal = waitWalletCons wal Nothing (S.getHistoryCache wal)

getAccountWAddresses
    :: MonadWalletDBReadWithMempool ctx m
    => AddressLookupMode -> AccountId -> m (Maybe [CWAddressMeta])
getAccountWAddresses mode accId@AccountId{..} =
    waitWalletCons aiWId Nothing (S.getAccountWAddresses mode accId)

getWalletWAddresses
    :: MonadWalletDBReadWithMempool ctx m
    => AddressLookupMode -> CId Wal -> m (Maybe [CWAddressMeta])
getWalletWAddresses mode wid =
    waitWalletCons wid Nothing (S.getWalletWAddresses mode wid)

doesWAddressExist
    :: MonadWalletDBReadWithMempool ctx m
    => AddressLookupMode -> CWAddressMeta -> m Bool
doesWAddressExist mode addrId@CWAddressMeta{..} =
    waitWalletCons cwamWId False (S.doesWAddressExist mode addrId)

getAccountMeta :: MonadWalletDBReadWithMempool ctx m => AccountId -> m (Maybe CAccountMeta)
getAccountMeta accId@AccountId{..} =
    waitWalletCons aiWId Nothing (S.getAccountMeta accId)

getTxMeta :: MonadWalletDBReadWithMempool ctx m => CId Wal -> CTxId -> m (Maybe CTxMeta)
getTxMeta walId txId =
    waitWalletCons walId Nothing (S.getTxHistoryMeta walId txId)

getWalletTxHistory :: MonadWalletDBReadWithMempool ctx m => CId Wal -> m (Maybe [CTxMeta])
getWalletTxHistory walId =
    waitWalletCons walId Nothing (S.getWalletTxHistoryMetas walId)

-- Operations below which wait for synchronization of all wallets.
-- Some of them should depend on CId Wal, but we can't do it without wallet-db migration
-- which requires secret keys (it can't be performed in SafeCopy's Migration typeclass).

getCustomAddresses :: MonadWalletDBReadWithMempool ctx m => CustomAddressType -> m [CId Addr]
getCustomAddresses cType =
    waitWalletsCons [] (S.getCustomAddresses cType)

getAccountMetas :: MonadWalletDBReadWithMempool ctx m => m [CAccountMeta]
getAccountMetas =
    waitWalletsCons [] S.getAccountMetas

isCustomAddress :: MonadWalletDBReadWithMempool ctx m => CustomAddressType -> CId Addr -> m Bool
isCustomAddress cType addrId =
    waitWalletsCons False (fmap isJust $ S.getCustomAddress cType addrId)

getWalletUtxo :: MonadWalletDBReadWithMempool ctx m => m Utxo
getWalletUtxo = waitWalletsCons mempty S.getWalletUtxo

getWalletBalancesAndUtxo :: MonadWalletDBReadWithMempool ctx m => m (WalletBalances, Utxo)
getWalletBalancesAndUtxo = waitWalletsCons mempty S.getWalletBalancesAndUtxo

----------------------------------------------------------------------------
-- Query operations not affected by mempool
----------------------------------------------------------------------------

getWalletIds :: MonadWalletDBRead ctx m => m [CId Wal]
getWalletIds = queryDisk A.GetWalletIds

getWalletWAddressesDB
    :: MonadWalletDBRead ctx m
    => AddressLookupMode -> CId Wal -> m (Maybe [CWAddressMeta])
getWalletWAddressesDB mode = queryDisk . A.GetWalletWAddresses mode

getWalletMetaIncludeUnready :: MonadWalletDBRead ctx m => Bool -> CId Wal -> m (Maybe CWalletMeta)
getWalletMetaIncludeUnready includeReady = queryDisk . A.GetWalletMetaIncludeUnready includeReady

getWalletMeta :: MonadWalletDBRead ctx m => CId Wal -> m (Maybe CWalletMeta)
getWalletMeta = queryDisk . A.GetWalletMeta

getWalletMetas :: MonadWalletDBRead ctx m => m [CWalletMeta]
getWalletMetas = queryDisk A.GetWalletMetas

getWalletPassLU :: MonadWalletDBRead ctx m => CId Wal -> m (Maybe PassPhraseLU)
getWalletPassLU = queryDisk . A.GetWalletPassLU

getWalletSyncTip :: MonadWalletDBRead ctx m => CId Wal -> m (Maybe WalletTip)
getWalletSyncTip = queryDisk . A.GetWalletSyncTip

getProfile :: MonadWalletDBRead ctx m => m CProfile
getProfile = queryDisk A.GetProfile

getUpdates :: MonadWalletDBRead ctx m => m [CUpdateInfo]
getUpdates = queryDisk A.GetUpdates

getNextUpdate :: MonadWalletDBRead ctx m => m (Maybe CUpdateInfo)
getNextUpdate = queryDisk A.GetNextUpdate

-- Though CWalletModifier contains pending txs,
-- but they will be empty for mempool so
-- we can access to pending txs directly from wallet-db.
getPendingTxs :: MonadWalletDBRead ctx m => m [PendingTx]
getPendingTxs = queryDisk ... A.GetPendingTxs

getWalletPendingTxs :: MonadWalletDBRead ctx m => CId Wal -> m (Maybe [PendingTx])
getWalletPendingTxs = queryDisk ... A.GetWalletPendingTxs

getPendingTx :: MonadWalletDBRead ctx m => CId Wal -> TxId -> m (Maybe PendingTx)
getPendingTx = queryDisk ... A.GetPendingTx

getWalletStorage :: MonadWalletDBRead ctx m => m WalletStorage
getWalletStorage = queryDisk A.GetWalletStorage

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

ptxUpdateMeta
    :: MonadWalletDB ctx m
    => CId Wal -> TxId -> PtxMetaUpdate -> m ()
ptxUpdateMeta = updateDisk ... A.PtxUpdateMeta

addOnlyNewPendingTx :: MonadWalletDB ctx m => PendingTx -> m ()
addOnlyNewPendingTx = updateDisk ... A.AddOnlyNewPendingTx

flushWalletStorage :: MonadWalletDB ctx m => m ()
flushWalletStorage = updateDisk A.FlushWalletStorage

applyModifierToWallet :: MonadWalletDB ctx m => CId Wal -> HeaderHash -> WalletModifier -> m ()
applyModifierToWallet = updateDisk ... A.ApplyModifierToWallet

applyModifierToWallets :: MonadWalletDB ctx m => HeaderHash -> [(CId Wal, WalletModifier)] -> m ()
applyModifierToWallets = updateDisk ... A.ApplyModifierToWallets
