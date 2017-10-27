{-# LANGUAGE TypeFamilies #-}

module Pos.Wallet.Web.State.State
       ( WalletState
       , WalletTip (..)
       , PtxMetaUpdate (..)
       , getWalletWebState
       , MonadWalletDBAccess
       , MonadWalletDBRead
       , MonadWalletDB
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
       , getWalletMetas
       , getWalletMeta
       , getWalletMetaIncludeUnready
       , getWalletPassLU
       , getWalletSyncTip
       , getWalletAddresses
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
       ) where

import           Universum

import           Data.Acid                        (EventResult, EventState, QueryEvent,
                                                   UpdateEvent)
import qualified Data.Map                         as Map
import           Ether.Internal                   (lensOf)

import           Pos.Client.Txp.History           (TxHistoryEntry)
import           Pos.Core.Configuration           (HasConfiguration)
import           Pos.Txp                          (TxId, Utxo, UtxoModifier)
import           Pos.Types                        (HeaderHash)
import           Pos.Util.Servant                 (encodeCType)
import           Pos.Util.Util                    (HasLens')
import           Pos.Wallet.Web.ClientTypes       (AccountId, Addr, CAccountMeta, CId,
                                                   CProfile, CTxId, CTxMeta, CUpdateInfo,
                                                   CWAddressMeta, CWalletMeta,
                                                   PassPhraseLU, Wal)
import           Pos.Wallet.Web.Pending.Types     (PendingTx (..), PtxCondition)
import           Pos.Wallet.Web.State.Acidic      (WalletState, closeState, openMemState,
                                                   openState)
import           Pos.Wallet.Web.State.Acidic      as A
import           Pos.Wallet.Web.State.Storage     (AddressLookupMode (..),
                                                   CustomAddressType (..),
                                                   PtxMetaUpdate (..), WalletBalances,
                                                   WalletStorage, WalletTip (..))
import           Pos.Wallet.Web.Tracking.Modifier (WalletModifier (..))

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
-- Query operations affected by mempool
----------------------------------------------------------------------------

getWalletAccountIds :: MonadWalletDBRead ctx m => CId Wal -> m [AccountId]
getWalletAccountIds = queryDisk ... A.GetWalletAccountIds

getHistoryCache :: MonadWalletDBRead ctx m => CId Wal -> m (Maybe (Map TxId TxHistoryEntry))
getHistoryCache = queryDisk . A.GetHistoryCache

getAccountWAddresses
    :: MonadWalletDBRead ctx m
    => AddressLookupMode -> AccountId -> m (Maybe [CWAddressMeta])
getAccountWAddresses mode = queryDisk . A.GetAccountWAddresses mode

doesWAddressExist
    :: MonadWalletDBRead ctx m
    => AddressLookupMode -> CWAddressMeta -> m Bool
doesWAddressExist mode = queryDisk . A.DoesWAddressExist mode

getWalletUtxo :: MonadWalletDBRead ctx m => m Utxo
getWalletUtxo = queryDisk A.GetWalletUtxo

getWalletBalancesAndUtxo :: MonadWalletDBRead ctx m => m (WalletBalances, Utxo)
getWalletBalancesAndUtxo = queryDisk A.GetWalletBalancesAndUtxo

getAccountMeta :: MonadWalletDBRead ctx m => AccountId -> m (Maybe CAccountMeta)
getAccountMeta = queryDisk . A.GetAccountMeta

getAccountMetas :: MonadWalletDBRead ctx m => m [CAccountMeta]
getAccountMetas = queryDisk A.GetAccountMetas

getTxMeta :: MonadWalletDBRead ctx m => CId Wal -> CTxId -> m (Maybe CTxMeta)
getTxMeta cWalId = queryDisk . A.GetTxHistoryMeta cWalId

getWalletTxHistory :: MonadWalletDBRead ctx m => CId Wal -> m (Maybe [CTxMeta])
getWalletTxHistory = queryDisk . A.GetWalletTxHistoryMetas

isCustomAddress :: MonadWalletDBRead ctx m => CustomAddressType -> CId Addr -> m Bool
isCustomAddress = fmap isJust . queryDisk ... A.GetCustomAddress

getCustomAddresses :: MonadWalletDBRead ctx m => CustomAddressType -> m [CId Addr]
getCustomAddresses = queryDisk ... A.GetCustomAddresses

----------------------------------------------------------------------------
-- Query operations not affected by mempool
----------------------------------------------------------------------------

getWalletAddresses :: MonadWalletDBRead ctx m => m [CId Wal]
getWalletAddresses = queryDisk A.GetWalletAddresses

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
