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
       , getAccountIds
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
       , getCustomAddresses
       , getCustomAddress
       , isCustomAddress
       , getWalletUtxo
       , getPendingTxs
       , getWalletPendingTxs
       , getPendingTx

       -- * Setters
       , testReset
       , createAccount
       , createWallet
       , addRemovedAccount
       , addWAddress
       , addCustomAddress
       , setProfile
       , setAccountMeta
       , setWalletMeta
       , setWalletReady
       , setWalletPassLU
       , setWalletSyncTip
       , setWalletTxMeta
       , addOnlyNewTxMetas
       , setWalletTxHistory
       , addOnlyNewTxMeta
       , removeWallet
       , removeWalletTxMetas
       , removeTxMetas
       , removeHistoryCache
       , removeAccount
       , removeWAddress
       , removeCustomAddress
       , totallyRemoveWAddress
       , addUpdate
       , removeNextUpdate
       , insertIntoHistoryCache
       , removeFromHistoryCache
       , setWalletUtxo
       , setPtxCondition
       , casPtxCondition
       , ptxUpdateMeta
       , addOnlyNewPendingTx
       , flushWalletStorage
       , getWalletStorage
       ) where

import           Data.Acid                    (EventResult, EventState, QueryEvent,
                                               UpdateEvent)
import qualified Data.Map                     as Map
import           Ether.Internal               (lensOf)
import           Universum

import           Pos.Client.Txp.History       (TxHistoryEntry)
import           Pos.Core.Configuration       (HasConfiguration)
import           Pos.Txp                      (TxId, Utxo)
import           Pos.Types                    (HeaderHash)
import           Pos.Util.Servant             (encodeCType)
import           Pos.Util.Util                (HasLens')
import           Pos.Wallet.Web.ClientTypes   (AccountId, Addr, CAccountMeta, CId,
                                               CProfile, CTxId, CTxMeta, CUpdateInfo,
                                               CWAddressMeta, CWalletMeta, PassPhraseLU,
                                               Wal)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition)
import           Pos.Wallet.Web.State.Acidic  (WalletState, closeState, openMemState,
                                               openState)
import           Pos.Wallet.Web.State.Acidic  as A
import           Pos.Wallet.Web.State.Storage (AddressLookupMode (..),
                                               CustomAddressType (..), PtxMetaUpdate (..),
                                               WalletStorage, WalletTip (..))

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

getAccountIds :: MonadWalletDBRead ctx m => m [AccountId]
getAccountIds = queryDisk A.GetAccountIds

getAccountMetas :: MonadWalletDBRead ctx m => m [CAccountMeta]
getAccountMetas = queryDisk A.GetAccountMetas

getAccountMeta :: MonadWalletDBRead ctx m => AccountId -> m (Maybe CAccountMeta)
getAccountMeta = queryDisk . A.GetAccountMeta

getWalletAddresses :: MonadWalletDBRead ctx m => m [CId Wal]
getWalletAddresses = queryDisk A.GetWalletAddresses

getWalletMeta :: MonadWalletDBRead ctx m => CId Wal -> m (Maybe CWalletMeta)
getWalletMeta = queryDisk . A.GetWalletMeta

getWalletMetaIncludeUnready :: MonadWalletDBRead ctx m => Bool -> CId Wal -> m (Maybe CWalletMeta)
getWalletMetaIncludeUnready includeReady = queryDisk . A.GetWalletMetaIncludeUnready includeReady

getWalletMetas :: MonadWalletDBRead ctx m => m ([CWalletMeta])
getWalletMetas = queryDisk A.GetWalletMetas

getWalletPassLU :: MonadWalletDBRead ctx m => CId Wal -> m (Maybe PassPhraseLU)
getWalletPassLU = queryDisk . A.GetWalletPassLU

getWalletSyncTip :: MonadWalletDBRead ctx m => CId Wal -> m (Maybe WalletTip)
getWalletSyncTip = queryDisk . A.GetWalletSyncTip

getAccountWAddresses
    :: MonadWalletDBRead ctx m
    => AddressLookupMode -> AccountId -> m (Maybe [CWAddressMeta])
getAccountWAddresses mode = queryDisk . A.GetAccountWAddresses mode

doesWAddressExist
    :: MonadWalletDBRead ctx m
    => AddressLookupMode -> CWAddressMeta -> m Bool
doesWAddressExist mode = queryDisk . A.DoesWAddressExist mode

getProfile :: MonadWalletDBRead ctx m => m CProfile
getProfile = queryDisk A.GetProfile

getTxMeta :: MonadWalletDBRead ctx m => CId Wal -> CTxId -> m (Maybe CTxMeta)
getTxMeta cWalId = queryDisk . A.GetTxMeta cWalId

getWalletTxHistory :: MonadWalletDBRead ctx m => CId Wal -> m (Maybe [CTxMeta])
getWalletTxHistory = queryDisk . A.GetWalletTxHistory

getUpdates :: MonadWalletDBRead ctx m => m [CUpdateInfo]
getUpdates = queryDisk A.GetUpdates

getNextUpdate :: MonadWalletDBRead ctx m => m (Maybe CUpdateInfo)
getNextUpdate = queryDisk A.GetNextUpdate

getHistoryCache :: MonadWalletDBRead ctx m => CId Wal -> m (Maybe (Map TxId TxHistoryEntry))
getHistoryCache = queryDisk . A.GetHistoryCache

getCustomAddresses :: MonadWalletDBRead ctx m => CustomAddressType -> m [CId Addr]
getCustomAddresses = queryDisk ... A.GetCustomAddresses

getCustomAddress :: MonadWalletDBRead ctx m => CustomAddressType -> CId Addr -> m (Maybe HeaderHash)
getCustomAddress = queryDisk ... A.GetCustomAddress

isCustomAddress :: MonadWalletDBRead ctx m => CustomAddressType -> CId Addr -> m Bool
isCustomAddress = fmap isJust . queryDisk ... A.GetCustomAddress

getPendingTxs :: MonadWalletDBRead ctx m => m [PendingTx]
getPendingTxs = queryDisk ... A.GetPendingTxs

getWalletPendingTxs :: MonadWalletDBRead ctx m => CId Wal -> m (Maybe [PendingTx])
getWalletPendingTxs = queryDisk ... A.GetWalletPendingTxs

getPendingTx :: MonadWalletDBRead ctx m => CId Wal -> TxId -> m (Maybe PendingTx)
getPendingTx = queryDisk ... A.GetPendingTx

createAccount :: MonadWalletDB ctx m => AccountId -> CAccountMeta -> m ()
createAccount accId = updateDisk . A.CreateAccount accId

createWallet :: MonadWalletDB ctx m => CId Wal -> CWalletMeta -> Bool -> PassPhraseLU -> m ()
createWallet cWalId cwMeta isReady = updateDisk . A.CreateWallet cWalId cwMeta isReady

addWAddress :: MonadWalletDB ctx m => CWAddressMeta -> m ()
addWAddress addr = updateDisk $ A.AddWAddress addr

addCustomAddress :: MonadWalletDB ctx m => CustomAddressType -> (CId Addr, HeaderHash) -> m Bool
addCustomAddress = updateDisk ... A.AddCustomAddress

addRemovedAccount :: MonadWalletDB ctx m => CWAddressMeta -> m ()
addRemovedAccount addr = updateDisk $ A.AddRemovedAccount addr

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

setWalletTxHistory :: MonadWalletDB ctx m => CId Wal -> [(CTxId, CTxMeta)] -> m ()
setWalletTxHistory cWalId = updateDisk . A.SetWalletTxHistory cWalId

getWalletUtxo :: MonadWalletDBRead ctx m => m Utxo
getWalletUtxo = queryDisk A.GetWalletUtxo

setWalletUtxo :: MonadWalletDB ctx m => Utxo -> m ()
setWalletUtxo = updateDisk . A.SetWalletUtxo

addOnlyNewTxMeta :: MonadWalletDB ctx m => CId Wal -> CTxId -> CTxMeta -> m ()
addOnlyNewTxMeta cWalId cTxId = updateDisk . A.AddOnlyNewTxMeta cWalId cTxId

removeWallet :: MonadWalletDB ctx m => CId Wal -> m ()
removeWallet = updateDisk . A.RemoveWallet

removeTxMetas :: MonadWalletDB ctx m => CId Wal -> m ()
removeTxMetas = updateDisk . A.RemoveTxMetas

removeWalletTxMetas :: MonadWalletDB ctx m => CId Wal -> [CTxId] -> m ()
removeWalletTxMetas = updateDisk ... A.RemoveWalletTxMetas

removeHistoryCache :: MonadWalletDB ctx m => CId Wal -> m ()
removeHistoryCache = updateDisk . A.RemoveHistoryCache

removeAccount :: MonadWalletDB ctx m => AccountId -> m ()
removeAccount = updateDisk . A.RemoveAccount

removeWAddress :: MonadWalletDB ctx m => CWAddressMeta -> m ()
removeWAddress = updateDisk . A.RemoveWAddress

totallyRemoveWAddress :: MonadWalletDB ctx m => CWAddressMeta -> m ()
totallyRemoveWAddress = updateDisk . A.TotallyRemoveWAddress

removeCustomAddress
    :: MonadWalletDB ctx m
    => CustomAddressType -> (CId Addr, HeaderHash) -> m Bool
removeCustomAddress = updateDisk ... A.RemoveCustomAddress

addUpdate :: MonadWalletDB ctx m => CUpdateInfo -> m ()
addUpdate = updateDisk . A.AddUpdate

removeNextUpdate :: MonadWalletDB ctx m => m ()
removeNextUpdate = updateDisk A.RemoveNextUpdate

testReset :: MonadWalletDB ctx m => m ()
testReset = updateDisk A.TestReset

insertIntoHistoryCache :: MonadWalletDB ctx m => CId Wal -> Map TxId TxHistoryEntry -> m ()
insertIntoHistoryCache cWalId cTxs
  | Map.null cTxs = return ()
  | otherwise     = updateDisk (A.InsertIntoHistoryCache cWalId cTxs)

removeFromHistoryCache :: MonadWalletDB ctx m => CId Wal -> Map TxId a -> m ()
removeFromHistoryCache cWalId cTxs
  | Map.null cTxs = return ()
  | otherwise     = updateDisk (A.RemoveFromHistoryCache cWalId cTxs')
  where
    cTxs' :: Map TxId ()
    cTxs' = Map.map (const ()) cTxs

setPtxCondition
    :: MonadWalletDB ctx m
    => CId Wal -> TxId -> PtxCondition -> m ()
setPtxCondition = updateDisk ... A.SetPtxCondition

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

getWalletStorage :: MonadWalletDBRead ctx m => m WalletStorage
getWalletStorage = queryDisk A.GetWalletStorage
