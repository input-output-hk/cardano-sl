{-# LANGUAGE TypeFamilies #-}

module Pos.Wallet.Web.State.State
       ( WalletState
       , MonadWalletWebDB
       , WalletTip (..)
       , PtxMetaUpdate (..)
       , getWalletWebState
       , WebWalletModeDB
       , openState
       , openMemState
       , closeState

       , AddressLookupMode (..)
       , CustomAddressType (..)

       -- * Getters
       , getProfile
       , doesAccountExist
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
       , getWalletBalancesAndUtxo
       , updateWalletBalancesAndUtxo
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
       , getWalletStorage
       , flushWalletStorage
       ) where

import           Data.Acid                    (EventResult, EventState, QueryEvent,
                                               UpdateEvent)
import qualified Data.Map                     as Map
import           Ether.Internal               (HasLens (..))
import           Mockable                     (MonadMockable)
import           Universum

import           Pos.Client.Txp.History       (TxHistoryEntry)
import           Pos.Core.Configuration       (HasConfiguration)
import           Pos.Txp                      (TxId, Utxo, UtxoModifier)
import           Pos.Types                    (HeaderHash)
import           Pos.Util.Servant             (encodeCType)
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
                                               WalletBalances, WalletStorage,
                                               WalletTip (..))

-- | MonadWalletWebDB stands for monad which is able to get web wallet state
type MonadWalletWebDB ctx m =
    ( MonadReader ctx m
    , HasLens WalletState ctx WalletState
    , HasConfiguration
    )

getWalletWebState :: MonadWalletWebDB ctx m => m WalletState
getWalletWebState = view (lensOf @WalletState)

-- | Constraint for working with web wallet DB
type WebWalletModeDB ctx m = (MonadWalletWebDB ctx m, MonadIO m, MonadMockable m)

queryDisk
    :: (EventState event ~ WalletStorage, QueryEvent event, WebWalletModeDB ctx m)
    => event -> m (EventResult event)
queryDisk e = getWalletWebState >>= flip A.query e

updateDisk
    :: (EventState event ~ WalletStorage, UpdateEvent event, WebWalletModeDB ctx m)
    => event -> m (EventResult event)
updateDisk e = getWalletWebState >>= flip A.update e

doesAccountExist :: WebWalletModeDB ctx m => AccountId -> m Bool
doesAccountExist = queryDisk . A.DoesAccountExist

getAccountIds :: WebWalletModeDB ctx m => m [AccountId]
getAccountIds = queryDisk A.GetAccountIds

getAccountMetas :: WebWalletModeDB ctx m => m [CAccountMeta]
getAccountMetas = queryDisk A.GetAccountMetas

getAccountMeta :: WebWalletModeDB ctx m => AccountId -> m (Maybe CAccountMeta)
getAccountMeta = queryDisk . A.GetAccountMeta

getWalletAddresses :: WebWalletModeDB ctx m => m [CId Wal]
getWalletAddresses = queryDisk A.GetWalletAddresses

getWalletMeta :: WebWalletModeDB ctx m => CId Wal -> m (Maybe CWalletMeta)
getWalletMeta = queryDisk . A.GetWalletMeta

getWalletMetaIncludeUnready :: WebWalletModeDB ctx m => Bool -> CId Wal -> m (Maybe CWalletMeta)
getWalletMetaIncludeUnready includeReady = queryDisk . A.GetWalletMetaIncludeUnready includeReady

getWalletMetas :: WebWalletModeDB ctx m => m ([CWalletMeta])
getWalletMetas = queryDisk A.GetWalletMetas

getWalletPassLU :: WebWalletModeDB ctx m => CId Wal -> m (Maybe PassPhraseLU)
getWalletPassLU = queryDisk . A.GetWalletPassLU

getWalletSyncTip :: WebWalletModeDB ctx m => CId Wal -> m (Maybe WalletTip)
getWalletSyncTip = queryDisk . A.GetWalletSyncTip

getAccountWAddresses
    :: WebWalletModeDB ctx m
    => AddressLookupMode -> AccountId -> m (Maybe [CWAddressMeta])
getAccountWAddresses mode = queryDisk . A.GetAccountWAddresses mode

doesWAddressExist
    :: WebWalletModeDB ctx m
    => AddressLookupMode -> CWAddressMeta -> m Bool
doesWAddressExist mode = queryDisk . A.DoesWAddressExist mode

getProfile :: WebWalletModeDB ctx m => m CProfile
getProfile = queryDisk A.GetProfile

getTxMeta :: WebWalletModeDB ctx m => CId Wal -> CTxId -> m (Maybe CTxMeta)
getTxMeta cWalId = queryDisk . A.GetTxMeta cWalId

getWalletTxHistory :: WebWalletModeDB ctx m => CId Wal -> m (Maybe [CTxMeta])
getWalletTxHistory = queryDisk . A.GetWalletTxHistory

getUpdates :: WebWalletModeDB ctx m => m [CUpdateInfo]
getUpdates = queryDisk A.GetUpdates

getNextUpdate :: WebWalletModeDB ctx m => m (Maybe CUpdateInfo)
getNextUpdate = queryDisk A.GetNextUpdate

getHistoryCache :: WebWalletModeDB ctx m => CId Wal -> m (Maybe (Map TxId TxHistoryEntry))
getHistoryCache = queryDisk . A.GetHistoryCache

getCustomAddresses :: WebWalletModeDB ctx m => CustomAddressType -> m [CId Addr]
getCustomAddresses = queryDisk ... A.GetCustomAddresses

getCustomAddress :: WebWalletModeDB ctx m => CustomAddressType -> CId Addr -> m (Maybe HeaderHash)
getCustomAddress = queryDisk ... A.GetCustomAddress

isCustomAddress :: WebWalletModeDB ctx m => CustomAddressType -> CId Addr -> m Bool
isCustomAddress = fmap isJust . queryDisk ... A.GetCustomAddress

getPendingTxs :: WebWalletModeDB ctx m => m [PendingTx]
getPendingTxs = queryDisk ... A.GetPendingTxs

getWalletPendingTxs :: WebWalletModeDB ctx m => CId Wal -> m (Maybe [PendingTx])
getWalletPendingTxs = queryDisk ... A.GetWalletPendingTxs

getPendingTx :: WebWalletModeDB ctx m => CId Wal -> TxId -> m (Maybe PendingTx)
getPendingTx = queryDisk ... A.GetPendingTx

createAccount :: WebWalletModeDB ctx m => AccountId -> CAccountMeta -> m ()
createAccount accId = updateDisk . A.CreateAccount accId

createWallet :: WebWalletModeDB ctx m => CId Wal -> CWalletMeta -> Bool -> PassPhraseLU -> m ()
createWallet cWalId cwMeta isReady = updateDisk . A.CreateWallet cWalId cwMeta isReady

addWAddress :: WebWalletModeDB ctx m => CWAddressMeta -> m ()
addWAddress addr = updateDisk $ A.AddWAddress addr

addCustomAddress :: WebWalletModeDB ctx m => CustomAddressType -> (CId Addr, HeaderHash) -> m Bool
addCustomAddress = updateDisk ... A.AddCustomAddress

addRemovedAccount :: WebWalletModeDB ctx m => CWAddressMeta -> m ()
addRemovedAccount addr = updateDisk $ A.AddRemovedAccount addr

setAccountMeta :: WebWalletModeDB ctx m => AccountId -> CAccountMeta -> m ()
setAccountMeta accId = updateDisk . A.SetAccountMeta accId

setWalletMeta :: WebWalletModeDB ctx m => CId Wal -> CWalletMeta -> m ()
setWalletMeta cWalId = updateDisk . A.SetWalletMeta cWalId

setWalletReady :: WebWalletModeDB ctx m => CId Wal -> Bool -> m ()
setWalletReady cWalId = updateDisk . A.SetWalletReady cWalId

setWalletPassLU :: WebWalletModeDB ctx m => CId Wal -> PassPhraseLU -> m ()
setWalletPassLU cWalId = updateDisk . A.SetWalletPassLU cWalId

setWalletSyncTip :: WebWalletModeDB ctx m => CId Wal -> HeaderHash -> m ()
setWalletSyncTip cWalId = updateDisk . A.SetWalletSyncTip cWalId

setProfile :: WebWalletModeDB ctx m => CProfile -> m ()
setProfile = updateDisk . A.SetProfile

setWalletTxMeta :: WebWalletModeDB ctx m => CId Wal -> CTxId -> CTxMeta -> m ()
setWalletTxMeta cWalId cTxId = updateDisk . A.SetWalletTxMeta cWalId cTxId

addOnlyNewTxMetas :: WebWalletModeDB ctx m => CId Wal -> Map TxId CTxMeta -> m ()
addOnlyNewTxMetas cWalId cTxMetas = updateDisk (A.AddOnlyNewTxMetas cWalId cTxMetaList)
    where
      cTxMetaList = [ (encodeCType txId, cTxMeta) | (txId, cTxMeta) <- Map.toList cTxMetas ]

setWalletTxHistory :: WebWalletModeDB ctx m => CId Wal -> [(CTxId, CTxMeta)] -> m ()
setWalletTxHistory cWalId = updateDisk . A.SetWalletTxHistory cWalId

getWalletUtxo :: WebWalletModeDB ctx m => m Utxo
getWalletUtxo = queryDisk A.GetWalletUtxo

getWalletBalancesAndUtxo :: WebWalletModeDB ctx m => m (WalletBalances, Utxo)
getWalletBalancesAndUtxo = queryDisk A.GetWalletBalancesAndUtxo

updateWalletBalancesAndUtxo :: WebWalletModeDB ctx m => UtxoModifier -> m ()
updateWalletBalancesAndUtxo = updateDisk . A.UpdateWalletBalancesAndUtxo

setWalletUtxo :: WebWalletModeDB ctx m => Utxo -> m ()
setWalletUtxo = updateDisk . A.SetWalletUtxo

addOnlyNewTxMeta :: WebWalletModeDB ctx m => CId Wal -> CTxId -> CTxMeta -> m ()
addOnlyNewTxMeta cWalId cTxId = updateDisk . A.AddOnlyNewTxMeta cWalId cTxId

removeWallet :: WebWalletModeDB ctx m => CId Wal -> m ()
removeWallet = updateDisk . A.RemoveWallet

removeTxMetas :: WebWalletModeDB ctx m => CId Wal -> m ()
removeTxMetas = updateDisk . A.RemoveTxMetas

removeWalletTxMetas :: WebWalletModeDB ctx m => CId Wal -> [CTxId] -> m ()
removeWalletTxMetas = updateDisk ... A.RemoveWalletTxMetas

removeHistoryCache :: WebWalletModeDB ctx m => CId Wal -> m ()
removeHistoryCache = updateDisk . A.RemoveHistoryCache

removeAccount :: WebWalletModeDB ctx m => AccountId -> m ()
removeAccount = updateDisk . A.RemoveAccount

removeWAddress :: WebWalletModeDB ctx m => CWAddressMeta -> m ()
removeWAddress = updateDisk . A.RemoveWAddress

totallyRemoveWAddress :: WebWalletModeDB ctx m => CWAddressMeta -> m ()
totallyRemoveWAddress = updateDisk . A.TotallyRemoveWAddress

removeCustomAddress
    :: WebWalletModeDB ctx m
    => CustomAddressType -> (CId Addr, HeaderHash) -> m Bool
removeCustomAddress = updateDisk ... A.RemoveCustomAddress

addUpdate :: WebWalletModeDB ctx m => CUpdateInfo -> m ()
addUpdate = updateDisk . A.AddUpdate

removeNextUpdate :: WebWalletModeDB ctx m => m ()
removeNextUpdate = updateDisk A.RemoveNextUpdate

testReset :: WebWalletModeDB ctx m => m ()
testReset = updateDisk A.TestReset

insertIntoHistoryCache :: WebWalletModeDB ctx m => CId Wal -> Map TxId TxHistoryEntry -> m ()
insertIntoHistoryCache cWalId cTxs
  | Map.null cTxs = return ()
  | otherwise     = updateDisk (A.InsertIntoHistoryCache cWalId cTxs)

removeFromHistoryCache :: WebWalletModeDB ctx m => CId Wal -> Map TxId a -> m ()
removeFromHistoryCache cWalId cTxs
  | Map.null cTxs = return ()
  | otherwise     = updateDisk (A.RemoveFromHistoryCache cWalId cTxs')
  where
    cTxs' :: Map TxId ()
    cTxs' = Map.map (const ()) cTxs

setPtxCondition
    :: WebWalletModeDB ctx m
    => CId Wal -> TxId -> PtxCondition -> m ()
setPtxCondition = updateDisk ... A.SetPtxCondition

casPtxCondition
    :: WebWalletModeDB ctx m
    => CId Wal -> TxId -> PtxCondition -> PtxCondition -> m Bool
casPtxCondition = updateDisk ... A.CasPtxCondition

ptxUpdateMeta
    :: WebWalletModeDB ctx m
    => CId Wal -> TxId -> PtxMetaUpdate -> m ()
ptxUpdateMeta = updateDisk ... A.PtxUpdateMeta

addOnlyNewPendingTx :: WebWalletModeDB ctx m => PendingTx -> m ()
addOnlyNewPendingTx = updateDisk ... A.AddOnlyNewPendingTx

flushWalletStorage :: WebWalletModeDB ctx m => m ()
flushWalletStorage = updateDisk A.FlushWalletStorage

getWalletStorage :: WebWalletModeDB ctx m => m WalletStorage
getWalletStorage = queryDisk A.GetWalletStorage
