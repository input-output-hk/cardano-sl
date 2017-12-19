{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Wallet.Web.State.State
       ( WalletState
       , MonadWalletWebDB
       , WalletTip (..)
       , PtxMetaUpdate (..)
       , AddressInfo (..)
       , getWalletWebState
       , WebWalletModeDB
       , openState
       , openMemState
       , closeState

       , AddressLookupMode (..)
       , CurrentAndRemoved (..)
       , CustomAddressType (..)

       -- * Getters
       , WalletSnapshot
       , getWalletSnapshot
       , getProfile
       , doesAccountExist
       , getAccountIds
       , getAccountMetas
       , getAccountMeta
       , getAccountAddrMaps
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
       , cancelApplyingPtxs
       , cancelSpecificApplyingPtx
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
import qualified Pos.Wallet.Web.State.Storage as S
import           Pos.Wallet.Web.State.Storage (AddressInfo (..), AddressLookupMode (..),
                                               CAddresses, CurrentAndRemoved(..) , CustomAddressType (..),
                                               PtxMetaUpdate (..), WalletBalances,
                                               WalletStorage, WalletTip (..))

-- | MonadWalletWebDB stands for monad which is able to get web wallet state
type MonadWalletWebDB ctx m =
    ( MonadReader ctx m
    , HasLens WalletState ctx WalletState
    )

getWalletWebState :: MonadWalletWebDB ctx m => m WalletState
getWalletWebState = view (lensOf @WalletState)

-- | Constraint for working with web wallet DB
type WebWalletModeDB ctx m =
    ( MonadWalletWebDB ctx m
    , MonadIO m
    , MonadMockable m
    , HasConfiguration
    )

type WalletSnapshot = WalletStorage

queryDisk
    :: (EventState event ~ WalletStorage, QueryEvent event,
        MonadWalletWebDB ctx m, MonadIO m)
    => event -> m (EventResult event)
queryDisk e = getWalletWebState >>= flip A.query e

queryValue
    :: WalletStorage -> S.Query a -> a
queryValue ws q = runReader q ws

updateDisk
    :: (EventState event ~ WalletStorage, UpdateEvent event,
        MonadWalletWebDB ctx m, MonadIO m)
    => event -> m (EventResult event)
updateDisk e = getWalletWebState >>= flip A.update e

-- | All queries work by doing a /single/ read of the DB state and then
-- by using pure functions to extract the relevant information. A single read
-- guarantees that we see a self-consistent snapshot of the wallet state.
--
getWalletSnapshot :: WebWalletModeDB ctx m => m WalletSnapshot
getWalletSnapshot = queryDisk A.GetWalletStorage

doesAccountExist :: WalletSnapshot -> AccountId -> Bool
doesAccountExist ws accid = queryValue ws (S.doesAccountExist accid)

getAccountIds :: WalletSnapshot -> [AccountId]
getAccountIds ws = queryValue ws S.getAccountIds

getAccountMetas :: WalletSnapshot -> [CAccountMeta]
getAccountMetas ws = queryValue ws S.getAccountMetas

getAccountMeta :: WalletSnapshot -> AccountId -> Maybe CAccountMeta
getAccountMeta ws accid = queryValue ws (S.getAccountMeta accid)

getAccountAddrMaps :: WalletSnapshot -> AccountId -> CurrentAndRemoved CAddresses
getAccountAddrMaps ws accid = queryValue ws (S.getAccountAddrMaps accid)

getWalletAddresses :: WalletSnapshot -> [CId Wal]
getWalletAddresses ws = queryValue ws S.getWalletAddresses

getWalletMeta :: WalletSnapshot -> CId Wal -> Maybe CWalletMeta
getWalletMeta ws wid = queryValue ws (S.getWalletMeta wid)

getWalletMetaIncludeUnready
    :: WalletSnapshot -> Bool -> CId Wal -> Maybe CWalletMeta
getWalletMetaIncludeUnready ws includeReady wid =
    queryValue ws (S.getWalletMetaIncludeUnready includeReady wid)

getWalletMetas :: WalletSnapshot -> [CWalletMeta]
getWalletMetas ws = queryValue ws S.getWalletMetas

getWalletPassLU :: WalletSnapshot -> CId Wal -> Maybe PassPhraseLU
getWalletPassLU ws wid = queryValue ws (S.getWalletPassLU wid)

getWalletSyncTip :: WalletSnapshot -> CId Wal -> Maybe WalletTip
getWalletSyncTip ws wid = queryValue ws (S.getWalletSyncTip wid)

getAccountWAddresses
    :: WalletSnapshot -> AddressLookupMode -> AccountId -> Maybe [AddressInfo]
getAccountWAddresses ws mode wid =
    queryValue ws (S.getAccountWAddresses mode wid)

doesWAddressExist
    :: WalletSnapshot -> AddressLookupMode -> CWAddressMeta -> Bool
doesWAddressExist ws mode addr = queryValue ws (S.doesWAddressExist mode addr)

getProfile :: WalletSnapshot -> CProfile
getProfile ws = queryValue ws S.getProfile

getTxMeta :: WalletSnapshot -> CId Wal -> CTxId -> Maybe CTxMeta
getTxMeta ws wid txid = queryValue ws (S.getTxMeta wid txid)

getWalletTxHistory :: WalletSnapshot -> CId Wal -> Maybe [CTxMeta]
getWalletTxHistory ws wid = queryValue ws (S.getWalletTxHistory wid)

getUpdates :: WalletSnapshot -> [CUpdateInfo]
getUpdates ws = queryValue ws S.getUpdates

getNextUpdate :: WalletSnapshot -> Maybe CUpdateInfo
getNextUpdate ws = queryValue ws S.getNextUpdate

getHistoryCache :: WalletSnapshot -> CId Wal -> Maybe (Map TxId TxHistoryEntry)
getHistoryCache ws wid = queryValue ws (S.getHistoryCache wid)

getCustomAddresses :: WalletSnapshot -> CustomAddressType -> [CId Addr]
getCustomAddresses ws addrtype = queryValue ws (S.getCustomAddresses addrtype)

getCustomAddress
    :: WalletSnapshot -> CustomAddressType -> CId Addr -> Maybe HeaderHash
getCustomAddress ws addrtype addrid =
    queryValue ws (S.getCustomAddress addrtype addrid)

isCustomAddress :: WalletSnapshot -> CustomAddressType -> CId Addr -> Bool
isCustomAddress ws addrtype addrid =
    isJust (getCustomAddress ws addrtype addrid)

getPendingTxs :: WalletSnapshot -> [PendingTx]
getPendingTxs ws = queryValue ws S.getPendingTxs

getWalletPendingTxs :: WalletSnapshot -> CId Wal -> Maybe [PendingTx]
getWalletPendingTxs ws wid = queryValue ws (S.getWalletPendingTxs wid)

getPendingTx :: WalletSnapshot -> CId Wal -> TxId -> Maybe PendingTx
getPendingTx ws wid txid = queryValue ws (S.getPendingTx wid txid)

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

getWalletUtxo :: WalletSnapshot -> Utxo
getWalletUtxo ws = queryValue ws S.getWalletUtxo

getWalletBalancesAndUtxo :: WalletSnapshot -> (WalletBalances, Utxo)
getWalletBalancesAndUtxo ws = queryValue ws S.getWalletBalancesAndUtxo

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

cancelApplyingPtxs :: WebWalletModeDB ctx m => m ()
cancelApplyingPtxs = updateDisk ... A.CancelApplyingPtxs

cancelSpecificApplyingPtx :: WebWalletModeDB ctx m => TxId -> m ()
cancelSpecificApplyingPtx txid = updateDisk ... A.CancelSpecificApplyingPtx txid

flushWalletStorage :: WebWalletModeDB ctx m => m ()
flushWalletStorage = updateDisk A.FlushWalletStorage

