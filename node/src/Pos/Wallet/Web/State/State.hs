{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Wallet.Web.State.State
       ( WalletDB
       , WalletDbReader
       , WalletDbWriter
       , WalletTip (..)
       , PtxMetaUpdate (..)
       , AddressInfo (..)
       , askWalletDB
       , openState
       , openMemState
       , closeState

       , AddressLookupMode (..)
       , CurrentAndRemoved (..)
       , CustomAddressType (..)

       -- * Getters
       , WalletSnapshot
       , askWalletSnapshot
       , getWalletSnapshot
       , getProfile
       , doesAccountExist
       , getAccountIds
       , getAccountMetas
       , getAccountMeta
       , getAccountAddrMaps
       , getAccountWAddresses
       , getWAddresses
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
import           Universum

import           Pos.Client.Txp.History       (TxHistoryEntry)
import           Pos.Txp                      (TxId, Utxo, UtxoModifier)
import           Pos.Types                    (HeaderHash)
import           Pos.Util.Servant             (encodeCType)
import           Pos.Wallet.Web.ClientTypes   (AccountId, Addr, CAccountMeta, CId,
                                               CProfile, CTxId, CTxMeta, CUpdateInfo,
                                               CWAddressMeta, CWalletMeta, PassPhraseLU,
                                               Wal)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition)
import           Pos.Wallet.Web.State.Acidic  (WalletDB, closeState, openMemState,
                                               openState)
import           Pos.Wallet.Web.State.Acidic  as A
import qualified Pos.Wallet.Web.State.Storage as S
import           Pos.Wallet.Web.State.Storage (AddressInfo (..), AddressLookupMode (..),
                                               CAddresses, CurrentAndRemoved(..) , CustomAddressType (..),
                                               PtxMetaUpdate (..), WalletBalances,
                                               WalletStorage, WalletTip (..))

-- | The 'WalletDbReader' constraint encapsulates the set of effects which
-- are able to read the 'WalletDB'.
type WalletDbReader ctx m =
    ( MonadReader ctx m
    , HasLens WalletDB ctx WalletDB
    )

-- | Reads the 'WalletDB'.
askWalletDB :: WalletDbReader ctx m => m WalletDB
askWalletDB = view (lensOf @WalletDB)

-- | The 'WalletDbWriter' constraint encapsulate the fact we can write an updated
-- copy of the 'WalletStorage', but that we cannot read it back, if not as part
-- of an atomic update API.
--
-- Note that this is not a counterpart to 'WalletDbReader'. 'WalletDbReader'
-- governs the ability to acquire a DB handle from an environment. 'WalletDbWriter'
-- however relates to performing updates to the actual database.
type WalletDbWriter event m =
    ( MonadIO m
    , EventState event ~ WalletStorage
    , UpdateEvent event
    )

type WalletSnapshot = WalletStorage

queryDisk
    :: (EventState event ~ WalletStorage, QueryEvent event,
        WalletDbReader ctx m, MonadIO m)
    => event -> m (EventResult event)
queryDisk e = askWalletDB >>= flip A.query e

queryValue
    :: WalletStorage -> S.Query a -> a
queryValue ws q = runReader q ws

updateDisk :: WalletDbWriter event m => event -> WalletDB -> m (EventResult event)
updateDisk evt db = A.update db evt

-- | All queries work by doing a /single/ read of the DB state and then
-- by using pure functions to extract the relevant information. A single read
-- guarantees that we see a self-consistent snapshot of the wallet state.
--
askWalletSnapshot :: (WalletDbReader ctx m, MonadIO m) => m WalletSnapshot
askWalletSnapshot = queryDisk A.GetWalletStorage

-- | Get a snapshot of the wallet from an existing DB handle.
getWalletSnapshot :: MonadIO m
                  => WalletDB
                  -> m WalletSnapshot
getWalletSnapshot db = A.query db A.GetWalletStorage

--
-- Pure functions (Queries)
--

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

-- | Get the 'AddressInfo' corresponding to all accounts in this wallet.
getWAddresses :: WalletSnapshot -> AddressLookupMode -> CId Wal -> [AddressInfo]
getWAddresses ws mode wid = queryValue ws (S.getWAddresses mode wid)

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

getWalletUtxo :: WalletSnapshot -> Utxo
getWalletUtxo ws = queryValue ws S.getWalletUtxo

getWalletBalancesAndUtxo :: WalletSnapshot -> (WalletBalances, Utxo)
getWalletBalancesAndUtxo ws = queryValue ws S.getWalletBalancesAndUtxo


--
-- Effectful function (Updates)
--

createAccount :: (WalletDbWriter A.CreateAccount m)
              => WalletDB
              -> AccountId
              -> CAccountMeta
              -> m ()
createAccount db accId accMeta =
    updateDisk (A.CreateAccount accId accMeta) db

createWallet :: (WalletDbWriter A.CreateWallet m)
             => WalletDB
             -> CId Wal
             -> CWalletMeta
             -> Bool
             -> PassPhraseLU
             -> m ()
createWallet db cWalId cwMeta isReady lastUpdate =
    updateDisk (A.CreateWallet cWalId cwMeta isReady lastUpdate) db

addWAddress :: (WalletDbWriter A.AddWAddress m)
            => WalletDB
            -> CWAddressMeta
            -> m ()
addWAddress db addr = updateDisk (A.AddWAddress addr) db

addCustomAddress :: (WalletDbWriter A.AddCustomAddress m)
                 => WalletDB
                 -> CustomAddressType
                 -> (CId Addr, HeaderHash)
                 -> m Bool
addCustomAddress db customAddrType addrAndHash =
    updateDisk (A.AddCustomAddress customAddrType addrAndHash) db

addRemovedAccount :: (WalletDbWriter A.AddRemovedAccount m)
                  => WalletDB
                  -> CWAddressMeta
                  -> m ()
addRemovedAccount db addrMeta =
    updateDisk (A.AddRemovedAccount addrMeta) db

setAccountMeta :: (WalletDbWriter A.SetAccountMeta m)
               => WalletDB -> AccountId -> CAccountMeta  -> m ()
setAccountMeta db accId accMeta =
    updateDisk (A.SetAccountMeta accId accMeta) db

setWalletMeta :: (WalletDbWriter A.SetWalletMeta m)
              => WalletDB -> CId Wal -> CWalletMeta  -> m ()
setWalletMeta db cWalId walletMeta =
    updateDisk (A.SetWalletMeta cWalId walletMeta) db

setWalletReady :: (WalletDbWriter A.SetWalletReady m)
               => WalletDB -> CId Wal -> Bool  -> m ()
setWalletReady db cWalId isReady =
    updateDisk (A.SetWalletReady cWalId isReady) db

setWalletPassLU :: (WalletDbWriter A.SetWalletPassLU m)
                => WalletDB -> CId Wal -> PassPhraseLU  -> m ()
setWalletPassLU db cWalId lastUpdate =
    updateDisk (A.SetWalletPassLU cWalId lastUpdate) db

setWalletSyncTip :: (WalletDbWriter A.SetWalletSyncTip m)
                 => WalletDB -> CId Wal -> HeaderHash  -> m ()
setWalletSyncTip db cWalId headerHash =
    updateDisk (A.SetWalletSyncTip cWalId headerHash) db

setProfile :: (WalletDbWriter A.SetProfile m)
           => WalletDB -> CProfile  -> m ()
setProfile db cProfile = updateDisk (A.SetProfile cProfile) db

setWalletTxMeta :: (WalletDbWriter A.SetWalletTxMeta m)
                => WalletDB -> CId Wal -> CTxId -> CTxMeta  -> m ()
setWalletTxMeta db cWalId cTxId cTxMeta =
    updateDisk (A.SetWalletTxMeta cWalId cTxId cTxMeta) db

addOnlyNewTxMetas :: (WalletDbWriter A.AddOnlyNewTxMetas m)
                  => WalletDB -> CId Wal -> Map TxId CTxMeta  -> m ()
addOnlyNewTxMetas db cWalId cTxMetas =
    updateDisk (A.AddOnlyNewTxMetas cWalId cTxMetaList) db
    where
      cTxMetaList = [ (encodeCType txId, cTxMeta) | (txId, cTxMeta) <- Map.toList cTxMetas ]

setWalletTxHistory :: (WalletDbWriter A.SetWalletTxHistory m)
                   => WalletDB -> CId Wal -> [(CTxId, CTxMeta)]  -> m ()
setWalletTxHistory db cWalId idsAndMetas =
    updateDisk (A.SetWalletTxHistory cWalId idsAndMetas) db

updateWalletBalancesAndUtxo :: (WalletDbWriter A.UpdateWalletBalancesAndUtxo m)
                            => WalletDB
                            -> UtxoModifier
                            -> m ()
updateWalletBalancesAndUtxo db utxoModifier =
    updateDisk (A.UpdateWalletBalancesAndUtxo utxoModifier) db

setWalletUtxo :: (WalletDbWriter A.SetWalletUtxo m)
              => WalletDB -> Utxo  -> m ()
setWalletUtxo db utxo = updateDisk (A.SetWalletUtxo utxo) db

addOnlyNewTxMeta :: (WalletDbWriter A.AddOnlyNewTxMeta m)
                 => WalletDB -> CId Wal -> CTxId -> CTxMeta  -> m ()
addOnlyNewTxMeta db walletId txId txMeta =
    updateDisk (A.AddOnlyNewTxMeta walletId txId txMeta) db

removeWallet :: (WalletDbWriter A.RemoveWallet m)
             => WalletDB -> CId Wal  -> m ()
removeWallet db walletId = updateDisk (A.RemoveWallet walletId) db

removeTxMetas :: (WalletDbWriter A.RemoveTxMetas m)
              => WalletDB -> CId Wal  -> m ()
removeTxMetas db walletId = updateDisk (A.RemoveTxMetas walletId) db

removeWalletTxMetas :: (WalletDbWriter A.RemoveWalletTxMetas m)
                    => WalletDB -> CId Wal -> [CTxId]  -> m ()
removeWalletTxMetas db walletId txIds =
    updateDisk (A.RemoveWalletTxMetas walletId txIds) db

removeHistoryCache :: (WalletDbWriter A.RemoveHistoryCache m)
                   => WalletDB
                   -> CId Wal
                   -> m ()
removeHistoryCache db walletId = updateDisk (A.RemoveHistoryCache walletId) db

removeAccount :: (WalletDbWriter A.RemoveAccount m)
              => WalletDB
              -> AccountId
              -> m ()
removeAccount db accountId = updateDisk (A.RemoveAccount accountId) db

removeWAddress :: (WalletDbWriter A.RemoveWAddress m)
               => WalletDB
               -> CWAddressMeta
               -> m ()
removeWAddress db addrMeta = updateDisk (A.RemoveWAddress addrMeta) db

totallyRemoveWAddress :: (WalletDbWriter A.TotallyRemoveWAddress m)
                      => WalletDB
                      -> CWAddressMeta
                      -> m ()
totallyRemoveWAddress db addrMeta =
    updateDisk (A.TotallyRemoveWAddress addrMeta) db

removeCustomAddress :: (WalletDbWriter A.RemoveCustomAddress m)
                    => WalletDB
                    -> CustomAddressType
                    -> (CId Addr, HeaderHash)
                    -> m Bool
removeCustomAddress db customAddrType aIdAndHeaderHash =
    updateDisk (A.RemoveCustomAddress customAddrType aIdAndHeaderHash) db

addUpdate :: (WalletDbWriter A.AddUpdate m)
          => WalletDB
          -> CUpdateInfo
          -> m ()
addUpdate db updateInfo =
    updateDisk (A.AddUpdate updateInfo) db

removeNextUpdate :: (WalletDbWriter A.RemoveNextUpdate m)
                 => WalletDB
                 -> m ()
removeNextUpdate = updateDisk A.RemoveNextUpdate

testReset :: (WalletDbWriter A.TestReset m)
          => WalletDB
          -> m ()
testReset = updateDisk A.TestReset

insertIntoHistoryCache :: (WalletDbWriter A.InsertIntoHistoryCache m)
                       => WalletDB
                       -> CId Wal
                       -> Map TxId TxHistoryEntry
                       -> m ()
insertIntoHistoryCache db cWalId cTxs
  | Map.null cTxs = return ()
  | otherwise     = updateDisk (A.InsertIntoHistoryCache cWalId cTxs) db

removeFromHistoryCache :: ( WalletDbWriter A.RemoveFromHistoryCache m)
                       => WalletDB
                       -> CId Wal
                       -> Map TxId a
                       -> m ()
removeFromHistoryCache db cWalId cTxs
  | Map.null cTxs = return ()
  | otherwise     = updateDisk (A.RemoveFromHistoryCache cWalId cTxs') db
  where
    cTxs' :: Map TxId ()
    cTxs' = Map.map (const ()) cTxs

setPtxCondition :: ( WalletDbWriter A.SetPtxCondition m)
                => WalletDB
                -> CId Wal
                -> TxId
                -> PtxCondition
                -> m ()
setPtxCondition db walletId txId condition =
    updateDisk (A.SetPtxCondition walletId txId condition) db

casPtxCondition :: (WalletDbWriter A.CasPtxCondition m)
                => WalletDB
                -> CId Wal
                -> TxId
                -> PtxCondition
                -> PtxCondition
                -> m Bool
casPtxCondition db walletId txId old new =
    updateDisk (A.CasPtxCondition walletId txId old new) db

ptxUpdateMeta :: (WalletDbWriter A.PtxUpdateMeta m)
              => WalletDB
              -> CId Wal
              -> TxId
              -> PtxMetaUpdate
              -> m ()
ptxUpdateMeta db walletId txId metaUpdate =
    updateDisk (A.PtxUpdateMeta walletId txId metaUpdate) db

addOnlyNewPendingTx :: (WalletDbWriter A.AddOnlyNewPendingTx m)
                    => WalletDB
                    -> PendingTx
                    -> m ()
addOnlyNewPendingTx db pendingTx = updateDisk (A.AddOnlyNewPendingTx pendingTx) db

cancelApplyingPtxs :: (WalletDbWriter A.CancelApplyingPtxs m)
                   => WalletDB
                   -> m ()
cancelApplyingPtxs = updateDisk A.CancelApplyingPtxs

cancelSpecificApplyingPtx :: (WalletDbWriter A.CancelSpecificApplyingPtx m)
                          => WalletDB -> TxId  -> m ()
cancelSpecificApplyingPtx db txid = updateDisk (A.CancelSpecificApplyingPtx txid) db

flushWalletStorage :: ( WalletDbWriter A.FlushWalletStorage m)
                   => WalletDB
                   -> m ()
flushWalletStorage = updateDisk A.FlushWalletStorage
