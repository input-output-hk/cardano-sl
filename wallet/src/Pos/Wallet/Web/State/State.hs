{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Wallet.Web.State.State
       ( WalletDB
       , WalletDbReader
       , WalletSyncState (..)
       , RestorationBlockDepth (..)
       , SyncThroughput (..)
       , SyncStatistics (..)
       , PtxMetaUpdate (..)
       , AddressInfo (..)
       , S.WalBalancesAndUtxo
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
       , getAccountMeta
       , getAccountAddrMaps
       , getAccountWAddresses
       , getWAddresses
       , getWalletMeta
       , getWalletMetaIncludeUnready
       , getWalletPassLU
       , getWalletInfo
       , getWalletSyncState
       , getWalletAddresses
       , doesWAddressExist
       , getTxMeta
       , getWalletTxHistory
       , getNextUpdate
       , getHistoryCache
       , getCustomAddresses
       , getCustomAddress
       , isCustomAddress
       , isWalletRestoring
       , getWalletUtxo
       , getWalletBalancesAndUtxo
       , updateWalletBalancesAndUtxo
       , getPendingTxs
       , getWalletPendingTxs
       , getPendingTx

       -- * Setters
       , testReset
       , createAccount
       , createAccountWithAddress
       , createWallet
       , addWAddress
       , addCustomAddress
       , setProfile
       , setAccountMeta
       , setWalletMeta
       , setWalletReady
       , setWalletPassLU
       , setWalletSyncTip
       , setWalletRestorationSyncTip
       , updateSyncStatistics
       , addOnlyNewTxMetas
       , addOnlyNewTxMeta
       , removeWallet
       , removeWalletTxMetas
       , removeHistoryCache
       , removeAccount
       , removeWAddress
       , removeCustomAddress
       , addUpdate
       , removeNextUpdate
       , insertIntoHistoryCache
       , removeFromHistoryCache
       , setWalletUtxo
       , setPtxCondition
       , casPtxCondition
       , removeOnlyCreatingPtx
       , ptxUpdateMeta
       , addOnlyNewPendingTx
       , cancelApplyingPtxs
       , cancelSpecificApplyingPtx
       , resetFailedPtxs
       , flushWalletStorage
       , applyModifierToWallet
       , rollbackModifierFromWallet
       ) where

import           Data.Acid (EventResult, EventState, QueryEvent, UpdateEvent)
import qualified Data.Map as Map
import           Pos.Client.Txp.History (TxHistoryEntry)
import           Pos.Core (Address, ChainDifficulty, HasConfiguration, HasProtocolConstants, SlotId)
import           Pos.Core.Common (HeaderHash)
import           Pos.Txp (TxId, Utxo, UtxoModifier)
import           Pos.Util.Servant (encodeCType)
import           Pos.Util.Util (HasLens', lensOf)
import           Pos.Wallet.Web.ClientTypes (AccountId, CAccountMeta, CId, CProfile, CTxId, CTxMeta,
                                             CUpdateInfo, CWalletMeta, PassPhraseLU, Wal)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition)
import           Pos.Wallet.Web.State.Acidic (WalletDB, closeState, openMemState, openState)
import           Pos.Wallet.Web.State.Acidic as A
import           Pos.Wallet.Web.State.Storage (AddressInfo (..), AddressLookupMode (..), CAddresses,
                                               CurrentAndRemoved (..), CustomAddressType (..),
                                               PtxMetaUpdate (..), RestorationBlockDepth (..),
                                               SyncStatistics, SyncThroughput, WalletBalances,
                                               WalletInfo, WalletStorage, WalletSyncState (..))
import qualified Pos.Wallet.Web.State.Storage as S
import           Universum

-- TODO, remove HasConfiguration

-- | The 'WalletDbReader' constraint encapsulates the set of effects which
-- are able to read the 'WalletDB'.
type WalletDbReader ctx m =
    ( MonadReader ctx m
    , HasLens' ctx WalletDB
    )

-- | Reads the 'WalletDB'.
askWalletDB :: WalletDbReader ctx m => m WalletDB
askWalletDB = view (lensOf @WalletDB)

type WalletSnapshot = WalletStorage

queryDisk
    :: (EventState event ~ WalletStorage, QueryEvent event,
        WalletDbReader ctx m, MonadIO m)
    => event -> m (EventResult event)
queryDisk e = askWalletDB >>= flip A.query e

queryValue
    :: WalletStorage -> S.Query a -> a
queryValue ws q = runReader q ws

updateDisk :: (MonadIO m, EventState event ~ WalletStorage, UpdateEvent event)
           => event -> WalletDB -> m (EventResult event)
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

getWalletPassLU :: WalletSnapshot -> CId Wal -> Maybe PassPhraseLU
getWalletPassLU ws wid = queryValue ws (S.getWalletPassLU wid)

getWalletInfo :: WalletSnapshot -> CId Wal -> Maybe WalletInfo
getWalletInfo ws wid = queryValue ws (S.getWalletInfo wid)

getWalletSyncState :: WalletSnapshot -> CId Wal -> Maybe WalletSyncState
getWalletSyncState ws wid = queryValue ws (S.getWalletSyncState wid)

getAccountWAddresses
    :: WalletSnapshot -> AddressLookupMode -> AccountId -> Maybe [AddressInfo]
getAccountWAddresses ws mode wid =
    queryValue ws (S.getAccountWAddresses mode wid)

-- | Get the 'AddressInfo' corresponding to all accounts in this wallet.
getWAddresses :: WalletSnapshot -> AddressLookupMode -> CId Wal -> [AddressInfo]
getWAddresses ws mode wid = queryValue ws (S.getWAddresses mode wid)

doesWAddressExist
    :: WalletSnapshot -> AddressLookupMode -> S.WAddressMeta -> Bool
doesWAddressExist ws mode addr = queryValue ws (S.doesWAddressExist mode addr)

isWalletRestoring :: WalletSnapshot -> CId Wal -> Bool
isWalletRestoring ws walletId = queryValue ws (S.isWalletRestoring walletId)

getProfile :: WalletSnapshot -> CProfile
getProfile ws = queryValue ws S.getProfile

getTxMeta :: WalletSnapshot -> CId Wal -> CTxId -> Maybe CTxMeta
getTxMeta ws wid txid = queryValue ws (S.getTxMeta wid txid)

getWalletTxHistory :: WalletSnapshot -> CId Wal -> Maybe [CTxMeta]
getWalletTxHistory ws wid = queryValue ws (S.getWalletTxHistory wid)

getNextUpdate :: WalletSnapshot -> Maybe CUpdateInfo
getNextUpdate ws = queryValue ws S.getNextUpdate

getHistoryCache :: WalletSnapshot -> CId Wal -> Map TxId TxHistoryEntry
getHistoryCache ws wid = queryValue ws (S.getHistoryCache wid)

getCustomAddresses :: WalletSnapshot -> CustomAddressType -> [(Address, HeaderHash)]
getCustomAddresses ws addrtype = queryValue ws (S.getCustomAddresses addrtype)

getCustomAddress
    :: WalletSnapshot -> CustomAddressType -> Address -> Maybe HeaderHash
getCustomAddress ws addrtype addrid =
    queryValue ws (S.getCustomAddress addrtype addrid)

isCustomAddress :: WalletSnapshot -> CustomAddressType -> Address -> Bool
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

createAccount :: (MonadIO m, HasConfiguration)
              => WalletDB
              -> AccountId
              -> CAccountMeta
              -> m ()
createAccount db accId accMeta =
    updateDisk (A.CreateAccount accId accMeta) db

createAccountWithAddress :: (MonadIO m, HasConfiguration)
                         => WalletDB
                         -> AccountId
                         -> CAccountMeta
                         -> S.WAddressMeta
                         -> m ()
createAccountWithAddress db accId accMeta addrMeta =
    updateDisk (A.CreateAccountWithAddress accId accMeta addrMeta) db

createWallet :: (MonadIO m, HasConfiguration)
             => WalletDB
             -> CId Wal
             -> CWalletMeta
             -> Bool
             -> PassPhraseLU
             -> m ()
createWallet db cWalId cwMeta isReady lastUpdate =
    updateDisk (A.CreateWallet cWalId cwMeta isReady lastUpdate) db

addWAddress :: (MonadIO m, HasConfiguration)
            => WalletDB
            -> S.WAddressMeta
            -> m ()
addWAddress db addr = updateDisk (A.AddWAddress addr) db

addCustomAddress :: (MonadIO m, HasConfiguration)
                 => WalletDB
                 -> CustomAddressType
                 -> (Address, HeaderHash)
                 -> m Bool
addCustomAddress db customAddrType addrAndHash =
    updateDisk (A.AddCustomAddress customAddrType addrAndHash) db

setAccountMeta :: (MonadIO m, HasConfiguration)
               => WalletDB -> AccountId -> CAccountMeta  -> m ()
setAccountMeta db accId accMeta =
    updateDisk (A.SetAccountMeta accId accMeta) db

setWalletMeta :: (MonadIO m, HasConfiguration)
              => WalletDB -> CId Wal -> CWalletMeta  -> m ()
setWalletMeta db cWalId walletMeta =
    updateDisk (A.SetWalletMeta cWalId walletMeta) db

setWalletReady :: (MonadIO m, HasConfiguration)
               => WalletDB -> CId Wal -> Bool  -> m ()
setWalletReady db cWalId isReady =
    updateDisk (A.SetWalletReady cWalId isReady) db

setWalletPassLU :: (MonadIO m, HasConfiguration)
                => WalletDB -> CId Wal -> PassPhraseLU  -> m ()
setWalletPassLU db cWalId lastUpdate =
    updateDisk (A.SetWalletPassLU cWalId lastUpdate) db

setWalletSyncTip :: (MonadIO m, HasConfiguration)
                 => WalletDB -> CId Wal -> HeaderHash  -> m ()
setWalletSyncTip db cWalId headerHash =
    updateDisk (A.SetWalletSyncTip cWalId headerHash) db

setWalletRestorationSyncTip :: (MonadIO m, HasConfiguration)
                            => WalletDB
                            -> CId Wal
                            -> RestorationBlockDepth
                            -> HeaderHash  -> m ()
setWalletRestorationSyncTip db cWalId rbd headerHash =
    updateDisk (A.SetWalletRestorationSyncTip cWalId rbd headerHash) db

updateSyncStatistics :: (MonadIO m, HasConfiguration)
                     => WalletDB
                     -> CId Wal
                     -> SyncStatistics
                     -> m ()
updateSyncStatistics db cWalId stats =
    updateDisk (A.UpdateSyncStatistics cWalId stats) db

setProfile :: (MonadIO m, HasConfiguration)
           => WalletDB -> CProfile  -> m ()
setProfile db cProfile = updateDisk (A.SetProfile cProfile) db

addOnlyNewTxMetas :: (MonadIO m, HasConfiguration)
                  => WalletDB -> CId Wal -> Map TxId CTxMeta  -> m ()
addOnlyNewTxMetas db cWalId cTxMetas =
    updateDisk (A.AddOnlyNewTxMetas cWalId cTxMetaList) db
    where
      cTxMetaList = [ (encodeCType txId, cTxMeta) | (txId, cTxMeta) <- Map.toList cTxMetas ]

updateWalletBalancesAndUtxo :: (MonadIO m, HasConfiguration)
                            => WalletDB
                            -> UtxoModifier
                            -> m ()
updateWalletBalancesAndUtxo db utxoModifier =
    updateDisk (A.UpdateWalletBalancesAndUtxo utxoModifier) db

setWalletUtxo :: (MonadIO m, HasConfiguration)
              => WalletDB -> Utxo  -> m ()
setWalletUtxo db utxo = updateDisk (A.SetWalletUtxo utxo) db

addOnlyNewTxMeta :: (MonadIO m, HasConfiguration)
                 => WalletDB -> CId Wal -> CTxId -> CTxMeta  -> m ()
addOnlyNewTxMeta db walletId txId txMeta =
    updateDisk (A.AddOnlyNewTxMeta walletId txId txMeta) db

-- | Remove a wallet and all associated data:
--   - Associated accounts
--   - Transaction metadata
--   - History cache
--
--   Note that this functionality has changed - the old version of
--   'removeWallet' did not used to remove the associated data.
--   This functionality was not used anywhere and was therefore
--   removed. Should it be needed again, one should add 'removeWallet'
--   to the set of acidic updates and add a suitable function in this
--   module to invoke it.
removeWallet :: (MonadIO m, HasConfiguration)
             => WalletDB -> CId Wal  -> m ()
removeWallet db walletId = updateDisk (A.RemoveWallet2 walletId) db

removeWalletTxMetas :: (MonadIO m, HasConfiguration)
                    => WalletDB -> CId Wal -> [CTxId]  -> m ()
removeWalletTxMetas db walletId txIds =
    updateDisk (A.RemoveWalletTxMetas walletId txIds) db

removeHistoryCache :: (MonadIO m, HasConfiguration)
                   => WalletDB
                   -> CId Wal
                   -> m ()
removeHistoryCache db walletId = updateDisk (A.RemoveHistoryCache walletId) db

removeAccount :: (MonadIO m, HasConfiguration)
              => WalletDB
              -> AccountId
              -> m ()
removeAccount db accountId = updateDisk (A.RemoveAccount accountId) db

removeWAddress :: (MonadIO m, HasConfiguration)
               => WalletDB
               -> S.WAddressMeta
               -> m ()
removeWAddress db addrMeta = updateDisk (A.RemoveWAddress addrMeta) db

removeCustomAddress :: (MonadIO m, HasConfiguration)
                    => WalletDB
                    -> CustomAddressType
                    -> (Address, HeaderHash)
                    -> m Bool
removeCustomAddress db customAddrType aIdAndHeaderHash =
    updateDisk (A.RemoveCustomAddress customAddrType aIdAndHeaderHash) db

addUpdate :: (MonadIO m, HasConfiguration)
          => WalletDB
          -> CUpdateInfo
          -> m ()
addUpdate db updateInfo =
    updateDisk (A.AddUpdate updateInfo) db

removeNextUpdate :: (MonadIO m, HasConfiguration)
                 => WalletDB
                 -> m ()
removeNextUpdate = updateDisk A.RemoveNextUpdate

testReset :: (MonadIO m, HasConfiguration)
          => WalletDB
          -> m ()
testReset = updateDisk A.TestReset

insertIntoHistoryCache :: (MonadIO m, HasConfiguration)
                       => WalletDB
                       -> CId Wal
                       -> Map TxId TxHistoryEntry
                       -> m ()
insertIntoHistoryCache db cWalId cTxs
  | Map.null cTxs = return ()
  | otherwise     = updateDisk (A.InsertIntoHistoryCache cWalId cTxs) db

removeFromHistoryCache :: (MonadIO m, HasConfiguration)
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

setPtxCondition :: (MonadIO m, HasConfiguration)
                => WalletDB
                -> CId Wal
                -> TxId
                -> PtxCondition
                -> m ()
setPtxCondition db walletId txId condition =
    updateDisk (A.SetPtxCondition walletId txId condition) db

casPtxCondition :: (MonadIO m, HasConfiguration)
                => WalletDB
                -> CId Wal
                -> TxId
                -> PtxCondition
                -> PtxCondition
                -> m Bool
casPtxCondition db walletId txId old new =
    updateDisk (A.CasPtxCondition walletId txId old new) db

removeOnlyCreatingPtx
    :: (HasConfiguration, MonadIO m)
    => WalletDB
    -> CId Wal
    -> TxId
    -> m Bool
removeOnlyCreatingPtx db walletId txId =
    updateDisk (A.RemoveOnlyCreatingPtx walletId txId) db

ptxUpdateMeta :: (HasProtocolConstants, HasConfiguration, MonadIO m)
              => WalletDB
              -> CId Wal
              -> TxId
              -> PtxMetaUpdate
              -> m ()
ptxUpdateMeta db walletId txId metaUpdate =
    updateDisk (A.PtxUpdateMeta walletId txId metaUpdate) db

addOnlyNewPendingTx :: (MonadIO m, HasConfiguration)
                    => WalletDB
                    -> PendingTx
                    -> m ()
addOnlyNewPendingTx db pendingTx = updateDisk (A.AddOnlyNewPendingTx pendingTx) db

cancelApplyingPtxs :: (MonadIO m, HasConfiguration)
                   => WalletDB
                   -> m ()
cancelApplyingPtxs = updateDisk A.CancelApplyingPtxs

cancelSpecificApplyingPtx :: (MonadIO m, HasConfiguration)
                          => WalletDB -> TxId  -> m ()
cancelSpecificApplyingPtx db txid = updateDisk (A.CancelSpecificApplyingPtx txid) db

resetFailedPtxs :: (MonadIO m, HasConfiguration)
                => WalletDB
                -> SlotId
                -> m ()
resetFailedPtxs db slotId = updateDisk (A.ResetFailedPtxs slotId) db

flushWalletStorage :: (MonadIO m, HasConfiguration)
                   => WalletDB
                   -> m ()
flushWalletStorage = updateDisk A.FlushWalletStorage

applyModifierToWallet
  :: (MonadIO m, HasConfiguration)
  => WalletDB
  -> CId Wal
  -> [S.WAddressMeta] -- ^ Wallet addresses to add
  -> [(S.CustomAddressType, [(Address, HeaderHash)])] -- ^ Custom addresses to add
  -> UtxoModifier
  -> [(CTxId, CTxMeta)] -- ^ Transaction metadata to add
  -> Map TxId TxHistoryEntry -- ^ Entries for the history cache
  -> [(TxId, PtxCondition)] -- ^ PTX Conditions
  -> ChainDifficulty -- ^ The current depth of the blockchain
  -> WalletSyncState -- ^ New 'WalletSyncState'
  -> m ()
applyModifierToWallet db walId wAddrs custAddrs utxoMod
                      txMetas historyEntries ptxConditions
                      currentDepth syncState =
    updateDisk
      ( A.ApplyModifierToWallet2
          walId wAddrs custAddrs utxoMod
          txMetas historyEntries ptxConditions currentDepth syncState
      )
      db

rollbackModifierFromWallet
  :: (MonadIO m, HasConfiguration, HasProtocolConstants)
  => WalletDB
  -> CId Wal
  -> [S.WAddressMeta] -- ^ Addresses to remove
  -> [(S.CustomAddressType, [(Address, HeaderHash)])] -- ^ Custom addresses to remove
  -> UtxoModifier
     -- We use this odd representation because Data.Map does not get 'withoutKeys'
     -- until 5.8.1
  -> Map TxId a -- ^ Entries to remove from history cache.
  -> [(TxId, PtxCondition, S.PtxMetaUpdate)] -- ^ Deleted PTX candidates
  -> WalletSyncState -- ^ New 'WalletSyncState'
  -> m ()
rollbackModifierFromWallet db walId wAddrs custAddrs utxoMod
                            historyEntries ptxConditions
                            syncState =
    updateDisk
      ( A.RollbackModifierFromWallet2
          walId wAddrs custAddrs utxoMod
          historyEntries' ptxConditions syncState
      )
      db
  where
    historyEntries' = Map.map (const ()) historyEntries
