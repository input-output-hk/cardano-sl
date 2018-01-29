{-# LANGUAGE TypeFamilies #-}

-- | A module which contains an interface to acidic Wallet DB.
-- See "Pos.Wallet.Web.State.Storage" for documentation for
-- DB methods.
module Pos.Wallet.Web.State.State
       ( WalletState
       , WalletTip (..)
       , PtxMetaUpdate (..)
       , AddressInfo (..)
       , getWalletWebState
       , MonadWalletDBAccess
       , MonadWalletDBRead
       , MonadWalletDB
       , openState
       , openMemState
       , closeState

       , AddressLookupMode (..)
       , CustomAddressType (..)
       , CurrentAndRemoved (..)
       , WalBalancesAndUtxo

       -- * Getters
       , getProfile
       , getAccountIds
       , getAccountMeta
       , getAccountAddrMaps
       , getAccountWAddresses
       , getWalletMeta
       , getWalletMetaIncludeUnready
       , getWalletPassLU
       , getWalletSyncTip
       , getWalletAddresses
       , doesAccountExist
       , doesWAddressExist
       , getTxMeta
       , getWalletTxHistory
       , getNextUpdate
       , getHistoryCache
       , getCustomAddresses
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
       , addOnlyNewTxMeta
       , removeWallet
       , removeWalletTxMetas
       , removeTxMetas
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
       , getWalletStorage
       , flushWalletStorage
       ) where

import           Data.Acid (EventResult, EventState, QueryEvent, UpdateEvent)
import qualified Data.Map as Map
import           Ether.Internal (lensOf)
import           Universum

import           Pos.Client.Txp.History (TxHistoryEntry)
import           Pos.Core (HeaderHash, SlotId)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Txp (TxId)
import           Pos.Txp (Utxo, UtxoModifier)
import           Pos.Util.Servant (encodeCType)
import           Pos.Util.Util (HasLens')
import           Pos.Wallet.Web.ClientTypes (AccountId, Addr, CAccountMeta, CId, CProfile, CTxId,
                                             CTxMeta, CUpdateInfo, CWAddressMeta, CWalletMeta,
                                             PassPhraseLU, Wal)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition)
import           Pos.Wallet.Web.State.Acidic (WalletState, closeState, openMemState, openState)
import           Pos.Wallet.Web.State.Acidic as A
import           Pos.Wallet.Web.State.Storage (AddressInfo (..), AddressLookupMode (..),
                                               CurrentAndRemoved (..), CustomAddressType (..),
                                               PtxMetaUpdate (..), WalBalancesAndUtxo,
                                               WalletBalances, WalletStorage, WalletTip (..))

-- | Type constraint which only allows access to
-- wallet DB state handler.
type MonadWalletDBAccess ctx m =
    ( HasLens' ctx WalletState
    , MonadReader ctx m
    )

-- | Type constraint which only allows reading from
-- wallet DB.
type MonadWalletDBRead ctx m =
    ( MonadIO m
    , MonadWalletDBAccess ctx m
    , HasConfiguration
    )

-- | Dummy typeclass which allows writing to wallet DB.
-- Why it's done this way:
--
--   * In practice, 'MonadWalletDBRead' allows applying
--     update events to database
--   * To separate type constraints for read-only and writing
--     modes to database artificially we explicitly require different
--     constraints for 'queryDisk' and 'updateDisk'
--   * If 'MonadWalletDB' was simply a type alias, no type error
--     would arise if 'updateDisk' is used in 'MonadWalletDBRead' context.
class MonadWalletDBRead ctx m => MonadWalletDB ctx m

-- | Get wallet DB state handler.
getWalletWebState :: (MonadReader ctx m, HasLens' ctx WalletState) => m WalletState
getWalletWebState = view (lensOf @WalletState)

-- | Perform a query to wallet DB.
queryDisk
    :: (EventState event ~ WalletStorage, QueryEvent event, MonadWalletDBRead ctx m)
    => event -> m (EventResult event)
queryDisk e = getWalletWebState >>= flip A.query e

-- | Perform an update to wallet DB.
updateDisk
    :: (EventState event ~ WalletStorage, UpdateEvent event, MonadWalletDB ctx m)
    => event -> m (EventResult event)
updateDisk e = getWalletWebState >>= flip A.update e

---------------------------------------------------------------------------------
-- Wallet DB API
---------------------------------------------------------------------------------

-- Following methods are wrappers over acidic actions.
-- For documentation on those methods look up their counterparts of the same name
-- in "Pos.Wallet.Web.State.Storage".

getAccountIds :: MonadWalletDBRead ctx m => m [AccountId]
getAccountIds = queryDisk A.GetAccountIds

getAccountMeta :: MonadWalletDBRead ctx m => AccountId -> m (Maybe CAccountMeta)
getAccountMeta = queryDisk . A.GetAccountMeta

getAccountAddrMaps
    :: MonadWalletDBRead ctx m
    => AccountId -> m (CurrentAndRemoved (HashMap (CId Addr) AddressInfo))
getAccountAddrMaps = queryDisk . A.GetAccountAddrMaps

getWalletAddresses :: MonadWalletDBRead ctx m => m [CId Wal]
getWalletAddresses = queryDisk A.GetWalletAddresses

getWalletMeta :: MonadWalletDBRead ctx m => CId Wal -> m (Maybe CWalletMeta)
getWalletMeta = queryDisk . A.GetWalletMeta

getWalletMetaIncludeUnready :: MonadWalletDBRead ctx m => Bool -> CId Wal -> m (Maybe CWalletMeta)
getWalletMetaIncludeUnready includeReady = queryDisk . A.GetWalletMetaIncludeUnready includeReady

getWalletPassLU :: MonadWalletDBRead ctx m => CId Wal -> m (Maybe PassPhraseLU)
getWalletPassLU = queryDisk . A.GetWalletPassLU

getWalletSyncTip :: MonadWalletDBRead ctx m => CId Wal -> m (Maybe WalletTip)
getWalletSyncTip = queryDisk . A.GetWalletSyncTip

getAccountWAddresses
    :: MonadWalletDBRead ctx m
    => AddressLookupMode -> AccountId -> m (Maybe [AddressInfo])
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

getNextUpdate :: MonadWalletDBRead ctx m => m (Maybe CUpdateInfo)
getNextUpdate = queryDisk A.GetNextUpdate

getHistoryCache :: MonadWalletDBRead ctx m => CId Wal -> m (Maybe (Map TxId TxHistoryEntry))
getHistoryCache = queryDisk . A.GetHistoryCache

getCustomAddresses :: MonadWalletDBRead ctx m => CustomAddressType -> m [(CId Addr, HeaderHash)]
getCustomAddresses = queryDisk ... A.GetCustomAddresses

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

doesAccountExist :: MonadWalletDBRead ctx m => AccountId -> m Bool
doesAccountExist = queryDisk . A.DoesAccountExist

setWalletTxMeta :: MonadWalletDB ctx m => CId Wal -> CTxId -> CTxMeta -> m ()
setWalletTxMeta cWalId cTxId = updateDisk . A.SetWalletTxMeta cWalId cTxId

addOnlyNewTxMetas :: MonadWalletDB ctx m => CId Wal -> Map TxId CTxMeta -> m ()
addOnlyNewTxMetas cWalId cTxMetas = updateDisk (A.AddOnlyNewTxMetas cWalId cTxMetaList)
    where
      cTxMetaList = [ (encodeCType txId, cTxMeta) | (txId, cTxMeta) <- Map.toList cTxMetas ]

getWalletUtxo :: MonadWalletDBRead ctx m => m Utxo
getWalletUtxo = queryDisk A.GetWalletUtxo

getWalletBalancesAndUtxo :: MonadWalletDBRead ctx m => m (WalletBalances, Utxo)
getWalletBalancesAndUtxo = queryDisk A.GetWalletBalancesAndUtxo

updateWalletBalancesAndUtxo :: MonadWalletDB ctx m => UtxoModifier -> m ()
updateWalletBalancesAndUtxo = updateDisk . A.UpdateWalletBalancesAndUtxo

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

cancelApplyingPtxs :: MonadWalletDB ctx m => m ()
cancelApplyingPtxs = updateDisk ... A.CancelApplyingPtxs

cancelSpecificApplyingPtx :: MonadWalletDB ctx m => TxId -> m ()
cancelSpecificApplyingPtx txid = updateDisk ... A.CancelSpecificApplyingPtx txid

resetFailedPtxs :: MonadWalletDB ctx m => SlotId -> m ()
resetFailedPtxs = updateDisk ... A.ResetFailedPtxs

flushWalletStorage :: MonadWalletDB ctx m => m ()
flushWalletStorage = updateDisk A.FlushWalletStorage

getWalletStorage :: MonadWalletDBRead ctx m => m WalletStorage
getWalletStorage = queryDisk A.GetWalletStorage
