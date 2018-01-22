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

updateDisk' :: forall m event. WalletDbWriter event m
            => event
            -> WalletDB
            -> m (EventResult event, WalletSnapshot)
updateDisk' evt db = do
    result       <- A.update db evt
    newSnapshot  <- readUpdatedDb
    pure (result, newSnapshot)
    where
      readUpdatedDb :: m WalletSnapshot
      readUpdatedDb = A.query db A.GetWalletStorage

-- | All queries work by doing a /single/ read of the DB state and then
-- by using pure functions to extract the relevant information. A single read
-- guarantees that we see a self-consistent snapshot of the wallet state.
--
getWalletSnapshot :: (WalletDbReader ctx m, MonadIO m) => m WalletSnapshot
getWalletSnapshot = queryDisk A.GetWalletStorage

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

createAccount :: ( WalletDbReader ctx m
                 , WalletDbWriter A.CreateAccount m
                 )
              => AccountId -> CAccountMeta -> m ((), WalletSnapshot)
createAccount accId accMeta =
    askWalletDB >>= updateDisk' (A.CreateAccount accId accMeta)

createWallet :: ( WalletDbReader ctx m
                , WalletDbWriter A.CreateWallet m
                ) => CId Wal -> CWalletMeta -> Bool -> PassPhraseLU -> m ()
createWallet cWalId cwMeta isReady lastUpdate =
    askWalletDB >>= updateDisk (A.CreateWallet cWalId cwMeta isReady lastUpdate)

addWAddress :: ( WalletDbReader ctx m
               , WalletDbWriter A.AddWAddress m
               ) => CWAddressMeta -> m ()
addWAddress addr = askWalletDB >>= updateDisk (A.AddWAddress addr)

addCustomAddress :: ( WalletDbReader ctx m
                    , WalletDbWriter A.AddCustomAddress m
                    ) => CustomAddressType -> (CId Addr, HeaderHash) -> m Bool
addCustomAddress customAddrType addrAndHash =
    askWalletDB >>= updateDisk (A.AddCustomAddress customAddrType addrAndHash)

addRemovedAccount :: ( WalletDbReader ctx m
                     , WalletDbWriter A.AddRemovedAccount m
                     ) => CWAddressMeta -> m ()
addRemovedAccount addrMeta =
    askWalletDB >>= updateDisk (A.AddRemovedAccount addrMeta)

setAccountMeta :: ( WalletDbReader ctx m
                  , WalletDbWriter A.SetAccountMeta m
                  ) => AccountId -> CAccountMeta -> m ()
setAccountMeta accId accMeta =
    askWalletDB >>= updateDisk (A.SetAccountMeta accId accMeta)

setWalletMeta :: ( WalletDbReader ctx m
                 , WalletDbWriter A.SetWalletMeta m
                 ) => CId Wal -> CWalletMeta -> m ()
setWalletMeta cWalId walletMeta =
    askWalletDB >>= updateDisk (A.SetWalletMeta cWalId walletMeta)

setWalletReady :: ( WalletDbReader ctx m
                  , WalletDbWriter A.SetWalletReady m
                  ) => CId Wal -> Bool -> m ()
setWalletReady cWalId isReady =
    askWalletDB >>= updateDisk (A.SetWalletReady cWalId isReady)

setWalletPassLU :: ( WalletDbReader ctx m
                   , WalletDbWriter A.SetWalletPassLU m
                   ) => CId Wal -> PassPhraseLU -> m ()
setWalletPassLU cWalId lastUpdate =
    askWalletDB >>= updateDisk (A.SetWalletPassLU cWalId lastUpdate)

setWalletSyncTip :: ( WalletDbReader ctx m
                    , WalletDbWriter A.SetWalletSyncTip m
                    ) => CId Wal -> HeaderHash -> m ()
setWalletSyncTip cWalId headerHash =
    askWalletDB >>= updateDisk (A.SetWalletSyncTip cWalId headerHash)

setProfile :: ( WalletDbReader ctx m
              , WalletDbWriter A.SetProfile m
              ) => CProfile -> m ()
setProfile cProfile = askWalletDB >>= updateDisk (A.SetProfile cProfile)

setWalletTxMeta :: ( WalletDbReader ctx m
                   , WalletDbWriter A.SetWalletTxMeta m
                   ) => CId Wal -> CTxId -> CTxMeta -> m ()
setWalletTxMeta cWalId cTxId cTxMeta =
    askWalletDB >>= updateDisk (A.SetWalletTxMeta cWalId cTxId cTxMeta)

addOnlyNewTxMetas :: ( WalletDbReader ctx m
                     , WalletDbWriter A.AddOnlyNewTxMetas m
                     ) => CId Wal -> Map TxId CTxMeta -> m ()
addOnlyNewTxMetas cWalId cTxMetas =
    askWalletDB >>= updateDisk (A.AddOnlyNewTxMetas cWalId cTxMetaList)
    where
      cTxMetaList = [ (encodeCType txId, cTxMeta) | (txId, cTxMeta) <- Map.toList cTxMetas ]

setWalletTxHistory :: ( WalletDbReader ctx m
                      , WalletDbWriter A.SetWalletTxHistory m
                      ) => CId Wal -> [(CTxId, CTxMeta)] -> m ()
setWalletTxHistory cWalId idsAndMetas =
    askWalletDB >>= updateDisk (A.SetWalletTxHistory cWalId idsAndMetas)

updateWalletBalancesAndUtxo :: ( WalletDbReader ctx m
                               , WalletDbWriter A.UpdateWalletBalancesAndUtxo m
                               ) => UtxoModifier -> m ()
updateWalletBalancesAndUtxo utxoModifier =
    askWalletDB >>= updateDisk (A.UpdateWalletBalancesAndUtxo utxoModifier)

setWalletUtxo :: ( WalletDbReader ctx m
                 , WalletDbWriter A.SetWalletUtxo m
                 ) => Utxo -> m ()
setWalletUtxo utxo = askWalletDB >>= updateDisk (A.SetWalletUtxo utxo)

addOnlyNewTxMeta :: ( WalletDbReader ctx m
                    , WalletDbWriter A.AddOnlyNewTxMeta m
                    ) => CId Wal -> CTxId -> CTxMeta -> m ()
addOnlyNewTxMeta walletId txId txMeta =
    askWalletDB >>= updateDisk (A.AddOnlyNewTxMeta walletId txId txMeta)

removeWallet :: ( WalletDbReader ctx m
                , WalletDbWriter A.RemoveWallet m
                ) => CId Wal -> m ()
removeWallet walletId = askWalletDB >>= updateDisk (A.RemoveWallet walletId)

removeTxMetas :: ( WalletDbReader ctx m
                 , WalletDbWriter A.RemoveTxMetas m
                 ) => CId Wal -> m ()
removeTxMetas walletId = askWalletDB >>= updateDisk (A.RemoveTxMetas walletId)

removeWalletTxMetas :: ( WalletDbReader ctx m
                       , WalletDbWriter A.RemoveWalletTxMetas m)
                    => CId Wal -> [CTxId] -> m ()
removeWalletTxMetas walletId txIds =
    askWalletDB >>= updateDisk (A.RemoveWalletTxMetas walletId txIds)

removeHistoryCache :: ( WalletDbReader ctx m
                      , WalletDbWriter A.RemoveHistoryCache m
                      ) => CId Wal -> m ()
removeHistoryCache walletId = askWalletDB >>= updateDisk (A.RemoveHistoryCache walletId)

removeAccount :: ( WalletDbReader ctx m
                 , WalletDbWriter A.RemoveAccount m
                 ) => AccountId -> m ()
removeAccount accountId = askWalletDB >>= updateDisk (A.RemoveAccount accountId)

removeWAddress :: ( WalletDbReader ctx m
                  , WalletDbWriter A.RemoveWAddress m
                  ) => CWAddressMeta -> m ()
removeWAddress addrMeta = askWalletDB >>= updateDisk (A.RemoveWAddress addrMeta)

totallyRemoveWAddress :: ( WalletDbReader ctx m
                         , WalletDbWriter A.TotallyRemoveWAddress m
                         ) => CWAddressMeta -> m ()
totallyRemoveWAddress addrMeta =
    askWalletDB >>= updateDisk (A.TotallyRemoveWAddress addrMeta)

removeCustomAddress
    :: ( WalletDbReader ctx m
       , WalletDbWriter A.RemoveCustomAddress m
       ) => CustomAddressType -> (CId Addr, HeaderHash) -> m Bool
removeCustomAddress customAddrType aIdAndHeaderHash =
    askWalletDB >>= updateDisk (A.RemoveCustomAddress customAddrType aIdAndHeaderHash)

addUpdate :: ( WalletDbReader ctx m
             , WalletDbWriter A.AddUpdate m
             ) => CUpdateInfo -> m ()
addUpdate updateInfo =
    askWalletDB >>= updateDisk (A.AddUpdate updateInfo)

removeNextUpdate :: ( WalletDbReader ctx m
                    , WalletDbWriter A.RemoveNextUpdate m
                    ) => m ()
removeNextUpdate = askWalletDB >>= updateDisk A.RemoveNextUpdate

testReset :: ( WalletDbReader ctx m
             , WalletDbWriter A.TestReset m
             ) => m ()
testReset = askWalletDB >>= updateDisk A.TestReset

insertIntoHistoryCache :: ( WalletDbReader ctx m
                          , WalletDbWriter A.InsertIntoHistoryCache m
                          ) => CId Wal -> Map TxId TxHistoryEntry -> m ()
insertIntoHistoryCache cWalId cTxs
  | Map.null cTxs = return ()
  | otherwise     = askWalletDB >>= updateDisk (A.InsertIntoHistoryCache cWalId cTxs)

removeFromHistoryCache :: ( WalletDbReader ctx m
                          , WalletDbWriter A.RemoveFromHistoryCache m
                          ) => CId Wal -> Map TxId a -> m ()
removeFromHistoryCache cWalId cTxs
  | Map.null cTxs = return ()
  | otherwise     = askWalletDB >>= updateDisk (A.RemoveFromHistoryCache cWalId cTxs')
  where
    cTxs' :: Map TxId ()
    cTxs' = Map.map (const ()) cTxs

setPtxCondition :: ( WalletDbReader ctx m
                   , WalletDbWriter A.SetPtxCondition m
                   ) => CId Wal -> TxId -> PtxCondition -> m ()
setPtxCondition walletId txId condition =
    askWalletDB >>= updateDisk (A.SetPtxCondition walletId txId condition)

casPtxCondition :: ( WalletDbReader ctx m
                   , WalletDbWriter A.CasPtxCondition m
                   ) => CId Wal -> TxId -> PtxCondition -> PtxCondition -> m Bool
casPtxCondition walletId txId old new =
    askWalletDB >>= updateDisk (A.CasPtxCondition walletId txId old new)

ptxUpdateMeta :: ( WalletDbReader ctx m
                 , WalletDbWriter A.PtxUpdateMeta m
                 ) => CId Wal -> TxId -> PtxMetaUpdate -> m ()
ptxUpdateMeta walletId txId metaUpdate =
    askWalletDB >>= updateDisk (A.PtxUpdateMeta walletId txId metaUpdate)

addOnlyNewPendingTx :: ( WalletDbReader ctx m
                       , WalletDbWriter A.AddOnlyNewPendingTx m
                       ) => PendingTx -> m ()
addOnlyNewPendingTx pendingTx = askWalletDB >>= updateDisk (A.AddOnlyNewPendingTx pendingTx)

cancelApplyingPtxs :: ( WalletDbReader ctx m
                      , WalletDbWriter A.CancelApplyingPtxs m
                      ) => m ()
cancelApplyingPtxs = askWalletDB >>= updateDisk A.CancelApplyingPtxs

cancelSpecificApplyingPtx :: ( WalletDbReader ctx m
                             , WalletDbWriter A.CancelSpecificApplyingPtx m
                             ) => TxId -> m ()
cancelSpecificApplyingPtx txid = askWalletDB >>= updateDisk (A.CancelSpecificApplyingPtx txid)

flushWalletStorage :: ( WalletDbReader ctx m
                      , WalletDbWriter A.FlushWalletStorage m
                      ) => m ()
flushWalletStorage = askWalletDB >>= updateDisk A.FlushWalletStorage

