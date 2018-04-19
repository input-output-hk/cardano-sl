{-# LANGUAGE RankNTypes #-}
-- | This module contains higher level transctions atop of
--   'Post.Wallet.Web.State.Storage'. These are defined as
--   specific (named) functions in order to generate acidic
--   guarantees for them.
module Pos.Wallet.Web.State.Transactions
    ( createAccountWithAddress
    , removeWallet2
    , applyModifierToWallet
    , applyModifierToWallet2
    , rollbackModifierFromWallet
    , rollbackModifierFromWallet2
    )
    where

import           Universum hiding (for_)

import           Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import           Pos.Client.Txp.History (TxHistoryEntry)
import           Pos.Core (Address, ChainDifficulty, HasProtocolConstants)
import           Pos.Core.Common (HeaderHash)
import           Pos.Txp (TxId, UtxoModifier)
import           Pos.Util.Servant (encodeCType)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccountMeta, CId, CTxId, CTxMeta, Wal)
import           Pos.Wallet.Web.Pending.Types (PtxCondition)
import           Pos.Wallet.Web.State.Storage (Update)
import qualified Pos.Wallet.Web.State.Storage as WS

-- | Create an account with an address.
createAccountWithAddress
    :: AccountId
    -> CAccountMeta
    -> WS.WAddressMeta
    -> Update ()
createAccountWithAddress accId accMeta addrMeta = do
    WS.createAccount accId accMeta
    WS.addWAddress addrMeta

-- | Delete a wallet (and all associated data).
--   Compared to the low-level 'removeWallet', this function:
--   - Removes all accounts associated with the wallet.
--   - Removes transaction metadata.
--   - Removes the history cache.
removeWallet2
    :: CId Wal
    -> Update ()
removeWallet2 walId = do
    accIds <- filter ((== walId) . aiWId) . HM.keys <$> use WS.wsAccountInfos
    for_ accIds WS.removeAccount
    WS.removeWallet walId
    WS.removeTxMetas walId
    WS.removeHistoryCache walId

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}
-- | Unlike 'applyModifierToWallet', this function doesn't assume we want to blindly
-- update the 'WalletStorage' with all the information passed, but only with the ones
-- relevant to the 'WalletSyncState'.
applyModifierToWallet2
    :: CId Wal
    -> [WS.WAddressMeta] -- ^ Wallet addresses to add
    -> [(WS.CustomAddressType, [(Address, HeaderHash)])] -- ^ Custom addresses to add
    -> UtxoModifier
    -> [(CTxId, CTxMeta)] -- ^ Transaction metadata to add
    -> Map TxId TxHistoryEntry -- ^ Entries for the history cache
    -> [(TxId, PtxCondition)] -- ^ PTX Conditions
    -> ChainDifficulty
    -- ^ The current depth of the blockchain.
    -> WS.WalletSyncState -- ^ New 'WalletSyncState'
    -> Update ()
applyModifierToWallet2 walId wAddrs custAddrs utxoMod
                      txMetas historyEntries ptxConditions
                      currentBlockchainDepth syncState = do
    case syncState of
        (WS.RestoringFrom rhh newSyncTip) -> do
            for_ wAddrs WS.addWAddress
            for_ custAddrs $ \(cat, addrs) ->
                for_ addrs $ WS.addCustomAddress cat
            -- Allow the transactions to influence the 'UTXO' and the balance only
            -- if we are looking at transactions happened _after_ the point where we
            -- originally restored this wallet.
            when (currentBlockchainDepth > WS.getRestorationBlockDepth rhh) $
                WS.updateWalletBalancesAndUtxo utxoMod
            for_ txMetas $ uncurry $ WS.addOnlyNewTxMeta walId
            WS.insertIntoHistoryCache walId historyEntries
            for_ ptxConditions $ uncurry $ WS.setPtxCondition walId
            WS.setWalletRestorationSyncTip walId rhh newSyncTip
        (WS.SyncedWith newSyncTip) ->
            applyModifierToWallet walId wAddrs custAddrs utxoMod txMetas historyEntries ptxConditions newSyncTip
        -- A wallet should never be in a "NotSynced" state when we call this
        -- function, because:
        -- 1. When creating a new wallet (cfr. Pos.Wallet.Web.Methods.Restore.newWallet)
        --    we immediately set its syncTip appropriately.
        -- 2. When we restore from seed (or backup), we eventually call
        --    Pos.Wallet.Web.Tracking.Restore.restoreWallet, which actively sets
        --    the wallet restoration sync tip correctly.
        --
        -- Therefore, it's actually an invariant violation to even end up in
        -- this branch of the pattern match, and we call out this mistake
        -- explicitly.
        WS.NotSynced -> let msg = "applyModifierToWallet2: invariant violated! "
                               <> "applied modifier to a 'NotSynced' wallet."
                        in error msg

-- | Apply some set of modifiers to a wallet.
--   TODO Find out the significance of this set of modifiers and document.
applyModifierToWallet
    :: CId Wal
    -> [WS.WAddressMeta] -- ^ Wallet addresses to add
    -> [(WS.CustomAddressType, [(Address, HeaderHash)])] -- ^ Custom addresses to add
    -> UtxoModifier
    -> [(CTxId, CTxMeta)] -- ^ Transaction metadata to add
    -> Map TxId TxHistoryEntry -- ^ Entries for the history cache
    -> [(TxId, PtxCondition)] -- ^ PTX Conditions
    -> HeaderHash -- ^ New sync tip
    -> Update ()
applyModifierToWallet walId wAddrs custAddrs utxoMod
                      txMetas historyEntries ptxConditions
                      syncTip = do
    for_ wAddrs WS.addWAddress
    for_ custAddrs $ \(cat, addrs) ->
        for_ addrs $ WS.addCustomAddress cat
    WS.updateWalletBalancesAndUtxo utxoMod
    for_ txMetas $ uncurry $ WS.addOnlyNewTxMeta walId
    WS.insertIntoHistoryCache walId historyEntries
    for_ ptxConditions $ uncurry $ WS.setPtxCondition walId
    WS.setWalletSyncTip walId syncTip

-- | Like 'rollbackModifierFromWallet', but it takes into account the given 'WalletSyncState'.
rollbackModifierFromWallet2
    :: HasProtocolConstants -- Needed for ptxUpdateMeta
    => CId Wal
    -> [WS.WAddressMeta] -- ^ Addresses to remove
    -> [(WS.CustomAddressType, [(Address, HeaderHash)])] -- ^ Custom addresses to remove
    -> UtxoModifier
       -- We use this odd representation because Data.Map does not get 'withoutKeys'
       -- until 5.8.1
    -> Map TxId () -- ^ Entries to remove from history cache.
    -> [(TxId, PtxCondition, WS.PtxMetaUpdate)] -- ^ Deleted PTX candidates
    -> WS.WalletSyncState -- ^ New 'WalletSyncState'
    -> Update ()
rollbackModifierFromWallet2 walId wAddrs custAddrs utxoMod
                            historyEntries ptxConditions
                            syncState = do
    case syncState of
        (WS.RestoringFrom rhh newSyncTip) -> do
            for_ wAddrs WS.removeWAddress
            for_ custAddrs $ \(cat, addrs) ->
                for_ addrs $ WS.removeCustomAddress cat
            WS.updateWalletBalancesAndUtxo utxoMod
            WS.removeFromHistoryCache walId historyEntries
            WS.removeWalletTxMetas walId (encodeCType <$> M.keys historyEntries)
            for_ ptxConditions $ \(txId, cond, meta) -> do
                WS.ptxUpdateMeta walId txId meta
                WS.setPtxCondition walId txId cond
            WS.setWalletRestorationSyncTip walId rhh newSyncTip
        (WS.SyncedWith newSyncTip) ->
            rollbackModifierFromWallet walId wAddrs custAddrs utxoMod
                                       historyEntries ptxConditions
                                       newSyncTip
        -- See similar comment as for 'applyModifierToWallet2'.
        WS.NotSynced -> let msg = "rollbackModifierFromWallet2: invariant violated! "
                               <> "applied modifier to a 'NotSynced' wallet."
                        in error msg

-- | Rollback some set of modifiers to a wallet.
--   TODO Find out the significance of this set of modifiers and document.
rollbackModifierFromWallet
    :: HasProtocolConstants -- Needed for ptxUpdateMeta
    => CId Wal
    -> [WS.WAddressMeta] -- ^ Addresses to remove
    -> [(WS.CustomAddressType, [(Address, HeaderHash)])] -- ^ Custom addresses to remove
    -> UtxoModifier
       -- We use this odd representation because Data.Map does not get 'withoutKeys'
       -- until 5.8.1
    -> Map TxId () -- ^ Entries to remove from history cache.
    -> [(TxId, PtxCondition, WS.PtxMetaUpdate)] -- ^ Deleted PTX candidates
    -> HeaderHash -- ^ New sync tip
    -> Update ()
rollbackModifierFromWallet walId wAddrs custAddrs utxoMod
                           historyEntries ptxConditions
                           syncTip = do
    for_ wAddrs WS.removeWAddress
    for_ custAddrs $ \(cat, addrs) ->
        for_ addrs $ WS.removeCustomAddress cat
    WS.updateWalletBalancesAndUtxo utxoMod
    WS.removeFromHistoryCache walId historyEntries
    WS.removeWalletTxMetas walId (encodeCType <$> M.keys historyEntries)
    for_ ptxConditions $ \(txId, cond, meta) -> do
        WS.ptxUpdateMeta walId txId meta
        WS.setPtxCondition walId txId cond
    WS.setWalletSyncTip walId syncTip
