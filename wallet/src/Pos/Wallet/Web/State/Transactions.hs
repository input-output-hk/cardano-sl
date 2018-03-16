{-# LANGUAGE RankNTypes #-}
-- | This module contains higher level transctions atop of
--   'Post.Wallet.Web.State.Storage'. These are defined as
--   specific (named) functions in order to generate acidic
--   guarantees for them.
module Pos.Wallet.Web.State.Transactions
    ( createAccountWithAddress
    , removeWallet2
    , applyModifierToWallet
    , rollbackModifierFromWallet
    )
    where

import           Universum hiding (for_)

import           Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import           Pos.Client.Txp.History (TxHistoryEntry)
import           Pos.Core (HasProtocolConstants)
import           Pos.Core.Common (HeaderHash)
import           Pos.Txp (TxId, UtxoModifier)
import           Pos.Util.Servant (encodeCType)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CAccountMeta, CId, CTxId,
                                             CTxMeta, CWAddressMeta (..), Wal)
import           Pos.Wallet.Web.Pending.Types (PtxCondition)
import           Pos.Wallet.Web.State.Storage (Update)
import qualified Pos.Wallet.Web.State.Storage as WS

-- | Create an account with an address.
createAccountWithAddress
    :: AccountId
    -> CAccountMeta
    -> CWAddressMeta
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

-- | Apply some set of modifiers to a wallet.
--   TODO Find out the significance of this set of modifiers and document.
applyModifierToWallet
    :: CId Wal
    -> [CWAddressMeta] -- ^ Wallet addresses to add
    -> [(WS.CustomAddressType, [(CId Addr, HeaderHash)])] -- ^ Custom addresses to add
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

-- | Rollback some set of modifiers to a wallet.
--   TODO Find out the significance of this set of modifiers and document.
rollbackModifierFromWallet
    :: HasProtocolConstants -- Needed for ptxUpdateMeta
    => CId Wal
    -> [CWAddressMeta] -- ^ Addresses to remove
    -> [(WS.CustomAddressType, [(CId Addr, HeaderHash)])] -- ^ Custom addresses to remove
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
