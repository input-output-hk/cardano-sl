module Pos.Wallet.Web.Tracking.Restore
       ( restoreWallet ) where

import           Universum
import           UnliftIO (MonadUnliftIO)

import qualified Data.Map as M
import           System.Wlog (CanLog, HasLoggerName, WithLogger, logInfo, modifyLoggerName)

import           Pos.Core (Address, HasConfiguration, HasDifficulty (..), headerHash)
import           Pos.Core.Txp (TxIn, TxOut (..), TxOutAux (..))
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadDBRead (..))
import           Pos.Slotting (MonadSlotsData)
import           Pos.StateLock (StateLock)
import           Pos.Txp (genesisUtxo, unGenesisUtxo, utxoToModifier)
import           Pos.Txp.DB.Utxo (filterUtxo)
import           Pos.Util (HasLens (..))

import           Pos.Wallet.Web.State (WalletDB, WalletDbReader, askWalletDB,
                                       setWalletRestorationSyncTip, updateWalletBalancesAndUtxo)
import qualified Pos.Wallet.Web.State as WS
import           Pos.Wallet.Web.Tracking.Decrypt (WalletDecrCredentials, decryptAddress,
                                                  selectOwnAddresses)
import           Pos.Wallet.Web.Tracking.Sync (firstGenesisHeader, processSyncError)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue, newRestoreRequest, submitSyncRequest)


-- | Restores a wallet from seed, by synchronously restoring its balance (and the initial address
-- set) from the global UTXO, and then asynchronously restoring this wallet transaction history.
restoreWallet :: ( WalletDbReader ctx m
                 , MonadDBRead m
                 , WithLogger m
                 , HasLens StateLock ctx StateLock
                 , HasLens SyncQueue ctx SyncQueue
                 , MonadMask m
                 , MonadSlotsData ctx m
                 , MonadUnliftIO m
                 , HasConfiguration
                 ) => WalletDecrCredentials -> m ()
restoreWallet credentials = do
    db <- askWalletDB
    let (_, walletId) = credentials
    modifyLoggerName (const "syncWalletWorker") $ do
        logInfo "New Restoration request for a wallet..."
        genesisBlockHeaderE <- firstGenesisHeader
        case genesisBlockHeaderE of
            Left syncError -> processSyncError syncError
            Right genesisBlock -> do
                restoreGenesisAddresses db credentials
                restoreWalletBalance db credentials
                -- At this point, we consider ourselves synced with the UTXO up-to the
                -- 'RestorationBlockDepth' we compute now. During 'syncWalletWithBlockchain',
                -- we will restore the wallet history from the beginning of the chain by ignoring
                -- any Utxo changes, but we will always add transactions to the pool of known ones.
                -- By doing so, the BListener is free to track new blocks (both in terms of balance update
                -- & tx tracking), allowing the user to use the wallet even if is technically restoring.
                restorationBlockDepth <- WS.RestorationBlockDepth . view difficultyL <$> DB.getTipHeader

                -- Mark this wallet as officially in restore. As soon as we will pass the point where
                -- the 'RestorationBlockDepth' is greater than the current store one, we would flip the
                -- state of this wallet to a "normal" sync, and the two paths will be reunited once for all.
                setWalletRestorationSyncTip db walletId restorationBlockDepth (headerHash genesisBlock)

                -- Once we have a consistent update of the model, we submit the request to the worker.
                submitSyncRequest (newRestoreRequest credentials restorationBlockDepth)

-- | Restores the wallet balance by looking at the global Utxo and trying to decrypt
-- each unspent output address. If we get a match, it means it belongs to us.
restoreWalletBalance :: ( WalletDbReader ctx m
                        , HasLoggerName m
                        , CanLog m
                        , MonadDBRead m
                        , MonadUnliftIO m
                        , HasConfiguration
                        ) => WalletDB -> WalletDecrCredentials -> m ()
restoreWalletBalance db credentials = do
    utxo <- filterUtxo walletUtxo
    -- FIXME(adn): Quite inefficient for now as it requires another linear scan
    -- of the addresses in the wallet's UTXO.
    -- NOTE: Due to the fact that in the current implementation of the wallet the balance
    -- (as returned in a 'CWallet') is computed by looking at the coins on each account of
    -- the wallet, and subsequently summing all the coins found for all the addresses
    -- associated to each of those accounts, we also need to add these addresses
    -- to the wallet for the balance computation to consider them.
    -- If we don't do that, the final balance for a wallet would always be 0 up until
    -- the full restoration is completed, which is the point where all the addresses belonging
    -- to us have been found and added to the database.
    forM_ (M.elems utxo) $ \txOutAux -> do
        whenJust (decryptAddress credentials . getAddress $ txOutAux) (WS.addWAddress db)
    updateWalletBalancesAndUtxo db (utxoToModifier utxo)
    where
      getAddress :: TxOutAux -> Address
      getAddress (TxOutAux (TxOut addr _)) = addr

      walletUtxo :: (TxIn, TxOutAux) -> Bool
      walletUtxo (_, TxOutAux (TxOut addr _)) =
          isJust (decryptAddress credentials addr)

-- | Restores the genesis addresses for a wallet, given its 'WalletDecrCredentials'.
-- NOTE: This doesn't have any effect on the balance as if these addresses still have
-- coins on them, this will be captured by the call to 'restoreWalletBalance', but yet
-- we want to add them to the pool of known addresses for history-rebuilding purposes.
restoreGenesisAddresses :: (HasConfiguration, MonadIO m) => WalletDB -> WalletDecrCredentials -> m ()
restoreGenesisAddresses db credentials =
    let ownGenesisData =
            selectOwnAddresses credentials (txOutAddress . toaOut . snd) $
            M.toList $ unGenesisUtxo genesisUtxo
        ownGenesisAddrs = map snd ownGenesisData
    in mapM_ (WS.addWAddress db) ownGenesisAddrs
