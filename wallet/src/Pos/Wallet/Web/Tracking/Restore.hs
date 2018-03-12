module Pos.Wallet.Web.Tracking.Restore
       ( restoreWallet ) where

import           Universum
import           UnliftIO (MonadUnliftIO)

import qualified Data.Map as M
import           System.Wlog (CanLog, HasLoggerName, WithLogger, logInfo, modifyLoggerName)

import           Pos.Core (Address, HasConfiguration)
import           Pos.Core.Txp (TxIn, TxOut (..), TxOutAux (..))
import           Pos.DB.Class (MonadDBRead (..))
import           Pos.Slotting (MonadSlotsData)
import           Pos.StateLock (StateLock)
import           Pos.Txp (genesisUtxo, unGenesisUtxo, utxoToModifier)
import           Pos.Txp.DB.Utxo (filterUtxo)
import           Pos.Util (HasLens (..))

import           Pos.Wallet.Web.State (MonadWalletDB, updateWalletBalancesAndUtxo)
import qualified Pos.Wallet.Web.State as WS
import           Pos.Wallet.Web.Tracking.Decrypt (WalletDecrCredentials, decryptAddress,
                                                  selectOwnAddresses)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue, newRestoreRequest, submitSyncRequest)


-- | Restores a wallet from seed, by synchronously restoring its balance (and the initial address
-- set) from the global UTXO, and then asynchronously restoring this wallet transaction history.
restoreWallet :: ( MonadWalletDB ctx m
                 , MonadDBRead m
                 , WithLogger m
                 , HasLens StateLock ctx StateLock
                 , HasLens SyncQueue ctx SyncQueue
                 , MonadMask m
                 , MonadSlotsData ctx m
                 , MonadUnliftIO m
                 ) => WalletDecrCredentials -> m ()
restoreWallet credentials = do
    modifyLoggerName (const "syncWalletWorker") $ do
        logInfo "New Restoration request for a wallet..."
        restoreGenesisAddresses credentials
        restoreWalletBalance credentials
        submitSyncRequest (newRestoreRequest credentials)

-- | Restores the wallet balance by looking at the global Utxo and trying to decrypt
-- each unspent output address. If we get a match, it means it belongs to us.
restoreWalletBalance :: ( MonadWalletDB ctx m
                        , HasLoggerName m
                        , CanLog m
                        , MonadDBRead m
                        , MonadUnliftIO m
                        ) => WalletDecrCredentials -> m ()
restoreWalletBalance credentials = do
    utxo <- filterUtxo walletUtxo
    -- FIXME(adn): Quite inefficient for now.
    -- NOTE: Due to the fact that in the current implementation of the wallet the balance
    -- (as returned in a 'CWallet') is computed by looking at the coins on each account of
    -- the wallet, we also need to add these addresses to the wallet for the balance
    -- computation to consider them.
    forM_ (M.elems utxo) $ \txOutAux -> do
        whenJust (decryptAddress credentials . getAddress $ txOutAux) WS.addWAddress
    updateWalletBalancesAndUtxo (utxoToModifier utxo)
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
restoreGenesisAddresses :: (HasConfiguration, MonadWalletDB ctx m, Monad m) => WalletDecrCredentials -> m ()
restoreGenesisAddresses credentials =
    let ownGenesisData =
            selectOwnAddresses credentials (txOutAddress . toaOut . snd) $
            M.toList $ unGenesisUtxo genesisUtxo
        ownGenesisAddrs = map snd ownGenesisData
    in mapM_ WS.addWAddress ownGenesisAddrs
