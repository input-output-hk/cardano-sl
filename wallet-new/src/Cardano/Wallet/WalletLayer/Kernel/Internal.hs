module Cardano.Wallet.WalletLayer.Kernel.Internal (
    nextUpdate
  , applyUpdate
  , postponeUpdate
  , resetWalletState
  , importWallet

  , waitForUpdate
  , addUpdate
  ) where

import           Universum

import           Control.Concurrent.MVar (modifyMVar_)
import           Data.Acid.Advanced (update')
import           System.IO.Error (isDoesNotExistError)

import           Pos.Chain.Update (ConfirmedProposalState, SoftwareVersion)

import           Cardano.Wallet.API.V1.Types (V1 (..), Wallet,
                     WalletImport (..))
import           Cardano.Wallet.Kernel.DB.AcidState (AddUpdate (..),
                     ClearDB (..), GetNextUpdate (..), RemoveNextUpdate (..))
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.TxMeta
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
import qualified Cardano.Wallet.Kernel.Submission as Submission
import           Cardano.Wallet.WalletLayer (CreateWallet (..),
                     ImportWalletError (..))
import           Cardano.Wallet.WalletLayer.Kernel.Wallets (createWallet)

-- | Get next update (if any)
--
-- NOTE (legacy): 'nextUpdate", "Pos.Wallet.Web.Methods.Misc"
-- Most of the behaviour of the legacy 'nextUpdate' is now actually implemented
-- directly in the AcidState 'getNextUpdate' update.
nextUpdate :: MonadIO m
           => Kernel.PassiveWallet -> m (Maybe (V1 SoftwareVersion))
nextUpdate w = liftIO $ do
    current <- Node.curSoftwareVersion (w ^. Kernel.walletNode)
    fmap (fmap (V1 . _fromDb)) $
      update' (w ^. Kernel.wallets) $ GetNextUpdate (InDb current)

-- | Apply an update
--
-- NOTE (legacy): 'applyUpdate', "Pos.Wallet.Web.Methods.Misc".
--
-- The legacy implementation does two things:
--
-- 1. Remove the update from the wallet's list of updates
-- 2. Call 'applyLastUpdate' from 'MonadUpdates'
--
-- The latter is implemented in 'applyLastUpdateWebWallet', which literally just
-- a call to 'triggerShutdown'.
--
-- TODO: The other side of the story is 'launchNotifier', where the wallet
-- is /notified/ of updates.
applyUpdate :: MonadIO m => Kernel.PassiveWallet -> m ()
applyUpdate w = liftIO $ do
    update' (w ^. Kernel.wallets) $ RemoveNextUpdate
    Node.withNodeState (w ^. Kernel.walletNode) $ \_lock ->
      Node.triggerShutdown

-- | Postpone update
--
-- NOTE (legacy): 'postponeUpdate', "Pos.Wallet.Web.Methods.Misc".
postponeUpdate :: MonadIO m => Kernel.PassiveWallet -> m ()
postponeUpdate w = update' (w ^. Kernel.wallets) $ RemoveNextUpdate

-- | Wait for an update notification
waitForUpdate :: MonadIO m => Kernel.PassiveWallet -> m ConfirmedProposalState
waitForUpdate w = liftIO $
    Node.withNodeState (w ^. Kernel.walletNode) $ \_lock ->
        Node.waitForUpdate

-- | Add an update in the DB, this is triggered by the notifier once getting
-- a new proposal from the blockchain
addUpdate :: MonadIO m => Kernel.PassiveWallet -> SoftwareVersion -> m ()
addUpdate w v = liftIO $
    update' (w ^. Kernel.wallets) $ AddUpdate (InDb v)

-- | Reset wallet state
resetWalletState :: MonadIO m => Kernel.PassiveWallet -> m ()
resetWalletState w = liftIO $ do
    -- TODO: reset also the wallet worker (CBR-415)

    -- stop restoration and empty it`s state.
    -- TODO: A restoration may start between this call and the db modification
    -- but as this is for testing only we keep it that way for now. (CBR-415)
    Kernel.stopAllRestorations w

    -- This pauses any effect the Submission worker can have.
    -- We don`t actually stop and restart the thread, but once
    -- we have the MVar the worker can have no effects.
    modifyMVar_ (w ^. Kernel.walletSubmission) $ \_ -> do

        -- clear both dbs.
        update' (w ^. Kernel.wallets) $ ClearDB
        clearMetaDB (w ^. Kernel.walletMeta)
        -- clear submission state.
        return Submission.emptyWalletSubmission

-- | Imports a 'Wallet' from a backup on disk.
importWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> WalletImport
             -> m (Either ImportWalletError Wallet)
importWallet pw WalletImport{..} = liftIO $ do
    secretE <- try $ Keystore.readWalletSecret wiFilePath
    case secretE of
         Left e ->
             if isDoesNotExistError e
                 then return (Left $ ImportWalletFileNotFound wiFilePath)
                 else throwM e
         Right mbEsk -> do
             case mbEsk of
                 Nothing  -> return (Left $ ImportWalletNoWalletFoundInBackup wiFilePath)
                 Just esk -> do
                     res <- liftIO $ createWallet pw (ImportWalletFromESK esk wiSpendingPassword)
                     return $ case res of
                          Left e               -> Left (ImportWalletCreationFailed e)
                          Right importedWallet -> Right importedWallet
