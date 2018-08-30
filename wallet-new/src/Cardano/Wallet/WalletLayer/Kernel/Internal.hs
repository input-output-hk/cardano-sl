module Cardano.Wallet.WalletLayer.Kernel.Internal (
    nextUpdate
  , applyUpdate
  , postponeUpdate
  , resetWalletState
  , importWallet
  ) where

import           Universum

import           Data.Acid.Advanced (update')
import           System.Directory (doesFileExist)

import           Pos.Core.Update (SoftwareVersion)

import           Cardano.Wallet.API.V1.Types (V1 (..), Wallet,
                     WalletImport (..))
import           Cardano.Wallet.Kernel.DB.AcidState (GetNextUpdate (..),
                     RemoveNextUpdate (..))
import           Cardano.Wallet.Kernel.DB.InDb
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
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

-- | Reset wallet state
resetWalletState :: Kernel.PassiveWallet -> m ()
resetWalletState = error "TODO: resetWaletState [CBR-393]"

-- | Imports a 'Wallet' from a backup on disk.
importWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> WalletImport
             -> m (Either ImportWalletError Wallet)
importWallet pw WalletImport{..} = liftIO $ do
    -- Check if the file exists on disk. Considerations on atomicity here are
    -- less important as this is an internal endpoint anyway.
    fileIsThere <- doesFileExist wiFilePath
    if not fileIsThere
       then return (Left $ ImportWalletFileNotFound wiFilePath)
       else Keystore.bracketImportLegacyKeystore wiFilePath $ \legacyKeystore -> do
           mbEsk <- Keystore.lookupLegacyRootKey legacyKeystore
           case mbEsk of
               Nothing  -> return (Left $ ImportWalletNoWalletFoundInBackup wiFilePath)
               Just esk -> do
                   res <- liftIO $ createWallet pw (ImportWalletFromESK esk wiSpendingPassword)
                   return $ case res of
                        Left e               -> Left (ImportWalletCreationFailed e)
                        Right importedWallet -> Right importedWallet

