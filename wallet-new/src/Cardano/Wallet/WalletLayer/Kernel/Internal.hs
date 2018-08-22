module Cardano.Wallet.WalletLayer.Kernel.Internal (
    nextUpdate
  , applyUpdate
  , postponeUpdate
  , resetWalletState
  ) where

import           Universum

import           Data.Acid.Advanced (update')

import           Pos.Core.Update (SoftwareVersion)

import           Cardano.Wallet.API.V1.Types (V1 (..))
import           Cardano.Wallet.Kernel.DB.AcidState (GetNextUpdate (..),
                     RemoveNextUpdate (..))
import           Cardano.Wallet.Kernel.DB.InDb
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node

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
