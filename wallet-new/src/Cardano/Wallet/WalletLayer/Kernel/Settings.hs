module Cardano.Wallet.WalletLayer.Kernel.Settings (
    getNodeSettings
  ) where

import           Universum

import qualified Data.Text as T
import           Data.Time.Units (Millisecond)

import           Pos.Util.CompileInfo (CompileTimeInfo, ctiGitRevision)

import           Cardano.Wallet.API.V1.Types (V1 (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor)
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node

import           Paths_cardano_sl_wallet_new (version)

getNodeSettings :: MonadIO m => Kernel.PassiveWallet -> m V1.NodeSettings
getNodeSettings w = liftIO $
    V1.NodeSettings
      <$> (mkSlotDuration <$> Node.getNextEpochSlotDuration node)
      <*> (V1 <$> Node.curSoftwareVersion node)
      <*> pure (V1 version)
      <*> (mkGitRevision <$> Node.compileInfo node)
  where
    mkSlotDuration :: Millisecond -> V1.SlotDuration
    mkSlotDuration = V1.mkSlotDuration . fromIntegral

    mkGitRevision :: CompileTimeInfo -> Text
    mkGitRevision = T.replace "\n" mempty . ctiGitRevision

    node :: NodeStateAdaptor IO
    node = w ^. Kernel.walletNode
