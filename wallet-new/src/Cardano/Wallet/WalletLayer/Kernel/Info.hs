module Cardano.Wallet.WalletLayer.Kernel.Info (
    getNodeInfo
  ) where

import           Universum

import qualified Cardano.Wallet.API.V1.Types as V1
import           Cardano.Wallet.Kernel.Diffusion (walletGetSubscriptionStatus)
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor)
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node

getNodeInfo :: MonadIO m => Kernel.ActiveWallet -> V1.ForceNtpCheck -> m V1.NodeInfo
getNodeInfo aw ntpCheckBehavior = liftIO $
    V1.NodeInfo
      <$> (pure $ V1.mkSyncPercentage 100) -- TODO (Restoration [CBR-243])
      <*> (pure $ Nothing)                 -- TODO (Restoration [CBR-243])
      <*> (pure $ V1.mkBlockchainHeight 0) -- TODO (Restoration [CBR-243])
      <*> (Node.getNtpDrift node ntpCheckBehavior)
      <*> (walletGetSubscriptionStatus (Kernel.walletDiffusion aw))
  where
    node :: NodeStateAdaptor IO
    node = pw ^. Kernel.walletNode

    pw :: Kernel.PassiveWallet
    pw = Kernel.walletPassive aw
