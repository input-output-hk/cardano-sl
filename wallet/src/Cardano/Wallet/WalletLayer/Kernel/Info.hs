module Cardano.Wallet.WalletLayer.Kernel.Info (
    getNodeInfo
  ) where

import           Universum

import qualified Pos.Core as Core

import qualified Cardano.Wallet.API.V1.Types as V1
import           Cardano.Wallet.Kernel.Diffusion (walletGetSubscriptionStatus)
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor)
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node

getNodeInfo :: MonadIO m => Kernel.ActiveWallet -> V1.ForceNtpCheck -> m V1.NodeInfo
getNodeInfo aw ntpCheckBehavior = liftIO $ do
    (mbNodeHeight, localHeight) <- Node.getNodeSyncProgress node Node.NotYetLocked
    V1.NodeInfo
      <$> (pure $ v1SyncPercentage mbNodeHeight localHeight)
      <*> (pure $ V1.mkBlockchainHeight <$> mbNodeHeight)
      <*> (pure $ V1.mkBlockchainHeight localHeight)
      <*> (Node.getNtpDrift node ntpCheckBehavior)
      <*> (walletGetSubscriptionStatus (Kernel.walletDiffusion aw))
  where
    node :: NodeStateAdaptor IO
    node = pw ^. Kernel.walletNode

    pw :: Kernel.PassiveWallet
    pw = Kernel.walletPassive aw

-- | Computes the V1 'SyncPercentage' out of the global & local blockchain heights.
v1SyncPercentage :: Maybe Core.BlockCount -> Core.BlockCount -> V1.SyncPercentage
v1SyncPercentage nodeHeight walletHeight =
    let percentage = case nodeHeight of
            Nothing -> 0
            Just nd | walletHeight >= nd -> 100 :: Int
            Just nd -> floor @Double $ (fromIntegral walletHeight / max 1.0 (fromIntegral nd)) * 100.0
    in V1.mkSyncPercentage (fromIntegral percentage)
