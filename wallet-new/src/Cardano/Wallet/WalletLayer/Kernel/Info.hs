module Cardano.Wallet.WalletLayer.Kernel.Info (
    getNodeInfo
  ) where

import           Universum

import           Data.Time.Units (toMicroseconds)
import           Ntp.Client (NtpStatus (..))

import qualified Cardano.Wallet.API.V1.Types as V1
import           Cardano.Wallet.Kernel.Diffusion (walletGetSubscriptionStatus)
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor)
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node

getNodeInfo :: MonadIO m => Kernel.ActiveWallet -> m V1.NodeInfo
getNodeInfo aw = liftIO $ do
    V1.NodeInfo
      <$> (pure $ V1.mkSyncPercentage 100) -- TODO (Restoration [CBR-243])
      <*> (pure $ Nothing)                 -- TODO (Restoration [CBR-243])
      <*> (pure $ V1.mkBlockchainHeight 0) -- TODO (Restoration [CBR-243])
      <*> (mkTimeInfo <$> Node.getNtpStatus node)
      <*> (walletGetSubscriptionStatus (Kernel.walletDiffusion aw))
  where
    mkTimeInfo :: NtpStatus -> V1.TimeInfo
    mkTimeInfo = V1.TimeInfo . fmap V1.mkLocalTimeDifference . diff

    diff :: NtpStatus -> Maybe Integer
    diff (NtpDrift time)    = Just (toMicroseconds time)
    diff NtpSyncPending     = Nothing
    diff NtpSyncUnavailable = Nothing

    node :: NodeStateAdaptor IO
    node = pw ^. Kernel.walletNode

    pw :: Kernel.PassiveWallet
    pw = Kernel.walletPassive aw
