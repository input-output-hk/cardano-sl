module Cardano.Wallet.API.V1.Handlers.Info where

import           Universum

import qualified Cardano.Wallet.API.V1.Info as Info
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1

import           Mockable (MonadMockable)
import           Pos.NtpCheck (NtpCheckMonad)
import           Servant

import qualified Pos.Wallet.Web.Methods.Misc as V0

-- | All the @Servant@ handlers for settings-specific operations.
handlers :: ( HasConfigurations
            , HasCompileInfo
            )
         => ServerT Info.API MonadV1
handlers = getInfo

-- | Returns the @dynamic@ settings for this wallet node,
-- like the local time difference (the NTP drift), the sync progress,
-- etc.
getInfo :: (HasConfigurations, NtpCheckMonad m, MonadMockable m)
            => m NodeInfo
getInfo = NodeInfo <$> fmap MeasuredIn V0.localTimeDifference
