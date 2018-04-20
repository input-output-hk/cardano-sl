module Cardano.Wallet.API.V1.LegacyHandlers.Info where

import           Universum

import           Data.Map.Strict (lookup)
import           System.Wlog (WithLogger)

import           Cardano.Wallet.API.Response (WalletResponse, single)
import qualified Cardano.Wallet.API.V1.Info as Info
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1

import           Mockable (MonadMockable)
import           Ntp.Client (NtpStatus)
import           Pos.Diffusion.Types (Diffusion (..))
import           Pos.Util.TimeWarp (NetworkAddress, addressToNodeId)
import           Pos.Wallet.WalletMode (MonadBlockchainInfo)
import           Servant

import qualified Pos.Core as Core
import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods.Misc as V0

-- | All the @Servant@ handlers for settings-specific operations.
handlers :: ( HasConfigurations
            , HasCompileInfo
            )
         => Diffusion MonadV1
         -> TVar NtpStatus
         -> NetworkAddress
         -> ServerT Info.API MonadV1
handlers = getInfo

-- | Returns the @dynamic@ settings for this wallet node,
-- like the local time difference (the NTP drift), the sync progress,
-- etc.
getInfo :: ( HasConfigurations
           , MonadIO m
           , WithLogger m
           , MonadMockable m
           , MonadBlockchainInfo m
           )
        => Diffusion MonadV1
        -> TVar NtpStatus
        -> NetworkAddress
        -> m (WalletResponse NodeInfo)
getInfo Diffusion{..} ntpStatus addr = do
    subscribers <- atomically $ readTVar subscriptionStatus
    let status = (addressToNodeId addr) `lookup` subscribers
    spV0 <- V0.syncProgress
    syncProgress   <- migrate spV0
    timeDifference <- V0.localTimeDifference ntpStatus
    return $ single NodeInfo
        { nfoSyncProgress          = syncProgress
        , nfoSubscriptionStatus    = V1.SubscriptionStatusInfo status
        , nfoBlockchainHeight      = V1.mkBlockchainHeight . Core.getChainDifficulty <$> V0._spNetworkCD spV0
        , nfoLocalBlockchainHeight = V1.mkBlockchainHeight . Core.getChainDifficulty . V0._spLocalCD $ spV0
        , nfoLocalTimeInformation  = TimeInfo
            { timeDifferenceFromNtpServer = fmap V1.mkLocalTimeDifference timeDifference
            }
        }
