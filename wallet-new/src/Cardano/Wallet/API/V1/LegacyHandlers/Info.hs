module Cardano.Wallet.API.V1.LegacyHandlers.Info where

import           Universum

import           Control.Monad.STM (retry)

import           Cardano.Wallet.API.Response (WalletResponse, single)
import qualified Cardano.Wallet.API.V1.Info as Info
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1

import           Ntp.Client (NtpStatus (NtpSyncPending))
import           Pos.Infra.Diffusion.Subscription.Status (ssMap)
import           Pos.Infra.Diffusion.Types (Diffusion (..))
import           Pos.Util.Wlog (WithLogger)
import           Pos.Wallet.WalletMode (MonadBlockchainInfo)
import           Servant

import qualified Pos.Core as Core
import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods.Misc as V0

-- | All the @Servant@ handlers for settings-specific operations.
handlers :: Diffusion MonadV1
         -> TVar NtpStatus
         -> ServerT Info.API MonadV1
handlers = getInfo

-- | Returns the @dynamic@ settings for this wallet node,
-- like the local time difference (the NTP drift), the sync progress,
-- etc.
getInfo :: ( MonadIO m
           , WithLogger m
           , MonadMask m
           , MonadBlockchainInfo m
           )
        => Diffusion MonadV1
        -> TVar NtpStatus
        -> ForceNtpCheck
        -> m (WalletResponse NodeInfo)
getInfo  Diffusion{..} ntpStatus ntpCheck = do
    timeDifference <- V0.localTimeDifferencePure <$>
        if ntpCheck == ForceNtpCheck
            then do
                atomically $ writeTVar ntpStatus NtpSyncPending
                atomically $ do
                    s <- readTVar ntpStatus
                    case s of
                        NtpSyncPending -> retry
                        _              -> pure s
            else readTVarIO ntpStatus
    subscribers <- readTVarIO (ssMap subscriptionStates)
    spV0 <- V0.syncProgress
    syncProgress   <- migrate spV0
    return $ single NodeInfo
        { nfoSyncProgress          = syncProgress
        , nfoSubscriptionStatus    = subscribers
        , nfoBlockchainHeight      = V1.mkBlockchainHeight . Core.getChainDifficulty <$> V0._spNetworkCD spV0
        , nfoLocalBlockchainHeight = V1.mkBlockchainHeight . Core.getChainDifficulty . V0._spLocalCD $ spV0
        , nfoLocalTimeInformation  = TimeInfo
            { timeDifferenceFromNtpServer = fmap V1.mkLocalTimeDifference timeDifference
            }
        }
