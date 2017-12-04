module Cardano.Wallet.API.V1.Handlers.Info where

import           Universum

import qualified Cardano.Wallet.API.V1.Info as Info
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1

import           Mockable (MonadMockable)
import           Pos.NtpCheck (NtpCheckMonad)
import           Pos.Wallet.WalletMode (MonadBlockchainInfo)
import           Servant

import qualified Pos.Core as Core
import qualified Pos.Wallet.Web.ClientTypes.Types as V0
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
getInfo :: ( HasConfigurations
           , NtpCheckMonad m
           , MonadMockable m
           , MonadBlockchainInfo m
           )
        => m NodeInfo
getInfo = do
    spV0 <- V0.syncProgress
    syncProgress   <- migrate spV0
    timeDifference <- fmap V1.mkLocalTimeDifference V0.localTimeDifference
    return NodeInfo {
          nfoSyncProgress = syncProgress
        , nfoBlockchainHeight = V1.mkBlockchainHeight . Core.getChainDifficulty <$> V0._spNetworkCD spV0
        , nfoLocalBlockchainHeight = V1.mkBlockchainHeight . Core.getChainDifficulty . V0._spLocalCD $ spV0
        , nfoLocalTimeDifference = timeDifference
        }
