module Cardano.Wallet.API.V1.Handlers.Settings where

import           Universum

import           Cardano.Wallet.API.V1.Migration
import qualified Cardano.Wallet.API.V1.Settings as Settings
import           Cardano.Wallet.API.V1.Types as V1

import           Pos.Update.Configuration (curSoftwareVersion)
import           Pos.Wallet.WalletMode (MonadBlockchainInfo, blockchainSlotDuration)
import           Servant

-- | All the @Servant@ handlers for settings-specific operations.
handlers :: ( HasConfigurations
            , HasCompileInfo
            )
         => ServerT Settings.API MonadV1
handlers = getSettings

-- | Creates a new @wallet@ given a 'NewWallet' payload.
-- Returns to the client the representation of the created
-- wallet in the 'Wallet' type.
getSettings :: (HasConfigurations, MonadBlockchainInfo m)
            => m WalletSettings
getSettings = WalletSettings <$> (MeasuredIn . fromIntegral <$> blockchainSlotDuration)
                             <*> pure curSoftwareVersion
