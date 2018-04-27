module Cardano.Wallet.WalletLayer.Legacy.Settings
    ( getSettings
    ) where

import           Universum

import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1
import qualified Data.Text as T
import           Paths_cardano_sl_wallet_new (version)

import           Pos.Update.Configuration (curSoftwareVersion)
import           Pos.Util.CompileInfo (compileInfo, ctiGitRevision)
import           Pos.Wallet.WalletMode (MonadBlockchainInfo, blockchainSlotDuration)


-- Returns the @static@ settings for this wallet node,
-- like the slot duration or the current 'SoftwareVersion'.
getSettings
    :: forall m. (HasConfigurations, HasCompileInfo, MonadBlockchainInfo m)
    => m NodeSettings
getSettings = do
    settings <- NodeSettings <$> (V1.mkSlotDuration . fromIntegral <$> blockchainSlotDuration)
                             <*> pure (V1 curSoftwareVersion)
                             <*> pure version
                             <*> pure (T.replace "\n" mempty $ ctiGitRevision compileInfo)

    pure settings

