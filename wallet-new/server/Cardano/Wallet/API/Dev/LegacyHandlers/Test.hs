module Cardano.Wallet.API.Dev.LegacyHandlers.Test where

import           Universum

import           Cardano.Wallet.API.Response (WalletResponse, single)
import qualified Cardano.Wallet.API.Dev.Test as Test
import           Cardano.Wallet.API.V1.Migration

import           Pos.Wallet.Web.State as V0
import           Pos.Wallet.Web.Methods.Misc (WalletStateSnapshot)
import           Servant

import qualified Pos.Wallet.Web.Methods.Misc as V0

-- | @Servant@ handlers needed by test cases. They are not public, but for development only.
handlers :: ( HasConfigurations
            , HasCompileInfo
            )
         => ServerT Test.API MonadV1
handlers = getWalletState


getWalletState :: (V0.MonadWalletDBRead ctx m)
    => m (WalletResponse WalletStateSnapshot)
getWalletState =
    single <$> V0.dumpState
