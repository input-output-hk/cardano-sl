module Cardano.Wallet.API.V1.Handlers.Users where

import           Universum

import qualified Pos.Wallet.Web.Methods.Misc as V0

import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.V1.Users as Users

import           Pos.Wallet.Web.State (MonadWalletDBRead)
import           Servant

-- | All the @Servant@ handlers for users-specific operations.
handlers :: ( HasConfigurations
            , HasCompileInfo
            )
         => ServerT Users.API MonadV1
handlers =  getUserProfile
       :<|> updateUserProfile

getUserProfile :: (MonadThrow m, MonadWalletDBRead ctx m) => m V1.UserProfile
getUserProfile = V0.getUserProfile >>= migrate

-- TODO(adinapoli) Will be done as part of https://iohk.myjetbrains.com/youtrack/issue/CSL-1969
updateUserProfile :: V1.UserProfile -> MonadV1 V1.UserProfile
updateUserProfile x = return x
