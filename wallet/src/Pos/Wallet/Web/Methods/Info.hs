-- | Client information.

module Pos.Wallet.Web.Methods.Info
       ( getClientInfo
       ) where

import           Universum

import           Paths_cardano_sl_wallet (version)
import           Pos.Update.Configuration (HasUpdateConfiguration, curSoftwareVersion)
import           Pos.Util.CompileInfo (HasCompileInfo, compileInfo, ctiGitRevision)
import           Pos.Wallet.Web.ClientTypes (ApiVersion (..), ClientInfo (..))

getClientInfo :: (HasCompileInfo, HasUpdateConfiguration, Applicative m) => m ClientInfo
getClientInfo =
    pure
        ClientInfo
        { ciGitRevision = ctiGitRevision compileInfo
        , ciSoftwareVersion = curSoftwareVersion
        , ciCabalVersion = version
        , ciApiVersion = ApiVersion0
        }
