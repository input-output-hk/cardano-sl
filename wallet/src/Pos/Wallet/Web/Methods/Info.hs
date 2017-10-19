-- | Client information.

module Pos.Wallet.Web.Methods.Info
       ( getClientInfo
       ) where

import           Universum

import           Paths_cardano_sl_wallet    (version)
import           Pos.Launcher               (gitRev)
import           Pos.Update.Configuration   (HasUpdateConfiguration, curSoftwareVersion)
import           Pos.Wallet.Web.ClientTypes (ApiVersion (..), ClientInfo (..))

getClientInfo :: (HasUpdateConfiguration, Applicative m) => m ClientInfo
getClientInfo =
    pure
        ClientInfo
        { ciGitRevision = gitRev
        , ciSoftwareVersion = curSoftwareVersion
        , ciCabalVersion = version
        , ciApiVersion = ApiVersion0
        }
