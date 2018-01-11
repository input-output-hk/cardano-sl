-- | Run endpoint client.

module Client.Pos.Wallet.Web.Run
    ( runEndpointClient
    ) where

import           Universum

import           Servant.Client         (ClientM, ClientEnv (..), runClientM)
import           Servant.Common.BaseUrl (BaseUrl (..), Scheme (..))
import           Network.HTTP.Client    (newManager, defaultManagerSettings)

-- |
runEndpointClient :: ClientM a -> IO (Either Text a)
runEndpointClient realClient = do
    manager' <- newManager defaultManagerSettings
    resp <- runClientM realClient $ ClientEnv manager' (BaseUrl Http "localhost" 8090 "")
    case resp of
        Left problem   -> return . Left . toText $ (show problem :: String)
        Right response -> return . Right $ response
