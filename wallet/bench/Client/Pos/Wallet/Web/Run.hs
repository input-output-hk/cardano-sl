-- | Run endpoint client.

module Client.Pos.Wallet.Web.Run
    ( runEndpointClient
    ) where

import           Universum

import           Servant.Client         (ClientM, ClientEnv (..), runClientM)
import           Servant.Common.BaseUrl (BaseUrl (..), Scheme (..))
import           Network.HTTP.Client    (newManager, defaultManagerSettings)

-- | Run client for particular endpoint. It is assumed that
-- node is already running, with enabled Wallet Web API.
runEndpointClient :: ClientM a -> IO (Either Text a)
runEndpointClient realClient = do
    manager <- newManager defaultManagerSettings
    runClientM realClient (ClientEnv manager nodeURL) >>= \case
        Left problem   -> return . Left  $ toText (show problem :: String)
        Right response -> return . Right $ response
  where
    -- TODO: Check if we must use 'Https' instead of 'Http'.
    nodeURL  = BaseUrl Http nodeHost nodePort ""
    nodeHost = "localhost"
    nodePort = 8090
