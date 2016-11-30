-- | Web server.

module Pos.Web.Server
       ( serveWeb
       ) where

import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant.Server           (Server, serve)
import           Universum

import           Pos.Web.Api              (NodeApi, nodeApi)

serveWeb :: Word16 -> IO ()
serveWeb port = run (fromIntegral port) application

application :: Application
application = serve nodeApi servantServer

servantServer :: Server NodeApi
servantServer = pure "bardaq"
