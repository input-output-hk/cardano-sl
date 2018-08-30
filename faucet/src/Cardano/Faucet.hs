{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Cardano.Faucet (
    FaucetAppAPI
  , faucetAppAPI
  , faucetHandler
  , module Cardano.Faucet.Types
  , module Cardano.Faucet.Init
  ) where

import           Cardano.Faucet.Endpoints
import           Data.Tagged (retag)
import           Network.HTTP.Types (hContentType, hLocation, status301)
import           Network.Wai (responseLBS)
import           Network.Wai.Application.Static
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Servant
import           Universum
import           WaiAppStatic.Types (unsafeToPiece)

import           Cardano.Faucet.Init
import           Cardano.Faucet.Swagger
import           Cardano.Faucet.Types


-- | Combined swagger UI, Faucet API, and home page.
type FaucetAppAPI = FaucetDoc :<|> FaucetAPI :<|> Raw

faucetAppAPI :: Proxy FaucetAppAPI
faucetAppAPI = Proxy

faucetHandler :: HasCompileInfo => Maybe FilePath -> ServerT FaucetAppAPI M
faucetHandler home =      (hoistServer faucetDoc liftToM swaggerServer)
                     :<|> faucetServer
                     :<|> (serveHomePage home)

-- | Depending on the config, either redirect to swagger docs, or
-- serve up a directory of HTML which contains a home page or demo
-- frontend.
serveHomePage :: Maybe FilePath -> Tagged M Application
serveHomePage (Just home) = retag $ serveDirectoryWebApp' home
serveHomePage Nothing     = Tagged redirectDocs

-- | Same as serveDirectoryWebApp, except with index.html as the index
-- page.
serveDirectoryWebApp' :: FilePath -> Server Raw
serveDirectoryWebApp' = serveDirectoryWith . withIndex . defaultWebAppSettings
  where withIndex ss = ss { ssIndices = [unsafeToPiece "index.html"] }

-- | Simple WAI application which always redirects to the swagger UI.
redirectDocs :: Application
redirectDocs _ respond = respond $ responseLBS status301 hdrs "Redirecting"
  where
    hdrs = [(hContentType, "text/plain"), (hLocation, docs)]
    docs = "/docs/"
