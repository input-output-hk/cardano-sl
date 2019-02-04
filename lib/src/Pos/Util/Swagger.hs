{-# LANGUAGE QuasiQuotes #-}

module Pos.Util.Swagger where

import           Universum

import           Data.Swagger
import           NeatInterpolation (text)
import           Servant.Server (Handler, Server)
import           Servant.Swagger.UI.Core (SwaggerSchemaUI',
                     swaggerSchemaUIServerImpl)
import           Servant.Swagger.UI.ReDoc (redocFiles)

-- | Provide an alternative UI (ReDoc) for rendering Swagger documentation.
swaggerSchemaUIServer
    :: (Server api ~ Handler Swagger)
    => Swagger -> Server (SwaggerSchemaUI' dir api)
swaggerSchemaUIServer =
    swaggerSchemaUIServerImpl redocIndexTemplate redocFiles
  where
    redocIndexTemplate :: Text
    redocIndexTemplate = [text|
<!doctype html>
<html lang="en">
  <head>
    <title>ReDoc</title>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <style>
      body { margin: 0; padding: 0; }
    </style>
    <script>
        // Force Strict-URL Routing for assets relative paths
        (function onload() {
            if (!window.location.pathname.endsWith("/")) {
                window.location.pathname += "/";
            }
        }());
    </script>
  </head>
  <body>
    <redoc spec-url="../SERVANT_SWAGGER_UI_SCHEMA"></redoc>
    <script src="redoc.min.js"> </script>
  </body>
</html>|]

