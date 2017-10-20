{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where

import           Universum

import           Data.Aeson.Encode.Pretty             (encodePretty)
import qualified Data.ByteString.Lazy.Char8           as BL8
import           Data.Function                        ((&))
import           Data.String                          (fromString)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Servant

import qualified Cardano.Wallet.API                   as API
import qualified Cardano.Wallet.API.V1.Swagger        as Swagger
import qualified Cardano.Wallet.Server                as Server

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
  { configHost :: String  -- ^ Hostname to serve on, e.g. "127.0.0.1"
  , configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

-- | Run the Wallet server at the provided host and port.
runWalletServer :: MonadIO m => ServerConfig -> m ()
runWalletServer ServerConfig{..} = do
  let app = serve API.walletAPI Server.walletServer
  liftIO $ Warp.runSettings warpSettings (logStdout app)
  where
    warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)

main :: IO ()
main = do
  let cfg@ServerConfig{..} = ServerConfig "127.0.0.1" 3000
  putStrLn $ "Wallet listening on " <> configHost <> ":" <> show configPort <> " ..."
  -- Generates the updated spec and store it in the appropriate folder.
  -- the reason why we don't generate a yaml file is because for swagger-ui is actually
  -- much better to start with the JSON input, as the tool is capable of generating
  -- better-looking YAMLs.
  BL8.writeFile "spec/swagger.json" (encodePretty Swagger.api)
  runWalletServer cfg
