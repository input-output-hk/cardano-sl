{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where

import           Universum

import qualified Control.Monad.Catch                  as Catch
import           Control.Monad.Reader                 (MonadReader, ReaderT (..))
import           Data.Aeson.Encode.Pretty             (encodePretty)
import qualified Data.ByteString.Lazy.Char8           as BL8
import           Data.Function                        ((&))
import           Data.String                          (fromString)
import           Mockable                             (Production (..))
import           Network.Wai                          (Middleware)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.Cors          (cors, corsMethods,
                                                       corsRequestHeaders,
                                                       simpleCorsResourcePolicy,
                                                       simpleMethods)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Pos.Launcher                         (withConfigurations)
import           Pos.Launcher.Configuration           (ConfigurationOptions,
                                                       defaultConfigurationOptions)
import           Pos.Util.CompileInfo                 (HasCompileInfo,
                                                       retrieveCompileTimeInfo,
                                                       withCompileInfo)
import           Servant

import qualified Cardano.Wallet.API                   as API
import qualified Cardano.Wallet.API.V1.Swagger        as Swagger
import qualified Cardano.Wallet.Server                as Server


-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
  { configHost :: String  -- ^ Hostname to serve on, e.g. "127.0.0.1"
  , configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

{- V1 placeholders -}

-- Placeholder types, to be stubbed out.
data V1Context = V1Context
    { v1WalletState     :: ()
    }

-- | The main monad for all the wallet logic(?).
newtype V1 a = V1 { runV1Api :: ReaderT V1Context Production a }
             deriving (Functor, Applicative, Monad, MonadReader V1Context)

-- | Hoist an `Icarus` monad to a Servant's Handler.
-- See: http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#natural-transformations
hoistV1Monad :: V1 (V1 :~> Handler)
hoistV1Monad = do
    ctx <- ask
    pure $ NT (toServantHandler ctx)

-- | Converts our domain-specific monad into a standard Servant `Handler`.
toServantHandler :: V1Context -> V1 a -> Handler a
toServantHandler ctx handler =
    liftIO (hoistHandler handler) `Catch.catches` excHandlers
  where

    hoistHandler :: forall a . V1 a -> IO a
    hoistHandler = runProduction . flip runReaderT ctx . runV1Api

    excHandlers = [Catch.Handler throwError]

-- | Run the Wallet server at the provided host and port.
runWalletServer :: HasCompileInfo  => ServerConfig -> Production ()
runWalletServer ServerConfig{..} = do
  withConfigurations conf $ do
    let app = serve API.walletAPI Server.walletServer
    let middleware = corsMiddleware . logStdout
    liftIO $ Warp.runSettings warpSettings (middleware app)
  where
    warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)

    conf :: ConfigurationOptions
    conf = defaultConfigurationOptions

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"]
        , corsMethods = "PUT" : simpleMethods }

-- | Generates the updated spec and store it in the appropriate folder.
-- the reason why we don't generate a yaml file is because for swagger-ui is actually
-- much better to start with the JSON input, as the tool is capable of generating
-- better-looking YAMLs.
generateSwaggerDocumentation :: IO ()
generateSwaggerDocumentation = do
    BL8.writeFile "wallet-new/spec/swagger.json" (encodePretty Swagger.api)
    putText "Swagger API written on disk."

-- | The main entrypoint for the Wallet.
main :: IO ()
main = withCompileInfo $(retrieveCompileTimeInfo) $ do
  let cfg@ServerConfig{..} = ServerConfig "127.0.0.1" 8090
  putStrLn $ "Wallet listening on " <> configHost <> ":" <> show configPort <> " ..."
  generateSwaggerDocumentation
  runProduction $ runWalletServer cfg
