{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module APISpec where

import           Universum

import           Data.Default (def)
import           Data.Maybe (fromJust)

import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds)
import           Formatting (format, shown, string, (%))
import           Network.HTTP.Client hiding (Proxy)
import           Network.HTTP.Types
import           System.Wlog (HasLoggerName (..), LoggerName (..))

import           Pos.Client.CLI (CommonArgs (..), CommonNodeArgs (..), NodeArgs (..), getNodeParams,
                                 gtSscParams)
import           Pos.Core (Timestamp (..))
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Rocks.Functions (openNodeDBs)
import           Pos.DB.Rocks.Types (NodeDBs)
import qualified Pos.Diffusion.Types as D
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations, NodeResources (..),
                               allocateNodeResources, defaultConfigurationOptions, npBehaviorConfig,
                               npUserSecret, withConfigurations)
import           Pos.Network.CLI (NetworkConfigOpts (..), intNetworkConfigOpts)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Wallet.Web.Methods (AddrCIdHashes (..))
import           Pos.Wallet.Web.Mode (WalletWebModeContext (..))
import           Pos.Wallet.Web.State (WalletDB, openState)
import           Pos.WorkMode (RealModeContext (..))
import           System.Directory (createDirectoryIfMissing, doesPathExist, getCurrentDirectory,
                                   removeDirectoryRecursive)

import           Mockable (Production, runProduction)
import           Pos.Util.JsonLog (jsonLogConfigFromHandle)
import           Pos.Util.UserSecret (usVss)

import           Servant
import           Servant.QuickCheck
import           Servant.QuickCheck.Internal

import           Test.Hspec
import           Test.Pos.Configuration (withDefConfigurations)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1 as V0
import qualified Cardano.Wallet.API.V1 as V1
import qualified Cardano.Wallet.API.V1.LegacyHandlers as V0
import qualified Cardano.Wallet.API.V1.LegacyHandlers as V1
import qualified Cardano.Wallet.API.V1.Migration as Migration
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types ()

--
-- Instances to allow use of `servant-quickcheck`.
--

instance HasGenRequest (apiType a :> sub) =>
         HasGenRequest (WithDefaultApiArg apiType a :> sub) where
    genRequest _ = genRequest (Proxy @(apiType a :> sub))

instance HasGenRequest (argA a :> argB a :> sub) =>
         HasGenRequest (AlternativeApiArg argA argB a :> sub) where
    genRequest _ = genRequest (Proxy @(argA a :> argB a :> sub))

-- NOTE(adinapoli): This can be improved to produce proper filtering & sorting
-- queries.
instance HasGenRequest sub => HasGenRequest (SortBy syms res :> sub) where
    genRequest _ = genRequest (Proxy @sub)

instance HasGenRequest sub => HasGenRequest (FilterBy syms res :> sub) where
    genRequest _ = genRequest (Proxy @sub)

instance HasGenRequest sub => HasGenRequest (Tags tags :> sub) where
    genRequest _ = genRequest (Proxy :: Proxy sub)

instance HasGenRequest (sub :: *) => HasGenRequest (WalletRequestParams :> sub) where
    genRequest _ = genRequest (Proxy @(WithWalletRequestParams sub))

--
-- RESTful-abiding predicates
--

-- | Checks that every DELETE request should return a 204 NoContent.
deleteReqShouldReturn204 :: RequestPredicate
deleteReqShouldReturn204 = RequestPredicate $ \req mgr ->
     if (method req == methodDelete)
       then do
         resp <- httpLbs req mgr
         let status = responseStatus resp
         when (statusIsSuccessful status && status /= status204) $
           throwM $ PredicateFailure "deleteReqShouldReturn204" (Just req) resp
         return [resp]
       else return []

-- | Checks that every PUT request is idempotent. Calling an endpoint with a PUT
-- twice should return the same result.
putIdempotency :: RequestPredicate
putIdempotency = RequestPredicate $ \req mgr ->
     if (method req == methodPut)
       then do
         resp1 <- httpLbs req mgr
         resp2 <- httpLbs req mgr
         let body1 = responseBody resp1
         let body2 = responseBody resp2
         when (body1 /= body2) $
           throwM $ PredicateFailure "putIdempotency" (Just req) resp1
         return [resp1, resp2]
       else return []

-- | Checks that every request which is not a 204 No Content
-- does not have an empty body, but it always returns something.
noEmptyBody :: RequestPredicate
noEmptyBody = RequestPredicate $ \req mgr -> do
  resp <- httpLbs req mgr
  let body   = responseBody resp
  let status = responseStatus resp
  when (status /= status204 && body == mempty) $
    throwM $ PredicateFailure "noEmptyBody" (Just req) resp
  return [resp]

-- | All the predicates we want to enforce in our API.
predicates :: Predicates
predicates = not500
         <%> deleteReqShouldReturn204
         <%> putIdempotency
         <%> noEmptyBody
         <%> mempty

-- | "Lowers" V0 Handlers from our domain-specific monad to a @Servant@ 'Handler'.
v0Server :: ( Migration.HasConfigurations
            , Migration.HasCompileInfo
            ) => D.Diffusion Migration.MonadV1 -> IO (Server V0.API)
v0Server diffusion = do
  -- TODO(adinapoli): If the monadic stack ends up diverging between V0 and V1,
  -- it's obviously incorrect using 'testV1Context' here.
  ctx <- testV1Context
  return (V0.handlers (Migration.v1MonadNat ctx) diffusion)

-- | "Lowers" V1 Handlers from our domain-specific monad to a @Servant@ 'Handler'.
v1Server :: ( Migration.HasConfigurations
            , Migration.HasCompileInfo
            ) => D.Diffusion Migration.MonadV1 -> IO (Server V1.API)
v1Server diffusion = do
  ctx <- testV1Context
  return (V1.handlers (Migration.v1MonadNat ctx) diffusion)


getCurrentTime :: MonadIO m => m Microsecond
getCurrentTime = liftIO $ fromMicroseconds . round . ( * 1000000) <$> getPOSIXTime

-- | Returns a test 'V1Context' which can be used for the API specs.
-- Such context will use an in-memory database.
testV1Context :: Migration.HasConfiguration => IO Migration.V1Context
testV1Context = do

    let configurationPath = "../lib/configuration.yaml"

    let testPath          = "../run/integration-test/"
    let nodeDBPath        = testPath <> "node-integration-db"
    let walletDBPath      = testPath <> "node-wallet-db"
    let secretKeyPath     = testPath <> "secret-integration-buahaha?.key"

    -- Let's first clear the test directory.
    whenM (doesPathExist testPath) $ removeDirectoryRecursive testPath

    -- Create it.
    createDirectoryIfMissing True testPath

    currentTime       <- getCurrentTime
    currentDirectory  <- getCurrentDirectory

    -- We want to have this since it can be useful if it fails.
    putStrLn $ format
        ("Integration test run on '" % shown % "', current dir - '" % string % "'.")
        currentTime
        currentDirectory

    let cfg = defaultConfigurationOptions
            { cfoSystemStart  = Just . Timestamp $ currentTime
            , cfoFilePath     = configurationPath
            , cfoKey          = "dev"
            }

    -- Open wallet state. Delete if exists.
    ws <- liftIO $ openState True walletDBPath

    liftIO $ withConfigurations cfg $
        withCompileInfo $(retrieveCompileTimeInfo) $ do
            dbs  <- openNodeDBs False nodeDBPath

            -- We probably need to close this, but it should close
            -- after the test is done.
            walletRunner cfg dbs secretKeyPath ws
            -- closeState ws


-- | Required instance for IO.
instance HasLoggerName IO where
    askLoggerName = pure $ LoggerName "APISpec"
    modifyLoggerName _ x = x


newRealModeContext
    :: HasConfigurations
    => NodeDBs
    -> ConfigurationOptions
    -> FilePath
    -> Production (RealModeContext ())
newRealModeContext dbs confOpts secretKeyPath = do

    let nodeArgs = NodeArgs {
      behaviorConfigPath = Nothing
    }

    let networkOps = NetworkConfigOpts {
          ncoTopology = Nothing
        , ncoKademlia = Nothing
        , ncoSelf     = Nothing
        , ncoPort     = 3030
        , ncoPolicies = Nothing
        , ncoBindAddress = Nothing
        , ncoExternalAddress = Nothing
        }

    let cArgs@CommonNodeArgs {..} = CommonNodeArgs {
           dbPath                 = Just "node-db"
         , rebuildDB              = True
         , devGenesisSecretI      = Nothing
         , keyfilePath            = secretKeyPath
         , networkConfigOpts      = networkOps
         , jlPath                 = Nothing
         , commonArgs             = CommonArgs {
               logConfig            = Nothing
             , logPrefix            = Nothing
             , reportServers        = mempty
             , updateServers        = mempty
             , configurationOptions = confOpts
             }
         , updateLatestPath       = "update"
         , updateWithPackage      = False
         , noNTP                  = True
         , route53Params          = Nothing
         , enableMetrics          = False
         , ekgParams              = Nothing
         , statsdParams           = Nothing
         , cnaDumpGenesisDataPath = Nothing
         , cnaDumpConfiguration   = False
         }

    loggerName <- askLoggerName
    nodeParams <- getNodeParams loggerName cArgs nodeArgs

    let vssSK = fromJust $ npUserSecret nodeParams ^. usVss
    let gtParams = gtSscParams cArgs vssSK (npBehaviorConfig nodeParams)

    networkConfig <- intNetworkConfigOpts networkOps

    -- Maybe switch to bracketNodeResources?
    nodeResources <-  allocateNodeResources
                          networkConfig
                          nodeParams
                          gtParams
                          txpGlobalSettings
                          initNodeDBs

    RealModeContext <$> pure dbs
                    <*> pure (nrSscState nodeResources)
                    <*> pure (nrTxpState nodeResources)
                    <*> pure (nrDlgState nodeResources)
                    <*> jsonLogConfigFromHandle stdout
                    <*> pure (LoggerName "APISpec")
                    <*> pure (nrContext nodeResources)


-- | The runner we need to retunr out wallet context.
walletRunner
    :: (HasConfigurations, HasCompileInfo)
    => ConfigurationOptions
    -> NodeDBs
    -> FilePath
    -> WalletDB
    -> IO WalletWebModeContext
walletRunner confOpts dbs secretKeyPath ws =
    WalletWebModeContext <$> pure ws
                         <*> newTVarIO def
                         <*> (AddrCIdHashes <$> (newIORef mempty))
                         <*> runProduction (newRealModeContext dbs confOpts secretKeyPath)


-- Our API apparently is returning JSON Arrays which is considered bad practice as very old
-- browsers can be hacked: https://haacked.com/archive/2009/06/25/json-hijacking.aspx/
-- The general consensus, after discussing this with the team, is that we can be moderately safe.
-- stack test cardano-sl-wallet-new --fast --test-arguments '-m "Servant API Properties"'
spec :: Spec
spec = withCompileInfo def $ do
    withDefConfigurations $ do
      describe "Servant API Properties" $ do
        it "V0 API follows best practices & is RESTful abiding" $ do
          ddl <- D.dummyDiffusionLayer
          withServantServer (Proxy @V0.API) (v0Server (D.diffusion ddl)) $ \burl ->
            serverSatisfies (Proxy @V0.API) burl stdArgs predicates
        it "V1 API follows best practices & is RESTful abiding" $ do
          ddl <- D.dummyDiffusionLayer
          withServantServer (Proxy @V1.API) (v1Server (D.diffusion ddl)) $ \burl ->
            serverSatisfies (Proxy @V1.API) burl stdArgs predicates
