{-# LANGUAGE RecordWildCards #-}

-- | Getter params from Args

module Pos.Client.CLI.Params
       ( loggingParams
       , getBaseParams
       , getKeyfilePath
       , getNodeParams
       , gtSscParams
       ) where

import           Universum

import           Data.Default (def)
import qualified Data.Yaml as Yaml

import           Pos.Behavior (BehaviorConfig (..))
import           Pos.Chain.Genesis (GeneratedSecrets)
import           Pos.Chain.Ssc (SscParams (..))
import           Pos.Chain.Update (UpdateParams (..))
import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..), NodeArgs (..))
import           Pos.Client.CLI.Options (CommonArgs (..))
import           Pos.Client.CLI.Secrets (prepareUserSecret)
import           Pos.Crypto (VssKeyPair)
import           Pos.Infra.InjectFail (mkFInjects)
import           Pos.Infra.Network.CLI (intNetworkConfigOpts)
import           Pos.Launcher.Param (BaseParams (..), LoggingParams (..),
                     NodeParams (..))
import           Pos.Util.Trace (Severity (Info), Trace, contramap)
import           Pos.Util.UserSecret (peekUserSecret, usVss)
import           Pos.Util.Util (eitherToThrow)
import           Pos.Util.Wlog (LoggerName)

loggingParams :: LoggerName -> CommonNodeArgs -> LoggingParams
loggingParams defaultName CommonNodeArgs{..} =
    LoggingParams
    { lpHandlerPrefix = logPrefix commonArgs
    , lpConfigPath    = logConfig commonArgs
    , lpDefaultName   = defaultName
    , lpConsoleLog    = Just (not $ logConsoleOff commonArgs)
    }

getBaseParams :: LoggerName -> CommonNodeArgs -> BaseParams
getBaseParams defaultLoggerName args@CommonNodeArgs {..} =
    BaseParams { bpLoggingParams = loggingParams defaultLoggerName args }

gtSscParams :: CommonNodeArgs -> VssKeyPair -> BehaviorConfig -> SscParams
gtSscParams CommonNodeArgs {..} vssSK BehaviorConfig{..} =
    SscParams
    { spSscEnabled = True
    , spVssKeyPair = vssSK
    , spBehavior   = bcSscBehavior
    }

getKeyfilePath :: CommonNodeArgs -> FilePath
getKeyfilePath CommonNodeArgs {..}
    = case devGenesisSecretI of
          Nothing -> keyfilePath
          Just i  -> "node-" ++ show i ++ "." ++ keyfilePath

getNodeParams
    :: Trace IO (Severity, Text)
    -- ^ Must give a `Trace` because some things in here do logging. Yes
    -- that's weird because you also give a `LoggerName` which seems to imply
    -- logging depends upon the params returned.
    -> LoggerName
    -> CommonNodeArgs
    -> NodeArgs
    -> Maybe GeneratedSecrets
    -> IO (NodeParams, Maybe SscParams)
getNodeParams logTrace defaultLoggerName cArgs@CommonNodeArgs{..} NodeArgs{..} mGeneratedSecrets = do
    let logTraceInfo = contramap ((,) Info) logTrace
    (primarySK, userSecret) <- prepareUserSecret logTraceInfo cArgs mGeneratedSecrets
        =<< peekUserSecret logTrace (getKeyfilePath cArgs)
    npNetworkConfig <- intNetworkConfigOpts logTrace networkConfigOpts
    npBehaviorConfig <- case behaviorConfigPath of
        Nothing -> pure def
        Just fp -> eitherToThrow =<< liftIO (Yaml.decodeFileEither fp)
    npFInjects <- liftIO $ mkFInjects cnaFInjectsSpec

    let nodeParams = NodeParams
            { npDbPathM = dbPath
            , npRebuildDb = rebuildDB
            , npSecretKey = primarySK
            , npUserSecret = userSecret
            , npBaseParams = getBaseParams defaultLoggerName cArgs
            , npJLFile = jlPath
            , npReportServers = reportServers commonArgs
            , npUpdateParams = UpdateParams
                { upUpdatePath    = updateLatestPath
                , upUpdateWithPkg = updateWithPackage
                , upUpdateServers = updateServers commonArgs
                }
            , npRoute53Params = route53Params
            , npEnableMetrics = enableMetrics
            , npEkgParams = ekgParams
            , npStatsdParams = statsdParams
            , npAssetLockPath = cnaAssetLockPath
            , ..
            }

    let sscParams = gtSscParams cArgs
           <$> (userSecret ^. usVss)
           <*> pure npBehaviorConfig

    return (nodeParams, sscParams)
