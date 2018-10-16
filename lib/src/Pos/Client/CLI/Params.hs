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
import           Pos.Util.UserPublic (peekUserPublic)
import           Pos.Util.UserSecret (peekUserSecret, usVss)
import           Pos.Util.Util (eitherToThrow)
import           Pos.Util.Wlog (LoggerName, WithLogger)

loggingParams :: LoggerName -> CommonNodeArgs -> LoggingParams
loggingParams defaultName CommonNodeArgs{..} =
    LoggingParams
    { lpHandlerPrefix = logPrefix commonArgs
    , lpConfigPath    = logConfig commonArgs
    , lpDefaultName   = defaultName
    , lpConsoleLog    = Nothing -- no override by default
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

getNodeParams ::
       ( MonadIO m
       , WithLogger m
       , MonadCatch m
       )
    => LoggerName
    -> CommonNodeArgs
    -> NodeArgs
    -> Maybe GeneratedSecrets
    -> m (NodeParams, Maybe SscParams)
getNodeParams defaultLoggerName cArgs@CommonNodeArgs{..} NodeArgs{..} mGeneratedSecrets = do
    (primarySK, userSecret) <- prepareUserSecret cArgs mGeneratedSecrets
        =<< peekUserSecret (getKeyfilePath cArgs)
    userPublic <- peekUserPublic publicKeyfilePath
    npNetworkConfig <- intNetworkConfigOpts networkConfigOpts
    npBehaviorConfig <- case behaviorConfigPath of
        Nothing -> pure def
        Just fp -> eitherToThrow =<< liftIO (Yaml.decodeFileEither fp)
    npFInjects <- mkFInjects cnaFInjects

    let nodeParams = NodeParams
            { npDbPathM = dbPath
            , npRebuildDb = rebuildDB
            , npSecretKey = primarySK
            , npUserSecret = userSecret
            , npUserPublic = userPublic
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
