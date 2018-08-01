{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types    #-}

-- | Configuration for a node: values which are constant for the lifetime of
-- the running program, not for the lifetime of the executable binary itself.

module Pos.Launcher.Configuration
       ( AssetLockPath (..)
       , Configuration (..)
       , HasConfigurations

       , ConfigurationOptions (..)
       , defaultConfigurationOptions

       , withConfigurations

       -- Exposed mostly for testing.
       , readAssetLockedSrcAddrs
       , withConfigurationsM
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON,
                     genericToJSON, withObject, (.:), (.:?))
import           Data.Default (Default (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Time.Units (fromMicroseconds)

import           Data.Aeson.Options (defaultOptions)
import           System.FilePath (takeDirectory)
import           System.Wlog (LoggerName, WithLogger, askLoggerName, logInfo,
                     usingLoggerName)

import           Ntp.Client (NtpConfiguration)

import           Pos.Core (Address, decodeTextAddress)
import           Pos.Core.Genesis (GenesisData)
import           Pos.Core.Slotting (Timestamp (..))
import           Pos.Util.Config (parseYamlConfig)

import           Pos.Chain.Block
import           Pos.Chain.Delegation
import           Pos.Chain.Ssc hiding (filter)
import           Pos.Chain.Txp
import           Pos.Chain.Update
import           Pos.Configuration
import           Pos.Core.Configuration

-- | Product of all configurations required to run a node.
data Configuration = Configuration
    { ccCore   :: !CoreConfiguration
    , ccNtp    :: !NtpConfiguration
    , ccUpdate :: !UpdateConfiguration
    , ccSsc    :: !SscConfiguration
    , ccDlg    :: !DlgConfiguration
    , ccTxp    :: !TxpConfiguration
    , ccBlock  :: !BlockConfiguration
    , ccNode   :: !NodeConfiguration
    } deriving (Show, Generic)

instance FromJSON Configuration where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Configuration where
    toJSON = genericToJSON defaultOptions

type HasConfigurations =
    ( HasConfiguration
    , HasUpdateConfiguration
    , HasSscConfiguration
    , HasBlockConfiguration
    , HasDlgConfiguration
    , HasNodeConfiguration
    )

-- | Configuration yaml file location and the key to use. The file should
-- parse to a MultiConfiguration and the 'cfoKey' should be one of the keys
-- in the map.
data ConfigurationOptions = ConfigurationOptions
    { cfoFilePath    :: !FilePath
    , cfoKey         :: !Text
      -- | An optional system start time override. Required when using a
      -- testnet genesis configuration.
    , cfoSystemStart :: !(Maybe Timestamp)
      -- | Seed for secrets generation can be provided via CLI, in
      -- this case it overrides one from configuration file.
    , cfoSeed        :: !(Maybe Integer)
    } deriving (Show)

instance FromJSON ConfigurationOptions where
    parseJSON = withObject "ConfigurationOptions" $ \o -> do
        cfoFilePath    <- o .: "filePath"
        cfoKey         <- o .: "key"
        cfoSystemStart <- (Timestamp . fromMicroseconds . (*) 1000000) <<$>> o .:? "systemStart"
        cfoSeed        <- o .:? "seed"
        pure ConfigurationOptions {..}

defaultConfigurationOptions :: ConfigurationOptions
defaultConfigurationOptions = ConfigurationOptions
    { cfoFilePath    = "lib/configuration.yaml"
    , cfoKey         = "default"
    , cfoSystemStart = Nothing
    , cfoSeed        = Nothing
    }

instance Default ConfigurationOptions where
    def = defaultConfigurationOptions

-- | Parse some big yaml file to 'MultiConfiguration' and then use the
-- configuration at a given key.
withConfigurationsM
    :: forall m r. (MonadThrow m, MonadIO m)
    => LoggerName
    -> Maybe AssetLockPath
    -> ConfigurationOptions
    -> (GenesisData -> GenesisData)
    -- ^ change genesis data; this is useful if some parameters are passed as
    -- comand line arguments for some tools (profiling executables, benchmarks).
    -> (HasConfigurations => ProtocolMagic -> TxpConfiguration -> NtpConfiguration -> m r)
    -> m r
withConfigurationsM logName mAssetLockPath cfo fn act = do
    logInfo' ("using configurations: " <> show cfo)
    cfg <- parseYamlConfig (cfoFilePath cfo) (cfoKey cfo)
    assetLock <- case mAssetLockPath of
        Nothing -> pure mempty
        Just fp -> liftIO $ readAssetLockedSrcAddrs fp
    let configDir = takeDirectory $ cfoFilePath cfo
    withCoreConfigurations (ccCore cfg) fn configDir (cfoSystemStart cfo) (cfoSeed cfo) $
        withUpdateConfiguration (ccUpdate cfg) $
        withSscConfiguration (ccSsc cfg) $
        withDlgConfiguration (ccDlg cfg) $
        withBlockConfiguration (ccBlock cfg) $
        withNodeConfiguration (ccNode cfg) $ \ pm -> act pm (addAssetLock assetLock $ ccTxp cfg) (ccNtp cfg)

    where
    logInfo' :: Text -> m ()
    logInfo' = liftIO . usingLoggerName logName . logInfo

withConfigurations
    :: (WithLogger m, MonadThrow m, MonadIO m)
    => Maybe AssetLockPath
    -> ConfigurationOptions
    -> (HasConfigurations => ProtocolMagic -> TxpConfiguration -> NtpConfiguration -> m r)
    -> m r
withConfigurations mAssetLockPath cfo act = do
    loggerName <- askLoggerName
    withConfigurationsM loggerName mAssetLockPath cfo id act

addAssetLock :: Set Address -> TxpConfiguration -> TxpConfiguration
addAssetLock bset tcfg =
    tcfg { tcAssetLockedSrcAddrs = Set.union (tcAssetLockedSrcAddrs tcfg) bset }

newtype AssetLockPath = AssetLockPath FilePath

readAssetLockedSrcAddrs :: AssetLockPath -> IO (Set Address)
readAssetLockedSrcAddrs (AssetLockPath fp) = do
    res <- filter keepLine . fmap Text.strip . lines <$> readFile fp
    case partitionEithers $ map decodeTextAddress res of
        ([], xs)  -> pure $ Set.fromList xs
        (errs, _) -> error $ "Error reading assetLock file:\n" <> unlines errs
  where
    keepLine t =
      not (Text.null t || "#" `Text.isPrefixOf` t)
