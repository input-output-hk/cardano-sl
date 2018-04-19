{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types    #-}

-- | Configuration for a node: values which are constant for the lifetime of
-- the running program, not for the lifetime of the executable binary itself.

module Pos.Launcher.Configuration
       ( Configuration (..)
       , HasConfigurations

       , ConfigurationOptions (..)
       , defaultConfigurationOptions

       , withConfigurations
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON,
                             withObject, (.:), (.:?))
import           Data.Default (Default (..))
import           Serokell.Aeson.Options (defaultOptions)
import           Serokell.Util (sec)
import           System.FilePath (takeDirectory)
import           System.Wlog (WithLogger, logInfo)

-- FIXME consistency on the locus of the JSON instances for configuration.
-- Core keeps them separate, infra update and ssc define them on-site.
import           Pos.Aeson.Core.Configuration ()
import           Pos.Core.Slotting (Timestamp (..))
import           Pos.Util.Config (parseYamlConfig)

import           Pos.Block.Configuration
import           Pos.Configuration
import           Pos.Core.Configuration
import           Pos.Delegation.Configuration
import           Pos.Ntp.Configuration
import           Pos.Ssc.Configuration
import           Pos.Txp.Configuration
import           Pos.Update.Configuration

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
    , HasTxpConfiguration
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
        cfoSystemStart <- (Timestamp . sec) <<$>> o .:? "systemStart"
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
withConfigurations
    :: (WithLogger m, MonadThrow m, MonadIO m)
    => ConfigurationOptions
    -> (HasConfigurations => NtpConfiguration -> m r)
    -> m r
withConfigurations co@ConfigurationOptions{..} act = do
    logInfo ("using configurations: " <> show co)
    Configuration{..} <- parseYamlConfig cfoFilePath cfoKey
    let configurationDir = takeDirectory cfoFilePath
    withCoreConfigurations ccCore configurationDir cfoSystemStart cfoSeed $
        withUpdateConfiguration ccUpdate $
        withSscConfiguration ccSsc $
        withDlgConfiguration ccDlg $
        withTxpConfiguration ccTxp $
        withBlockConfiguration ccBlock $
        withNodeConfiguration ccNode $ act ccNtp
