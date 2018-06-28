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
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON,
                             withObject, (.:), (.:?))
import           Data.Default (Default (..))
import           Data.Time.Units (fromMicroseconds)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text

import           Serokell.Aeson.Options (defaultOptions)
import           System.FilePath (takeDirectory)

-- FIXME consistency on the locus of the JSON instances for configuration.
-- Core keeps them separate, infra update and ssc define them on-site.
import           Pos.Aeson.Core.Configuration ()
import           Pos.Core (Address, decodeTextAddress)
import           Pos.Core.Slotting (Timestamp (..))

import           Pos.Block.Configuration
import           Pos.Configuration
import           Pos.Core.Configuration
import           Pos.Delegation.Configuration
import           Pos.Infra.Ntp.Configuration
import           Pos.Ssc.Configuration
import           Pos.Txp.Configuration
import           Pos.Update.Configuration
import           Pos.Util.Config (parseYamlConfig)
import           Pos.Util.Log (WithLogger, logInfo)

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
withConfigurations
    :: (WithLogger m, MonadThrow m)
    => Maybe AssetLockPath
    -> ConfigurationOptions
    -> (HasConfigurations => NtpConfiguration -> ProtocolMagic -> m r)
    -> m r
withConfigurations mAssetLockPath cfo act = do
    logInfo ("using configurations: " <> show cfo)
    cfg <- parseYamlConfig (cfoFilePath cfo) (cfoKey cfo)
    assetLock <- case mAssetLockPath of
        Nothing -> pure mempty
        Just fp -> liftIO $ readAssetLockedSrcAddrs fp
    let configDir = takeDirectory $ cfoFilePath cfo
    withCoreConfigurations (ccCore cfg) configDir (cfoSystemStart cfo) (cfoSeed cfo) $
        withUpdateConfiguration (ccUpdate cfg) $
        withSscConfiguration (ccSsc cfg) $
        withDlgConfiguration (ccDlg cfg) $
        withTxpConfiguration (addAssetLock assetLock $ ccTxp cfg) $
        withBlockConfiguration (ccBlock cfg) $
        withNodeConfiguration (ccNode cfg) $ act (ccNtp cfg)

addAssetLock :: Set Address -> TxpConfiguration -> TxpConfiguration
addAssetLock bset tcfg =
    tcfg { tcAssetLockedSrcAddrs = Set.union (tcAssetLockedSrcAddrs tcfg) bset }

newtype AssetLockPath = AssetLockPath FilePath

readAssetLockedSrcAddrs :: AssetLockPath -> IO (Set Address)
readAssetLockedSrcAddrs (AssetLockPath fp) = do
    res <- filter keepLine . fmap Text.strip . lines <$> readFile fp
    case partitionEithers $ map decodeTextAddress res of
        ([], xs) -> pure $ Set.fromList xs
        (errs, _) -> error $ "Error reading assetLock file:\n" <> unlines errs
  where
    keepLine t =
      not (Text.null t || "#" `Text.isPrefixOf` t)
