{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}

-- | Configuration for a node: values which are constant for the lifetime of
-- the running program, not for the lifetime of the executable binary itself.

module Pos.Launcher.Configuration
       ( AssetLockPath (..)
       , Configuration (..)
       , WalletConfiguration(..)
       , defaultWalletConfiguration
       , ThrottleSettings(..)
       , defaultThrottleSettings
       , HasConfigurations

       , ConfigurationOptions (..)
       , defaultConfigurationOptions

       , withConfigurations

       , dumpGenesisData

       -- Exposed mostly for testing.
       , readAssetLockedSrcAddrs
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON,
                     genericToJSON, withObject, (.!=), (.:), (.:?))
import qualified Data.ByteString.Lazy as BSL
import           Data.Default (Default (..))
import qualified Data.HashMap.Strict as HM
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Time.Units (fromMicroseconds)
import qualified Data.Yaml as Yaml
import           Formatting (sformat, shown, (%))

import           Data.Aeson.Options (defaultOptions)
import           System.FilePath (takeDirectory)

import           Ntp.Client (NtpConfiguration)

import           Pos.Chain.Genesis as Genesis (Config (..), GenesisData (..),
                     StaticConfig, canonicalGenesisJson,
                     mkConfigFromStaticConfig, prettyGenesisJson)
import           Pos.Core (Address, decodeTextAddress)
import           Pos.Core.Conc (currentTime)
import           Pos.Core.Slotting (Timestamp (..))
import           Pos.Crypto (RequiresNetworkMagic (..))
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.Config (parseYamlConfig)
import           Pos.Util.Wlog (WithLogger, logInfo)

import           Pos.Chain.Block
import           Pos.Chain.Delegation
import           Pos.Chain.Ssc hiding (filter)
import           Pos.Chain.Txp
import           Pos.Chain.Update
import           Pos.Configuration

-- | Product of all configurations required to run a node.
data Configuration = Configuration
    { ccGenesis     :: !StaticConfig
    , ccNtp         :: !NtpConfiguration
    , ccUpdate      :: !UpdateConfiguration
    , ccSsc         :: !SscConfiguration
    , ccDlg         :: !DlgConfiguration
    , ccTxp         :: !TxpConfiguration
    , ccBlock       :: !BlockConfiguration
    , ccNode        :: !NodeConfiguration
    , ccWallet      :: !WalletConfiguration
    , ccReqNetMagic :: !RequiresNetworkMagic
    } deriving (Show, Generic)

instance FromJSON Configuration where
    parseJSON = withObject "Configuration" $ \o -> do
        ccGenesis <- if
            | HM.member "genesis" o -> o .: "genesis"
            | HM.member "core" o -> do
                coreO <- o .: "core"
                coreO .: "genesis"
            | otherwise -> fail "Incorrect JSON encoding for Configuration"
        ccNtp    <- o .: "ntp"
        ccUpdate <- o .: "update"
        ccSsc    <- o .: "ssc"
        ccDlg    <- o .: "dlg"
        ccTxp    <- o .: "txp"
        ccBlock  <- o .: "block"
        ccNode   <- o .: "node"
        ccWallet <- o .: "wallet"
        ccReqNetMagic <- if
            -- If the "requiresNetworkMagic" key is specified, use the
            -- mapped value.
            | HM.member "requiresNetworkMagic" o -> o .: "requiresNetworkMagic"

            -- (for backward-compat with the old CoreConfiguration format)
            -- else if the "core" key is specified and the
            -- "requiresNetworkMagic" key is specified within its mapped
            -- object, use that value. Otherwise, default to NMMustBeJust
            | HM.member "core" o -> do
                coreO <- o .: "core"
                coreO .:? "requiresNetworkMagic" .!= NMMustBeJust

            -- else default to NMMustBeJust
            | otherwise -> pure NMMustBeJust
        pure $ Configuration {..}

instance ToJSON Configuration where
     toJSON = genericToJSON defaultOptions

data WalletConfiguration = WalletConfiguration
    { ccThrottle :: !(Maybe ThrottleSettings)
    } deriving (Show, Generic)

defaultWalletConfiguration :: WalletConfiguration
defaultWalletConfiguration = WalletConfiguration
    { ccThrottle = Nothing
    }

instance FromJSON WalletConfiguration where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON WalletConfiguration where
    toJSON = genericToJSON defaultOptions

data ThrottleSettings = ThrottleSettings
    { tsRate   :: !Word64
    , tsPeriod :: !Word64
    , tsBurst  :: !Word64
    } deriving (Show, Generic)

defaultThrottleSettings :: ThrottleSettings
defaultThrottleSettings = ThrottleSettings
    { tsRate = 30
    , tsPeriod = 1000000
    , tsBurst = 30
    }

instance FromJSON ThrottleSettings where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON ThrottleSettings where
    toJSON = genericToJSON defaultOptions

type HasConfigurations =
    ( HasUpdateConfiguration
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
withConfigurations
    :: (WithLogger m, MonadThrow m, MonadIO m)
    => Maybe AssetLockPath
    -> Maybe FilePath
    -> Bool
    -> ConfigurationOptions
    -> (HasConfigurations => Genesis.Config -> WalletConfiguration -> TxpConfiguration -> NtpConfiguration -> m r)
    -> m r
withConfigurations mAssetLockPath dumpGenesisPath dumpConfig cfo act = do
    logInfo ("using configurations: " <> show cfo)
    cfg <- parseYamlConfig (cfoFilePath cfo) (cfoKey cfo)
    assetLock <- case mAssetLockPath of
        Nothing -> pure mempty
        Just fp -> liftIO $ readAssetLockedSrcAddrs fp
    let configDir = takeDirectory $ cfoFilePath cfo
    genesisConfig <- mkConfigFromStaticConfig
        configDir
        (cfoSystemStart cfo)
        (cfoSeed cfo)
        (ccReqNetMagic cfg)
        (ccGenesis cfg)
    withUpdateConfiguration (ccUpdate cfg) $
        withSscConfiguration (ccSsc cfg) $
        withDlgConfiguration (ccDlg cfg) $
        withBlockConfiguration (ccBlock cfg) $
        withNodeConfiguration (ccNode cfg) $ do
            let txpConfig = addAssetLock assetLock $ ccTxp cfg
            printInfoOnStart
                dumpGenesisPath
                dumpConfig
                (configGenesisData genesisConfig)
                (ccGenesis cfg)
                (ccNtp cfg)
                (ccWallet cfg)
                txpConfig
                (ccReqNetMagic cfg)
            act genesisConfig (ccWallet cfg) txpConfig (ccNtp cfg)

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

printInfoOnStart ::
       (HasConfigurations, WithLogger m, MonadIO m)
    => Maybe FilePath
    -> Bool
    -> GenesisData
    -> StaticConfig
    -> NtpConfiguration
    -> WalletConfiguration
    -> TxpConfiguration
    -> RequiresNetworkMagic
    -> m ()
printInfoOnStart dumpGenesisPath dumpConfig genesisData genesisConfig ntpConfig walletConfig txpConfig rnm = do
    whenJust dumpGenesisPath $ dumpGenesisData genesisData True
    when dumpConfig $ dumpConfiguration genesisConfig ntpConfig walletConfig txpConfig rnm
    printFlags
    t <- currentTime
    mapM_ logInfo $
        [ sformat ("System start time is " % shown) $ gdStartTime genesisData
        , sformat ("Current time is "%shown) (Timestamp t)
        ]

printFlags :: WithLogger m => m ()
printFlags = do
    inAssertMode $ logInfo "Asserts are ON"

-- | Dump our 'GenesisData' into a file.
dumpGenesisData ::
       (MonadIO m, WithLogger m) => GenesisData -> Bool -> FilePath -> m ()
dumpGenesisData genesisData canonical path = do
    let (canonicalJsonBytes, jsonHash) = canonicalGenesisJson genesisData
    let prettyJsonStr = prettyGenesisJson genesisData
    logInfo $ sformat ("Writing JSON with hash "%shown%" to "%shown) jsonHash path
    liftIO $ case canonical of
        True  -> BSL.writeFile path canonicalJsonBytes
        False -> writeFile path (toText prettyJsonStr)

-- | Dump our configuration into stdout and exit.
dumpConfiguration
    :: (HasConfigurations, MonadIO m)
    => StaticConfig
    -> NtpConfiguration
    -> WalletConfiguration
    -> TxpConfiguration
    -> RequiresNetworkMagic
    -> m ()
dumpConfiguration genesisConfig ntpConfig walletConfig txpConfig rnm = do
    let conf =
            Configuration
            { ccGenesis = genesisConfig
            , ccNtp = ntpConfig
            , ccUpdate = updateConfiguration
            , ccSsc = sscConfiguration
            , ccDlg = dlgConfiguration
            , ccTxp = txpConfig
            , ccBlock = blockConfiguration
            , ccNode = nodeConfiguration
            , ccWallet = walletConfig
            , ccReqNetMagic = rnm
            }
    putText . decodeUtf8 . Yaml.encode $ conf
    exitSuccess
