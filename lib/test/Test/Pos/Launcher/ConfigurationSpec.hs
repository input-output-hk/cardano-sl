module Test.Pos.Launcher.ConfigurationSpec
    ( spec
    ) where

import           Universum

import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import           Ntp.Client (NtpConfiguration)
import qualified Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Core.Slotting (Timestamp (..))
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))
import           Pos.Launcher.Configuration (ConfigurationOptions (..),
                     WalletConfiguration, defaultConfigurationOptions,
                     withConfigurations)
import           Pos.Util.Config (ConfigurationException)
import           Pos.Util.Trace (noTrace)

configFilePath :: FilePath
configFilePath = "configuration.yaml"

checkYamlSection :: Text -> Spec
checkYamlSection key = describe ("key: " ++ show key) $ do
    it "should be RequiresNoMagic" $ do
        startTime <- Timestamp . round . (* 1000000) <$> liftIO getPOSIXTime
        let cfo = ConfigurationOptions configFilePath key (Just startTime) Nothing
        rnm  <- liftIO (withConfigurations noTrace Nothing Nothing False cfo getRNM)
        rnm `shouldBe` RequiresNoMagic

getRNM
    :: Genesis.Config
    -> WalletConfiguration
    -> TxpConfiguration
    -> NtpConfiguration
    -> IO RequiresNetworkMagic
getRNM cfg _ _ _ =
    pure $ getRequiresNetworkMagic $ Genesis.configProtocolMagic cfg

spec :: Spec
spec = describe "Pos.Launcher.Configuration" $ do
    describe "withConfigurations" $ do
        it ("should parse `" <> configFilePath <> "` file") $ do
            startTime <- Timestamp . round . (* 1000000) <$> liftIO getPOSIXTime
            let cfo = defaultConfigurationOptions
                        { cfoFilePath = configFilePath
                        , cfoSystemStart = Just startTime
                        }
            let catchFn :: ConfigurationException -> IO (Maybe ConfigurationException)
                catchFn e = return $ Just e
            res  <- liftIO $ catch
                (withConfigurations noTrace Nothing Nothing False cfo (\_ _ _ _ -> return Nothing))
                catchFn
            res `shouldSatisfy` isNothing

    -- Ensuring that all of the config objects mapped to each of the specified
    -- keys contain RequiresNoMagic.
    mapM_ checkYamlSection
        [ "mainnet_full" -- mainnet core/relay nodes and exchange wallets
        , "mainnet_dryrun_full" -- staging core/relay nodes and exchange wallets
        , "mainnet_wallet_win64" -- mainnet wallets (daedalus)
        , "mainnet_wallet_macos64"
        , "mainnet_wallet_linux64"
        , "mainnet_dryrun_wallet_win64" -- staging wallets (daedalus)
        , "mainnet_dryrun_wallet_macos64"
        , "mainnet_dryrun_wallet_linux64"
        ]

