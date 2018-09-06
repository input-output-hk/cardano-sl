module Test.Pos.Launcher.ConfigurationSpec
    ( spec
    ) where

import           Universum

import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Test.Hspec (Spec, describe, it, shouldSatisfy)

import           Pos.Core.Slotting (Timestamp (..))
import           Pos.Launcher.Configuration (ConfigurationOptions (..),
                     defaultConfigurationOptions, withConfigurationsM)
import           Pos.Util.Config (ConfigurationException)
import           Pos.Util.Log.LoggerConfig (defaultTestConfiguration)
import           Pos.Util.Wlog (Severity (Debug), setupLogging)

spec :: Spec
spec = describe "Pos.Launcher.Configuration" $ do
    describe "withConfigurationsM" $ do
        it "should parse `lib/configuration.yaml` file" $ do
            liftIO $ setupLogging (defaultTestConfiguration Debug)
            startTime <- Timestamp . round . (* 1000000) <$> liftIO getPOSIXTime
            let cfo = defaultConfigurationOptions
                        { cfoFilePath = "./configuration.yaml"
                        , cfoSystemStart = Just startTime
                        }
            let catchFn :: ConfigurationException -> IO (Maybe ConfigurationException)
                catchFn e = return $ Just e
            res  <- liftIO $ catch
                (withConfigurationsM "test" Nothing Nothing False cfo (\_ _ _ _ -> return Nothing))
                catchFn
            res `shouldSatisfy` isNothing
