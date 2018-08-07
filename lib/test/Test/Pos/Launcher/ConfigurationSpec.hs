module Test.Pos.Launcher.ConfigurationSpec
    ( spec
    ) where

import           Universum

import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.Wlog (LoggerName (..))
import           Test.Hspec (Spec, describe, it, shouldSatisfy)

import           Pos.Core.Slotting (Timestamp (..))
import           Pos.Launcher.Configuration (ConfigurationOptions (..),
                     defaultConfigurationOptions, withConfigurationsM)
import           Pos.Util.Config (ConfigurationException)

spec :: Spec
spec = describe "Pos.Launcher.Configuration" $ do
    describe "withConfigurationsM" $ do
        it "should parse `lib/configuration.yaml` file" $ do
            startTime <- Timestamp . round . (* 1000000) <$> liftIO getPOSIXTime
            let cfo = defaultConfigurationOptions
                        { cfoFilePath = "./configuration.yaml"
                        , cfoSystemStart = Just startTime
                        }
            let catchFn :: ConfigurationException -> IO (Maybe ConfigurationException)
                catchFn e = return $ Just e
            res  <- liftIO $ catch
                (withConfigurationsM (LoggerName "test") Nothing cfo id (\_ _ _ -> return Nothing))
                catchFn
            res `shouldSatisfy` isNothing

