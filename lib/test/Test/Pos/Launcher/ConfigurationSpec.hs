module Test.Pos.Launcher.ConfigurationSpec
    ( spec
    ) where

import           Universum

import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Test.Hspec (Spec, describe, it, shouldSatisfy)

import           Pos.Core.Slotting (Timestamp (..))
import           Pos.Launcher.Configuration (ConfigurationOptions (..),
                     defaultConfigurationOptions, withConfigurations)
import           Pos.Util.Config (ConfigurationException)
import           Pos.Util.Wlog (setupTestLogging)

spec :: Spec
spec = describe "Pos.Launcher.Configuration" $ do
    describe "withConfigurations" $ do
        it "should parse `lib/configuration.yaml` file" $ do
            liftIO setupTestLogging
            startTime <- Timestamp . round . (* 1000000) <$> liftIO getPOSIXTime
            let cfo = defaultConfigurationOptions
                        { cfoFilePath = "./configuration.yaml"
                        , cfoSystemStart = Just startTime
                        }
            let catchFn :: ConfigurationException -> IO (Maybe ConfigurationException)
                catchFn e = return $ Just e
            res  <- liftIO $ catch
                (withConfigurations Nothing Nothing False cfo (\_ _ _ _ -> return Nothing))
                catchFn
            res `shouldSatisfy` isNothing
