{-# LANGUAGE ApplicativeDo #-}

-- | Functions for working with Endpoints configuration file.

module Bench.Cardano.Wallet.Config.Endpoints
    ( getEndpointsConfig
    , extractEndpointConfigFor
    ) where

import           Universum

import           Data.Csv                   (FromRecord (..), HasHeader (..),
                                             (.!), decode)

import           Control.Exception          (SomeException)
import qualified Data.ByteString.Lazy       as Lazy
import           Data.List.NonEmpty         (fromList)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V

import           Bench.Cardano.Wallet.Types (CompleteConfig (..),
                                             EndpointConfig (..),
                                             BenchEndpoint (..))

-- | Read Endpoints configuration from the local .csv-file. The format is:
--
-- BenchName,BenchDuration,MinDelayBetweenCalls,MaxDelayBetweenCalls,PathToReportFile
-- GetHistoryBench,5.0,0.5,2.5,wallet/bench/results/GetHistoryBenchReport.csv
getEndpointsConfig :: FilePath -> IO (NonEmpty EndpointConfig)
getEndpointsConfig pathToConfig = do
    content <- Lazy.readFile pathToConfig `catch` anyProblems
    case extractConfig content of
        Left problem -> reportAboutInvalidConfig problem
        Right (V.toList -> configs) -> do
            makeSureConfigsAreNonEmpty configs
            mapM_ checkDelayRange configs
            mapM_ checkNumberOfMeasures configs
            return $ fromList configs
  where
    extractConfig rawContent =
        decode HasHeader rawContent :: Either String (Vector EndpointConfig)

    anyProblems :: SomeException -> IO a
    anyProblems whatHappened = error . toText $
        "Unable to open configuration " <> pathToConfig <> ": " <> show whatHappened

    reportAboutInvalidConfig :: String -> IO a
    reportAboutInvalidConfig problem = error . toText $
        "Invalid configuration " <> pathToConfig <> ": " <> show problem

    makeSureConfigsAreNonEmpty :: [EndpointConfig] -> IO ()
    makeSureConfigsAreNonEmpty configs =
        when (null configs) $ error "Endpoints.csv must contain at least one endpoint for benchmarking."

    checkDelayRange :: EndpointConfig -> IO ()
    checkDelayRange (EndpointConfig name _ from to _ _) =
        when (from < 0.0 || to < 0.0 || from > to) $
            error . toText $ "Invalid delay range for bench '" <> name <> "'"

    checkNumberOfMeasures :: EndpointConfig -> IO ()
    checkNumberOfMeasures (EndpointConfig name number _ _ _ _) =
        when (number < 10) $
            error . toText $ "Invalid number of measures for bench '" <> name <> "', use at least 10."

-- | This instance is used for parsing @EndpointConfig@
-- from one record (line) in .csv-config.
instance FromRecord EndpointConfig where
    parseRecord r
        -- We assume that each record contains 6 values.
        | length r == 6 =
            EndpointConfig
            <$> r .! 0
            <*> r .! 1
            <*> r .! 2
            <*> r .! 3
            <*> r .! 4
            <*> r .! 5
        | otherwise = mzero

-- | Extracts configuration for a single endpoint's benchmark,
-- if it is presented in the complete configuration.
extractEndpointConfigFor
    :: BenchEndpoint
    -> CompleteConfig
    -> Maybe EndpointConfig
extractEndpointConfigFor bench CompleteConfig {..} = find (byName bench) endpointsConfig
  where
    byName benchEp config = benchName config == show benchEp
