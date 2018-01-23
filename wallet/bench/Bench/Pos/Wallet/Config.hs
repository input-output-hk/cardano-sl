-- | Functions for working with configuration file.

module Bench.Pos.Wallet.Config
    ( getBenchConfig
    , extractConfigFor
    ) where

import           Universum

import           Data.Csv               (FromRecord (..), HasHeader (..),
                                         (.!), decode)

import           Data.Monoid            ((<>))
import qualified Data.ByteString.Lazy   as Lazy
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import           Control.Exception      (SomeException)

import           Bench.Pos.Wallet.Types (AdditionalBenchConfig (..),
                                         BenchEndpoint (..))

-- | Read benchmark configuration from the local .csv-file. The format is:
--
-- BenchName,BenchDuration,MinDelayBetweenCalls,MaxDelayBetweenCalls,PathToReportFile
-- GetHistoryBench,5.0,0.5,2.5,/tmp/GetHistoryBenchReport.csv
-- GetWalletsBench,5.5,0.2,2.1,/tmp/GetWalletsBenchReport.csv
--
getBenchConfig :: IO [AdditionalBenchConfig]
getBenchConfig = do
    content <- Lazy.readFile pathToConfig `catch` anyProblems
    case extractConfig content of
        Left problem -> reportAboutInvalidConfig problem
        Right (V.toList -> configs) -> do
            mapM_ checkDelayRange configs
            return configs
  where
    extractConfig rawContent =
        decode HasHeader rawContent :: Either String (Vector AdditionalBenchConfig)

    pathToConfig :: String
    pathToConfig = "wallet/bench/config/Endpoints.csv" 

    anyProblems :: SomeException -> IO a
    anyProblems whatHappened = error . toText $
        "Unable to open configuration " <> pathToConfig <> ": " <> show whatHappened

    reportAboutInvalidConfig :: String -> IO a
    reportAboutInvalidConfig problem = error . toText $
        "Invalid configuration " <> pathToConfig <> ": " <> show problem

    checkDelayRange :: AdditionalBenchConfig -> IO ()
    checkDelayRange (AdditionalBenchConfig name _ from to _) =
        when (from < 0.0 || to < 0.0 || from > to) $
            error . toText $ "Invalid delay range for bench '" <> name <> "'"

-- | This instance is used for parsing @AdditionalBenchConfig@
-- from one record (line) in .csv-config.
instance FromRecord AdditionalBenchConfig where
    parseRecord r
        -- We assume that each record contains 5 values.
        | length r == 5 =
            AdditionalBenchConfig
            <$> r .! 0
            <*> r .! 1
            <*> r .! 2
            <*> r .! 3
            <*> r .! 4
        | otherwise = mzero

-- | Extracts configuration for a single endpoint's benchmark,
-- if it is presented in the complete configuration.
extractConfigFor
    :: BenchEndpoint
    -> [AdditionalBenchConfig]
    -> Maybe AdditionalBenchConfig
extractConfigFor bench configs = find (byName bench) configs
  where
    byName benchEp config = benchName config == show benchEp
