{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

-- | Executable for collecting stats data from nodes

import           Control.Concurrent       (forkIO)
import           Control.Concurrent.Chan  (Chan)
import qualified Control.Concurrent.Chan  as C
import           Control.Concurrent.MVar  (newEmptyMVar, putMVar, tryTakeMVar)
import           Control.TimeWarp.Logging (Severity (..), WithNamedLogger, logInfo)
import           Control.TimeWarp.Timed   (for, fork_, ms, wait)
import           Data.Aeson.TH            (deriveJSON)
import           Data.Aeson.Types         (FromJSON)
import           Data.Default             (def)
import           Data.Maybe               (catMaybes)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Data.Time.Clock          (UTCTime, addUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
import qualified Data.Yaml                as Y
import           Formatting               (build, int, sformat, (%))
import           Serokell.Aeson.Options   (defaultOptions)
import           System.Directory         (createDirectoryIfMissing)
import           System.FilePath          ((</>))
import           Text.Parsec              (parse)
import           Universum                hiding ((<>))

import           Formatting               (fixed, sformat, shown, (%))
import           Pos.CLI                  (addrParser)
import           Pos.Communication        (RequestStat (..), ResponseStat (..))
import           Pos.DHT                  (DHTNodeType (..), ListenerDHT (..), sendToNode)
import           Pos.Launcher             (BaseParams (..), LoggingParams (..),
                                           bracketDHTInstance, runServiceMode)
import           Pos.Statistics           (CountStat (..), StatBlockCreated (..),
                                           StatLabel (..), StatProcessTx (..))
import           Pos.Types                (Timestamp)
import           Pos.Util                 (eitherPanic)

import           Plotting                 (perEntryPlots, plotTPS)
import qualified SarCollector             as SAR
import qualified StatsOptions             as O

------------------------------------------------
-- YAML config
-----------------------------------------------

readRemoteConfig :: FromJSON config => FilePath -> IO config
readRemoteConfig fp =
    eitherPanic "[FATAL] Failed to parse config: " <$>
    Y.decodeFileEither fp

data CollectorConfig = CollectorConfig
    { ccNodes :: ![(Text,Int)]
    } deriving (Show)

deriveJSON defaultOptions ''CollectorConfig


collectorListener
    :: (MonadIO m, WithNamedLogger m)
    => Chan (ResponseStat StatProcessTx (Timestamp, CountStat))
    -> ResponseStat StatProcessTx (Timestamp, CountStat)
    -> m ()
collectorListener channel res@(ResponseStat _ l _) = do
    logInfo $ sformat ("Received stats response: "%build) l
    liftIO $ writeChan channel res

tpsToCsv :: [(UTCTime, Double)] -> Text
tpsToCsv entries =
    "time,tps\n" <>
    mconcat (map formatter entries)
  where
    formatter (time, tps) = sformat (shown%","%fixed 2%"\n") time tps
------------------------------------------------
-- Main
------------------------------------------------

main :: IO ()
main = do
    opts@O.StatOpts{..} <- O.readOptions
    CollectorConfig{..} <- readRemoteConfig soConfigPath
    startTime <-
        if soLoop
        then getCurrentTime
        else ((fromInteger $ - soInterval) `addUTCTime`) <$> getCurrentTime
    print startTime
    putText $ "Launched with options: " <> show opts
    putText $ "Current time is: " <> show startTime

    createDirectoryIfMissing True soOutputDir
    TIO.writeFile (soOutputDir </> "how") $ "StartTime: " <> show startTime

    let mConfigs =
            flip map ccNodes $ \(host,_) ->
                SAR.MachineConfig
                host "statReader" "123123123123" "/var/log/saALL"

    let logParams =
            def
            { lpRootLogger = "stats-collector"
            , lpMainSeverity = Debug
            , lpDhtSeverity = Just Info
            }
        params =
            BaseParams
            { bpLogging = logParams
            , bpPort = 8095
            , bpDHTPeers = []
            , bpDHTKeyOrType = Right DHTClient
            , bpDHTExplicitInitial = False
            }
        worker dirPath = do
            -- stats <-
            --     map (filter ((> startTime) . SAR.statTimestamp)) . catMaybes <$>
            --     SAR.getNodesStats mConfigs

            -- void $ flip mapM (stats `zip` [0..]) $ \(stat,i::Int) -> do
            --     let foldername = dirPath </> (soOutputPrefix ++ show i)
            --     -- perEntryPlots foldername startTime stat
            --     createDirectoryIfMissing True foldername
            --     TIO.writeFile (foldername </> "sysstat.csv") $ SAR.statsToText stat

            endTime <- getCurrentTime
            -- let sarTimestamps = map SAR.statTimestamp $
            --                     fromMaybe [] $ head stats
            --     endTime :: UTCTime
            --     endTime = if null sarTimestamps
            --               then curTime
            --               else maximum sarTimestamps
            let addrs = eitherPanic "Invalid address: " $
                    mapM (\(h,p) -> parse addrParser "" $ toString (h <> ":" <> show p))
                         ccNodes
                enumAddrs = zip [0..] addrs

            ch1 <- C.newChan
            let listeners = [ListenerDHT $ collectorListener ch1]

            bracketDHTInstance params $ \inst -> do
                runServiceMode inst params listeners $ do
                    forM_ enumAddrs $ \(idx, addr) -> do
                        logInfo $ sformat ("Requested stats for node #"%int) idx
                        sendToNode addr (RequestStat idx StatProcessTx)
                        -- sendToNode addr (RequestStat idx StatBlockCreated)

                    forM_ [0 .. (length addrs)-1] $ \_ -> do
                        timeoutLock <- liftIO $ newEmptyMVar
                        fork_ $ do
                            wait $ for $ ms 3000
                            mlock <- liftIO $ tryTakeMVar timeoutLock
                            case mlock of
                                Nothing -> liftIO $ writeChan ch1 (ResponseStat 0 StatProcessTx Nothing)
                                Just _ -> return ()

                        (ResponseStat id _ mres) <- liftIO $ readChan ch1
                        liftIO $ putMVar timeoutLock ()

                        case mres of
                            Nothing -> logInfo $ sformat ("No stats for node #"%int) id
                            Just res -> do
                                logInfo $ sformat ("Got stats for node #"%int%"!") id
                                let mapper = bimap (posixSecondsToUTCTime . fromIntegral .
                                                    (`div` 1000000))
                                                   fromIntegral
                                    timeSeries = map mapper res
                                    foldername = dirPath </> (soOutputPrefix ++ show id)
                                    curSeries = filter ((> startTime) . fst) $
                                                filter ((< endTime) . fst) $
                                                timeSeries
                                liftIO $ do
                                    createDirectoryIfMissing True foldername
                                    TIO.writeFile (foldername </> "tps.csv") $ tpsToCsv curSeries
                                -- plotTPS foldername startTime
                                logInfo $ sformat ("TPS stats for node "%int%" are done") id

    when soLoop $ forM_ [1..] $ \(i ::Int) -> do
        threadDelay $ (fromIntegral soInterval) * 1000 * 1000
        forkIO $ worker $ soOutputDir </> ("run" <> show i)
    worker soOutputDir
