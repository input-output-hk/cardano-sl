{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

-- | Executable for collecting stats data from nodes

import           Control.Concurrent.Chan  (Chan)
import qualified Control.Concurrent.Chan  as C
import           Control.TimeWarp.Logging (Severity (..), WithNamedLogger, logInfo)
import           Data.Aeson.TH            (deriveJSON)
import           Data.Aeson.Types         (FromJSON)
import           Data.Default             (def)
import           Data.Monoid              ((<>))
import           Data.Time.Clock          (addUTCTime, getCurrentTime)
import qualified Data.Yaml                as Y
import           Formatting               (build, int, sformat, (%))
import           Serokell.Aeson.Options   (defaultOptions)
import           Text.Parsec              (parse)
import           Universum                hiding ((<>))

import           Pos.CLI                  (addrParser)
import           Pos.Communication        (RequestStat (..), ResponseStat (..))
import           Pos.DHT                  (DHTNodeType (..), ListenerDHT (..),
                                           dhtNodeType, sendToNode)
import           Pos.Launcher             (BaseParams (..), LoggingParams (..),
                                           bracketDHTInstance, runServiceMode)
import           Pos.Statistics           (StatBlockVerifying (..), StatLabel (..),
                                           StatProcessTx (..))
import           Pos.Types                (Timestamp)
import           Pos.Util                 (eitherPanic)

import           Plotting                 (perEntryPlots)
import qualified SarCollector             as SAR
import qualified StatsOptions             as O

------------------------------------------------
-- YAML config
------------------------------------------------

readRemoteConfig :: FromJSON config => FilePath -> IO config
readRemoteConfig fp =
    eitherPanic "[FATAL] Failed to parse config: " <$>
    Y.decodeFileEither fp

data CollectorConfig = CollectorConfig
    { ccNodes :: ![(Text,Int)]
    } deriving (Show)

deriveJSON defaultOptions ''CollectorConfig


collectorListener
    :: (StatLabel l, MonadIO m, WithNamedLogger m)
    => Chan (ResponseStat l (Timestamp, EntryType l))
    -> ResponseStat l (Timestamp, EntryType l)
    -> m ()
collectorListener channel res@(ResponseStat _ l _) = do
    logInfo $ sformat ("Received stats response: "%build) l
    liftIO $ writeChan channel res

------------------------------------------------
-- Main
------------------------------------------------

main :: IO ()
main = do
    opts@O.StatOpts{..} <- O.readOptions
    CollectorConfig{..} <- readRemoteConfig soConfigPath
    startTime <- ((fromInteger $ - 60) `addUTCTime`) <$> getCurrentTime
    threadDelay 10
    print startTime
    putText $ "Launched with options: " <> show opts

    let mConfigs =
            flip map ccNodes $ \(host,_) ->
                SAR.MachineConfig
                host "statReader" "123123123123" "/var/log/saALL"
    stats <- map (filter ((> startTime) . SAR.statTimestamp)) <$> SAR.getNodesStats mConfigs
    void $ mapConcurrently
        (\(stat,i) -> perEntryPlots ("./statsNode"<>show i) startTime stat)
        (stats `zip` [0..])

    forM_ sarStats $ \nodeStat ->
        forM_ (take 10 nodeStat) print

    let addrs = eitherPanic "Invalid address: " $
            mapM (\(h,p) -> parse addrParser "" $ toString (h <> ":" <> show p))
                 ccNodes
        enumAddrs = zip [0..] addrs
        logParams =
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

    ch1 <- C.newChan
    ch2 <- C.newChan
    let listeners = [ ListenerDHT $ collectorListener @StatProcessTx ch1
                    , ListenerDHT $ collectorListener @StatBlockVerifying ch2
                    ]

    bracketDHTInstance params $ \inst -> do
        runServiceMode inst params listeners $ do
            forM_ enumAddrs $ \(idx, addr) -> do
                logInfo $ sformat ("Requested stats for node #"%int) idx
                sendToNode addr (RequestStat idx StatProcessTx)
                -- sendToNode addr (RequestStat idx StatBlockVerifying)

            forM_ [0 .. length addrs] $ \_ -> liftIO $ do
                (ResponseStat id label res) <- readChan ch1
                print id
                print label
                print res
