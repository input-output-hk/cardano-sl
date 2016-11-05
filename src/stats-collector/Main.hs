{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
-- | Executable for collecting stats data from nodes

import           Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as C
import           Data.Aeson.TH           (deriveJSON)
import           Data.Aeson.Types        (FromJSON)
import           Data.Default            (def)
import           Data.Monoid             ((<>))
import qualified Data.Yaml               as Y
import           Serokell.Aeson.Options  (defaultOptions)
import           Text.Parsec             (parse)
import           Universum               hiding ((<>))

import           Pos.CLI                 (addrParser)
import           Pos.Communication       (RequestStat (..), ResponseStat (..))
import           Pos.DHT                 (DHTNodeType (..), ListenerDHT (..), dhtNodeType,
                                          sendToNode)
import           Pos.Launcher            (BaseParams (..), LoggingParams (..),
                                          NodeParams (..), bracketDHTInstance,
                                          runServiceMode)
import           Pos.Statistics          (StatBlockVerifying (..), StatLabel (..),
                                          StatProcessTx (..))
import           Pos.Types               (Timestamp)
import           Pos.Util                (eitherPanic)

import qualified SarCollector as SAR
import qualified StatsOptions            as O

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
    :: (StatLabel l, MonadIO m)
    => Chan (ResponseStat l (Timestamp, EntryType l))
    -> ResponseStat l (Timestamp, EntryType l)
    -> m ()
collectorListener channel res = liftIO $ writeChan channel res

------------------------------------------------
-- Main
------------------------------------------------

main :: IO ()
main = do
    opts@O.StatOpts{..} <- O.readOptions
    CollectorConfig {..} <- readRemoteConfig soConfigPath
    putText $ "launched with options: " <> show opts

    let (m0Host,_) = fromMaybe (panic "ccNodes list is empty") $ head ccNodes
        mConfig =
            SAR.MachineConfig m0Host "statReader" "123123123123" "/var/log/saALL"
    print =<< (take 10 <$> SAR.getNodeStats mConfig)

    let addrs = eitherPanic "Invalid address: " $
            mapM (\(h,p) -> parse addrParser "" $ toString (h <> show p))
                 ccNodes
        enumAddrs = zip [0..] addrs
        params =
            BaseParams
            { bpLogging = def
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
                sendToNode addr (RequestStat idx StatProcessTx)
                sendToNode addr (RequestStat idx StatBlockVerifying)

            forM_ [0 .. 2 * length addrs] $ \_ -> liftIO $ do
                (ResponseStat id label res) <- readChan ch1
                print id
                print label
                print res
