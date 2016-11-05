{-# LANGUAGE ViewPatterns #-}

-- | Fetching and accumulating nodes info using sar

module SarCollector
    ( MachineConfig (..)
    , StatisticsEntry (..)
    , getNodeStats
    ) where

import           Data.Hashable         (Hashable (hashWithSalt))
import qualified Data.HashMap.Strict   as M
import           Data.List             (dropWhileEnd)
import qualified Data.Text             as T
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Time.Format      (defaultTimeLocale, parseTimeM)
import           Formatting            (sformat, stext, (%))
import           Turtle                (shellStrict)
import           Universum

-- | Basic description of remote machine we're fetching stats from
data MachineConfig = MachineConfig
    { mHost        :: Text
    , mUsername    :: Text
    , mPassword    :: Text
    , mSarFilepath :: Text
    } deriving (Show)

-- | Statistic entry
data StatisticsEntry = StatisticsEntry
    { statTimestamp      :: UTCTime
    , cpuLoadUser        :: Double
    , cpuLoadSystem      :: Double
    , memUsed            :: Double
    , readSectPerSecond  :: Double
    , writeSectPerSecond :: Double
    , netRxKbPerSecond   :: Double
    , netTxKbPerSecond   :: Double
    } deriving (Show)

-- mda
instance Hashable UTCTime where
    hashWithSalt s t =
        hashWithSalt s $ (show (utcTimeToPOSIXSeconds t) :: Text)

parseDate :: Text -> UTCTime
parseDate =
    runIdentity . parseTimeM False defaultTimeLocale "%H:%M:%S" . T.unpack

readFail :: Read c => Text -> Text -> c
readFail msg = fromMaybe (panic msg) . readMaybe . T.unpack

parseCpu :: Text -> (UTCTime, (Double, Double))
parseCpu (words -> (d:_:cpuUser:_:cpuSystem:_)) =
    ( parseDate d,
    ( readFail "Can't parse cpuUser" cpuUser
    , readFail "Can't parse cpuSystem" cpuSystem))
parseCpu _ = panic "parseCpu"

parseMem :: Text -> (UTCTime, Double)
parseMem (words -> (d:_:_:memPercent:_)) =
    (parseDate d, readFail "Can't parse memPercent" memPercent)
parseMem _ = panic "parseMem"

parseDisk :: Text -> (UTCTime, (Double, Double))
parseDisk (words -> (d:_:_:rd:wt:_)) =
    ( parseDate d,
    ( readFail "Can't parse rd_sec/s" rd
    , readFail "Can't parse wt_sec/s" wt))
parseDisk _ = panic "parseDisk"

parseNet :: Text -> (UTCTime, (Double, Double))
parseNet (words -> (d:_:_:_:recv:transm:_)) =
    ( parseDate d,
    ( readFail "Can't parse rxkB/s" recv
    , readFail "Can't parse txkB/s" transm))
parseNet _ = panic "parseNet"

getNodeStats :: (MonadIO m) => MachineConfig -> m [StatisticsEntry]
getNodeStats MachineConfig{..} = do
    cpuInfo <- M.fromList . map parseCpu <$> fetchStats ""
    memInfo <- M.fromList . map parseMem <$> fetchStats "-r"
    diskInfo <-
        M.fromList . map parseDisk .
        filter (\x -> let (_:dev:_) = words x in "0" `T.isSuffixOf` dev) <$>
        fetchStats "-d"
    netInfo <-
        M.fromList . map parseNet .
        filter (\x -> let (_:iface:_) = words x in iface /= "lo") <$>
        fetchStats "-n DEV"
    pure $ (flip map $ M.keys cpuInfo) $ \t ->
        StatisticsEntry
          { statTimestamp = t
          , cpuLoadUser = fst $ cpuInfo M.! t
          , cpuLoadSystem = snd $ cpuInfo M.! t
          , memUsed = memInfo M.! t
          , readSectPerSecond = fst $ diskInfo M.! t
          , writeSectPerSecond = snd $ diskInfo M.! t
          , netRxKbPerSecond =  fst $ netInfo M.! t
          , netTxKbPerSecond =  snd $ netInfo M.! t
          }
  where
    -- heuristically dropping some lines in the beginning (may be ssh trash..? :))
    -- and average last lines
    fetchStats sarFlag =
        take (23 * 60 * 60) .
        dropWhileEnd ("Average:" `T.isPrefixOf`) . drop 3 . T.lines . snd <$>
        shellStrict (sshCommand sarFlag) (return "")
    sshCommandFormat =
        "sshpass -p '"%stext%"' "%
        "ssh -o StrictHostKeyChecking=no "%
        "-o PreferredAuthentications=password "%
        "-o PubkeyAuthentication=no "%
        stext%"@"%stext%" -- LC_TIME=en_UK.utf8 sar "%stext%" -f "%stext
    sshCommand sarFlag =
        sformat sshCommandFormat mPassword
                mUsername mHost sarFlag mSarFilepath
