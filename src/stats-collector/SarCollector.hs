{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

-- | Fetching and accumulating nodes info using sar
module SarCollector
    ( MachineConfig (..)
    , StatisticsEntry (..)
    , statsToText
    , getNodesStats
    , getNodeStats
    ) where

import Control.Monad (fail)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Control.Concurrent.Async.Lifted (mapConcurrently)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Data.Hashable                   (Hashable (hashWithSalt))
import qualified Data.HashMap.Strict             as M
import           Data.List                       (dropWhileEnd)
import           Data.Maybe                      (mapMaybe)
import qualified Data.Text                       as T
import           Data.Time.Clock                 (UTCTime (utctDay), getCurrentTime)
import           Data.Time.Clock.POSIX           (utcTimeToPOSIXSeconds)
import           Data.Time.Format                (defaultTimeLocale, parseTimeM)
import           Formatting                      (int, sformat, stext, (%), shown, fixed)
import           Turtle                          (shellStrict)
import           Universum                       hiding (mapConcurrently)

-- | Basic description of remote machine we're fetching stats from
data MachineConfig = MachineConfig
    { mHost        :: Text
    , mUsername    :: Text
    , mPassword    :: Text
    , mSarFilepath :: Text
    } deriving (Show)

-- | Statistics entry
data StatisticsEntry = StatisticsEntry
    { statTimestamp      :: !UTCTime
    , cpuLoadUser        :: !Double
    , cpuLoadSystem      :: !Double
    , memUsed            :: !Double
    , readSectPerSecond  :: !Double
    , writeSectPerSecond :: !Double
    , netRxKbPerSecond   :: !Double
    , netTxKbPerSecond   :: !Double
    } deriving (Show)

statsToText :: [StatisticsEntry] -> Text
statsToText entries =
    header <> "\n" <>
    mconcat
        (map (\StatisticsEntry{..} ->
           sformat formatter
                   statTimestamp
                   cpuLoadUser
                   cpuLoadSystem
                   memUsed
                   readSectPerSecond
                   writeSectPerSecond
                   netRxKbPerSecond
                   netTxKbPerSecond)
        entries)
  where
    fixd = fixed 2
    formatter =
        shown%","%fixd%","%fixd%","%fixd%","%fixd%","%fixd%","%fixd%","%fixd%"\n"
    header = "UTCTime,cpuUser,cpuSystem,mem,diskReadSect/s,diskWriteSect/s,netRcvKbps,netSendKbps"

-- mda
instance Hashable UTCTime where
    hashWithSalt s t =
        hashWithSalt s $ (show (utcTimeToPOSIXSeconds t) :: Text)

parseDate :: (MonadIO m) => Text -> m UTCTime
parseDate t = liftIO $ do
    day <- utctDay <$> getCurrentTime
    parsed <- parseTimeM False defaultTimeLocale "%H:%M:%S" $ T.unpack t
    pure $ parsed { utctDay = day }

readFail :: Read c => Text -> Text -> c
readFail msg = fromMaybe (panic msg) . readMaybe . T.unpack

parseCpu :: Text -> (Text, (Double, Double))
parseCpu (words -> (d:_:cpuUser:_:cpuSystem:_)) =
    ( d,
    ( readFail "Can't parse cpuUser" cpuUser
    , readFail "Can't parse cpuSystem" cpuSystem))
parseCpu _ = panic "parseCpu"

parseMem :: Text -> (Text, Double)
parseMem (words -> (d:_:_:memPercent:_)) =
    (d, readFail "Can't parse memPercent" memPercent)
parseMem _ = panic "parseMem"

parseDisk :: Text -> (Text, (Double, Double))
parseDisk (words -> (d:_:_:rd:wt:_)) =
    ( d,
    ( readFail "Can't parse rd_sec/s" rd
    , readFail "Can't parse wt_sec/s" wt))
parseDisk _ = panic "parseDisk"

parseNet :: Text -> (Text, (Double, Double))
parseNet (words -> (d:_:_:_:recv:transm:_)) =
    ( d,
    ( readFail "Can't parse rxkB/s" recv
    , readFail "Can't parse txkB/s" transm))
parseNet _ = panic "parseNet"

-- | Concurrently get statistics
getNodesStats
    :: (MonadIO m, MonadBaseControl IO m)
    => [MachineConfig] -> m [Maybe [StatisticsEntry]]
getNodesStats = mapConcurrently (runMaybeT . getNodeStats)

-- | Queries the node to get stats from it. Number of requests can be
-- one in fact, but i didn't figure out how to do that in ~5m. Lazy
-- evaluation,
getNodeStats
    :: (MonadIO m, MonadBaseControl IO m)
    => MachineConfig -> MaybeT m [StatisticsEntry]
getNodeStats MachineConfig{..} = do
    putText "Querying stats..."
    [cpuInfo0, memInfo0, diskInfo0, netInfo0] <-
        mapConcurrently fetchStats ["", "-r", "-d", "-n DEV"]
    putText "Done querying"
    cpuInfo <- M.fromList <$> fixTime (map parseCpu cpuInfo0)
    memInfo <- M.fromList <$> fixTime (map parseMem memInfo0)
    diskInfo <-
        M.fromList <$> fixTime
        (map parseDisk $
         filter (\x -> let (_:dev:_) = words x in "0" `T.isSuffixOf` dev) $
         filter (/= "") diskInfo0)
    netInfo <-
        M.fromList <$> fixTime
        (map parseNet $
         filter (\x -> let (_:iface:_) = words x in iface /= "lo") $
         filter ((> 2) . length . words) netInfo0)
    pure $ (flip mapMaybe $ sort $ M.keys cpuInfo) $ \t -> do
        let statTimestamp = t
        (cpuLoadUser,cpuLoadSystem) <- t `M.lookup` cpuInfo
        memUsed <- t `M.lookup` memInfo
        (readSectPerSecond, writeSectPerSecond) <- t `M.lookup` diskInfo
        (netRxKbPerSecond, netTxKbPerSecond) <- t `M.lookup` netInfo
        pure $ StatisticsEntry{..}
  where
    maxItems = 3600
    fixTime = mapM (\(t,a) -> (,a) <$> parseDate t)
    -- heuristically dropping some lines in the beginning (may be ssh trash..? :))
    -- and average last lines
    fetchStats :: MonadIO m => Text -> MaybeT m [Text]
    fetchStats sarFlag =
        take maxItems .
        dropWhileEnd ("Average:" `T.isPrefixOf`) .
        drop 3 . T.lines . T.strip <$> shellResult
      where
        shellResult = do
            (status, res) <- shellStrict (sshCommand sarFlag) (return "")
            if status == ExitSuccess
                then return res
                else fail "Shell error"

    sshCommandFormat =
        "sshpass -p '"%stext%"' "%
        "ssh -o StrictHostKeyChecking=no "%
        "-o PreferredAuthentications=password "%
        "-o PubkeyAuthentication=no "%
        stext%"@"%stext%" -- \"LC_TIME=en_UK.utf8 sar "%stext%" -f "%stext%" | tail -n\""%int
    sshCommand sarFlag =
        sformat sshCommandFormat mPassword
                mUsername mHost sarFlag mSarFilepath (maxItems+20)
