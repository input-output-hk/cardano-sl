{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Generating plots from statistics

module Plotting
    ( perEntryPlots
    , plotTPS
    ) where

import           Control.Concurrent.Async.Lifted           (mapConcurrently)
import           Control.Monad.Trans.Control               (MonadBaseControl)
import           Data.Bifunctor                            (first)
import           Data.List                                 ((!!))
import           Data.Time.Clock                           (UTCTime, diffUTCTime)
import           Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import           Graphics.Rendering.Chart.Easy             (blue, def, green,
                                                            laxis_generate, layout_title,
                                                            layout_y_axis, line, opaque,
                                                            plot, red, scaledAxis,
                                                            setColors, (.=))
import           System.Directory                          (createDirectoryIfMissing)
import           System.FilePath                           ((</>))
import           Universum                                 hiding (mapConcurrently)

import           SarCollector                              (StatisticsEntry (..))

smooth :: [(a, Double)] -> [(a,Double)]
smooth xs = map (\i -> let m2 = snd $ xs !! (max 0 $ i-2)
                           m1 = snd $ xs !! (max 0 $ i-1)
                           (t,m0) = xs !! max 0 i
                       in (t,(m0+m2+m1)/3))
                [0..length xs-1]


-- | Given the directory, puts 4 graphs into it -- per statistics
perEntryPlots
    :: (MonadIO m, MonadBaseControl IO m)
    => FilePath -> UTCTime -> [StatisticsEntry] -> m ()
perEntryPlots filepath startTime stats = do
    putStrLn $ "Plotting to " <> filepath
    liftIO $ createDirectoryIfMissing True filepath
    void $ mapConcurrently (\(c,n) -> liftIO $ toFile def (filepath </> n) c) $
        [ (chartCpu, "cpuStats.svg")
        , (chartMem, "memStats.svg")
        , (chartDisk, "diskStats.svg")
        , (chartNet, "netStats.svg")
        ]
  where
    fromStamp :: (StatisticsEntry -> b) -> StatisticsEntry -> (Integer, b)
    fromStamp foo x = (round $ statTimestamp x `diffUTCTime` startTime, foo x)
    chartCpu = do
        layout_title .= "CPU usage"
        layout_y_axis . laxis_generate .= scaledAxis def (0,100)
        let cpuLineUser = map (fromStamp cpuLoadUser) stats
        let cpuLineSystem = map (fromStamp cpuLoadSystem) stats
        setColors [opaque red]
        plot (line "Cpu user (%)" [cpuLineUser])
        setColors [opaque blue]
        plot (line "Cpu system (%)" [cpuLineSystem])
    chartMem = do
        layout_title .= "Memory usage"
        layout_y_axis . laxis_generate .= scaledAxis def (0,100)
        setColors [opaque green]
        let memLine = map (fromStamp memUsed) stats
        plot (line "Memory used (%)" [memLine])
    chartDisk = do
        layout_title .= "Disk usage"
        let diskRead =  map (fromStamp readSectPerSecond) stats
        let diskWrite = map (fromStamp writeSectPerSecond) stats
        setColors [opaque red]
        plot (line "Disk read (sect/s)" [diskRead])
        setColors [opaque blue]
        plot (line "Disk write (sect/s)" [diskWrite])
    chartNet = do
        layout_title .= "Networking"
        let netRecv = map (fromStamp netRxKbPerSecond) stats
        let netSend = map (fromStamp netTxKbPerSecond) stats
        setColors [opaque red]
        plot (line "Network receive (Kb/s)" [netRecv])
        setColors [opaque blue]
        plot (line "Network send (Kb/s)" [netSend])

plotTPS
    :: (MonadIO m)
    => FilePath -> UTCTime -> [(UTCTime, Double)] -> m ()
plotTPS filepath startTime tpsStats = do
    putStrLn $ "Plotting tps for " <> filepath
    traceShowM $ take 5 tpsStats
    liftIO $ createDirectoryIfMissing True filepath
    void $ liftIO $ toFile def (filepath </> "dpsStats.svg") chartTps
  where
    chartTps = do
        layout_title .= "TPS"
        setColors [opaque red]
        plot $ line "Transactions per second" $
            [map (first (\x -> round (x `diffUTCTime` startTime) :: Int)) tpsStats]
