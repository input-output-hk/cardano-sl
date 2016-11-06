{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Generating plots from statistics

module Plotting (perEntryPlots) where

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
import           System.FilePath                           ((</>))
import           Universum                                 hiding (mapConcurrently)

import           SarCollector                              (StatisticsEntry (..))


-- | Given the directory, puts 4 graphs into it -- per statistics
perEntryPlots
    :: (MonadIO m, MonadBaseControl IO m)
    => FilePath -> UTCTime -> [(UTCTime, Double)] -> [StatisticsEntry] -> m ()
perEntryPlots filepath startTime tpsStats stats = do
    putText "Plotting..."
    void $ mapConcurrently (\(c,n) -> liftIO $ toFile def (filepath </> n) c) $
        [ (chartCpu, "cpuStats.svg")
        , (chartMem, "memStats.svg")
        , (chartDisk, "diskStats.svg")
        , (chartNet, "netStats.svg")
        , (chartTps, "tpsStats.svg")
        ]
  where
    smooth xs =
        map (\i -> let m2 = snd $ xs !! (max 0 $ i-2)
                       m1 = snd $ xs !! (max 0 $ i-1)
                       (t,m0) = xs !! max 0 i
                   in (t,(m0+m2+m1)/3))
        [0..length xs-1]
    fromStamp :: (StatisticsEntry -> b) -> StatisticsEntry -> (Integer, b)
    fromStamp foo x = (round $ statTimestamp x `diffUTCTime` startTime, foo x)
    chartCpu = do
        layout_title .= "CPU usage"
        layout_y_axis . laxis_generate .= scaledAxis def (0,100)
        let cpuLineUser = smooth $ map (fromStamp cpuLoadUser) stats
        let cpuLineSystem = smooth $ map (fromStamp cpuLoadSystem) stats
        setColors [opaque red]
        plot (line "Cpu user (%)" [cpuLineUser])
        setColors [opaque blue]
        plot (line "Cpu system (%)" [cpuLineSystem])
    chartMem = do
        layout_title .= "Memory usage"
        layout_y_axis . laxis_generate .= scaledAxis def (0,100)
        setColors [opaque green]
        let memLine = smooth $ map (fromStamp memUsed) stats
        plot (line "Memory used (%)" [memLine])
    chartDisk = do
        layout_title .= "Disk usage"
        let diskRead = smooth $ map (fromStamp readSectPerSecond) stats
        let diskWrite = smooth $ map (fromStamp writeSectPerSecond) stats
        setColors [opaque red]
        plot (line "Disk read (sect/s)" [diskRead])
        setColors [opaque blue]
        plot (line "Disk write (sect/s)" [diskWrite])
    chartNet = do
        layout_title .= "Networking"
        let netRecv = smooth $ map (fromStamp netRxKbPerSecond) stats
        let netSend = smooth $ map (fromStamp netTxKbPerSecond) stats
        setColors [opaque red]
        plot (line "Network receive (Kb/s)" [netRecv])
        setColors [opaque blue]
        plot (line "Network send (Kb/s)" [netSend])
    chartTps = do
        layout_title .= "TPS"
        setColors [opaque red]
        plot $ line "Network receive (Kb/s)" $
            [map (first (\x -> round $ x `diffUTCTime` startTime)) tpsStats]
