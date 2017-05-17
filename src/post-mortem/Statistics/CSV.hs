module Statistics.CSV
    ( txCntInChainMemPoolToCSV
    ) where

import System.IO (hPutStrLn)
import Types
import Universum

txCntInChainMemPoolToCSV :: FilePath -> [(NodeIndex, Timestamp, Int)] -> [(NodeIndex, Timestamp, Int)] -> IO ()
txCntInChainMemPoolToCSV f txCnt mp = withFile f WriteMode $ \h -> do
    hPutStrLn h "time,txCount,txType,node"
    for_ txCnt $ \(n, ts, cnt) -> csvLine h "written" n ts cnt
    for_ mp $ \(n, ts, cnt) -> csvLine h "mempool" n ts cnt
  where
    csvLine :: Handle -> String -> NodeIndex -> Timestamp -> Int -> IO ()
    csvLine h txType node time txCount = hPutStrLn h $ show time ++ "," ++ show txCount ++ "," ++ txType ++ "," ++ show node
