module Statistics.CSV
    ( txCntInChainMemPoolToCSV
    ) where

import System.IO (hPutStrLn)

import Pos.Util.JsonLog (JLMemPool (..))
import Types
import Universum

txCntInChainMemPoolToCSV :: FilePath 
                         -> [(NodeIndex, Timestamp, Int)] 
                         -> [(NodeIndex, Timestamp, JLMemPool)] 
                         -> IO ()
txCntInChainMemPoolToCSV f txCnt mp = withFile f WriteMode $ \h -> do
    hPutStrLn h "time,txCount,txType,node"
    for_ txCnt $ \(n, ts, cnt) -> csvLine h "written" n ts (fromIntegral cnt)
    for_ mp $ \(n, ts, p@JLMemPool{..}) -> do
        csvLine h (toTxType "Wait" p) n ts jlmWait
        csvLine h (toTxType "Modify" p) n ts jlmModify
        csvLine h (toTxType "SizeAfter" p) n ts (fromIntegral jlmSizeAfter)
  where
    csvLine :: Handle -> String -> NodeIndex -> Timestamp -> Integer -> IO ()
    csvLine h txType node time txCount = hPutStrLn h $ show time ++ "," ++ show txCount ++ "," ++ txType ++ "," ++ show node
    
    toTxType :: String -> JLMemPool -> String
    toTxType s JLMemPool{..} = "mp_" ++ show jlmReason ++ "_" ++ s
