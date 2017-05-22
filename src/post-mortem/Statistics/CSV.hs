module Statistics.CSV
    ( txCntInChainMemPoolToCSV
    , focusToCSV
    ) where

import           System.IO        (hPutStrLn)
import qualified Data.Text        as T

import           Statistics.Focus (Focus (..))
import           Pos.Util.JsonLog (JLMemPool (..))
import           Types
import           Universum

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

focusToCSV :: FilePath -> [(Timestamp, NodeIndex, Focus)] -> IO ()
focusToCSV f xs = withFile f WriteMode $ \h -> do
    hPutStrLn h "time,delta_first_seconds,delta_step_seconds,node,type,block/error"
    case xs of
        []                -> return ()
        ((ts0, _, _) : _) -> foldM_ (step h ts0) ts0 xs
  where
    step :: Handle -> Timestamp -> Timestamp -> (Timestamp, NodeIndex, Focus) -> IO Timestamp
    step h ts0 ts (ts', n, y) = do
        let dt0 = fromIntegral (ts' - ts0) / 1000000 :: Double
            dt  = fromIntegral (ts' - ts ) / 1000000 :: Double
        case y of
            Received me         -> csvLine h ts' dt0 dt n "received" $ maybe "" show me
            InCreatedBlock hash -> csvLine h ts' dt0 dt n "created" $ T.take 6 hash
            InAdoptedBlock hash -> csvLine h ts' dt0 dt n "adopted" $ T.take 6 hash
        return ts'

    csvLine :: Handle -> Timestamp -> Double -> Double -> NodeIndex -> String -> BlockHash -> IO ()
    csvLine h ts dt0 dt node t he =
        hPutStrLn h $ show ts ++ "," ++ show dt0 ++ "," ++ show dt ++ "," ++ show node ++ "," ++ t ++ "," ++ toString he 
