module Statistics.Report
    ( reportTxFate
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           System.IO       (hPutStrLn)

import Statistics.Block (TxFate (..))
import Types
import Universum

reportTxFate :: FilePath -> Map TxHash TxFate -> IO ()
reportTxFate f m = withFile f WriteMode $ \h -> do
    let total = M.size m
        m'    = M.filter isFork m
        fork  = M.size m'
        m''   = M.filter isChain m
        chain = M.size m''
        lost  = total - fork - chain
    hPutStrLn h $ "total transactions: " ++ show total
    hPutStrLn h $ "in blockchain: " ++ show chain
    hPutStrLn h $ "in fork: " ++ show fork
    hPutStrLn h $ "lost: " ++ show lost
    hPutStrLn h ""
    hPutStrLn h "transactions in fork:"
    for_ (M.toList m') $ \(tx, InFork s) ->
        hPutStrLn h $ show tx ++ ": " ++ show (unwords $ map snd $ S.toList s)
        

isFork :: TxFate -> Bool
isFork (InFork _) = True
isFork _          = False

isChain :: TxFate -> Bool
isChain (InBlockChain{}) = True
isChain _                    = False
