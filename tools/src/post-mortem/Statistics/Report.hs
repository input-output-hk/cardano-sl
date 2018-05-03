module Statistics.Report
    ( reportTxFate
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           System.IO (hPutStrLn)

import           Statistics.Block (TxFate (..))
import           Types
import           Universum

reportTxFate :: FilePath -> Map TxHash TxFate -> IO ([TxHash], [TxHash], [TxHash])
reportTxFate f m = withFile f WriteMode $ \h -> do
    let total = M.size m
        m'    = M.filter isFork m
        fork  = M.size m'
        m''   = M.filter isChain m
        chain = M.size m''
        m'''  = M.filter isLost m
        lost  = M.size m'''
        chains = fst <$> M.toList m''
        forks  = fst <$> M.toList m'
        losts  = fst <$> M.toList m'''
    hPutStrLn h $ "total transactions: " ++ show total
    hPutStrLn h $ "in blockchain: " ++ show chain
    hPutStrLn h $ "in fork: " ++ show fork
    hPutStrLn h $ "lost: " ++ show lost
    hPutStrLn h ""
    hPutStrLn h "transactions in blockchain:"
    for_ (M.toList m'') $ \(tx, InBlockChain _ hash _) ->
        hPutStrLn h $ toString tx ++ ": " ++ toString (T.take 6 hash)
    hPutStrLn h ""
    hPutStrLn h "transactions in fork:"
    for_ (M.toList m') $ \(tx, InFork s) ->
        hPutStrLn h $ toString tx ++ ": " ++ toString (unwords $ map (T.take 6 . snd) $ S.toList s)
    hPutStrLn h ""
    hPutStrLn h "lost transactions:"
    for_ (M.toList m''') $ \(tx, InNoBlock) ->
        hPutStrLn h (toString tx)
    return (chains, forks, losts)

isFork :: TxFate -> Bool
isFork (InFork _) = True
isFork _          = False

isLost :: TxFate -> Bool
isLost InNoBlock = True
isLost _         = False

isChain :: TxFate -> Bool
isChain (InBlockChain{}) = True
isChain _                = False
