import qualified Data.GraphViz.Commands.IO as G
import qualified Data.Map.Strict           as M
import           System.IO                 (hPutStrLn, IOMode (WriteMode))

import           Statistics
import           Universum


main :: IO ()
main = do
    [logDir, timesFile, graphFile] <- getArgs
    err $ "logs directory: " ++ show logDir
    err $ "times file: " ++ show timesFile
    err $ "graph file: " ++ show graphFile
    (rc, g) <- runJSONFold logDir $ (,) <$> receivedCreatedF <*> graphF
    let total    = M.size rc
        included = sort $ mapMaybe snd $ M.toList rc
        lost     = total - length included
    err $ "total number of received transactions: " ++ show total
    err $ "included in blockchain: " ++ show (length included)
    err $ "lost transactions: " ++ show lost
    withFile timesFile WriteMode $ \h -> do
        hPutStrLn h "tx,time"
        for_ (M.toList rc) $ \(tx, mts) ->
            hPutStrLn h $ toString tx ++ "," ++ maybe "" show mts
    void $ G.writeDotFile graphFile g
  where
    err :: String -> IO ()
    err = hPutStrLn stderr
