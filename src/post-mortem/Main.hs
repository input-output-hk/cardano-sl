import qualified Data.GraphViz.Commands.IO as G
import qualified Data.Map.Strict           as M
import           System.IO                 (hPutStrLn, IOMode (WriteMode))

import           Options
import           Statistics
import           Universum


main :: IO ()
main = do
    Options{..} <- parseOptions
    err $ "logs directory: " ++ show logDir
    err $ "times csv file: " ++ show timesCSV
    err $ "graph dot file: " ++ show graphDOT
    (rc, g) <- runJSONFold logDir $ (,) <$> receivedCreatedF <*> graphF
    let total    = M.size rc
        included = sort $ mapMaybe snd $ M.toList rc
        lost     = total - length included
    err $ "total number of received transactions: " ++ show total
    err $ "included in blockchain: " ++ show (length included)
    err $ "lost transactions: " ++ show lost
    withFile timesCSV WriteMode $ \h -> do
        hPutStrLn h "tx,time"
        for_ (M.toList rc) $ \(tx, mts) ->
            hPutStrLn h $ toString tx ++ "," ++ maybe "" show mts
    void $ G.writeDotFile graphDOT g
  where
    err :: String -> IO ()
    err = hPutStrLn stderr
