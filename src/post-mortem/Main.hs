import qualified Data.Map.Strict as M
import           Data.List       (last)
import           System.FilePath
import           System.IO       (hPutStrLn)

import           Options
import           Statistics
import           Types
import           Universum

main :: IO ()
main = do
    opts <- parseOptions
    case opts of
        Overview logDirs    -> do
            err "logs directories: "
            for_ logDirs $ \d -> err $ " - " ++ show d
            err ""
            xs <- forM logDirs processLogDir
            chart xs "times.svg"
            err "wrote times chart"
        Focus txHash logDir -> do
            err $ "transaction hash: " ++ show txHash
            err $ "logs directory: " ++ show logDir 
            let focusFile = ("focus_" ++ extractName logDir ++ "_" ++ toString txHash) <.> "csv"
            runJSONFold logDir (focusF txHash) >>= focusToCSV focusFile
            err $ "wrote result to " ++ show focusFile

processLogDir :: FilePath -> IO (String, Map TxHash (Maybe Timestamp))
processLogDir logDir = do
    err $ "processing log directory " ++ show logDir ++ " ..."

    (rc, g, mp, cr, ft) <- runJSONFold logDir $ (,,,,) <$> receivedCreatedF <*> graphF <*> memPoolF <*> txCntInChainF <*> txFateF
    let total    = M.size rc
        included = sort $ mapMaybe snd $ M.toList rc
        lost     = total - length included
    err $ "total number of received transactions: " ++ show total
    err $ "included in blockchain: " ++ show (length included)
    err $ "lost transactions: " ++ show lost
    err $ "blocks in chain: " ++ show (length cr)
    err $ "mem pool entries: " ++ show (length mp)

    let dirName   = extractName logDir
    let graphFile = getName "graph" dirName "png"
    b <- writeGraph graphFile g
    when b $ err $ "wrote graph png to " ++ show graphFile

    let csvFile = getName "csv" dirName "csv"
    txCntInChainMemPoolToCSV csvFile cr mp
    err $ "wrote csv file to " ++ show csvFile

    let reportFile = getName "report" dirName "txt"
    void (reportTxFate reportFile ft)
    err $ "wrote report file to " ++ show reportFile

    err $ "processing log directory " ++ show logDir ++ " done"
    err ""
    return (dirName, rc) 

err :: String -> IO ()
err = hPutStrLn stderr

getName :: FilePath -> String -> String -> FilePath
getName template name ext =
    let dir   = takeDirectory template
        base  = takeBaseName template
        base' =  base ++ "_" ++ name
    in  dir </> base' <.> ext

extractName :: FilePath -> String
extractName = last . splitDirectories
