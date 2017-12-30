-- | Post-mortem tool main.
-- cardano-post-mortem
-- Usage: cardano-post-mortem COMMAND
--  analyzes the json logs from several directories or focused on a single transaction

import           Universum

import           Data.List (last)
import qualified Data.Map.Strict as M
import           System.FilePath
import           System.IO (hPutStrLn)
import           Text.Printf (hPrintf)

import           Pos.Util.Util (histogram)

import           Options
import           Statistics
import           Types

main :: IO ()
main = parseOptions >>= \case

  -- | command 'overview'
  -- analyzes the json logs from LOGDIRS given a sample probability.

    Overview sampleProb logDirs -> do
        showLogDirs logDirs
        err $ "sample probability: " ++ show sampleProb
        err ""
        xs <- forM logDirs $ flip processLogDirOverview sampleProb
        chart xs "times.png"
        err "wrote times chart"

    -- | command 'focus'
    -- analyzes transaction FOCUS (txHash) in log folder LOGDIR (logDir)

    Focus txHash logDir         -> do
        err $ "transaction hash: " ++ show txHash
        err $ "logs directory: " ++ show logDir
        let focusFile = ("focus_" ++ extractName logDir ++ "_" ++ toString txHash) <.> "csv"
        runJSONFold logDir (focusF txHash) >>= focusToCSV focusFile
        err $ "wrote result to " ++ show focusFile

    -- | command 'txrelay'
    -- analyzes transaction relays in the json logs from  LOGDIRS...
    
    TxRelay logDirs             -> do
        showLogDirs logDirs
        err ""
        for_ logDirs processLogDirTxRelay

    -- |  command 'throughput'
    -- analyzes transaction throughput and waiting time per
    -- time windows TXWINDOW and WAITWINDOW in the json logs
    -- from LOGDIRS.

    Throughput txWindow waitWindow logDirs   -> do
        showLogDirs logDirs
        err $ "tx window: " ++ show txWindow
        err $ "wait window: " ++ show waitWindow
        err ""
        for_ logDirs $ processLogDirThroughput txWindow waitWindow

      
-- | Function 'showLogDirs' is a helper function to print out a list of directories (FilePaths)
showLogDirs :: [FilePath] -> IO ()
showLogDirs logDirs = do
    err "log directories: "
    for_ logDirs $ \d -> err $ " - " ++ show d

-- | Function 'processLogDirOverview' is a helper function to implement the overview feature
-- it takes the FilePath logDir and the Double @sampleProb as a parameter. 
processLogDirOverview :: FilePath -> Double -> IO (String, Map TxHash (Maybe Timestamp))
processLogDirOverview logDir sampleProb = do
    err $ "processing log directory " ++ show logDir ++ " ..."

    (rc, g, mp, cr, ft) <-
        runJSONFold logDir $ (,,,,) <$> receivedCreatedF
                                    <*> graphF
                                    <*> memPoolF
                                    <*> txCntInChainF
                                    <*> txFateF
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
    txCntInChainMemPoolToCSV csvFile sampleProb cr mp
    err $ "wrote csv file to " ++ show csvFile
    let reportFile = getName "report" dirName "txt"
    void (reportTxFate reportFile ft)
    err $ "wrote report file to " ++ show reportFile
    err $ "processing log directory " ++ show logDir ++ " done"
    err ""
    return (dirName, rc)

processLogDirTxRelay :: FilePath -> IO ()
processLogDirTxRelay logDir = do
    err $ "processing log directory " ++ show logDir ++ " ..."
    m <- runJSONFold logDir txReceivedF
    err $ "total number of received transactions: " ++ show (M.size m)
    let xs         = sortBy (compare `on` \(_, ys) -> negate $ length ys) $ M.toList m
        dirName    = extractName logDir
        relaysFile = getName "relays" dirName "txt"
    withFile relaysFile WriteMode $ \h ->
        for_ xs $ \(tx, ys) ->
            hPutStrLn h $ toString tx ++ ": " ++ show ys
    err $ "wrote relay data to " ++ show relaysFile
    let hist     = M.toList $ histogram $ map (length . snd) xs
        histFile = getName "histogram" dirName "txt"
    withFile histFile WriteMode $ \h ->
        for_ hist $ uncurry $ hPrintf h "%4d:%6d\n"
    err $ "wrote histogram to " ++ show histFile

processLogDirThroughput :: Double -> Double -> FilePath -> IO ()
processLogDirThroughput txWindow waitWindow logDir = do
    err $ "processing log directory " ++ show logDir ++ " ..."
    (xs, ys) <- runJSONFold logDir $ (,) <$> txCntInChainF <*> memPoolF
    err $ "chain length: " ++ show (length xs) ++ " block(s)"
    err $ show (length ys) ++ " mem pool event(s)"
    let svgFile = getName "throughput" (extractName logDir) "svg"
    throughput svgFile txWindow waitWindow 1000 xs ys
    err $ "wrote chart to " ++ show svgFile

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
