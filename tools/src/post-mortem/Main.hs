-- | Post-mortem tool main.

import           Universum hiding (last)

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Csv as C (encode)
import           Data.List (last)
import qualified Data.Map.Strict as M
import           Formatting (sformat, shown, (%))
import           System.FilePath
import           System.IO (hPutStrLn)
import           Text.Printf (hPrintf)

import           Pos.Util.Log (Severity (Debug), loggerBracket, setupLogging)
import           Pos.Util.LoggerConfig (defaultStdErrConfiguration)
import           Pos.Util.Trace.Named (TraceNamed, appendName, logInfo,
                     namedTrace)
import           Pos.Util.Util (histogram)

import           Options
import           Statistics
import           Types


main :: IO ()
main = do
    lh <- setupLogging $ defaultStdErrConfiguration Debug
    let logTrace = appendName "post-mortem" $ namedTrace lh

    loggerBracket lh "post-mortem" $ liftIO $ do
        parseOptions >>= \case
            Overview sampleProb logDirs -> do
                showLogDirs logTrace logDirs
                logInfo logTrace $ sformat ("sample probability: " % shown) sampleProb
                logInfo logTrace ""
                xs <- forM logDirs $ flip (processLogDirOverview logTrace) sampleProb
                chart xs "times.svg"
                logInfo logTrace "wrote times chart"
                BSL.writeFile "times.csv" (C.encode $ foldl' (\ acc (_, m) -> getData m ++  acc) [] xs)
                logInfo logTrace "wrote times csv"
            Focus txHash logDir         -> do
                logInfo logTrace $ sformat ("transaction hash: " % shown) txHash
                logInfo logTrace $ sformat ("logs directory: " % shown) logDir
                let focusFile = ("focus_" ++ extractName logDir ++ "_" ++ toString txHash) <.> "csv"
                runJSONFold logDir (focusF txHash) >>= focusToCSV focusFile
                logInfo logTrace $ sformat ("wrote result to " % shown) focusFile
            TxRelay logDirs             -> do
                showLogDirs logTrace logDirs
                logInfo logTrace ""
                for_ logDirs $ processLogDirTxRelay logTrace
            Throughput txWindow waitWindow logDirs   -> do
                showLogDirs logTrace logDirs
                logInfo logTrace $ sformat ("tx window: " % shown) txWindow
                logInfo logTrace $ sformat ("wait window: " % shown) waitWindow
                logInfo logTrace ""
                for_ logDirs $ processLogDirThroughput logTrace txWindow waitWindow

showLogDirs :: MonadIO m => TraceNamed m -> [FilePath] -> m ()
showLogDirs logTrace logDirs = do
    logInfo logTrace "log directories: "
    for_ logDirs $ \d -> logInfo logTrace $ sformat (" - " % shown) d

processLogDirOverview
    :: TraceNamed IO
    -> FilePath
    -> Double
    -> IO (String, Map TxHash (Maybe Timestamp))
processLogDirOverview logTrace logDir sampleProb = do
    logInfo logTrace $ sformat ("processing log directory " % shown % " ...") logDir

    (rc, g, mp, cr, ft) <-
        runJSONFold logDir $ (,,,,) <$> receivedCreatedF
                                    <*> graphF
                                    <*> memPoolF
                                    <*> txCntInChainF
                                    <*> txFateF
    let total    = M.size rc
        included = sort $ mapMaybe snd $ M.toList rc
        lost     = total - length included
    logInfo logTrace $ sformat ("total number of received transactions: " % shown) total
    logInfo logTrace $ sformat ("included in blockchain: " % shown) (length included)
    logInfo logTrace $ sformat ("lost transactions: " % shown) lost
    logInfo logTrace $ sformat ("blocks in chain: " % shown) (length cr)
    logInfo logTrace $ sformat ("mem pool entries: " % shown) (length mp)

    let dirName   = extractName logDir
    let graphFile = getName "graph" dirName "png"
    b <- writeGraph graphFile g
    when b $ logInfo logTrace $ sformat ("wrote graph png to " % shown) graphFile

    let csvFile = getName "csv" dirName "csv"
    txCntInChainMemPoolToCSV csvFile sampleProb cr mp
    logInfo logTrace $ sformat ("wrote csv file to " % shown) csvFile
    let reportFile = getName "report" dirName "txt"
    void (reportTxFate reportFile ft)
    logInfo logTrace $ sformat ("wrote report file to " % shown) reportFile
    logInfo logTrace $ sformat ("processing log directory " % shown % " done") logDir
    logInfo logTrace ""
    return (dirName, rc)

processLogDirTxRelay :: TraceNamed IO -> FilePath -> IO ()
processLogDirTxRelay logTrace logDir = do
    logInfo logTrace $ sformat ("processing log directory " % shown %  " ...") logDir
    m <- runJSONFold logDir txReceivedF
    logInfo logTrace $ sformat ("total number of received transactions: " % shown) (M.size m)
    let xs         = sortBy (compare `on` \(_, ys) -> negate $ length ys) $ M.toList m
        dirName    = extractName logDir
        relaysFile = getName "relays" dirName "txt"
    withFile relaysFile WriteMode $ \h ->
        for_ xs $ \(tx, ys) ->
            hPutStrLn h $ toString tx ++ ": " ++ show ys
    logInfo logTrace $ sformat ("wrote relay data to " % shown) relaysFile
    let hist     = M.toList $ histogram $ map (length . snd) xs
        histFile = getName "histogram" dirName "txt"
    withFile histFile WriteMode $ \h ->
        for_ hist $ uncurry $ hPrintf h "%4d:%6d\n"
    logInfo logTrace $ sformat ("wrote histogram to " % shown) histFile

processLogDirThroughput :: TraceNamed IO -> Double -> Double -> FilePath -> IO ()
processLogDirThroughput logTrace txWindow waitWindow logDir = do
    logInfo logTrace $ sformat ("processing log directory " % shown % " ...") logDir
    (xs, ys) <- runJSONFold logDir $ (,) <$> txCntInChainF <*> memPoolF
    logInfo logTrace $ sformat ("chain length: " % shown % " block(s)") (length xs)
    logInfo logTrace $ sformat (shown % " mem pool event(s)") (length ys)
    let svgFile = getName "throughput" (extractName logDir) "svg"
    throughput svgFile txWindow waitWindow 1000 xs ys
    logInfo logTrace $ sformat ("wrote chart to " % shown) svgFile

getName :: FilePath -> String -> String -> FilePath
getName template name ext =
    let dir   = takeDirectory template
        base  = takeBaseName template
        base' =  base ++ "_" ++ name
    in  dir </> base' <.> ext

extractName :: FilePath -> String
extractName = last . splitDirectories
