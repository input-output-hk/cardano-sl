-- | Post-mortem tool main.

import           Universum

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Csv as C (encode)
import           Data.List (last)
import qualified Data.Map.Strict as M
import           System.FilePath
import           System.IO (hPutStrLn)
import           Text.Printf (hPrintf)

import           Pos.Util.Util (histogram)
import qualified Pos.Util.Log

import           Options
import           Statistics
import           Types


main :: IO ()
main = do
  Log.loggerBracket Log.Info "post-mortem" $ do
    parseOptions >>= \case
      Overview sampleProb logDirs -> do
          showLogDirs logDirs
          Log.logInfo $ "sample probability: " ++ show sampleProb
          Log.logInfo ""
          xs <- forM logDirs $ flip processLogDirOverview sampleProb
          chart xs "times.svg"
          Log.logInfo "wrote times chart"
          BSL.writeFile "times.csv" (C.encode $ foldl' (\ acc (_, m) -> getData m ++  acc) [] xs)
          Log.logInfo "wrote times csv"
      Focus txHash logDir         -> do
          Log.logInfo $ "transaction hash: " ++ show txHash
          Log.logInfo $ "logs directory: " ++ show logDir
          let focusFile = ("focus_" ++ extractName logDir ++ "_" ++ toString txHash) <.> "csv"
          runJSONFold logDir (focusF txHash) >>= focusToCSV focusFile
          Log.logInfo $ "wrote result to " ++ show focusFile
      TxRelay logDirs             -> do
          showLogDirs logDirs
          Log.logInfo ""
          for_ logDirs processLogDirTxRelay
      Throughput txWindow waitWindow logDirs   -> do
          showLogDirs logDirs
          Log.logInfo $ "tx window: " ++ show txWindow
          Log.logInfo $ "wait window: " ++ show waitWindow
          Log.logInfo ""
          for_ logDirs $ processLogDirThroughput txWindow waitWindow

showLogDirs :: (MonadIO m, Log.LogContext m) => [FilePath] -> m ()
showLogDirs logDirs = do
    Log.logInfo "log directories: "
    for_ logDirs $ \d -> Log.logInfo $ " - " ++ show d

processLogDirOverview :: (MonadIO m, Log.LogContext m) => FilePath -> Double -> m (String, Map TxHash (Maybe Timestamp))
processLogDirOverview logDir sampleProb = do
    Log.logInfo $ "processing log directory " ++ show logDir ++ " ..."

    (rc, g, mp, cr, ft) <-
        runJSONFold logDir $ (,,,,) <$> receivedCreatedF
                                    <*> graphF
                                    <*> memPoolF
                                    <*> txCntInChainF
                                    <*> txFateF
    let total    = M.size rc
        included = sort $ mapMaybe snd $ M.toList rc
        lost     = total - length included
    Log.logInfo $ "total number of received transactions: " ++ show total
    Log.logInfo $ "included in blockchain: " ++ show (length included)
    Log.logInfo $ "lost transactions: " ++ show lost
    Log.logInfo $ "blocks in chain: " ++ show (length cr)
    Log.logInfo $ "mem pool entries: " ++ show (length mp)

    let dirName   = extractName logDir
    let graphFile = getName "graph" dirName "png"
    b <- writeGraph graphFile g
    when b $ Log.logInfo $ "wrote graph png to " ++ show graphFile

    let csvFile = getName "csv" dirName "csv"
    txCntInChainMemPoolToCSV csvFile sampleProb cr mp
    Log.logInfo $ "wrote csv file to " ++ show csvFile
    let reportFile = getName "report" dirName "txt"
    void (reportTxFate reportFile ft)
    Log.logInfo $ "wrote report file to " ++ show reportFile
    Log.logInfo $ "processing log directory " ++ show logDir ++ " done"
    Log.logInfo ""
    return (dirName, rc)

processLogDirTxRelay :: (MonadIO m, Log.LogContext m) => FilePath -> m ()
processLogDirTxRelay logDir = do
    Log.logInfo $ "processing log directory " ++ show logDir ++ " ..."
    m <- runJSONFold logDir txReceivedF
    Log.logInfo $ "total number of received transactions: " ++ show (M.size m)
    let xs         = sortBy (compare `on` \(_, ys) -> negate $ length ys) $ M.toList m
        dirName    = extractName logDir
        relaysFile = getName "relays" dirName "txt"
    withFile relaysFile WriteMode $ \h ->
        for_ xs $ \(tx, ys) ->
            hPutStrLn h $ toString tx ++ ": " ++ show ys
    Log.logInfo $ "wrote relay data to " ++ show relaysFile
    let hist     = M.toList $ histogram $ map (length . snd) xs
        histFile = getName "histogram" dirName "txt"
    withFile histFile WriteMode $ \h ->
        for_ hist $ uncurry $ hPrintf h "%4d:%6d\n"
    Log.logInfo $ "wrote histogram to " ++ show histFile

processLogDirThroughput :: (MonadIO m, Log.LogContext m) => Double -> Double -> FilePath -> m ()
processLogDirThroughput txWindow waitWindow logDir = do
    Log.logInfo $ "processing log directory " ++ show logDir ++ " ..."
    (xs, ys) <- runJSONFold logDir $ (,) <$> txCntInChainF <*> memPoolF
    Log.logInfo $ "chain length: " ++ show (length xs) ++ " block(s)"
    Log.logInfo $ show (length ys) ++ " mem pool event(s)"
    let svgFile = getName "throughput" (extractName logDir) "svg"
    throughput svgFile txWindow waitWindow 1000 xs ys
    Log.logInfo $ "wrote chart to " ++ show svgFile

--err :: String -> IO ()
--err = hPutStrLn stderr

getName :: FilePath -> String -> String -> FilePath
getName template name ext =
    let dir   = takeDirectory template
        base  = takeBaseName template
        base' =  base ++ "_" ++ name
    in  dir </> base' <.> ext

extractName :: FilePath -> String
extractName = last . splitDirectories
