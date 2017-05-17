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
    Options{..} <- parseOptions
    let timesSVG = replaceExtension times "svg" 
    err $ "times svg file: " ++ show timesSVG
    err $ "graph file template: " ++ show graph
    err "logs directories: "
    for_ logDirs $ \d -> err $ " - " ++ show d
    err ""
   
    xs <- forM logDirs $ processLogDir graph
    chart xs timesSVG
    err $ "wrote times to " ++ show timesSVG

processLogDir :: FilePath -> FilePath -> IO (String, Map TxHash (Maybe Timestamp))
processLogDir graphFileTemplate logDir = do
    err $ "processing log directory " ++ show logDir ++ " ..."

    (rc, g) <- runJSONFold logDir $ (,) <$> receivedCreatedF <*> graphF
    let total    = M.size rc
        included = sort $ mapMaybe snd $ M.toList rc
        lost     = total - length included
    err $ "total number of received transactions: " ++ show total
    err $ "included in blockchain: " ++ show (length included)
    err $ "lost transactions: " ++ show lost

    let dirName   = extractName logDir
    let graphFile = getName graphFileTemplate dirName "png"
    writeGraph graphFile g
    err $ "wrote graph png to " ++ show graphFile

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
