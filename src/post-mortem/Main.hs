import qualified Data.Map.Strict as M
import           System.IO       (hPutStrLn)

import           Options
import           Statistics
import           Universum

main :: IO ()
main = do
    Options{..} <- parseOptions
    err $ "logs directory: " ++ show logDir
    err $ "times svg file: " ++ show timesSVG
    err $ "graph png file: " ++ show graphPNG
    (rc, g) <- runJSONFold logDir $ (,) <$> receivedCreatedF <*> graphF
    let total    = M.size rc
        included = sort $ mapMaybe snd $ M.toList rc
        lost     = total - length included
    err $ "total number of received transactions: " ++ show total
    err $ "included in blockchain: " ++ show (length included)
    err $ "lost transactions: " ++ show lost

    chart rc timesSVG
    err $ "wrote times svg to " ++ show timesSVG

    writeGraph graphPNG g
    err $ "wrote graph png to " ++ show graphPNG
  where
    err :: String -> IO ()
    err = hPutStrLn stderr
