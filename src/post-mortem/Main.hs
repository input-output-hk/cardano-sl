import qualified Pipes.Prelude as P

import JSONLog
import Universum

main :: IO ()
main = do
    [logDir] <- getArgs
    n        <- test logDir
    putStrLn $ "The json logs in folder " ++ logDir ++ " contain " ++ show n ++ " events in total."

test :: FilePath -> IO Int
test logDir = runParseLogs logDir P.length
