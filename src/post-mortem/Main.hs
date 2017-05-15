import qualified Data.Map.Strict as M
import           System.IO       (hPutStrLn)

import           Statistics
import           Universum

main :: IO ()
main = do
    [logDir] <- getArgs
    (ds, n)  <- test logDir
    let c = n + length ds
    hPutStrLn stderr $ show c ++ " transactions, " ++ show n ++ " outliers"
    for_ (sort ds) print

test :: FilePath -> IO ([Integer], Int)
test logDir = do
    d <- runFoldTxData logDir
    let m = transformTxData f d
    return $ M.foldl' g ([], 0) m
    
  where
    f :: [(Int, Integer)] -> [(Int, Integer)] -> Maybe Integer
    f [] _  = Nothing
    f _  [] = Nothing
    f xs ys = Just $ maximum (map snd ys) - minimum (map snd xs)

    g :: ([Integer], Int) -> Maybe Integer -> ([Integer], Int)
    g (ds, !n) Nothing  = (ds    , n + 1)
    g (ds, !n) (Just d) = (d : ds, n)
