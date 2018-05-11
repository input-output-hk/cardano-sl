import           Distribution.Simple
import System.Environment
import Data.Monoid
main :: IO ()
main = do
    path <- getEnv "PATH"
    print $ "path is: " <> path
    defaultMain
