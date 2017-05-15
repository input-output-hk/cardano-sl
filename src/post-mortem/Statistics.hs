module Statistics
    ( runJSONFold
    , module Statistics.Tx
    ) where

import Control.Foldl (Fold (..))

import JSONLog
import Statistics.Tx
import Universum
import Util.Pipes    (fold')

runJSONFold :: FilePath -> Fold IndexedJLTimedEvent a -> IO a
runJSONFold logDir fd = runParseLogs logDir $ fold' fd
