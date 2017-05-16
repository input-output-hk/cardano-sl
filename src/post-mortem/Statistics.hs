module Statistics
    ( runJSONFold
    , receivedCreatedF
    , module Statistics.Tx
    , module Statistics.Block
    ) where

import           Control.Foldl   (Fold (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import JSONLog
import Statistics.Block
import Statistics.Tx
import Types
import Universum
import Util.Pipes    (fold')

runJSONFold :: FilePath -> Fold IndexedJLTimedEvent a -> IO a
runJSONFold logDir fd = runParseLogs logDir $ fold' fd

receivedCreatedF :: Fold IndexedJLTimedEvent (Map TxHash (Maybe Timestamp))
receivedCreatedF = f <$> txReceivedF <*> inBlockChainF
  where
    f :: Map TxHash Timestamp -> Map TxHash Timestamp -> Map TxHash (Maybe Timestamp)
    f rm cm = M.mapWithKey g rm
      where
        g :: TxHash -> Timestamp -> Maybe Timestamp
        g tx ts = maybe Nothing (\ts' -> Just $ ts' - ts) $ M.lookup tx cm
