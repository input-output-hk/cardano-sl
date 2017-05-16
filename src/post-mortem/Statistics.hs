module Statistics
    ( runJSONFold
    , txReceivedCreatedF
    , module Statistics.Tx
    , module Statistics.Block
    ) where

import           Control.Foldl   (Fold (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set        (Set)
import qualified Data.Set        as S

import JSONLog
import Statistics.Block
import Statistics.Tx
import Universum
import Util.Pipes    (fold')

runJSONFold :: FilePath -> Fold IndexedJLTimedEvent a -> IO a
runJSONFold logDir fd = runParseLogs logDir $ fold' fd

txReceivedCreatedF :: Fold IndexedJLTimedEvent (Map Text (Maybe Integer))
txReceivedCreatedF = f <$> txReceivedF <*> txBlocksF <*> (blockChain <$> blockHeadersF)
  where
    f :: Map Text Integer 
      -> Map Text [(Integer, Text)] 
      -> [Text] 
      -> Map Text (Maybe Integer)
    f rm bm cs = M.mapWithKey g rm
      where
        chain :: Set Text
        chain = S.fromList cs

        g :: Text -> Integer -> (Maybe Integer)
        g tx received =
            let tss = [ts | (ts, h) <-  M.findWithDefault [] tx bm, 
                            S.member h chain]
            in  case tss of
                    [] -> Nothing
                    _  -> Just $ minimum tss - received
