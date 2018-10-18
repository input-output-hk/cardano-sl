module Statistics.Chart
    ( getData
    ) where

import           Universum

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           Types (Timestamp, TxHash)

getData :: Map TxHash (Maybe Timestamp) -> [(Double, Double)]
getData m = map normalize pairs

  where
    total :: Int
    total = M.size m

    times :: [Timestamp]
    times = sort $ mapMaybe snd $ M.toList m

    step :: (Int, [(Timestamp, Int)]) -> Timestamp -> (Int, [(Timestamp, Int)])
    step (!n, xs) ts = let n' = n + 1
                           x  = (ts, n')
                       in  (n', x : xs)

    pairs :: [(Timestamp, Int)]
    pairs = reverse $ snd $ foldl' step (0, []) times

    normalize :: (Timestamp, Int) -> (Double, Double)
    normalize (ts, n) = ( fromIntegral ts / 1000000
                        , fromIntegral n / fromIntegral total
                        )
