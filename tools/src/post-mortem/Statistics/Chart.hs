module Statistics.Chart
    ( chart
    , getData
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import           Graphics.Rendering.Chart.Easy

import           Types
import           Universum

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

chart :: [(String, Map TxHash (Maybe Timestamp))] -> FilePath -> IO ()
chart xs f =  toFile def f $ do
    layout_title .= "Time to Inclusion into the Blockchain"
    layout_x_axis . laxis_title .= "time (seconds)"
    layout_y_axis . laxis_title .= "transaction ratio"

    setColors $ map opaque $ concat $ repeat
        [ blue, red, green, orchid, violet, salmon, yellow, pink
        , darkblue, darkred, darkgreen, darkorchid, darkviolet, darksalmon
        , lightblue, lightgreen, darksalmon, lightpink, orange, brown
        ]
    for_ xs $ \(s, m) -> plot (line s [(0, 0) : getData m])
