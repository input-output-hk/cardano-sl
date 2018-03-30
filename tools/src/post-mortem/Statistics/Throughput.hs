module Statistics.Throughput
    ( throughput
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Time.Units (Microsecond)
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Diagrams (renderableToFile)
import           Graphics.Rendering.Chart.Easy
import           Graphics.Rendering.Chart.Grid

import           Pos.Util.JsonLog.Events (JLMemPool (..), MemPoolModifyReason (..))
import           Types
import           Universum

throughput :: FilePath
           -> Double
           -> Double
           -> Int
           -> [(NodeId, Timestamp, Int)]
           -> [(NodeId, Timestamp, JLMemPool)]
           -> IO ()
throughput f txW waitW cnt xs ys =
    let xs'    = [(t, c) | (_, t, c) <- xs]
        ys'    = mapMaybe wait ys
        times  = map fst xs' ++ map fst ys'
        tmin   = minimum times
        tmax   = maximum times
        times' = sample tmin tmax cnt
        xs''   = scaleShift tmin $ sliding txW   times' (\ws -> fromIntegral (sum ws) / txW)       xs'
        ys''   = scaleShift tmin $ sliding waitW times' (lg 100 . average)                         ys'
    in grid f txW waitW xs'' ys''
  where
    wait :: (NodeId, Timestamp, JLMemPool) -> Maybe (Timestamp, Integer)
    wait (_, t, JLMemPool{..}) = case jlmReason of
        ProcessTransaction -> Just (t, jlmWait)
        _                    -> Nothing

    lg :: Double -> Double -> Double
    lg m x = logBase 10 (max x m) - 6

grid :: FilePath
     -> Double
     -> Double
     -> [(Double, Double)]
     -> [(Double, Double)]
     -> IO ()
grid f txW waitW xs ys =
    void $ renderableToFile def f $ fillBackground def $ gridToRenderable $ chart1 `above` chart2 -- `above` chart3
  where
    chart1 = layoutToGrid $ execEC $ do
        setColors [opaque blue]
        layout_x_axis . laxis_title .= "time (s)"
        layout_y_axis . laxis_title .= "tx/s"
        plot $ line ("tx throughput (window " ++ show txW ++ "s)") [xs]

    chart2 = layoutToGrid $ execEC $ do
        setColors [opaque red]
        layout_x_axis . laxis_title .= "time (s)"
        layout_y_axis . laxis_title .= "wait (lg s)"
        plot $ line ("mempool wait for tx processing (window " ++ show waitW ++ "s)") [ys]

sample :: Integral a => a -> a -> Int -> [a]
sample tmin tmax cnt =
    let tmin' = fromIntegral tmin
        tmax' = fromIntegral tmax
        step = (tmax' - tmin') / fromIntegral (pred cnt) :: Double
    in  [round (tmin' + fromIntegral i * step) | i <- [0 .. pred cnt]]

sliding :: Double -> [Timestamp] -> ([a] -> b) -> [(Timestamp, a)] -> [(Timestamp, b)]
sliding _      _     _ [] = []
sliding window times f xs =
    let m = M.fromList xs
    in  [(t, f $ g t m) | t <- times]
  where

    w2 :: Microsecond
    w2 = round $ window * 500000

    g :: Timestamp -> Map Timestamp a -> [a]
    g t m =
        let tmin = t - w2
            tmax = t + w2
            m'   = mapBetween (tmin, tmax) m
        in  map snd $ M.toList m'

scaleShift :: Microsecond -> [(Microsecond, a)] -> [(Double, a)]
scaleShift t = map $ \(t', x) -> (fromIntegral (t' - t) / 1000000, x)

mapGE :: Ord a => a -> Map a b -> Map a b
mapGE a m = case M.lookupLT a m of
    Nothing      -> m
    Just (a', _) -> snd $ M.split a' m

mapLE :: Ord a => a -> Map a b -> Map a b
mapLE a m = case M.lookupGT a m of
    Nothing      -> m
    Just (a', _) -> fst $ M.split a' m

mapBetween :: Ord a => (a, a) -> Map a b -> Map a b
mapBetween (l, u) = mapGE l . mapLE u

average :: [Integer] -> Double
average = f . foldl' g (0, 0)
  where
    f (s, c)
        | c == 0    = 0
        | otherwise = fromIntegral s / fromIntegral c

    g :: (Integer, Int) -> Integer -> (Integer, Int)
    g (s, c) x = (s + x, c + 1)
