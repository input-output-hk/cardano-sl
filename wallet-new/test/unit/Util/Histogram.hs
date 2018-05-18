{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Histograms
--
-- Indended for qualified import.
--
-- > import Util.Histogram (Histogram)
-- > import qualified Util.Histogram as Histogram
module Util.Histogram (
    -- * Basic definitions
    Bin
  , Count
  , Histogram  -- opaque
    -- * Output
  , writeFile
  , range
    -- * Construction
  , BinSize(..)
  , discretize
  , empty
  , singleton
    -- * Combinators
  , max
  , add
  , unionWith
  , pruneAbove
  ) where

import           Universum hiding (empty, max, writeFile)
import qualified Universum

import qualified Data.Map as Map
import qualified System.IO as IO

import           Util.Range

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

type Bin       = Int
type Count     = Int

data Histogram =
    -- | Non-empty histogram (known binsize)
    Histogram BinSize (Map Bin Count)

    -- | Empty histogram
    --
    -- We treat this case special so that we can construct a histogram without
    -- knowing the binsize.
  | Empty

histogramToMap :: Histogram -> Map Bin Count
histogramToMap (Histogram _ m) = m
histogramToMap Empty           = Map.empty

histogramToList :: Histogram -> [(Bin, Count)]
histogramToList = Map.toList . histogramToMap

{-------------------------------------------------------------------------------
  Output
-------------------------------------------------------------------------------}

-- | Write out histogram
--
-- Example plot using gnuplot:
--
-- > set grid
-- > set xrange [5:105]  -- for bins 10, 20, .. 100
-- > set yrange [0:10.5] -- for counts from 0 .. 10
-- > plot 'data.csv' using 1:2 with boxes
--
-- See 'range' to compute @xrange@ and @yrange@.
writeFile :: FilePath -> Histogram -> IO ()
writeFile fp hist =
    withFile fp WriteMode $ \h ->
      forM_ (histogramToList hist) $ \(step, count) ->
        IO.hPutStrLn h $ show step ++ "\t" ++ show count

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

newtype BinSize = BinSize Int
  deriving (Eq, Buildable)

-- | Construct histogram by counting all the doubles per bin
discretize :: BinSize -> [Double] -> Histogram
discretize (BinSize binSize) =
    Histogram (BinSize binSize) . go Map.empty
  where
    go :: Map Bin Count -> [Double] -> Map Bin Count
    go acc []     = acc
    go acc (d:ds) = let bin = floor (d / fromIntegral binSize) * binSize
                    in go (Map.alter incr bin acc) ds

    incr :: Maybe Count -> Maybe Count
    incr Nothing  = Just 1
    incr (Just n) = Just (n + 1)

-- | Empty histogram
empty :: Histogram
empty = Empty

-- | Singleton
singleton :: BinSize -> Bin -> Count -> Histogram
singleton binSize bin count = Histogram binSize $ Map.singleton bin count

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

-- | Takes maximum across all bins
--
-- This only makes sense if the two histograms uses the same bin size.
max :: Histogram -> Histogram -> Histogram
max = unionWith Universum.max

-- | Sum across all bins
add :: Histogram -> Histogram -> Histogram
add = unionWith (+)

-- | Like 'Map.unionWith'
unionWith :: (Count -> Count -> Count) -> Histogram -> Histogram -> Histogram
unionWith _ a Empty = a
unionWith _ Empty b = b
unionWith f (Histogram bz a) (Histogram bz' b) =
    if bz /= bz'
      then error "Cannot union two histograms with different bin sizes"
      else Histogram bz (Map.unionWith f a b)

-- | Prune any bins at or above the given threshold
pruneAbove :: Bin -> Histogram -> Histogram
pruneAbove _ Empty = Empty
pruneAbove b (Histogram bz m) = Histogram bz (aux m)
  where
    aux :: Map Bin Count -> Map Bin Count
    aux = Map.filterWithKey $ \b' _c -> b' < b

-- | X-range (bins) and Y-range (counts) for the given histogram
--
-- Calls 'error' if the histogram is empty.
range :: Histogram -> Ranges
range Empty = error "range: empty histogram"
range (Histogram (BinSize bz) m) = Ranges {
      _xRange = Range (fromIntegral xLo) (fromIntegral xHi)
    , _yRange = Range (fromIntegral yLo) (fromIntegral yHi)
    }
  where
    xLo, xHi :: Int
    xLo = fst (Map.findMin m) - (bz `div` 2)
    xHi = fst (Map.findMax m) + (bz `div` 2)

    yLo, yHi :: Int
    yLo = 0
    yHi = maximum m + 1
