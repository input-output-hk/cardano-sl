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
    -- * Conversion
  , toMap
  , toList
  , fromMap
  , fromList
    -- * I/O
  , writeFile
  , readFile
    -- * Ranges
  , SplitRanges(..)
  , splitRanges
  , splitGivenRanges
  , range
    -- * Construction
  , BinSize(..)
  , discretize
  , empty
  , insert
  , insertMany
    -- * Combinators
  , max
  , add
  , unionWith
  , filterBins
  , filterCounts
  , nLargestBins
  ) where

import           Universum hiding (empty, max, readFile, toList, writeFile)
import qualified Universum

import qualified Data.IntMap.Strict as Map
import qualified Data.Set as Set
import qualified Prelude
import qualified System.IO as IO

import           Util.Range (Range (..), Ranges (..), SplitRanges (..))
import qualified Util.Range as Range

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

type Bin   = Int
type Count = Int

data Histogram =
    -- | Non-empty histogram (known binsize)
    Histogram !BinSize !(IntMap Count)
  deriving (Show)

toMap :: Histogram -> IntMap Count
toMap (Histogram _ m) = m

toList :: Histogram -> [(Bin, Count)]
toList = Map.toList . toMap

-- | Construct a histogram from already-discretized data
fromMap :: BinSize -> IntMap Count -> Histogram
fromMap = Histogram

-- | Convenience wrapper around 'fromMap'. See 'fromMap' for more details.
fromList :: BinSize -> [(Bin, Count)] -> Histogram
fromList bz = fromMap bz . Map.fromList

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
      forM_ (toList hist) $ \(step, count) ->
        IO.hPutStrLn h $ show step ++ "\t" ++ show count

-- | Read file previously written by 'writeFile'
--
-- Since we do not store the bin size, this must be passed separately. We
-- cannot verify this value.
readFile :: BinSize -> FilePath -> IO Histogram
readFile binSize fp = parse <$> Prelude.readFile fp
  where
    parse :: String -> Histogram
    parse = fromList binSize
          . map parseLine
          . Prelude.lines

    parseLine :: String -> (Bin, Count)
    parseLine str =
        let (bin, '\t':count) = break (== '\t') str
        in (Prelude.read bin, Prelude.read count)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

newtype BinSize = BinSize { binSizeToInt :: Int }
  deriving (Eq, Buildable)

instance Show BinSize where
  show = show . binSizeToInt

-- | Construct histogram by counting all the doubles per bin
--
-- See also 'fromMap' if the data is already discretized.
discretize :: BinSize -> [Double] -> Histogram
discretize = flip insertMany . empty

-- | Empty histogram
empty :: BinSize -> Histogram
empty bz = Histogram bz Map.empty

-- | Discretize and insert value
insert :: Double -> Histogram -> Histogram
insert d (Histogram (BinSize bz) h) =
    Histogram (BinSize bz) (Map.alter incr bin h)
  where
    bin :: Bin
    bin = floor (d / fromIntegral bz) * bz

    incr :: Maybe Count -> Maybe Count
    incr Nothing  = Just 1
    incr (Just n) = Just (n + 1)

insertMany :: [Double] -> Histogram -> Histogram
insertMany ds h = foldl' (flip insert) h ds

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
unionWith f (Histogram bz a) (Histogram bz' b) =
    if bz /= bz'
      then error "Cannot union two histograms with different bin sizes"
      else Histogram bz (Map.unionWith f a b)

-- | Filter bins
filterBins :: (Bin -> Bool) -> Histogram -> Histogram
filterBins p (Histogram bz m) = Histogram bz (aux m)
  where
    aux :: IntMap Count -> IntMap Count
    aux = Map.filterWithKey $ \b _c -> p b

-- | Filter counts
filterCounts :: (Count -> Bool) -> Histogram -> Histogram
filterCounts p (Histogram bz m) = Histogram bz (aux m)
  where
    aux :: IntMap Count -> IntMap Count
    aux = Map.filter p

-- | Keep a percentage of the bins, prioritizing bins with more elements
nLargestBins :: Double -> Histogram -> Histogram
nLargestBins p (Histogram bz m) = Histogram bz (aux m)
  where
    allBins  = sortBy (flip (comparing snd)) $ Map.toList m
    keep     = ceiling $ p * fromIntegral (length allBins)
    nLargest = Set.fromList $ map fst $ take keep allBins
    aux      = Map.filterWithKey $ \b _c -> b `Set.member` nLargest

{-------------------------------------------------------------------------------
  Split ranges
-------------------------------------------------------------------------------}

-- | Compute the range in bins and counts
--
-- @splitRanges n m@ splits the histogram into different ranges at " markers ",
-- where a marker is a consecutive series of bins of exactly length @m@ where
-- each of the bin has a value at or less than the threshold @n@.
--
-- We make sure that the first range starts at bin 0.
splitRanges :: Int -> Int -> Histogram -> SplitRanges Bin Count
splitRanges n m (Histogram _ h) =
    case Map.toList h of
      []           -> error "splitRanges: empty histogram"
      (b,c) : bins -> go ([(Range 0 b, 1)], Range.singleton c) 0 False bins
  where
    go :: ([(Range Bin, Int)], Range Count)  -- Accumulator
       -> Int  -- Number of consecutive bins below the threshold
       -> Bool -- Have we ever exceeded the threshold?
       -> [(Bin, Count)]                     -- To do
       -> SplitRanges Bin Count
    go (xRanges, yRange) _ _ [] = SplitRanges {
          _splitXRanges = reverse xRanges
        , _splitYRange  = yRange
        }
    go (xRanges, yRange) numBelowThreshold haveExceeded ((b, c) : bins) =
        go (xRanges'', yRange') numBelowThreshold' haveExceeded' bins
      where
        numBelowThreshold' = if c <= n
                               then numBelowThreshold + 1
                               else 0
        haveExceeded'      = haveExceeded || c > n

        (xRange, numBins) : xRanges' = xRanges -- cannot be empty
        xRanges'' = if haveExceeded && numBelowThreshold' == m
                     then (Range.singleton b, 1)             : xRanges
                     else (Range.with b xRange, numBins + 1) : xRanges'
        yRange'   = Range.with c yRange

-- | Like 'splitRanges', but with explicitly specified ranges
splitGivenRanges :: [Bin] -> Histogram -> SplitRanges Bin Count
splitGivenRanges = \splits (Histogram _ h) ->
    case Map.toList h of
      []            -> error "splitGivenRanges: empty histogram"
      (b, c) : bins -> go ([(Range 0 b, 1)], Range.singleton c) splits bins
  where
    go :: ([(Range Bin, Int)], Range Count)  -- Accumulator
       -> [Bin]                              -- Unhandled splits
       -> [(Bin, Count)]                     -- To do
       -> SplitRanges Bin Count
    go (xRanges, yRange) _ [] = SplitRanges {
          _splitXRanges = reverse xRanges
        , _splitYRange  = yRange
        }
    go (xRanges, yRange) splits ((b, c) : bins) =
        case splits of
          s:splits' | b >= s ->
            let xRanges'' = (Range.singleton b, 1) : xRanges in
            go (xRanges'', yRange') splits' bins
          _otherwise ->
            let xRanges'' = (Range.with b xRange, numBins + 1) : xRanges' in
            go (xRanges'', yRange') splits bins
      where
        (xRange, numBins) : xRanges' = xRanges -- cannot be empty
        yRange' = Range.with c yRange

-- | Specialization of 'splitRanges' with no maximum gap.
range :: Histogram -> Ranges Bin Count
range h =
    let SplitRanges [(xRange, _numBins)] yRange = splitRanges 0 maxBound h
    in Ranges xRange yRange
