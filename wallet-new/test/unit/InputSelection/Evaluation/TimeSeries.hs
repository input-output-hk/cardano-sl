{-# LANGUAGE DeriveFunctor #-}

-- | Time series data
--
-- Intended for qualified import
module InputSelection.Evaluation.TimeSeries (
    -- * Slot numbers
    SlotNr(..)
  , OverallSlotNr
  , PolicyNr
  , PolicySlotNr
    -- * Time series
  , TimeSeries(..)
  , toList
  , fromList
  , empty
  , insert
  , range
    -- I/O
  , writeFile
  , readFile
  ) where

import           Universum hiding (empty, readFile, toList, writeFile)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Prelude

import           Util.Range (Range (..), Ranges (..))

{-------------------------------------------------------------------------------
  Slot numbers
-------------------------------------------------------------------------------}

type OverallSlotNr = Int
type PolicyNr      = Int
type PolicySlotNr  = Int

-- | Slot number
data SlotNr = SlotNr {
    -- | Overall slot number (across policies)
    overallSlotNr :: OverallSlotNr

    -- \ Policy number (only relevant when evaluating multiple policies)
  , policyNr      :: PolicyNr

    -- | Slot number for this policy
  , policySlotNr  :: PolicySlotNr
  }
  deriving (Eq, Ord)

{-------------------------------------------------------------------------------
  Time series
-------------------------------------------------------------------------------}

-- | Time series of values
--
-- When we render a single frame,
--
-- * for some vars we render the /current/ value;
--   for example, we render the current UTxO histogram
-- * for some vars we render the /summarized/ value;
--   for example, we render the overall histogram of number of inputs / tx
-- * for some vars we render a /time series/, from the start until this frame
--   for example, we show the UTxO growth over time
--
-- The 'TimeSeries' is meant to record the third kind of variable.
--
-- Since we don't same at each slot, we store this as a spare mapping form
-- slot numbers to values.
newtype TimeSeries a = TimeSeries {
      timeSeriesToMap :: Map SlotNr a
    }
  deriving (Functor)

toList :: TimeSeries a -> [(SlotNr, a)]
toList = Map.toList . timeSeriesToMap

fromList :: [(SlotNr, a)] -> TimeSeries a
fromList = TimeSeries . Map.fromList

empty :: TimeSeries a
empty = TimeSeries Map.empty

insert :: SlotNr -> a -> TimeSeries a -> TimeSeries a
insert slotNr a (TimeSeries m) = TimeSeries (Map.insert slotNr a m)

-- | Bounds for a time series
range :: forall a. Ord a => TimeSeries a -> Ranges OverallSlotNr a
range (TimeSeries m) = Ranges {
      _x = Range (minimum slots)
                 (maximum slots)
    , _y = Range (minimum (Map.elems m))
                 (maximum (Map.elems m))
    }
  where
    slots :: [OverallSlotNr]
    slots = map overallSlotNr (Map.keys m)

{-------------------------------------------------------------------------------
  I/O
-------------------------------------------------------------------------------}

-- | Write out a time series to disk
--
-- Implementation note: go through LBS to take advantage of it's chunking
-- policy, avoiding hPutStr and co's excessive lock taking and releasing.
writeFile :: Show a => FilePath -> TimeSeries a -> IO ()
writeFile fp =
       LBS.writeFile fp
     . LBS.pack
     . Prelude.unlines
     . map (\(SlotNr{..}, a) -> concat [
           Prelude.show overallSlotNr
         , "\t"
         , Prelude.show policyNr
         , "\t"
         , Prelude.show policySlotNr
         , "\t"
         , Prelude.show a
         ])
     . toList

readFile :: forall a. Read a => FilePath -> IO (TimeSeries a)
readFile fp = parse <$> Prelude.readFile fp
  where
    parse :: String -> TimeSeries a
    parse = fromList . map parseLine . Prelude.lines

    parseLine :: String -> (SlotNr, a)
    parseLine str =
        let [overall, policy, policySlot, a] = splitWhen (== '\t') str
        in ( SlotNr {
                 overallSlotNr = Prelude.read overall
               , policyNr      = Prelude.read policy
               , policySlotNr  = Prelude.read policySlot
               }
           , Prelude.read a
           )

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p xs =
    case break p xs of
      (prefix, [])               -> [prefix]
      (prefix, _match:remainder) -> prefix : splitWhen p remainder
