module Cardano.Wallet.API.V1.Handlers.Internal
    ( computeUtxoStatistics
    )  where


import           Prelude

import           Cardano.Wallet.API.V1.Types

import qualified Control.Foldl as L
import           Data.Map.Strict as MS
import qualified Data.Text as T
import           Data.Word


computeUtxoStatistics :: [Word64] ->  UtxoStatistics
computeUtxoStatistics xs = L.fold (summarizeUtxoStatistics $ generateBounds Log10) xs

-- | Using foldl library enable as to capture a number of aggregations in one pass. This thanks to L.Fold being an Applicative
summarizeUtxoStatistics :: [Word64] -> L.Fold Word64 UtxoStatistics
summarizeUtxoStatistics bounds = UtxoStatistics
  <$> populateBuckets bounds
  <*> L.sum

-- | Buckets boundaries can be constructed in different way
data BoundType = Log10 | Haphazard

generateBounds :: BoundType -> [Word64]
generateBounds bType =
    case bType of
        Log10 -> (zipWith (\ten toPower -> ten^toPower :: Word64) (repeat (10::Word64)) [(1::Word64)..16]) ++ [45 * (10^(15::Word64))]
        Haphazard -> [10, 100, 1000, 10000]


populateBuckets ::  [Word64] ->  L.Fold Word64 [HistogramBar]
populateBuckets bounds =
    case bounds of
        (x:_) -> L.Fold (addCountInBuckets x) (initalizeMap bounds) (fmap (\pair -> HistogramBarCount (T.pack $ show $ fst pair, snd pair) ) . MS.toList)
        _ -> error "populateBuckets has to be powered with nonempty bounds"
    where
        initalizeMap :: [Word64] -> MS.Map Word64 Word64
        initalizeMap b = MS.fromList $ zip b (repeat 0)
        retrieveProperBound :: (Ord a) => [a] -> a -> a -> a
        retrieveProperBound [] _ prev = prev
        retrieveProperBound (x:xs) stake _ =
            if (stake > x) then
                retrieveProperBound xs stake x
            else
                x
        addCountInBuckets :: Word64 -> MS.Map Word64 Word64 -> Word64 -> MS.Map Word64 Word64
        addCountInBuckets firstElem acc entry = MS.adjust (+1) (retrieveProperBound bounds entry firstElem) acc
