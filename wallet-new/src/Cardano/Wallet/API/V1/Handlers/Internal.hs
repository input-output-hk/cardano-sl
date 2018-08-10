module Cardano.Wallet.API.V1.Handlers.Internal
    ( computeUtxoStatistics
    )  where


import           Prelude

import           Cardano.Wallet.API.V1.Types

import qualified Control.Foldl as L
import           Data.Map.Strict as MS
import qualified Data.Text as T

computeUtxoStatistics :: [Integer] ->  UtxoStatistics
computeUtxoStatistics xs = L.fold (summarizeUtxoStatistics $ generateBounds Log10) xs

-- | Using foldl library enable as to capture a number of aggregations in one pass. This thanks to L.Fold being an Applicative
summarizeUtxoStatistics :: [Integer] -> L.Fold Integer UtxoStatistics
summarizeUtxoStatistics bounds = UtxoStatistics
  <$> populateBuckets bounds
  <*> L.sum

-- | Buckets boundaries can be constructed in different way
data BoundType = Log10 | Haphazard

generateBounds :: BoundType -> [Integer]
generateBounds bType =
    case bType of
        Log10 -> (zipWith (\ten toPower -> ten^toPower :: Integer) (repeat (10::Integer)) [(1::Integer)..16]) ++ [45 * (10^(15::Integer))]
        Haphazard -> [10, 100, 1000, 10000]


populateBuckets ::  [Integer] ->  L.Fold Integer [HistogramBar]
populateBuckets bounds =
    case bounds of
        (x:_) -> L.Fold (addCountInBuckets x) (initalizeMap bounds) (fmap (\pair -> HistogramBarCount (T.pack $ show $ fst pair, snd pair) ) . MS.toList)
        _ -> error "populateBuckets has to be powered with nonempty bounds"
    where
        initalizeMap :: [Integer] -> MS.Map Integer Integer
        initalizeMap b = MS.fromList $ zip b (repeat 0)
        retrieveProperBound :: (Ord a) => [a] -> a -> a -> a
        retrieveProperBound [] _ prev = prev
        retrieveProperBound (x:xs) stake _ =
            if (stake > x) then
                retrieveProperBound xs stake x
            else
                x
        addCountInBuckets :: Integer -> MS.Map Integer Integer -> Integer -> MS.Map Integer Integer
        addCountInBuckets firstElem acc entry = MS.adjust (+1) (retrieveProperBound bounds entry firstElem) acc
