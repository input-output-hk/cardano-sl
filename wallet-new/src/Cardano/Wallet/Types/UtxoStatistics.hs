{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Cardano.Wallet.Types.UtxoStatistics
    ( computeUtxoStatistics
    , UtxoStatistics (..)
    , HistogramBar (..)
    , BoundType (..)
    , generateBounds
    )  where


import           Universum

import qualified Control.Foldl as L
import           Control.Lens (at, (?~))
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HMS
import qualified Data.List.NonEmpty as NL
import qualified Data.Map.Strict as Map
import           Data.Swagger hiding (Example)
import qualified Data.Text as T
import           Data.Word (Word64)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJson)
import           Test.QuickCheck

import           Cardano.Wallet.API.V1.Swagger.Example (Example)
import           Pos.Infra.Util.LogSafe (BuildableSafeGen (..),
                     deriveSafeBuildable)


--  Utxo statistics for the wallet.
--  Histogram is composed of bars that represent the bucket. The bucket is tagged by upper bound of a given bucket.
--  The bar value corresponds to the number of stakes
--  In the future the bar value could be different things:
--  (a) sum of stakes in a bucket
--  (b) avg or std of stake in a bucket
--  (c) topN buckets
--  to name a few
data HistogramBar = HistogramBarCount
    { bucketName       :: !Text
    , bucketUpperBound :: !Word64
    } deriving (Show, Eq, Ord, Generic)

--  Buckets boundaries can be constructed in different way
data BoundType = Log10 | Haphazard

generateBounds :: BoundType -> NonEmpty Word64
generateBounds bType =
    let (^!) :: Word64 -> Word64 -> Word64
        (^!) = (^)
    in case bType of
        Log10 -> NL.fromList $ ( map (\toPower -> 10 ^! toPower) [1..16] ) ++ [45 * (10 ^! 15)]
        Haphazard -> NL.fromList [10, 100, 1000, 10000]

instance Arbitrary HistogramBar where
    arbitrary = do
        possiblenames <- elements $ map show (NL.toList $ generateBounds Log10)
        bound <- arbitrary
        pure (HistogramBarCount possiblenames bound)


deriveSafeBuildable ''HistogramBar
instance BuildableSafeGen HistogramBar where
    buildSafeGen _ HistogramBarCount{..} =
        bprint ("{"
                %" name="%build
                %" upperBound="%build
                %" }")
        bucketName
        bucketUpperBound


data UtxoStatistics = UtxoStatistics
    { theHistogram :: ![HistogramBar]
    , theAllStakes :: !Word64
    } deriving (Show, Generic, Ord)

toMap :: [HistogramBar] -> Map Text Word64
toMap = Map.fromList . map (\(HistogramBarCount key val) -> (key,val))

instance Eq UtxoStatistics where
    (UtxoStatistics h s) == (UtxoStatistics h' s') = s == s' && toMap h == toMap h'

instance ToJSON UtxoStatistics where
    toJSON (UtxoStatistics bars allStakes) =
        let histogramObject = Object . HMS.fromList . map extractBarKey
            extractBarKey (HistogramBarCount bound stake) = bound .= stake
        in object [ "histogram" .= histogramObject bars
                  , "allStakes" .= allStakes ]

instance FromJSON UtxoStatistics where
    parseJSON = withObject "UtxoStatistics" $ \o -> do
        histo <- o .: "histogram" :: Parser (HashMap Text Word64)
        stakes <- o .: "allStakes"
        case validateUtxoStatistics histo stakes of
            Right (histogram, allStakes) -> do
                let constructHistogram = uncurry HistogramBarCount
                let histoBars = map constructHistogram $ HMS.toList histogram
                pure $ UtxoStatistics histoBars allStakes
            Left err -> fail $ "Failed to parse UtxoStatistics: " <> show err

data UtxoStatisticsError
    = ErrHistogramEmpty
    | ErrHistogramNamesInvalid
    | ErrHistogramUpperBoundsNegative
    | ErrAllStakesNegative
    deriving (Show)

validateUtxoStatistics :: HashMap Text Word64 -> Word64 -> Either UtxoStatisticsError (HashMap Text Word64, Word64)
validateUtxoStatistics histogram allStakes
    | histogramBinNumCond histogram = Left ErrHistogramEmpty
    | histogramKeysCond histogram = Left ErrHistogramNamesInvalid
    | histogramValsCond histogram = Left ErrHistogramUpperBoundsNegative
    | allStakesCond allStakes = Left ErrAllStakesNegative
    | otherwise = Right (histogram, allStakes)
    where
        histogramBinNumCond histo = (length $ HMS.keys histo) <= 0
        validateKeys = any (\key -> notElem key $  map show (NL.toList $ generateBounds Log10) )
        histogramKeysCond = validateKeys . HMS.keys
        validateVals = any (< 0)
        histogramValsCond = validateVals . HMS.elems
        allStakesCond = (< 0)


instance Buildable UtxoStatisticsError where
    build = \case
        ErrHistogramEmpty ->
            bprint "Utxo statistics histogram cannot be empty of bins"
        ErrHistogramNamesInvalid ->
            bprint "All names of Utxo statistics histogram have to be valid"
        ErrHistogramUpperBoundsNegative ->
            bprint "All upper bounds of Utxo statistics histogram have to be nonnegative"
        ErrAllStakesNegative ->
            bprint "Utxo statistics allStakes has to be nonnegative"


instance ToSchema UtxoStatistics where
    declareNamedSchema _ = do
        wordRef <- declareSchemaRef (Proxy :: Proxy Word64)
        pure $ NamedSchema (Just "UtxoStatistics") $ mempty
            & type_ .~ SwaggerObject
            & required .~ ["histogram", "allStakes"]
            & properties .~ (mempty
                & at "histogram" ?~ Inline (mempty
                    & type_ .~ SwaggerObject
                    & properties .~ (mempty
                                     & at "10" ?~ wordRef
                                     & at "100" ?~ wordRef
                                     & at "1000" ?~ wordRef
                                     & at "10000" ?~ wordRef
                                     & at "100000" ?~ wordRef
                                     & at "1000000" ?~ wordRef
                                     & at "10000000" ?~ wordRef
                                     & at "100000000" ?~ wordRef
                                     & at "1000000000" ?~ wordRef
                                     & at "10000000000" ?~ wordRef
                                     & at "100000000000" ?~ wordRef
                                     & at "1000000000000" ?~ wordRef
                                     & at "10000000000000" ?~ wordRef
                                     & at "100000000000000" ?~ wordRef
                                     & at "1000000000000000" ?~ wordRef
                                     & at "10000000000000000" ?~ wordRef
                                     & at "45000000000000000" ?~ wordRef
                                    )
                )
                & at "allStakes" ?~ (Inline $ mempty
                    & type_ .~ SwaggerNumber
                    & minimum_ .~ Just 0
                )
            )

instance Arbitrary UtxoStatistics where
    arbitrary = UtxoStatistics <$> arbitrary
                               <*> arbitrary

instance Buildable [HistogramBar] where
    build =
        bprint listJson


deriveSafeBuildable ''UtxoStatistics
instance BuildableSafeGen UtxoStatistics where
    buildSafeGen _ UtxoStatistics{..} = bprint ("{"
        %" histogram="%build
        %" allStakes="%build
        %" }")
        theHistogram
        theAllStakes

instance Example HistogramBar
instance Example UtxoStatistics


computeUtxoStatistics :: [Word64] ->  UtxoStatistics
computeUtxoStatistics xs = L.fold (summarizeUtxoStatistics $ generateBounds Log10) xs

--  Using foldl library enable as to capture a number of aggregations in one pass. This thanks to L.Fold being an Applicative
summarizeUtxoStatistics :: NonEmpty Word64 -> L.Fold Word64 UtxoStatistics
summarizeUtxoStatistics bounds =
    UtxoStatistics
    <$> populateBuckets bounds
    <*> L.sum

populateBuckets ::  NonEmpty Word64 ->  L.Fold Word64 [HistogramBar]
populateBuckets bounds =
    L.Fold (addCountInBuckets $ head bounds) (initalizeMap bounds)
    (fmap (\(x1, x2) -> HistogramBarCount (T.pack $ show x1) x2) . Map.toList)
    where
        initalizeMap :: NonEmpty Word64 -> Map.Map Word64 Word64
        initalizeMap b = Map.fromList $ NL.toList  $ NL.zip b (NL.repeat 0)
        addCountInBuckets :: Word64 -> Map.Map Word64 Word64 -> Word64 -> Map.Map Word64 Word64
        addCountInBuckets thefirst acc entry =
            case Map.lookupGE entry acc of
                Just (k, v) -> Map.insert k (v+1) acc
                Nothing     -> Map.adjust (+1) thefirst acc
