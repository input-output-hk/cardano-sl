{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE ViewPatterns  #-}


module Cardano.Wallet.Types.UtxoStatistics
    ( computeUtxoStatistics
    , UtxoStatistics
    , mkUtxoStatistics
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
import           Data.Word (Word64)
import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJson)
import           Test.QuickCheck (Arbitrary (..), arbitrary, elements, suchThat)

import           Cardano.Wallet.API.V1.Swagger.Example (Example)
import           Pos.Chain.Txp (Utxo)
import           Pos.Core.Common (Coin (..))
import           Pos.Core.Txp (TxOut (..), TxOutAux (..))
import           Pos.Infra.Log.LogSafe (BuildableSafeGen (..),
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
    { bucketUpperBound :: !Word64
    , bucketCount      :: !Word64
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
        possiblenames <- elements (NL.toList $ generateBounds Log10)
        bound <- arbitrary `suchThat` (>= 0)
        pure (HistogramBarCount possiblenames bound)


deriveSafeBuildable ''HistogramBar
instance BuildableSafeGen HistogramBar where
    buildSafeGen _ HistogramBarCount{..} =
        bprint ("{"
                %" upperBound="%build
                %" count="%build
                %" }")
        bucketUpperBound
        bucketCount


data UtxoStatistics = UtxoStatistics
    { theHistogram :: ![HistogramBar]
    , theAllStakes :: !Word64
    } deriving (Show, Generic, Ord)


mkUtxoStatistics
    :: Map Word64 Word64
    -> Word64
    -> Either UtxoStatisticsError UtxoStatistics
mkUtxoStatistics histogram allStakes = do
    let (histoKeys, histoElems) = (Map.keys histogram, Map.elems histogram)
    let acceptedKeys = NL.toList $ generateBounds Log10
    let (minPossibleValue, maxPossibleValue) = getPossibleBounds histogram
    let constructHistogram = uncurry HistogramBarCount
    let histoBars = map constructHistogram $ Map.toList histogram

    when (length histoKeys <= 0) $
        Left ErrHistogramEmpty
    when (any (flip notElem acceptedKeys) histoKeys) $
        Left ErrHistogramNamesInvalid
    when (any (< 0) histoElems) $
        Left ErrHistogramUpperBoundsNegative
    when (allStakes < 0) $
        Left ErrAllStakesNegative
    when (allStakes < minPossibleValue && allStakes > maxPossibleValue) $
        Left ErrAllStakesValueNotCompatibleWithHistogram

    pure UtxoStatistics
        { theHistogram = histoBars
        , theAllStakes = allStakes
        }

eitherToParser :: Buildable a => Either a b -> Parser b
eitherToParser =
    either (fail . formatToString build) pure

toMap :: [HistogramBar] -> Map Word64 Word64
toMap = Map.fromList . map (\(HistogramBarCount key val) -> (key,val))

instance Eq UtxoStatistics where
    (UtxoStatistics h s) == (UtxoStatistics h' s') = s == s' && toMap h == toMap h'

instance ToJSON UtxoStatistics where
    toJSON (UtxoStatistics bars allStakes) =
        let histogramObject = Object . HMS.fromList . map extractBarKey
            extractBarKey (HistogramBarCount bound stake) = (show bound) .= stake
        in object [ "histogram" .= histogramObject bars
                  , "allStakes" .= allStakes ]

instance FromJSON UtxoStatistics where
    parseJSON = withObject "UtxoStatistics" $ \o -> do
        histo <- o .: "histogram" :: Parser (Map Word64 Word64)
        stakes <- o .: "allStakes"
        eitherToParser $ mkUtxoStatistics histo stakes


data UtxoStatisticsError
    = ErrHistogramEmpty
    | ErrHistogramNamesInvalid
    | ErrHistogramUpperBoundsNegative
    | ErrAllStakesNegative
    | ErrAllStakesValueNotCompatibleWithHistogram
    deriving (Show)


getPossibleBounds :: Map Word64 Word64 -> (Word64, Word64)
getPossibleBounds histogram =
    (calculatePossibleBound fst, calculatePossibleBound snd)
    where
        createBracketPairs :: Num a => [a] -> [(a,a)]
        createBracketPairs (reverse -> (x:xs)) = zip (map (+1) $ reverse (xs ++ [0])) (reverse (x:xs))
        createBracketPairs _ = []
        matching fromPair (key,value) =
            map ( (*value) . fromPair ) . filter (\(_,upper) -> key == upper)
        acceptedKeys = NL.toList $ generateBounds Log10
        calculatePossibleBound fromPair =
            sum .
            concatMap (\pair -> matching fromPair pair $ createBracketPairs acceptedKeys) $
            Map.toList histogram


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
        ErrAllStakesValueNotCompatibleWithHistogram ->
            bprint "Utxo statistics allStakes has value that is not possible given histogram distribution"

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
-- This code goes into nonstoping computation when checking swagger integration of WalletResponse UtxoStatistics
{--        do
        histogram <- arbitrary
        let (minPossibleValue, maxPossibleValue) = getPossibleBounds histogram
        let histoBars = map (uncurry HistogramBarCount) $ Map.toList histogram
        allStakes <- arbitrary `suchThat` (\s -> s >= minPossibleValue && s <= maxPossibleValue)
        return $ UtxoStatistics histoBars allStakes
--}
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
computeUtxoStatistics = L.fold $ UtxoStatistics
    <$> foldBuckets (generateBounds Log10)
    <*> L.sum

foldBuckets :: NonEmpty Word64 -> L.Fold Word64 [HistogramBar]
foldBuckets bounds =
    let
        step :: Map Word64 Word64 -> Word64 -> Map Word64 Word64
        step x a =
            case Map.lookupGE a x of
                Just (k, v) -> Map.insert k (v+1) x
                Nothing     -> Map.adjust (+1) (head bounds) x
        initial :: Map Word64 Word64
        initial =
            Map.fromList $ zip (NL.toList bounds) (repeat 0)
        extract :: Map Word64 Word64 -> [HistogramBar]
        extract =
            map (uncurry HistogramBarCount) . Map.toList
    in
        L.Fold step initial extract
