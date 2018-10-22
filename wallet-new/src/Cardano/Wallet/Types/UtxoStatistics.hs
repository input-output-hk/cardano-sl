{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE ViewPatterns  #-}

module Cardano.Wallet.Types.UtxoStatistics
    ( -- * Types
      UtxoStatistics
    , BoundType
    , UtxoStatisticsError(..)

    -- * Constructing 'UtxoStatistics'
    , computeUtxoStatistics

    -- * Constructing 'BoundType'
    , log10
    )  where


import           Universum

import           Control.Lens (at, (?~))
import           Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..),
                     genericParseJSON, genericToJSON, object, withObject, (.:),
                     (.=))
import           Data.Aeson.Types (Parser)
import           Data.Swagger (NamedSchema (..), Referenced (..),
                     SwaggerType (..), ToSchema (..), declareSchemaRef,
                     genericDeclareNamedSchema, minimum_, properties, required,
                     type_)
import           Data.Word (Word64)
import           Formatting (bprint, build, (%))
import           Serokell.Util (listJson)
import           Test.QuickCheck (Arbitrary (..), arbitrary, choose, elements,
                     infiniteListOf, shuffle)

import           Cardano.Wallet.API.V1.Swagger.Example (Example)
import           Cardano.Wallet.Util (eitherToParser)
import           Pos.Chain.Txp (TxOut (..), TxOutAux (..), Utxo)
import           Pos.Core.Common (Coin (..))
import           Pos.Infra.Util.LogSafe (BuildableSafeGen (..),
                     deriveSafeBuildable)

import qualified Control.Foldl as L
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HMS
import qualified Data.List.NonEmpty as NL
import qualified Data.Map.Strict as Map
import qualified Data.Swagger as Swagger
import qualified Formatting.Buildable


--
-- TYPES
--

data UtxoStatistics = UtxoStatistics
    { theHistogram :: ![HistogramBar]
    , theAllStakes :: !Word64
    } deriving (Show, Generic, Ord)

data UtxoStatisticsError
    = ErrEmptyHistogram
    | ErrInvalidBounds !Text
    | ErrInvalidTotalStakes !Text
    deriving (Eq, Show, Read, Generic)

--  Buckets boundaries can be constructed in different ways
data BoundType = Log10 deriving (Eq, Show, Read, Generic)

instance ToJSON BoundType where
    toJSON = genericToJSON aesonEnumOpts

instance FromJSON BoundType where
    parseJSON = genericParseJSON aesonEnumOpts

instance ToSchema BoundType where
    declareNamedSchema = genericDeclareNamedSchema Swagger.defaultSchemaOptions

instance Buildable UtxoStatisticsError where
    build = \case
        ErrEmptyHistogram ->
            bprint "Utxo statistics histogram cannot be empty."
        ErrInvalidBounds err ->
            bprint ("Utxo statistics have invalid bounds: "%build%".") err
        ErrInvalidTotalStakes err ->
            bprint ("Utxo statistics have invalid total stakes: "%build%".") err

instance Eq UtxoStatistics where
    (UtxoStatistics h s) == (UtxoStatistics h' s') =
        s == s' && sorted h == sorted h'
      where
        sorted :: [HistogramBar] -> [HistogramBar]
        sorted = sortOn (\(HistogramBarCount key _) -> key)

instance ToJSON UtxoStatistics where
    toJSON (UtxoStatistics bars allStakes) =
        let
            histogramObject =
                Object . HMS.fromList . map extractBarKey

            extractBarKey (HistogramBarCount bound stake) =
                show bound .= stake
        in
            object
                [ "histogram" .= histogramObject bars
                , "allStakes" .= allStakes
                , "boundType" .= log10
                ]

instance FromJSON UtxoStatistics where
    parseJSON = withObject "UtxoStatistics" parseUtxoStatistics
      where
        parseUtxoStatistics :: Object -> Parser UtxoStatistics
        parseUtxoStatistics o =
            eitherToParser =<< mkUtxoStatistics
                <$> (o .: "boundType")
                <*> (o .: "histogram")
                <*> (o .: "allStakes")

instance Arbitrary UtxoStatistics where
    arbitrary = do
        upperBounds <- shuffle (NL.toList $ generateBounds Log10)
        counts <- infiniteListOf arbitrary
        let histogram = zip upperBounds counts
        let histoBars = map (uncurry HistogramBarCount) histogram
        allStakes <- choose (getPossibleBounds $ Map.fromList histogram)
        return $ UtxoStatistics histoBars allStakes

instance BuildableSafeGen UtxoStatistics where
    buildSafeGen _ UtxoStatistics{..} = bprint ("{"
        %" histogram="%build
        %" allStakes="%build
        %" }")
        theHistogram
        theAllStakes

instance Example UtxoStatistics

instance ToSchema UtxoStatistics where
    declareNamedSchema _ = do
        wordRef <- declareSchemaRef (Proxy :: Proxy Word64)
        btypeRef <- declareSchemaRef (Proxy :: Proxy BoundType)
        pure $ NamedSchema (Just "UtxoStatistics") $ mempty
            & type_ .~ SwaggerObject
            & required .~ ["histogram", "allStakes"]
            & properties .~ (mempty
                & at "boundType" ?~ btypeRef
                & at "allStakes" ?~ (Inline $ mempty
                    & type_ .~ SwaggerNumber
                    & minimum_ .~ Just 0
                )
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
            )

--
-- CONSTRUCTING
--

-- | Smart-constructor to create bounds using a log-10 scale
log10 :: BoundType
log10 = Log10
{-# INLINE log10 #-}

-- | Compute UtxoStatistics from a bunch of UTXOs
computeUtxoStatistics :: BoundType -> [Utxo] -> UtxoStatistics
computeUtxoStatistics btype =
    L.fold foldStatistics . concatMap getCoins
  where
    getCoins :: Utxo ->  [Word64]
    getCoins =
        map (getCoin . txOutValue . toaOut) . Map.elems

    foldStatistics :: L.Fold Word64 UtxoStatistics
    foldStatistics = UtxoStatistics
        <$> foldBuckets (generateBounds btype)
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

--
-- INTERNALS
--

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

instance Example HistogramBar

instance Arbitrary HistogramBar where
    arbitrary = do
        upperBound <- elements (NL.toList $ generateBounds log10)
        count <- arbitrary
        pure (HistogramBarCount upperBound count)

instance Buildable [HistogramBar] where
    build =
        bprint listJson

instance BuildableSafeGen HistogramBar where
    buildSafeGen _ HistogramBarCount{..} =
        bprint ("{"
                %" upperBound="%build
                %" count="%build
                %" }")
        bucketUpperBound
        bucketCount

mkUtxoStatistics
    :: BoundType
    -> Map Word64 Word64
    -> Word64
    -> Either UtxoStatisticsError UtxoStatistics
mkUtxoStatistics btype histogram allStakes = do
    let (histoKeys, histoElems) = (Map.keys histogram, Map.elems histogram)
    let acceptedKeys = NL.toList $ generateBounds btype
    let (minPossibleValue, maxPossibleValue) = getPossibleBounds histogram
    let constructHistogram = uncurry HistogramBarCount
    let histoBars = map constructHistogram $ Map.toList histogram

    when (length histoKeys <= 0) $
        Left ErrEmptyHistogram
    when (any (`notElem` acceptedKeys) histoKeys) $
        Left $ ErrInvalidBounds $ "given bounds are incompatible with bound type (" <> show btype <> ")"
    when (any (< 0) histoElems) $
        Left $ ErrInvalidBounds "encountered negative bound"
    when (allStakes < 0) $
        Left $ ErrInvalidTotalStakes "total stakes is negative"
    when (allStakes < minPossibleValue && allStakes > maxPossibleValue) $
        Left $ ErrInvalidTotalStakes "inconsistent total stakes & histogram"

    pure UtxoStatistics
        { theHistogram = histoBars
        , theAllStakes = allStakes
        }

generateBounds :: BoundType -> NonEmpty Word64
generateBounds bType =
    let (^!) :: Word64 -> Word64 -> Word64
        (^!) = (^)
    in case bType of
        Log10 -> NL.fromList $ map (\toPower -> 10 ^! toPower) [1..16] ++ [45 * (10 ^! 15)]

getPossibleBounds :: Map Word64 Word64 -> (Word64, Word64)
getPossibleBounds histogram =
    (calculatePossibleBound fst, calculatePossibleBound snd)
  where
    createBracketPairs :: Num a => [a] -> [(a,a)]
    createBracketPairs (reverse -> (x:xs)) = zip (map (+1) $ reverse (xs ++ [0])) (reverse (x:xs))
    createBracketPairs _ = []
    matching fromPair (key,value) =
        map ( (*value) . fromPair ) . filter (\(_,upper) -> key == upper)
    acceptedKeys = NL.toList $ generateBounds log10
    calculatePossibleBound fromPair =
        sum .
        concatMap (\pair -> matching fromPair pair $ createBracketPairs acceptedKeys) $
        Map.toList histogram

aesonEnumOpts :: Aeson.Options
aesonEnumOpts = Aeson.defaultOptions
    { Aeson.tagSingleConstructors = True
    }


-- | TH at the end because it needs mostly everything to be declared first
deriveSafeBuildable ''UtxoStatistics
deriveSafeBuildable ''HistogramBar
