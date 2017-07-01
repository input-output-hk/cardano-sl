{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for core.

module Pos.Core.Arbitrary
       ( CoinPairOverflowSum (..)
       , CoinPairOverflowSub (..)
       , CoinPairOverflowMul (..)
       , DoubleInZeroToOneRange (..)
       , IntegerToCoinNoOverflow (..)
       , IntegerToCoinOverflow (..)
       , LessThanZeroOrMoreThanOne (..)
       , SafeCoinPairMul (..)
       , SafeCoinPairSum (..)
       , SafeCoinPairSub (..)
       , SafeWord (..)
       , SmallHashMap (..)
       ) where

import           Universum

import qualified Data.ByteString                   as BS (pack)
import           Data.Time.Units                   (Microsecond, Millisecond,
                                                    TimeUnit (..))
import           System.Random                     (Random)
import           Test.QuickCheck                   (Arbitrary (..), Gen, choose, oneof,
                                                    scale, shrinkIntegral, suchThat)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)
import           Test.QuickCheck.Instances         ()

import           Pos.Binary.Class                  (AsBinary, FixedSizeInt (..),
                                                    SignedVarInt (..),
                                                    UnsignedVarInt (..))
import           Pos.Binary.Core                   ()
import           Pos.Binary.Crypto                 ()
import           Pos.Core.Address                  (makePubKeyAddress, makeRedeemAddress,
                                                    makeScriptAddress)
import           Pos.Core.Coin                     (coinToInteger, divCoin, unsafeSubCoin)
import           Pos.Core.Constants                (sharedSeedLength)
import qualified Pos.Core.Fee                      as Fee
import qualified Pos.Core.Genesis                  as G
import qualified Pos.Core.Types                    as Types
import           Pos.Crypto                        (PublicKey, Share)
import           Pos.Crypto.Arbitrary              ()
import           Pos.Data.Attributes               (Attributes (..))
import           Pos.Util.Arbitrary                (makeSmall)
import           Pos.Util.Util                     (leftToPanic)

----------------------------------------------------------------------------
-- Arbitrary core types
----------------------------------------------------------------------------

instance Arbitrary Types.Script where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Types.Address where
    arbitrary = oneof [
        makePubKeyAddress <$> arbitrary,
        makeScriptAddress <$> arbitrary,
        makeRedeemAddress <$> arbitrary,
        Types.UnknownAddressType <$> choose (3, 255) <*> scale (min 150) arbitrary
        ]

deriving instance Arbitrary Types.ChainDifficulty

maxReasonableEpoch :: Integral a => a
maxReasonableEpoch = 5 * 1000 * 1000 * 1000 * 1000  -- 5 * 10^12, because why not

deriving instance Random Types.EpochIndex

instance Arbitrary Types.EpochIndex where
    arbitrary = choose (0, maxReasonableEpoch)
    shrink = genericShrink

instance Arbitrary Types.LocalSlotIndex where
    arbitrary =
        leftToPanic "arbitrary@LocalSlotIndex: " . Types.mkLocalSlotIndex <$>
        choose (Types.getSlotIndex minBound, Types.getSlotIndex maxBound)
    shrink = genericShrink

{- NOTE: Deriving an 'Arbitrary' instance
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(As of derive-2.6.2)

Using, as an example,

    {-# LANGUAGE TemplateHaskell #-}

    import Data.Derive.TH (derive, makeArbitrary)

    data A = A
        { getA  :: [(String, Int)]
        , getA2 :: Float
        } deriving (Show, Eq, Generic)
    -- `A`'s inner types can be anything for which the constraints make sense

    derive makeArbitrary ''A

means the generated 'Arbitrary' instance uses the default 'shrink' implementation:

    shrink = []

'Pos.Util.Util.dumpSplices' can be used to verify this.'
-}

instance Arbitrary Types.SlotId where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Types.EpochOrSlot where
    arbitrary = oneof [
          Types.EpochOrSlot . Left <$> arbitrary
        , Types.EpochOrSlot . Right <$> arbitrary
        ]
    shrink = genericShrink

instance Arbitrary Types.Coin where
    arbitrary = Types.mkCoin <$> choose (1, Types.unsafeGetCoin maxBound)
    shrink = genericShrink

-- | This datatype has two coins that will always overflow when added.
-- It is used in tests to make sure addition raises the appropriate exception when this
-- happens.
newtype CoinPairOverflowSum = TwoCoinsSum
    { get2CSum :: (Types.Coin, Types.Coin)
    } deriving (Show, Eq)

instance Arbitrary CoinPairOverflowSum where
    arbitrary = do
        c1 <- arbitrary
        let lowerBound = succ $ coinToInteger $ (maxBound @Types.Coin) `unsafeSubCoin` c1
            upperBound = coinToInteger (maxBound @Types.Coin)
        c2 <- Types.mkCoin . fromIntegral <$> choose (lowerBound, upperBound)
        return $ TwoCoinsSum (c1, c2)

-- | This datatype has two coins that will never overflow when added.
-- It is therefore safe to add them. Useful in tests to ensure adding two coins whose sum
-- is a valid 'Coin' always works.
newtype SafeCoinPairSum = CoinPairSum
    { getPairSum :: (Types.Coin, Types.Coin)
    } deriving (Show, Eq)

instance Arbitrary SafeCoinPairSum where
    arbitrary = do
        c1 <- arbitrary
        let upperBound = Types.unsafeGetCoin c1
            highestBound = Types.unsafeGetCoin maxBound
        c2 <- Types.mkCoin <$> choose (0, highestBound - upperBound)
        return $ CoinPairSum (c1, c2)

-- | This datatype has two coins that will always underflow when subtracted.
-- It is used in tests to make sure subtraction raises the appropriate exception when this
-- happens.
newtype CoinPairOverflowSub = TwoCoinsSub
    { get2CSub :: (Types.Coin, Types.Coin)
    } deriving (Show, Eq)

instance Arbitrary CoinPairOverflowSub where
    arbitrary = do
        firstCoin <- arbitrary
        let firstWord = Types.unsafeGetCoin firstCoin
            c1 = if firstCoin == maxBound
                then Types.mkCoin $ firstWord - 1
                else firstCoin
        c2 <- arbitrary `suchThat` (> c1)
        return $ TwoCoinsSub (c1, c2)

-- | This datatype has two coins that will never underflow when subtracted.
-- It is therefore safe to subtract them. Useful in tests to show that two coins whose
-- subtraction does not underflow always works.
newtype SafeCoinPairSub = CoinPairSub
    { getPairSub :: (Types.Coin, Types.Coin)
    } deriving (Show, Eq)

instance Arbitrary SafeCoinPairSub where
    arbitrary = do
        c1 <- arbitrary
        let upperBound = Types.unsafeGetCoin c1
        c2 <- Types.mkCoin <$> choose (0, upperBound)
        return $ CoinPairSub (c1, c2)

-- | This datatype has a 'Coin' and an 'Integer' that will always overflow when
-- multiplied.
-- It is used in tests to make sure multiplication raises the appropriate exception when
-- this happens.
newtype CoinPairOverflowMul = TwoCoinsM
    { get2CMul :: (Types.Coin, Integer)
    } deriving (Show, Eq)

instance Arbitrary CoinPairOverflowMul where
    arbitrary = do
        c1 <- arbitrary
        let integralC1 = coinToInteger c1
            lowerBound =
                1 + (coinToInteger $ (maxBound @Types.Coin) `divCoin` integralC1)
            upperBound = coinToInteger (maxBound @Types.Coin)
        c2 <- fromIntegral @Integer <$> choose (lowerBound, upperBound)
        return $ TwoCoinsM (c1, c2)

-- | This datatype has a 'Coin' and an 'Integer'  that will always overflow when
-- multiplied.
-- It is used to make sure coin multiplication by an integer raises the appropriate
-- exception when this happens.
newtype SafeCoinPairMul = CoinPairMul
    { getPairMul :: (Types.Coin, Integer)
    } deriving (Show, Eq)

instance Arbitrary SafeCoinPairMul where
    arbitrary = do
        c1 <- arbitrary
        let upperBound = coinToInteger c1
            highestBound = coinToInteger maxBound
        c2 <- choose (0, div highestBound upperBound)
        return $ CoinPairMul (c1, c2)

-- | 'IntegerToCoinOverflow' is a wrapped over 'Integer'. Its 'Arbitrary' instance makes
-- it so that these integers will always overflow when converted into a 'Coin'.
-- Used in tests to make sure an exception is raised when there is an attempt to turn an
-- excessively large 'Integer' into a 'Coin'.
newtype IntegerToCoinOverflow = LargeInteger
    { getLargeInteger :: Integer
    } deriving (Show, Eq)

instance Arbitrary IntegerToCoinOverflow where
    arbitrary = LargeInteger <$> do
        n <- succ . fromIntegral <$> (arbitrary :: Gen Word)
        let lowerBound = succ . coinToInteger $ maxBound @Types.Coin
        num <- choose (lowerBound, n * lowerBound)
        return $ toInteger num

-- | This datatype has an Integer that will never overflow when turned into a 'Coin'.
-- Useful for testing that conversion between valid 'Integer's and 'Coin's works properly.
newtype IntegerToCoinNoOverflow = Integer
    { getInteger :: Integer
    } deriving (Show, Eq)

instance Arbitrary IntegerToCoinNoOverflow where
    arbitrary =
      Integer . fromIntegral <$> choose (0, Types.unsafeGetCoin $ maxBound @Types.Coin)

instance Arbitrary Types.CoinPortion where
    arbitrary = Types.unsafeCoinPortionFromDouble . (1/) <$> choose (1, 20)

-- | A wrapper over 'Double'. Its 'Arbitrary' instance ensures the 'Double' within can
-- never be converted into a 'CoinPortion' without an exception being raised. Used in
-- tests to safeguard that converting an invalid 'Double' to a 'CoinPortion' always
-- raised an exception.
newtype LessThanZeroOrMoreThanOne = BadCoinPortion
    { getDouble :: Double
    } deriving (Show, Eq)

instance Arbitrary LessThanZeroOrMoreThanOne where
    arbitrary = BadCoinPortion <$> do
        d <- arbitrary
        return $ if (d >= 0 && d <= 1)
            then 10 / d
            else d

-- | Another wrapper over 'Double'. Its 'Arbitrary' instance guarantees the 'Double'
-- inside can always be safely turned into a 'CoinPortion'. Used in tests to ensure
-- converting a valid 'Double' to/from 'CoinPortion' works properly.
newtype DoubleInZeroToOneRange = DoubleInRange
    { getDoubleInRange :: Double
    } deriving (Show, Eq)

instance Arbitrary DoubleInZeroToOneRange where
    arbitrary = DoubleInRange <$> choose (0, 1)

-- | A wrapper over 'Word64'. Its 'Arbitrary' instance guarantees the 'Word64'
-- inside can always be safely converted into 'CoinPortion'. Used in tests to ensure
-- converting a valid 'Word64' to/from 'CoinPortion' works properly.
newtype SafeWord = SafeWord
    { getSafeWord :: Word64
    } deriving (Show, Eq)

instance Arbitrary SafeWord where
    arbitrary = SafeWord . Types.getCoinPortion <$> arbitrary

instance Arbitrary Types.SharedSeed where
    arbitrary = do
        bs <- replicateM sharedSeedLength (choose (0, 255))
        return $ Types.SharedSeed $ BS.pack bs

----------------------------------------------------------------------------
-- Arbitrary types from MainExtra[header/body]data
----------------------------------------------------------------------------

instance Arbitrary Types.ApplicationName where
    arbitrary =
        either (error . mappend "arbitrary @ApplicationName failed: ") identity .
        Types.mkApplicationName .
        toText . map (chr . flip mod 128) . take Types.applicationNameMaxLength <$>
        arbitrary
    shrink = genericShrink

instance Arbitrary Types.BlockVersion where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Types.SoftwareVersion where
    arbitrary = genericArbitrary
    shrink = genericShrink

----------------------------------------------------------------------------
-- Arbitrary types from 'Pos.Core.Fee'
----------------------------------------------------------------------------

deriving instance Arbitrary Fee.Coeff

instance Arbitrary Fee.TxSizeLinear where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Fee.TxFeePolicy where
    arbitrary = oneof
        [ Fee.TxFeePolicyTxSizeLinear <$> arbitrary
        , do
              policyCode <-
                  -- The lower bound is needed so that
                  -- we don't get codes for known policies.
                  choose (1, maxBound)
              policyPayload <- arbitrary
              return $ Fee.TxFeePolicyUnknown policyCode policyPayload
        ]
    shrink = \case
        Fee.TxFeePolicyTxSizeLinear a ->
            Fee.TxFeePolicyTxSizeLinear <$> shrink a
        Fee.TxFeePolicyUnknown v a ->
            Fee.TxFeePolicyUnknown v <$> shrink a

----------------------------------------------------------------------------
-- Arbitrary types from 'Pos.Core.Genesis'
----------------------------------------------------------------------------

instance Arbitrary G.GenesisCoreData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary G.StakeDistribution where
    arbitrary = oneof
      [ do stakeholders <- choose (1, 10000)
           coins <- Types.mkCoin <$> choose (stakeholders, 20*1000*1000*1000)
           return (G.FlatStakes (fromIntegral stakeholders) coins)
      , do stakeholders <- choose (1, 10000)
           coins <- Types.mkCoin <$> choose (stakeholders, 20*1000*1000*1000)
           return (G.BitcoinStakes (fromIntegral stakeholders) coins)
      , do sdRichmen <- choose (0, 20)
           sdRichStake <- Types.mkCoin <$> choose (100000, 5000000)
           sdPoor <- choose (0, 20)
           sdPoorStake <- Types.mkCoin <$> choose (1000, 50000)
           return G.RichPoorStakes{..}
      , return G.ExponentialStakes
      , G.ExplicitStakes <$> arbitrary
      ]
    shrink = genericShrink

----------------------------------------------------------------------------
-- Arbitrary miscellaneous types
----------------------------------------------------------------------------

instance Arbitrary Millisecond where
    arbitrary = fromMicroseconds <$> choose (0, 600 * 1000 * 1000)
    shrink = shrinkIntegral

instance Arbitrary Microsecond where
    arbitrary = fromMicroseconds <$> choose (0, 600 * 1000 * 1000)
    shrink = shrinkIntegral

deriving instance Arbitrary Types.Timestamp

newtype SmallHashMap =
    SmallHashMap (HashMap PublicKey (HashMap PublicKey (AsBinary Share)))
    deriving (Show, Generic)

instance Arbitrary SmallHashMap where
    arbitrary = SmallHashMap <$> makeSmall arbitrary
    shrink = genericShrink

instance (Arbitrary a, Integral a) => Arbitrary (UnsignedVarInt a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (Arbitrary a, Integral a) => Arbitrary (SignedVarInt a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (Arbitrary a, Integral a) => Arbitrary (FixedSizeInt a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (Attributes a) where
    arbitrary = genericArbitrary
    shrink = genericShrink
