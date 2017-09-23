{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for core.

module Pos.Arbitrary.Core
       ( CoinPairOverflowSum (..)
       , CoinPairOverflowSub (..)
       , CoinPairOverflowMul (..)
       , DoubleInZeroToOneRange (..)
       , EoSToIntOverflow (..)
       , IntegerToCoinNoOverflow (..)
       , IntegerToCoinOverflow (..)
       , LessThanZeroOrMoreThanOne (..)
       , SafeCoinPairMul (..)
       , SafeCoinPairSum (..)
       , SafeCoinPairSub (..)
       , SafeWord (..)
       , UnreasonableEoS (..)
       ) where

import           Universum

import qualified Data.ByteString                   as BS (pack)
import qualified Data.Map                          as M
import           Data.Time.Units                   (Microsecond, Millisecond,
                                                    TimeUnit (..))
import           System.Random                     (Random)
import           Test.QuickCheck                   (Arbitrary (..), Gen, NonNegative (..),
                                                    choose, oneof, scale, shrinkIntegral,
                                                    sized, suchThat, vector, vectorOf)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)
import           Test.QuickCheck.Instances         ()

import           Pos.Arbitrary.Crypto              ()
import           Pos.Binary.Class                  (FixedSizeInt (..), SignedVarInt (..),
                                                    TinyVarInt (..), UnsignedVarInt (..))
import           Pos.Binary.Core                   ()
import           Pos.Binary.Crypto                 ()
import           Pos.Core.Address                  (makeAddress, makePubKeyAddressBoot)
import           Pos.Core.Coin                     (coinToInteger, divCoin, unsafeSubCoin)
import           Pos.Core.Constants                (sharedSeedLength)
import           Pos.Core.Context                  (HasCoreConstants, epochSlots)
import qualified Pos.Core.Fee                      as Fee
import qualified Pos.Core.Genesis                  as G
import qualified Pos.Core.Slotting                 as Types
import qualified Pos.Core.Types                    as Types
import           Pos.Crypto                        (createPsk, toPublic)
import           Pos.Data.Attributes               (Attributes (..), UnparsedFields (..))
import           Pos.Util.Util                     (leftToPanic)

{- NOTE: Deriving an 'Arbitrary' instance
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(As of derive-2.6.2)

Using, as an example,

    {-# LANGUAGE TemplateHaskell #-}

    import Data.Derive.TH (derive, makeArbitrary)

    data A = A
        { getA1 :: [(String, Int)]
        , getA2 :: Float
        } deriving (Show, Eq, Generic)
    -- `A`'s inner types can be anything for which the constraints make sense

    derive makeArbitrary ''A

means the generated 'Arbitrary' instance uses the default 'shrink' implementation:

    shrink = []

'Pos.Util.Util.dumpSplices' can be used to verify this.'
-}

instance Arbitrary Types.Script where
    arbitrary = genericArbitrary
    shrink = genericShrink

deriving instance Arbitrary Types.BlockCount
deriving instance Arbitrary Types.ChainDifficulty

----------------------------------------------------------------------------
-- Slotting
----------------------------------------------------------------------------

deriving instance Arbitrary Types.SlotCount

maxReasonableEpoch :: Integral a => a
maxReasonableEpoch = 5 * 1000 * 1000 * 1000 * 1000  -- 5 * 10^12, because why not

deriving instance Random Types.EpochIndex

instance Arbitrary Types.EpochIndex where
    arbitrary = choose (0, maxReasonableEpoch)
    shrink = genericShrink

instance HasCoreConstants => Arbitrary Types.LocalSlotIndex where
    arbitrary =
        leftToPanic "arbitrary@LocalSlotIndex: " . Types.mkLocalSlotIndex <$>
        choose (Types.getSlotIndex minBound, Types.getSlotIndex maxBound)
    shrink = genericShrink

instance HasCoreConstants => Arbitrary Types.SlotId where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasCoreConstants => Arbitrary Types.EpochOrSlot where
    arbitrary = oneof [
          Types.EpochOrSlot . Left <$> arbitrary
        , Types.EpochOrSlot . Right <$> arbitrary
        ]
    shrink = genericShrink

-- | A wrapper over 'EpochOrSlot'. When converted to 'EpochOrSlot' via 'fromEnum', using
-- this type ensures there's an exception.
newtype EoSToIntOverflow = EoSToIntOverflow
    { getEoS :: Types.EpochOrSlot
    } deriving (Show, Eq, Generic)

instance HasCoreConstants => Arbitrary EoSToIntOverflow where
    arbitrary = EoSToIntOverflow <$> do
        let maxIntAsInteger = toInteger (maxBound :: Int)
            maxW64 = toInteger (maxBound :: Word64)
            (minDiv, minMod) = maxIntAsInteger `divMod` (fromIntegral $ succ epochSlots)
            maxDiv = maxW64 `div` (1 + fromIntegral epochSlots)
        leftEpoch <- Types.EpochIndex . fromIntegral <$> choose (minDiv + 1, maxDiv)
        localSlot <-
            leftToPanic "arbitrary@EoSToIntOverflow" .
            Types.mkLocalSlotIndex .
            fromIntegral <$> choose (minMod, toInteger epochSlots)
        let rightEpoch = Types.EpochIndex . fromIntegral $ minDiv
        Types.EpochOrSlot <$>
            oneof [ pure $ Left leftEpoch
                  , pure $ Right Types.SlotId { siEpoch = rightEpoch
                                              , siSlot = localSlot}
                  ]
    shrink = genericShrink

-- | Wrapper over 'EpochOrSlot'. Its 'Arbitrary' instance is made to guarantee its
-- 'EpochIndex' is in the interval (maxReasonableEpoch, maxBound :: Word64 ].
-- This is to ensure the property 'toEnum . fromEnum = id' holds for all 'EpochOrSlot',
-- not just the ones whose 'EpochIndex' uses the "reasonable" 'Arbitrary' instance.
newtype UnreasonableEoS = Unreasonable
    { getUnreasonable :: Types.EpochOrSlot
    } deriving (Show, Eq, Generic)

instance HasCoreConstants => Arbitrary UnreasonableEoS where
    arbitrary = Unreasonable . Types.EpochOrSlot <$> do
        let maxI = (maxBound :: Int) `div` (1 + fromIntegral epochSlots)
        localSlot <- arbitrary
        let lsIntegral = fromIntegral . Types.getSlotIndex $ localSlot
        let epoch n = Types.EpochIndex <$>
                choose (succ maxReasonableEpoch
                       , fromIntegral maxI - (n * fromIntegral (succ epochSlots)))
        leftEpoch <- Left <$> epoch 0
        rightSlot <- Right . (flip Types.SlotId localSlot) <$> epoch lsIntegral
        oneof [ pure leftEpoch
              , pure rightSlot
              ]
    shrink = genericShrink

----------------------------------------------------------------------------
-- Address and related
----------------------------------------------------------------------------

instance Arbitrary Types.AddrType where
    arbitrary =
        oneof
            [ pure Types.ATPubKey
            , pure Types.ATScript
            , pure Types.ATRedeem
            , Types.ATUnknown <$> choose (3, maxBound)
            ]

instance Arbitrary Types.AddrSpendingData where
    arbitrary =
        oneof
            [ Types.PubKeyASD <$> arbitrary
            , Types.ScriptASD <$> arbitrary
            , Types.RedeemASD <$> arbitrary
            -- For unknown spending data payload will be at most 120
            -- bytes long.
            , Types.UnknownASD <$> choose (3, 255) <*> scale (min 120) arbitrary
            ]

instance Arbitrary Types.AddrStakeDistribution where
    arbitrary =
        oneof
            [ pure Types.BootstrapEraDistr
            , Types.SingleKeyDistr <$> arbitrary
            , leftToPanic "arbitrary @AddrStakeDistribution: " .
              Types.mkMultiKeyDistr <$>
              genMultiKeyDistr
            ]
      where
        genMultiKeyDistr :: Gen (Map Types.StakeholderId Types.CoinPortion)
        -- We don't want to generate too much, hence 'scale'.
        genMultiKeyDistr =
            scale (min 16) $ do
                holder0 <- arbitrary
                holder1 <- arbitrary `suchThat` (/= holder0)
                moreHolders <- arbitrary @[Types.StakeholderId]
                -- Must be at least 2 non-repeating stakeholders.
                let holders = ordNub (holder0 : holder1 : moreHolders)
                portions <- genPortions (length holders) []
                return $ M.fromList $ holders `zip` portions
        genPortions :: Int -> [Types.CoinPortion] -> Gen [Types.CoinPortion]
        genPortions 0 res = pure res
        genPortions n res = do
            let limit =
                    foldl' (-) Types.coinPortionDenominator $
                    map Types.getCoinPortion res
            let unsafeMkCoinPortion =
                    leftToPanic @Text "genPortions" . Types.mkCoinPortion
            case (n, limit) of
                -- Limit is exhausted, can't create more.
                (_, 0) -> return res
                -- The last portion, we must ensure the sum is correct.
                (1, _) -> return (unsafeMkCoinPortion limit : res)
                -- We intentionally don't generate 'limit', because we
                -- want to generate at least 2 portions.  However, if
                -- 'limit' is 1, we will generate 1, because we must
                -- have already generated one portion.
                _ -> do
                    portion <-
                        unsafeMkCoinPortion <$> choose (1, max 1 (limit - 1))
                    genPortions (n - 1) (portion : res)

instance Arbitrary Types.AddrAttributes where
    arbitrary = genericArbitrary
    shrink = genericShrink

deriving instance Arbitrary Types.Address'

instance Arbitrary Types.Address where
    arbitrary = makeAddress <$> arbitrary <*> arbitrary
    shrink = genericShrink

----------------------------------------------------------------------------
-- Attributes
----------------------------------------------------------------------------

instance Arbitrary UnparsedFields where
    arbitrary = sized $ go M.empty
        where
            go !acc 0 = pure $ UnparsedFields acc
            go !acc n = do
                -- Assume that data type doesn't have more than 100 constructors.
                k <- choose (100, maxBound)
                v <- arbitrary
                go (M.insert k v acc) (n - 1)
    shrink = genericShrink

instance Arbitrary h => Arbitrary (Attributes h) where
    arbitrary = genericArbitrary
    shrink = genericShrink

----------------------------------------------------------------------------
-- Coin
----------------------------------------------------------------------------

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

instance Arbitrary Types.SoftforkRule where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Types.BlockVersionData where
    arbitrary = genericArbitrary
    shrink = genericShrink

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

instance Arbitrary G.GenesisDelegation where
    arbitrary =
        leftToPanic "arbitrary@GenesisDelegation" . G.mkGenesisDelegation <$> do
            secretKeys <- sized vector
            return $
                case secretKeys of
                    [] -> []
                    (delegate:issuers) ->
                        issuers <&> \sk -> createPsk sk (toPublic delegate) 0

instance Arbitrary G.GenesisCoreData where
    arbitrary = do
        -- This number'll be the length of every address list in the first argument of
        -- 'mkGenesisCoreData'.
        innerLen <- getNonNegative <$> arbitrary `suchThat` (<= (NonNegative 7))
        -- This number is the length of the first argument of 'mkGenesisCoreData'
        -- Because of the way 'PublicKey's are generated, 'innerLen * outerLen' cannot be
        -- greater than 50 if a list of unique adresses with that length is to be
        -- generated.
        -- '7 = (floor . sqrt) 50', and if 'a * b = 50', then at least one of 'a' or 'b'
        -- must be less than or equalto '7'.
        outerLen <- getNonNegative <$>
            arbitrary `suchThat` (\(NonNegative n) -> n * innerLen <= 50)
        let chop _ [] = []
            chop n l = taken : chop n dropped
              where (taken, dropped) = splitAt n l
        allAddrs <- fmap makePubKeyAddressBoot <$>
            vector (outerLen * innerLen)
        let listOfAddrList = chop innerLen allAddrs
        -- This may seem like boilerplate but it's necessary to pass the first check in
        -- 'mkGenesisCoreData'. Certain parameters in the generated 'BalanceDistribution'
        -- must be equal to the length of the first element of the tuple in
        -- 'AddrDistribution'
            wordILen = fromIntegral innerLen
            distributionGen = oneof
                [ G.FlatBalances wordILen <$> arbitrary
                , do a <- choose (0, wordILen)
                     G.RichPoorBalances a
                         <$> arbitrary
                         <*> pure (wordILen - a)
                         <*> arbitrary
                , pure $ G.safeExpBalances wordILen
                , G.CustomBalances <$> vector innerLen
                ]
        stakeDistrs <- vectorOf outerLen distributionGen
        hashmapOfHolders <- arbitrary :: Gen (Map Types.StakeholderId Word16)
        delegation <- arbitrary
        return $ leftToPanic "arbitrary@GenesisCoreData: " $
            G.mkGenesisCoreData (zip listOfAddrList stakeDistrs)
                                hashmapOfHolders
                                delegation

instance Arbitrary G.BalanceDistribution where
    arbitrary = oneof
      [ do stakeholders <- choose (1, 10000)
           coins <- Types.mkCoin <$> choose (stakeholders, 20*1000*1000*1000)
           return (G.FlatBalances (fromIntegral stakeholders) coins)
      , do sdRichmen <- choose (0, 20)
           sdRichBalance <- Types.mkCoin <$> choose (100000, 5000000)
           sdPoor <- choose (0, 20)
           sdPoorBalance <- Types.mkCoin <$> choose (1000, 50000)
           return G.RichPoorBalances{..}
      , G.safeExpBalances <$> choose (0::Integer, 20)
      , G.CustomBalances <$> arbitrary
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
deriving instance Arbitrary Types.TimeDiff

deriving instance Arbitrary a => Arbitrary (UnsignedVarInt a)
deriving instance Arbitrary a => Arbitrary (SignedVarInt a)
deriving instance Arbitrary a => Arbitrary (FixedSizeInt a)
deriving instance Arbitrary TinyVarInt
