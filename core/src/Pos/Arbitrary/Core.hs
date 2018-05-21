{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
       , UnreasonableEoS (..)

       , genVssCertificate
       , genSlotId
       , genLocalSlotIndex
       ) where

import           Nub (ordNub)
import           Universum

import qualified Data.ByteString as BS (pack)
import           Data.List ((!!))
import qualified Data.Map as M
import           Data.Time.Units (Microsecond, Millisecond, Second, TimeUnit (..), convertUnit)
import           System.Random (Random)
import           Test.QuickCheck (Arbitrary (..), Gen, choose, oneof, scale, shrinkIntegral, sized,
                                  suchThat)

import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)
import           Test.QuickCheck.Instances ()

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Core ()
import           Pos.Binary.Crypto ()
import           Pos.Core.Common (coinToInteger, divCoin, makeAddress, maxCoinVal, unsafeSubCoin)
import qualified Pos.Core.Common.Fee as Fee
import qualified Pos.Core.Common.Types as Types
import           Pos.Core.Configuration (HasGenesisBlockVersionData, HasProtocolConstants,
                                         epochSlots, protocolConstants)
import           Pos.Core.Constants (sharedSeedLength)
import           Pos.Core.Delegation (HeavyDlgIndex (..), LightDlgIndices (..))
import qualified Pos.Core.Genesis as G
import           Pos.Core.ProtocolConstants (ProtocolConstants (..), VssMaxTTL (..), VssMinTTL (..))
import qualified Pos.Core.Slotting as Types
import           Pos.Core.Slotting.Types (SlotId (..), Timestamp (..))
import           Pos.Core.Ssc.Vss (VssCertificate, mkVssCertificate, mkVssCertificatesMapLossy)
import           Pos.Core.Update.Types (BlockVersionData (..))
import qualified Pos.Core.Update.Types as U
import           Pos.Crypto (HasProtocolMagic, ProtocolMagic, createPsk, protocolMagic, toPublic)
import           Pos.Data.Attributes (Attributes (..), UnparsedFields (..))

import           Pos.Merkle (MerkleTree, mkMerkleTree)
import           Pos.Util.Util (leftToPanic)

import           Test.Pos.Crypto.Arbitrary ()

import           Test.Pos.Util.QuickCheck.Arbitrary (nonrepeating)


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

genLocalSlotIndex :: ProtocolConstants -> Gen Types.LocalSlotIndex
genLocalSlotIndex pc = Types.UnsafeLocalSlotIndex <$>
    choose ( Types.getSlotIndex Types.localSlotIndexMinBound
           , Types.getSlotIndex (Types.localSlotIndexMaxBound pc)
           )

instance HasProtocolConstants => Arbitrary Types.LocalSlotIndex where
    arbitrary = genLocalSlotIndex protocolConstants
    shrink = genericShrink

genSlotId :: ProtocolConstants -> Gen Types.SlotId
genSlotId pc = SlotId <$> arbitrary <*> genLocalSlotIndex pc

instance HasProtocolConstants => Arbitrary Types.SlotId where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasProtocolConstants => Arbitrary Types.EpochOrSlot where
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

instance HasProtocolConstants => Arbitrary EoSToIntOverflow where
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

instance HasProtocolConstants => Arbitrary UnreasonableEoS where
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
            case (n, limit) of
                -- Limit is exhausted, can't create more.
                (_, 0) -> return res
                -- The last portion, we must ensure the sum is correct.
                (1, _) -> return (Types.CoinPortion limit : res)
                -- We intentionally don't generate 'limit', because we
                -- want to generate at least 2 portions.  However, if
                -- 'limit' is 1, we will generate 1, because we must
                -- have already generated one portion.
                _ -> do
                    portion <-
                        Types.CoinPortion <$> choose (1, max 1 (limit - 1))
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

instance Arbitrary Types.SharedSeed where
    arbitrary = do
        bs <- replicateM sharedSeedLength (choose (0, 255))
        return $ Types.SharedSeed $ BS.pack bs

instance Arbitrary U.SoftforkRule where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary U.BlockVersionData where
    arbitrary = genericArbitrary
    shrink = genericShrink

----------------------------------------------------------------------------
-- Arbitrary types from MainExtra[header/body]data
----------------------------------------------------------------------------

instance Arbitrary U.ApplicationName where
    arbitrary =
        U.ApplicationName .
        toText . map selectAlpha . take U.applicationNameMaxLength <$>
        arbitrary
      where
        selectAlpha n = alphabet !! (n `mod` length alphabet)
        alphabet = "-0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

instance Arbitrary U.BlockVersion where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary U.SoftwareVersion where
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

instance HasGenesisBlockVersionData => Arbitrary G.TestnetBalanceOptions where
    arbitrary = do
        -- We have at least 2 owned addresses in system so we can send
        -- transactions in block-gen/tests.
        tboPoors <- choose (1, 100)
        tboRichmen <- choose (1, 12)
        tboTotalBalance <- choose (1000, maxCoinVal)
        tboRichmenShare <- choose (0.55, 0.996)
        let tboUseHDAddresses = False
        return G.TestnetBalanceOptions {..}

instance Arbitrary G.FakeAvvmOptions where
    arbitrary = do
        faoCount <- choose (0, 10)
        faoOneBalance <- choose (5, 30)
        return G.FakeAvvmOptions {..}

instance HasProtocolMagic => Arbitrary G.GenesisDelegation where
    arbitrary =
        leftToPanic "arbitrary@GenesisDelegation" . G.mkGenesisDelegation <$> do
            secretKeys <- sized (nonrepeating . min 10) -- we generate at most tens keys,
                                                        -- because 'nonrepeating' fails when
                                                        -- we want too many items, because
                                                        -- life is hard
            return $
                case secretKeys of
                    []                 -> []
                    (delegate:issuers) -> mkCert (toPublic delegate) <$> issuers
      where
        mkCert delegatePk issuer = createPsk protocolMagic issuer delegatePk (HeavyDlgIndex 0)

instance Arbitrary G.GenesisWStakeholders where
    arbitrary = G.GenesisWStakeholders <$> arbitrary

instance Arbitrary G.GenesisAvvmBalances where
    arbitrary = G.GenesisAvvmBalances <$> arbitrary

instance Arbitrary G.GenesisNonAvvmBalances where
    arbitrary = G.GenesisNonAvvmBalances <$> arbitrary


instance Arbitrary ProtocolConstants where
    arbitrary = do
        vssA <- arbitrary
        vssB <- arbitrary
        let (vssMin, vssMax) = if vssA > vssB
                               then (VssMinTTL vssB, VssMaxTTL vssA)
                               else (VssMinTTL vssA, VssMaxTTL vssB)
        ProtocolConstants <$> choose (1, 20000) <*> pure vssMin <*> pure vssMax

instance HasProtocolMagic => Arbitrary G.GenesisProtocolConstants where
    arbitrary = flip G.genesisProtocolConstantsFromProtocolConstants protocolMagic <$> arbitrary

instance (HasProtocolMagic, HasProtocolConstants) => Arbitrary G.GenesisData where
    arbitrary = G.GenesisData
        <$> arbitrary <*> arbitrary <*> arbitraryStartTime
        <*> arbitraryVssCerts <*> arbitrary <*> arbitraryBVD
        <*> arbitrary <*> arbitrary <*> arbitrary
      where
        -- System start time should be multiple of a second.
        arbitraryStartTime = Timestamp . convertUnit @Second <$> arbitrary
        -- Unknown tx fee policy in genesis is not ok.
        arbitraryBVD = arbitrary `suchThat` hasKnownFeePolicy
        hasKnownFeePolicy BlockVersionData {bvdTxFeePolicy = Fee.TxFeePolicyTxSizeLinear {}} =
            True
        hasKnownFeePolicy _ = False
        arbitraryVssCerts = G.GenesisVssCertificatesMap . mkVssCertificatesMapLossy <$> arbitrary
----------------------------------------------------------------------------
-- Arbitrary miscellaneous types
----------------------------------------------------------------------------

instance Arbitrary Millisecond where
    arbitrary = fromMicroseconds <$> choose (0, 600 * 1000 * 1000)
    shrink = shrinkIntegral

instance Arbitrary Microsecond where
    arbitrary = fromMicroseconds <$> choose (0, 600 * 1000 * 1000)
    shrink = shrinkIntegral

instance Arbitrary Second where
    arbitrary = convertUnit @Microsecond <$> arbitrary
    shrink = shrinkIntegral

instance Arbitrary Types.Timestamp where
    arbitrary = Timestamp . fromMicroseconds <$> choose (0, 2000000000 * 1000 * 1000)
    shrink = shrinkIntegral

deriving instance Arbitrary Types.TimeDiff

instance Arbitrary HeavyDlgIndex where
    arbitrary = HeavyDlgIndex <$> arbitrary
    shrink = genericShrink

instance Arbitrary LightDlgIndices where
    arbitrary = do
        l <- arbitrary
        r <- arbitrary
        pure $ LightDlgIndices $ if r >= l then (l,r) else (r,l)
    shrink = genericShrink

----------------------------------------------------------------------------
-- SSC
----------------------------------------------------------------------------

genVssCertificate :: ProtocolMagic -> Gen VssCertificate
genVssCertificate pm =
    mkVssCertificate pm <$> arbitrary -- secret key
                        <*> arbitrary -- AsBinary VssPublicKey
                        <*> arbitrary -- EpochIndex

instance HasProtocolMagic => Arbitrary VssCertificate where
    arbitrary = genVssCertificate protocolMagic
    -- The 'shrink' method wasn't implement to avoid breaking the datatype's invariant.

----------------------------------------------------------------------------
-- Merkle
----------------------------------------------------------------------------

instance (Bi a, Arbitrary a) => Arbitrary (MerkleTree a) where
    arbitrary = mkMerkleTree <$> arbitrary
