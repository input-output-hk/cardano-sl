{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Specification of Pos.Ssc.GodTossing.Toss.Base.computeSharesdistr

module Test.Pos.Ssc.GodTossing.ComputeSharesSpec
       ( spec
       ) where

import           Universum

import qualified Data.HashMap.Strict   as HM
import           Test.Hspec            (Expectation, Spec, describe, shouldBe)
import           Test.Hspec.QuickCheck (prop, modifyMaxSuccess)
import           Test.QuickCheck       (Property, (.&&.), (===))
import           Data.Reflection       (Reifies (..))

import           Pos.Arbitrary.Lrc     (GenesisMpcThd, InvalidRichmenStakes (..),
                                        ValidRichmenStakes (..))
import           Pos.Core              (Coin, mkCoin, unsafeAddressHash, StakeholderId, CoinPortion,
                                        unsafeCoinPortionFromDouble, unsafeGetCoin,
                                        unsafeSubCoin)
import           Pos.Core.Coin         (applyCoinPortionDown,
                                        sumCoins)
import           Pos.Lrc               (RichmenStakes, RichmenType (RTUsual),
                                        findRichmenPure)
import           Pos.Ssc.GodTossing    (SharesDistribution, TossVerFailure,
                                        computeSharesDistrPure, sharesDistrInaccuracy,
                                        computeDistrInaccuracy)

spec :: Spec
spec = describe "computeSharesDistr" $ do
    prop emptyRichmenStakesDesc emptyRichmenStakes
    modifyMaxSuccess (const 3) $
        prop invalidStakeErrorsDesc invalidStakeErrors
    prop lrcConsistencyDesc lrcConsistency

    describe "Distribution is limited" $ do
        prop oneRichRichmanDesc oneRichRichman
        prop twoRichRichmenDesc twoRichRichmen
        prop onePoorOneRichDesc onePoorOneRich
        prop severalPoorRichmenDesc severalPoorRichmen
        prop severalPoorOneRichRichmenDesc severalPoorOneRichRichmen

    describe "Validate distribution fairness" $ do
        prop severalSimilarRichmenDesc severalSimilarRichmen
        prop twentyRichmen1Desc twentyRichmen1
        prop twentyRichmen2Desc twentyRichmen2
        modifyMaxSuccess (const 30) $
            prop validateReasonableDesc validateReasonable
  where
    emptyRichmenStakesDesc = "Doesn't fail to calculate a share distribution when the richmen\
    \ stake is empty."
    invalidStakeErrorsDesc = "If the stake distribution has a 'StakeholderId' with\
    \ insufficient stake to participate in the distribution of shares, the distribution\
    \ fails to be calculated."
    lrcConsistencyDesc = "computeSharesDistr's definition of richmen is\
    \ consistent with one used by LRC."

    oneRichRichmanDesc = "One richman with total stake, sum of distribution mustn't be big"
    twoRichRichmenDesc = "Two richmen with almost half of stake, sum of distribution mustn't be big"
    onePoorOneRichDesc = "Two richmen: first with mpcThd stake, second with remained stake,\
    \ sum of distribution mustn't be big"
    severalPoorRichmenDesc =
        "Several poor richmen, sum of distribution mustn't be big"
    severalPoorOneRichRichmenDesc =
        "Several richmen, one of them control most of stake, sum of distribution mustn't be big"

    severalSimilarRichmenDesc = "Richmen with stake distribution [0.24, 0.25, 0.26, 0.29]\
    \ to test validity of @computeSharesDistr@"
    twentyRichmen1Desc = "20 richmen with similar stake to test fairness of generated distribution"
    twentyRichmen2Desc = "20 richmen with similar stake to test fairness of generated distribution"
    validateReasonableDesc = "Given a valid richmen, validate fairness and some reasonable statements"

data TestMpcThd

instance Reifies TestMpcThd CoinPortion where
    reflect _ = testMpcThdPortition

computeShares' :: RichmenStakes -> Either TossVerFailure SharesDistribution
computeShares' stake = computeSharesDistrPure stake testMpcThdPortition

testMpcThd :: Double
testMpcThd = 0.01

testMpcThdPortition :: CoinPortion
testMpcThdPortition = unsafeCoinPortionFromDouble testMpcThd

-- Check that sum of distribtuion is less than passed value
isLimitedBy :: Word16 -> SharesDistribution -> Bool
isLimitedBy mx sd = sum (toList sd) <= mx

-- We will call distribution fair, if the max inaccuracy is less than 0.05
-- and number of bad cases is less than 5% of all 2^n cases.
-- Max inaccuracy is more or less heuristic value
-- which means which inaccuracy we can get in the bad case.
-- This inaccuracy can lead to two bad situation:
-- 1. when nodes mustn't reveal commitment, but they can
-- 2. when nodes must reveal commitment, but they can't.
-- We can get these situations when sum of stakes of nodes which sent shares is close to 0.5.
-- Max inaccuracy is computed as difference between generated stake and 0.5.
-- We estimate inaccuracy as difference between real distribution and
-- generated.
isDistrFair :: RichmenStakes -> SharesDistribution -> Bool
isDistrFair (HM.map unsafeGetCoin -> rs) sd = do
    let !totalDistr = sum sd
    let !totalCoins = sum rs
    let stakeholders = HM.keys rs
    let coinsNDistr = map (first fromIntegral . findStk) stakeholders
    let !maxEr = computeDistrInaccuracy (fromIntegral totalCoins, totalDistr) coinsNDistr
    maxEr < sharesDistrInaccuracy
  where

    findStk :: StakeholderId -> (Word64, Word16)
    findStk stId = do
        let r = fromMaybe (error "Real stake isn't found") (HM.lookup stId rs)
        let g = fromMaybe (error "Distribution isn't found") (HM.lookup stId sd)
        (r, g)

isDistrReasonable :: Word16 -> RichmenStakes -> Either TossVerFailure SharesDistribution -> Bool
isDistrReasonable _ _ (Left _) = False
isDistrReasonable mx rs (Right sd) =
    -- (1) Each richman has at least @minStake@ coins and more than 0 coins.
    all (\x -> x >= minStake && x > mkCoin 0) rs &&
    -- (2) Each richman has to generate positive number of shares.
    (all (/= 0) sd) &&
    -- (3) Sum of distribution is less than some reasonable number.
    isLimitedBy mx sd &&
    -- (4) Distribution is fair (see explanation of @isDistrFair@)
    isDistrFair rs sd
  where
    totalCoins = sumCoins $ HM.elems rs
    minStake = mkCoin . ceiling $ (fromIntegral totalCoins) * testMpcThd

isDistrReasonableMax :: RichmenStakes -> Either TossVerFailure SharesDistribution -> Bool
isDistrReasonableMax = isDistrReasonable $ truncate $ toRational (3::Int) / toRational testMpcThd

maxCoin :: Coin
maxCoin = maxBound @Coin

(</>) :: Coin -> Word64 -> Coin
(</>) c d = mkCoin $ unsafeGetCoin c `div` d

(<->) :: Coin -> Word64 -> Coin
(<->) c w = unsafeSubCoin c (mkCoin w)

richmenStakesFromCoins :: [Coin] -> RichmenStakes
richmenStakesFromCoins coins = do
    let stakeholders = map unsafeAddressHash [0 :: Integer ..]
    HM.fromList $ zip stakeholders coins

richmenStakesFromFractions :: [Double] -> RichmenStakes
richmenStakesFromFractions fracts0 = do
    let sm = sum fracts0
    let ports = map (unsafeCoinPortionFromDouble . (/ sm)) fracts0
    let coins = map (flip applyCoinPortionDown maxCoin) ports
    richmenStakesFromCoins coins

----------------------------------------------------------------------------
-- Tests on total amount of shares
----------------------------------------------------------------------------

oneRichRichman :: Bool
oneRichRichman = isDistrReasonable 1 richmen $ computeShares' richmen
  where
    richmen = richmenStakesFromCoins [maxCoin]

twoRichRichmen :: Bool
twoRichRichmen = isDistrReasonable 2 richmen $ computeShares' richmen
  where
    richmen = richmenStakesFromCoins [maxCoin </> 2 <-> 1, maxCoin </> 2]

onePoorOneRich :: Bool
onePoorOneRich = isDistrReasonable 100 richmen $ computeShares' richmen
  where
    richmen = richmenStakesFromFractions [testMpcThd, 1 - testMpcThd]

severalPoorRichmen :: Bool
severalPoorRichmen = isDistrReasonable 19 richmen $ computeShares' richmen
  where
    richmen = richmenStakesFromFractions [testMpcThd, 0.051, 0.13]

severalPoorOneRichRichmen :: Bool
severalPoorOneRichRichmen = isDistrReasonable 99 richmen $ computeShares' richmen
  where
    distr = [testMpcThd, 0.051, 0.13]
    richmen = richmenStakesFromFractions $ 1 - sum distr : distr

----------------------------------------------------------------------------
-- Tests on fairness of distribution
----------------------------------------------------------------------------

severalSimilarRichmen :: Bool
severalSimilarRichmen = Right [1, 1, 1, 1] ==
    fmap toList (computeShares' $ richmenStakesFromFractions [0.24, 0.25, 0.26, 0.29])

twentyRichmen1 :: Bool
twentyRichmen1 = isDistrReasonableMax richmen $ computeShares' richmen
  where
    more = map (\x -> 0.05 + x * 0.001) [0..9]
    less = map (\x -> 0.05 - x * 0.001) [0..9]
    richmen = richmenStakesFromFractions $ more ++ less

twentyRichmen2 :: Bool
twentyRichmen2 = isDistrReasonableMax richmen $ computeShares' richmen
  where
    more = map (\x -> 0.05 + x * 0.001) [1..10]
    less = map (\x -> 0.05 - x * 0.001) [1..10]
    richmen = richmenStakesFromFractions $ more ++ less

validateReasonable :: ValidRichmenStakes TestMpcThd -> Bool
validateReasonable (getValid -> richmen) = isDistrReasonableMax richmen $ computeShares' richmen

----------------------------------------------------------------------------
-- Other tests
----------------------------------------------------------------------------

emptyRichmenStakes :: Expectation
emptyRichmenStakes =
    let emptyRes = computeShares' mempty
    in isRight emptyRes `shouldBe` True

invalidStakeErrors :: InvalidRichmenStakes GenesisMpcThd -> Bool
invalidStakeErrors (getInvalid -> richmen) =
    isLeft $ computeShares' richmen

lrcConsistency :: Property
lrcConsistency =
    let
        (_, richmen) = findRichmenPure stakes mpcThd' RTUsual
        Right sharesDistr = computeSharesDistrPure richmen mpcThd'
    in
        all (> 0) (toList sharesDistr) .&&.
        HM.keys sharesDistr === HM.keys richmen
  where
    -- stakes used for the test
    stakes       = zip stakeholders coins
    stakeholders = map unsafeAddressHash [0 :: Integer ..]
    coins        = map mkCoin [1,2,4,6,8,19,39,78,156,312]
    -- threshold used for the test
    mpcThd' = unsafeCoinPortionFromDouble 0.01
