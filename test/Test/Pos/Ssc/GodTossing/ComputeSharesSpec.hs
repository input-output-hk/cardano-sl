-- | Specification of Pos.Ssc.GodTossing.Toss.Base.computeSharesdistr

module Test.Pos.Ssc.GodTossing.ComputeSharesSpec
       ( spec
       ) where

import qualified Data.HashMap.Strict   as HM
import           Test.Hspec            (Expectation, Spec, describe, shouldBe)
import           Test.Hspec.QuickCheck (prop, modifyMaxSuccess)
import           Test.QuickCheck       (Property, (.&&.), (===))
import           Universum

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
                                        computeSharesDistrPure, sharesDistrInaccuracy)

spec :: Spec
spec = describe "computeSharesDistr" $ do
    prop emptyRichmenStakesDesc emptyRichmenStakes
    prop allRichmenGetShareDesc allRichmenGetShares
    prop invalidStakeErrorsDesc invalidStakeErrors
    prop totalStakeZeroDesc totalStakeIsZero
    prop validRichmenStakesWorksDesc validRichmenStakesWorks
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
        modifyMaxSuccess (const 3) $
            prop validateFairnessDesc validateFairness
  where
    emptyRichmenStakesDesc = "Doesn't fail to calculate a share distribution when the richmen\
    \ stake is empty."
    allRichmenGetShareDesc = "All richmen are awarded a non-zero share, and richmen who\
    \ do not participate in the share distribution are awarded none"
    invalidStakeErrorsDesc = "If the stake distribution has a 'StakeholderId' with\
    \ insufficient stake to participate in the distribution of shares, the distribution\
    \ fails to be calculated."
    totalStakeZeroDesc = "If the total stake is zero, then the distribution fails to be\
    \ calculated"
    validRichmenStakesWorksDesc = "Given a valid distribution of stake, calculating the\
    \ distribution of shares successfully works."
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
    validateFairnessDesc = "Given a valid richmen, validate fairness"

computeShares' :: RichmenStakes -> Either TossVerFailure SharesDistribution
computeShares' stake = computeSharesDistrPure stake testMpcThdPortition

testMpcThd :: Double
testMpcThd = 0.01

testMpcThdPortition :: CoinPortion
testMpcThdPortition = unsafeCoinPortionFromDouble testMpcThd

-- Check that sum of distribtuion is less than passed value
isLimitedBy :: Word16 -> SharesDistribution -> Bool
isLimitedBy mx sd = sum (toList sd) <= mx

-- This type describes the
-- (max inaccuracy, number of bad cases of the first type, number of bad cases of the second type)
-- More details see in the @isDistrFair@ description.
type Unfairness = (Rational, Int, Int)

-- We will call distribution fair, if the max inaccuracy is less than 0.05
-- and number of bad cases is less than 5% of all 2^n cases.
-- Max inaccuracy is more or less heuristic value
-- which means which inaccuracy we can get in the bad case.
-- This inaccuracy can lead to two bad situation:
-- 1. when nodes mustn't reveal commitment, but they can
-- 2. when nodes must reveal commitment, but they can't.
-- We can get these situations when sum of stakes of nodes which sent shares is close to 0.5.
-- Max inaccuracy is computed as difference between generated stake and 0.5.
-- There is way to estimate inaccuracy as difference between real distribution and
-- generated. On the one hand it makes sense but on the other hand this difference
-- is not so important.
isDistrFair :: RichmenStakes -> SharesDistribution -> Bool
isDistrFair rs sd
    | length rs > 20 = error "Too many richmen"
    | otherwise = do
        let n = length rs
        let totalCases = toRational (2::Int)^n :: Rational
        let fivePerc = 0.05 :: Rational
        let totalCoins = sum $ map unsafeGetCoin $ toList rs
        let totalDistr = sum (toList sd)
        let stakeholders = HM.keys rs
        let distrs = map (findStk totalCoins totalDistr) stakeholders
        let !(er, firstCase, secondCase) = fairBrute distrs 0 0
        er < sharesDistrInaccuracy &&
            toRational firstCase < totalCases * fivePerc &&
            toRational secondCase < totalCases * fivePerc
  where
    fairBrute :: [(Rational, Rational)] -> Rational -> Rational -> Unfairness
    fairBrute [] real generated = computeUnfairness real generated
    fairBrute ((r, g):xs) !real !generated
        -- We get the situation when both these values are greater than 0.5,
        -- so we can't get some unfairness afterwards.
        | real > 0.5 && generated > 0.5 = (0, 0, 0)
        | otherwise = do
            let !whenNodeOffline = fairBrute xs real generated
            let !whenNodeOnline  = fairBrute xs (real + r) (generated + g)
            whenNodeOnline `combineUnfair` whenNodeOffline
    divRat a b = toRational a / toRational b
    findStk :: Word64 -> Word16 -> StakeholderId -> (Rational, Rational)
    findStk totalCoins totalDistr stId = do
        let rFrac = fromMaybe (error "Real stake isn't found")
                              (unsafeGetCoin <$> HM.lookup stId rs) `divRat` totalCoins
        let gFrac = fromMaybe (error "Distribution isn't found")
                              (HM.lookup stId sd) `divRat` totalDistr
        (rFrac, gFrac)

    combineUnfair (er1, c1, c2) (er2, c3, c4) =
        (max er1 er2, c1 + c3, c2 + c4)
    computeUnfairness :: Rational -> Rational -> Unfairness
    computeUnfairness real generated
        -- Strictly say constant 0.5 depends on number of shares required
        -- to reveal of commitment.
        -- Bad case of the first type is
        -- nodes can reveal commitment using shares
        -- but real coin distribution says that nodes can't do it.
        | real < 0.5 && generated > 0.5 = (generated - 0.5, 1, 0)
        -- Bad case of the second type is
        -- nodes can't reveal commitment using shares of honest nodes
        -- but real coin distribution says that nodes can do it.
        | real > 0.5 && generated < 0.5 = (0.5 - generated, 0, 1)
        | otherwise = (0, 0, 0)

isDistrReasonable :: Word16 -> RichmenStakes -> Either TossVerFailure SharesDistribution -> Bool
isDistrReasonable _ _ (Left _) = False
isDistrReasonable mx rs (Right sd) =
    -- (1) Sum of distribution is less than some reasonable number.
    isLimitedBy mx sd &&
    -- (2) Distribution is fair (see explanation of @isDistrFair@)
    isDistrFair rs sd

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

validateFairness :: ValidRichmenStakes GenesisMpcThd -> Bool
validateFairness (getValid -> richmen) = isDistrReasonableMax richmen $ computeShares' richmen

----------------------------------------------------------------------------
-- Other tests
----------------------------------------------------------------------------

emptyRichmenStakes :: Expectation
emptyRichmenStakes =
    let emptyRes = computeShares' mempty
    in isRight emptyRes `shouldBe` True

allRichmenGetShares :: ValidRichmenStakes GenesisMpcThd -> Bool
allRichmenGetShares (getValid -> richmen) =
    let outputStakeholder = computeShares' richmen
    in case outputStakeholder of
        Left _ -> False
        Right result ->
            (HM.keys richmen) == (HM.keys result) && (all (/= 0) result)

validRichmenStakesWorks :: ValidRichmenStakes GenesisMpcThd -> Bool
validRichmenStakesWorks (getValid -> richmen) =
    let outputStakeholder = computeShares' richmen
        totalCoins = sumCoins $ HM.elems richmen
        minStake = mkCoin . ceiling $ (fromIntegral totalCoins) * testMpcThd
    in case outputStakeholder of
        Left _  -> False
        Right _ -> all (\x -> x >= minStake && x > (mkCoin 0)) richmen

totalStakeIsZero :: ValidRichmenStakes GenesisMpcThd -> Bool
totalStakeIsZero (getValid -> richmen) =
    let zeroStake = richmen $> (mkCoin 0)
    in isLeft $ computeShares' zeroStake

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
