{-# LANGUAGE RankNTypes #-}

-- | Specification of Pos.Ssc.Toss.Base.computeSharesdistr

module Test.Pos.Ssc.ComputeSharesSpec
       ( spec
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Data.Reflection (Reifies (..))
import           Test.Hspec (Expectation, Spec, describe, runIO, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Property, arbitrary, generate, (.&&.), (===))

import           Pos.Core (Coin, CoinPortion, StakeholderId, mkCoin, unsafeAddressHash,
                           unsafeCoinPortionFromDouble, unsafeGetCoin, unsafeSubCoin)
import           Pos.Core.Common (applyCoinPortionDown, sumCoins)
import           Pos.Core.Ssc (SharesDistribution)
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))
import           Pos.Lrc (RichmenStakes, RichmenType (RTUsual), findRichmenPure)
import           Pos.Ssc (SscVerifyError, computeSharesDistrPure, isDistrInaccuracyAcceptable,
                          sharesDistrMaxSumDistr)

import           Test.Pos.Configuration (withProvidedMagicConfig)
import           Test.Pos.Lrc.Arbitrary (GenesisMpcThd, InvalidRichmenStakes (..),
                                         ValidRichmenStakes (..))
import           Test.Pos.Util.QuickCheck.Property (qcIsLeft)

spec :: Spec
spec = do
    runWithMagic NMMustBeNothing
    runWithMagic NMMustBeJust

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        specBody pm

specBody :: ProtocolMagic -> Spec
specBody pm = withProvidedMagicConfig pm $ describe "computeSharesDistr" $ do
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

    describe "isDistrInaccuracyAcceptable" $ do
        prop distrEqDesc distrFits
        prop distrErrorType1Desc distrErrorType1
        prop distrErrorType2Desc distrErrorType2
        prop distrHalfSum1Desc distrHalfSum1
        prop distrHalfSum2Desc distrHalfSum2
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

    distrEqDesc = "Distribution fits coins"
    distrErrorType1Desc = "Distribution tells we can't reveal a secret, but coins tell we can"
    distrErrorType2Desc = "Distribution tells we can reveal a secret, but coins tell we can't"
    distrHalfSum1Desc =
        "Distribution equals @sumDistr / 2@ , sum of coins is greater than @sumCoins / 2@.\
        \ Inaccuracy is less than @sharesDistrInaccuracy@"
    distrHalfSum2Desc =
        "Distribution equals @sumDistr / 2@ , sum of coins is greater than @sumCoins / 2@.\
        \ Inaccuracy is greater than @sharesDistrInaccuracy@"

data TestMpcThd

instance Reifies TestMpcThd CoinPortion where
    reflect _ = testMpcThdPortition

computeShares' :: RichmenStakes -> Either SscVerifyError SharesDistribution
computeShares' stake = computeSharesDistrPure stake testMpcThdPortition

testMpcThd :: Double
testMpcThd = 0.01

testMpcThdPortition :: CoinPortion
testMpcThdPortition = unsafeCoinPortionFromDouble testMpcThd

-- Check that sum of distribtuion is less than passed value
isLimitedBy :: Word16 -> SharesDistribution -> Bool
isLimitedBy mx sd = sum (toList sd) <= mx

-- Distribution is fair if a max inaccuracy is less than @sharesDistrInaccuracy@.
isDistrFair :: RichmenStakes -> SharesDistribution -> Bool
isDistrFair (HM.map unsafeGetCoin -> rs) sd = do
    let stakeholders = HM.keys rs
    let coinsNDistr = map (first fromIntegral . findStk) stakeholders
    isDistrInaccuracyAcceptable coinsNDistr
  where

    findStk :: StakeholderId -> (Word64, Word16)
    findStk stId = do
        let r = fromMaybe (error "Real stake isn't found") (HM.lookup stId rs)
        let g = fromMaybe (error "Distribution isn't found") (HM.lookup stId sd)
        (r, g)

isDistrReasonable :: Word16 -> RichmenStakes -> Either SscVerifyError SharesDistribution -> Bool
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

isDistrReasonableMax :: RichmenStakes -> Either SscVerifyError SharesDistribution -> Bool
isDistrReasonableMax = isDistrReasonable $ sharesDistrMaxSumDistr testMpcThd

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

-- Note that 1 is the “most reasonable” distribution here but the algorithm
-- will output 4 because distributions size less than 4 can't be generated
-- by SCRAPE.
oneRichRichman :: Bool
oneRichRichman = isDistrReasonable 4 richmen $ computeShares' richmen
  where
    richmen = richmenStakesFromCoins [maxCoin]

-- Note that 2 is the “most reasonable” distribution here but the algorithm
-- will output 8 because distributions size less than 4 can't be generated
-- by SCRAPE and we multiply everything by four. 2×4 = 8
twoRichRichmen :: Bool
twoRichRichmen = isDistrReasonable 8 richmen $ computeShares' richmen
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

invalidStakeErrors :: InvalidRichmenStakes GenesisMpcThd -> Property
invalidStakeErrors (getInvalid -> richmen) =
    qcIsLeft $ computeShares' richmen

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

----------------------------------------------------------------------------
-- isDistrInaccuracyAcceptable tests
----------------------------------------------------------------------------

distrFits :: Bool
distrFits = isDistrInaccuracyAcceptable [(10, 1), (20, 2), (30, 3)]

distrErrorType1 :: Bool
distrErrorType1 = not $ isDistrInaccuracyAcceptable [(20, 2), (40, 2), (10, 1), (20, 2), (20, 2)]

distrErrorType2 :: Bool
distrErrorType2 = not $ isDistrInaccuracyAcceptable [(10, 2), (30, 4), (20, 2), (30, 3)]

distrHalfSum1 :: Bool
distrHalfSum1 =
    -- We can achieve the following distributions (61/111, 5/10)~(0.5495, 0.5)
    -- we should take             vvvvv                      vvvvv
    isDistrInaccuracyAcceptable [(20, 2), (10, 1), (20, 2), (41, 3), (20, 2)]

distrHalfSum2 :: Bool
distrHalfSum2 =
    -- We can achieve the following distributions (62/112, 5/10)~(0.55357, 0.5)
    -- we should take                   vvvvv             vvvvv
    not $ isDistrInaccuracyAcceptable [(20, 2), (10, 1), (42, 3), (20, 2), (20, 2)]
