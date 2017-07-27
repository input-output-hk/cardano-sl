-- | Specification of Pos.Ssc.GodTossing.Toss.Base.computeSharesdistr

module Test.Pos.Ssc.GodTossing.ComputeSharesSpec
       ( spec
       ) where

import qualified Data.HashMap.Strict   as HM
import           Test.Hspec            (Expectation, Spec, describe, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, (.&&.), (===))
import           Universum

import           Pos.Arbitrary.Lrc     (GenesisMpcThd, InvalidRichmenStakes (..),
                                        ValidRichmenStakes (..))
import           Pos.Constants         (genesisMpcThd)
import           Pos.Core              (Coin, mkCoin, unsafeAddressHash,
                                        unsafeCoinPortionFromDouble,
                                        unsafeCoinPortionFromDouble, unsafeGetCoin,
                                        unsafeSubCoin)
import           Pos.Core.Coin         (applyCoinPortionDown, coinPortionToDouble,
                                        sumCoins)
import           Pos.Lrc               (RichmenStakes, RichmenType (RTUsual),
                                        findRichmenPure)
import           Pos.Ssc.GodTossing    (SharesDistribution, TossVerFailure,
                                        computeSharesDistrPure)

spec :: Spec
spec = describe "computeSharesDistr" $ do
    prop emptyRichmenStakesDesc emptyRichmenStakes
    prop allRichmenGetShareDesc allRichmenGetShares
    prop invalidStakeErrorsDesc invalidStakeErrors
    prop totalStakeZeroDesc totalStakeIsZero
    prop validRichmenStakesWorksDesc validRichmenStakesWorks
    prop lrcConsistencyDesc lrcConsistency

    describe "distributionNotBig" $ do
        prop oneRichRichmanDesc oneRichRichman
        prop twoRichRichmenDesc twoRichRichmen
        prop onePoorOneRichDesc onePoorOneRich
        prop severalPoorOneRichRichmenDesc severalPoorOneRichRichmen
  where
    emptyRichmenStakesDesc = "Fails to calculate a share distribution when the richmen\
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
    severalPoorOneRichRichmenDesc =
        "Several richmen, one of them control most of stake, sum of distribution mustn't be big"

computeShares' :: RichmenStakes -> Either TossVerFailure SharesDistribution
computeShares' stake = computeSharesDistrPure stake genesisMpcThd

isDistrReasonable :: Word16 -> Either TossVerFailure SharesDistribution -> Bool
isDistrReasonable _ (Left _) = False
isDistrReasonable mx (Right sd) =
    -- (1) Sum of distribution is less than some reasonable number.
    limitedBy mx sd

-- Check that sum of distribtuion is less than passed value
limitedBy :: Word16 -> SharesDistribution -> Bool
limitedBy mx sd = sum (toList sd) <= mx

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

oneRichRichman :: Bool
oneRichRichman = isDistrReasonable 1 $
    computeShares' $ richmenStakesFromCoins [maxCoin]

twoRichRichmen :: Bool
twoRichRichmen = isDistrReasonable 2 $
    computeShares' $ richmenStakesFromCoins [maxCoin </> 2 <-> 1, maxCoin </> 2]

onePoorOneRich :: Bool
onePoorOneRich = isDistrReasonable 100 $
    computeShares' $ richmenStakesFromFractions [0.01, 0.99]

severalPoorOneRichRichmen :: Bool
severalPoorOneRichRichmen = isDistrReasonable 100 $ do
    let distr = [0.01, 0.051, 0.13]
    computeShares' $ richmenStakesFromFractions $ 1 - sum distr : distr

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
        mpcThreshold = coinPortionToDouble genesisMpcThd
        minStake = mkCoin . ceiling $ (fromIntegral totalCoins) * mpcThreshold
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
        (_, richmen) = findRichmenPure stakes mpcThd RTUsual
        Right sharesDistr = computeSharesDistrPure richmen mpcThd
    in
        all (> 0) (toList sharesDistr) .&&.
        HM.keys sharesDistr === HM.keys richmen
  where
    -- stakes used for the test
    stakes       = zip stakeholders coins
    stakeholders = map unsafeAddressHash [0 :: Integer ..]
    coins        = map mkCoin [1,2,4,6,8,19,39,78,156,312]
    -- threshold used for the test
    mpcThd = unsafeCoinPortionFromDouble 0.01
