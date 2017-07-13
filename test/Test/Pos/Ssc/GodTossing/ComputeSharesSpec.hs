-- | Specification of Pos.Ssc.GodTossing.Toss.Base.computeSharesdistr

module Test.Pos.Ssc.GodTossing.ComputeSharesSpec
       ( spec
       ) where

import qualified Data.HashMap.Strict   as HM
import           Test.Hspec            (Expectation, Spec, describe, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, (.&&.), (===))
import           Universum

import           Pos.Constants         (genesisMpcThd)
import           Pos.Core              (mkCoin, unsafeAddressHash,
                                        unsafeCoinPortionFromDouble)
import           Pos.Core.Coin         (coinPortionToDouble, sumCoins)
import           Pos.Lrc               (RichmenStakes, RichmenType (RTUsual),
                                        findRichmenPure)
import           Pos.Lrc.Arbitrary     (GenesisMpcThd, InvalidRichmenStakes (..),
                                        ValidRichmenStakes (..))
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

computeShares' :: RichmenStakes -> Either TossVerFailure SharesDistribution
computeShares' stake = computeSharesDistrPure stake genesisMpcThd

emptyRichmenStakes :: Expectation
emptyRichmenStakes =
    let emptyRes = computeShares' mempty
    in isLeft emptyRes `shouldBe` True

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
