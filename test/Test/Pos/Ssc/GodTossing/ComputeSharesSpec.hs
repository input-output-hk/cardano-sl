-- | Specification of Pos.Ssc.GodTossing.Toss.Base.computeSharesdistr

module Test.Pos.Ssc.GodTossing.ComputeSharesSpec
       ( spec
       ) where

import           Test.Hspec              (Expectation, Spec, describe, shouldBe)
import           Test.Hspec.QuickCheck   (prop)
import           Universum

import           Control.Monad.Except    (runExcept)
import qualified Data.HashMap.Strict     as HM
import           Data.Reflection         (Reifies (..))
import           Pos.Constants           (genesisMpcThd)
import           Pos.Types.Address       (StakeholderId)
import           Pos.Types.Coin          (coinPortionToDouble, mkCoin, sumCoins)
import           Pos.Types.Core          (CoinPortion, coinPortionDenominator,
                                          getCoinPortion)
import           Pos.Lrc                 (InvalidRichmenStake (..), RichmenStake,
                                          ValidRichmenStake (..))
import qualified Pos.Ssc.GodTossing      as T

spec :: Spec
spec = describe "computeSharesDistr" $ do
    prop emptyRichmenStakeDesc emptyRichmenStake
    prop allRichmenGetShareDesc allRichmenGetShares
    prop invalidStakeErrorsDesc invalidStakeErrors
    prop totalStakeZeroDesc totalStakeIsZero
    prop validRichmenStakeWorksDesc validRichmenStakeWorks
  where
    emptyRichmenStakeDesc = "Fails to calculate a share distribution when the richmen\
    \ stake is empty."
    allRichmenGetShareDesc = "All richmen are awarded a non-zero share, and richmen who\
    \ do not participate in the share distribution are awarded none"
    invalidStakeErrorsDesc = "If the stake distribution has a 'StakeholderId' with\
    \ insufficient stake to participate in the distribution of shares, the distribution\
    \ fails to be calculated."
    totalStakeZeroDesc = "If the total stake is zero, then the distribution fails to be\
    \ calculated"
    validRichmenStakeWorksDesc = "Given a valid distribution of stake, calculating the\
    \ distribution of shares successfully works."

emptyRichmenStake :: Expectation
emptyRichmenStake =
    let emptyRes = runExcept $ T.computeSharesDistr mempty
    in isLeft emptyRes `shouldBe` True

data GenesisMpcThd

instance Reifies GenesisMpcThd CoinPortion where
    reflect _ = genesisMpcThd

allRichmenGetShares :: ValidRichmenStake GenesisMpcThd -> Bool
allRichmenGetShares (getValid -> richmen) =
    let outputStakeholder = runExcept $ T.computeSharesDistr richmen
    in case outputStakeholder of
        Left _ -> False
        Right result ->
            (HM.keys richmen) == (HM.keys result) && (all (/= 0) result)

validRichmenStakeWorks :: ValidRichmenStake GenesisMpcThd -> Bool
validRichmenStakeWorks (getValid -> richmen) =
    let outputStakeholder = runExcept $ T.computeSharesDistr richmen
        totalCoins = sumCoins $ HM.elems richmen
        mpcThreshold = coinPortionToDouble genesisMpcThd
        minStake = mkCoin . ceiling $ (fromIntegral totalCoins) * mpcThreshold
    in case outputStakeholder of
        Left _ -> False
        Right _ -> all (\x -> x >= minStake && x > (mkCoin 0)) richmen

totalStakeIsZero :: ValidRichmenStake GenesisMpcThd -> Bool
totalStakeIsZero (getValid -> richmen) =
    let zeroStake = richmen $> (mkCoin 0)
    in isLeft $ runExcept $ T.computeSharesDistr zeroStake

invalidStakeErrors :: InvalidRichmenStake GenesisMpcThd -> Bool
invalidStakeErrors (getInvalid -> richmen) =
    isLeft $ runExcept $ T.computeSharesDistr richmen
