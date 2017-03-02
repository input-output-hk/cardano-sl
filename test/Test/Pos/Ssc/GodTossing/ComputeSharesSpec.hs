-- | Specification of Pos.Ssc.GodTossing.Toss.Base.computeSharesdistr

module Test.Pos.Ssc.GodTossing.ComputeSharesSpec
       ( spec
       ) where

import           Test.Hspec              (Spec, describe)
import           Test.Hspec.QuickCheck   (prop)
import           Universum

import           Control.Monad.Except    (runExcept)
import qualified Data.HashMap.Strict     as HM
import           Pos.Constants           (genesisMpcThd)
import           Pos.Types.Coin          (mkCoin, sumCoins)
import           Pos.Types.Core          (coinPortionDenominator, getCoinPortion)
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

emptyRichmenStake :: RichmenStake -> Bool
emptyRichmenStake richmen =
    let zeroCoin = mkCoin 0
        allZero = runExcept . T.computeSharesDistr . fmap (const zeroCoin) $ richmen
    in isLeft allZero

allRichmenGetShares :: ValidRichmenStake -> Bool
allRichmenGetShares (getValid -> richmen) =
    let outputStakeholder = runExcept $ T.computeSharesDistr richmen
    in ((Right . HM.keys $ richmen) == (HM.keys <$> outputStakeholder)) &&
        (either (const False) (all (/= 0)) outputStakeholder)

validRichmenStakeWorks :: ValidRichmenStake -> Bool
validRichmenStakeWorks (getValid -> richmen) =
    let outputStakeholder = runExcept $ T.computeSharesDistr richmen
        totalCoins = sumCoins $ HM.elems richmen
        mpcThreshold = toRational (getCoinPortion genesisMpcThd) / (toRational coinPortionDenominator)
        minStake = mkCoin . ceiling $ (fromIntegral totalCoins) * mpcThreshold
    in (isRight outputStakeholder) ==
       (all (\x -> x >= minStake && x > (mkCoin 0)) richmen)

totalStakeIsZero :: ValidRichmenStake -> Bool
totalStakeIsZero (getValid -> richmen) =
    let zeroStake = fmap (const $ mkCoin 0) richmen
    in isLeft $ runExcept $ T.computeSharesDistr zeroStake

invalidStakeErrors :: InvalidRichmenStake -> Bool
invalidStakeErrors (getInvalid -> richmen) =
    isLeft $ runExcept $ T.computeSharesDistr richmen
