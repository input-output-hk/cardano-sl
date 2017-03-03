-- | Specification of Pos.Ssc.GodTossing.Toss.Base.computeSharesdistr

module Test.Pos.Ssc.GodTossing.ComputeSharesSpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import           Control.Monad.Except  (runExcept)
import qualified Data.HashMap.Strict   as HM (elems, keys)
import           Pos.Core.Types        (mkCoin)
import           Pos.Lrc               (RichmenStake)
import qualified Pos.Ssc.GodTossing    as T
import           Test.QuickCheck       (Property, (==>))

spec :: Spec
spec = describe "computeSharesDistr" $ do
    prop emptyRichmenStakeDesc emptyRichmenStake
    -- prop allRichmenGetShareDesc allRichmenGetShares
  where
    emptyRichmenStakeDesc = "Fails to calculate a share distribution when the richmen\
    \ stake is empty."
    allRichmenGetShareDesc = "All richmen are awarded a share, and richmen who do not\
    \ participate in the share distribution are awarded none"

emptyRichmenStake :: RichmenStake -> Bool
emptyRichmenStake richmen =
    let zeroCoin = mkCoin 0
        allZero = runExcept . T.computeSharesDistr . fmap (const zeroCoin) $ richmen
    in isLeft allZero

allRichmenGetShares :: RichmenStake -> Property
allRichmenGetShares richmen =
    let outputStakeholder = runExcept $ T.computeSharesDistr richmen
    in isRight outputStakeholder ==>
        ((Right . HM.keys $ richmen) == (HM.keys <$> outputStakeholder)) &&
        (either (const False) (all (/= 0) . HM.elems) outputStakeholder)
