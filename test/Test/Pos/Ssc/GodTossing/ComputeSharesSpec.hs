-- | Specification of Pos.Ssc.GodTossing.Toss.Base.computeSharesdistr

module Test.Pos.Ssc.GodTossing.ComputeSharesSpec
       ( spec
       ) where

import           Test.Hspec              (Spec, describe, it)
import           Test.Hspec.QuickCheck   (prop)
import           Universum

import           Control.Monad.Except    (runExceptT)
import qualified Data.HashMap.Strict     as HM (elems, keys)
import           Pos.Lrc                 (RichmenStake)
import qualified Pos.Ssc.GodTossing      as T
import           Pos.Types               (sumCoins)
import           Test.QuickCheck         (Property, (===), (==>))
import           Test.QuickCheck.Monadic (assert, monadicIO)

spec :: Spec
spec = describe "computeSharesDistr" $ do
    prop emptyRichmenStakeDesc emptyRichmenStake
    prop allRichmenGetShareDesc allRichmenGetShares
  where
    emptyRichmenStakeDesc = "Outputs an empty distribution when the richmen stake is\
    \ empty."
    allRichmenGetShareDesc = "All richmen are awarded a share, and richmen who do not\
    \ participate in the share distribution are awarded none"

emptyRichmenStake :: RichmenStake -> Property
emptyRichmenStake richmen = monadicIO $ do
    let total = sumCoins $ toList richmen
    res <- runExceptT $ T.computeSharesDistr richmen
    pure $ (total == 0) ==> (isLeft res)

allRichmenGetShares :: RichmenStake -> Property
allRichmenGetShares richmen = monadicIO $ do
    let inputStakeholder = Right richmen
    outputStakeholder <- runExceptT $ T.computeSharesDistr richmen
    pure $ ((HM.keys <$> inputStakeholder) == (HM.keys <$> outputStakeholder)) &&
        (either (const False) (all (/= 0) . HM.elems) outputStakeholder)
