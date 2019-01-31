{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specification of Pos.Chain.LrcPure (which is basically a pure
-- version of 'Pos.Chain.Lrc').

module Test.Pos.Chain.Lrc.FtsSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), Property, infiniteListOf,
                     (===))

import           Pos.Chain.Lrc (followTheSatoshi)
import           Pos.Core (Coin, SharedSeed, StakeholderId, addressHash, pcK,
                     sumCoins, unsafeIntegerToCoin)
import           Pos.Crypto (PublicKey)

import           Test.Pos.Chain.Genesis.Dummy (dummyEpochSlots,
                     dummyProtocolConstants)
import           Test.Pos.Chain.Lrc.StakeAndHolder (StakeAndHolder (..))
import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Util.QuickCheck.Property (qcNotElem)

-- | Constant specifying the number of times 'ftsReasonableStake' will be
-- run.
numberOfRuns :: Int
-- The higher is 'blkSecurityParam', the longer epochs will be and the more
-- time FTS will take
numberOfRuns = 300000 `div` pcK dummyProtocolConstants

spec :: Spec
spec = do
  let smaller = modifyMaxSuccess (const 1)
  describe "Pos.Chain.LrcPure" $ do
    describe "followTheSatoshi" $ do
      describe "deterministic" $ do
        prop description_ftsListLength ftsListLength
        prop description_ftsNoStake    ftsNoStake
        prop description_ftsAllStake   ftsAllStake
      describe "probabilistic" $ smaller $ do
        prop description_ftsLowStake
             (ftsReasonableStake lowStake lowStakeTolerance)
        prop description_ftsHighStake
             (ftsReasonableStake highStake highStakeTolerance)
 where
  description_ftsListLength =
    "the amount of stakeholders is the same as the number of slots in an epoch"
  description_ftsNoStake =
    "a stakeholder with 0% stake won't ever be selected as slot leader"
  description_ftsAllStake
    = "a stakeholder with 100% stake will always be selected as slot leader, and he \
        \ will be the only stakeholder"
  description_ftsLowStake =
    "a stakeholder with low stake will be chosen seldom"
  description_ftsHighStake =
    "a stakeholder with high stake will be chosen often"
  lowStake  = 0.02
  highStake = 0.98
  acceptable x y = and [x >= y * 0.85, x <= y * 1.15]
  lowStakeTolerance (pLen, present, chosen) =
    acceptable present (1 - (1 - lowStake) ^ pLen) && acceptable chosen lowStake
  highStakeTolerance (pLen, present, chosen) =
    acceptable present (1 - (1 - highStake) ^ pLen)
      && acceptable chosen highStake

ftsListLength
    :: SharedSeed
    -> StakeAndHolder
    -> Property
ftsListLength seed (getNoStake -> (_, stakes)) =
    length (followTheSatoshi dummyEpochSlots seed stakes)
        === fromIntegral dummyEpochSlots

ftsNoStake
    :: SharedSeed
    -> StakeAndHolder
    -> Property
ftsNoStake seed (getNoStake -> (addressHash -> sId, stakes)) =
  sId `qcNotElem` followTheSatoshi dummyEpochSlots seed stakes

-- It will be broken if 'Coin' is 0, but 'arbitrary' can't generate 0
-- for unknown reason.
ftsAllStake :: SharedSeed -> PublicKey -> Coin -> Bool
ftsAllStake seed pk v =
  let stakes = [(addressHash pk, v)]
  in  all (== addressHash pk) $ followTheSatoshi dummyEpochSlots seed stakes

newtype FtsStream = Stream
    { getStream :: [SharedSeed]
    } deriving Show

instance Arbitrary FtsStream where
    arbitrary = Stream . take numberOfRuns <$> infiniteListOf arbitrary

newtype StakesStream = StakesStream
    { getStakesStream :: [StakeAndHolder]
    } deriving Show

instance Arbitrary StakesStream where
    arbitrary = StakesStream . take numberOfRuns <$> infiniteListOf arbitrary

-- | This test is a sanity check to verify that 'followTheSatoshi' does not
-- behave too extremely, i.e. someone with 2% of stake won't be chosen a
-- disproportionate number of times, and someone with 98% of it will be
-- chosen almost every time.
--
-- For an infinite list of stakes and an infinite list of 'SharedSeed's, the
-- 'followTheSatoshi' function will be ran many times with a different seed and
-- map each time and the absolute frequency of the choice of a given address
-- as stakeholder will be compared to a low/high threshold, depending on whether
-- the address has a low/high stake, respectively.
-- For a low/high stake, the test succeeds if this comparison is below/above the
-- threshold, respectively.
ftsReasonableStake
  :: Double
  -> ((Int, Double, Double) -> Bool)
  -> FtsStream
  -> StakesStream
  -> Bool
ftsReasonableStake stakeProbability threshold (getStream     -> ftsList) (getStakesStream -> utxoList)
  = let result = go numberOfRuns (0, 0, 0) ftsList utxoList in threshold result
 where
    -- We count how many times someone was present in selection and how many
    -- times someone was chosen overall.
  go
    :: Int
    -> (Int, Double, Double)
    -> [SharedSeed]
    -> [StakeAndHolder]
    -> (Int, Double, Double)
  go 0 (pl, p, c) _ _ = (pl, p, c)
  go _ (pl, p, c) [] _ = (pl, p, c)
  go _ (pl, p, c) _ [] = (pl, p, c)
  go total (_, !present, !chosen) (seed : nextSeed) (u : nextStakes) = go
    (total - 1)
    (pLen, newPresent, newChosen)
    nextSeed
    nextStakes
   where
    (pk, stakes) = getNoStake u
    stId         = addressHash pk
    totalStake :: Double
    totalStake = fromIntegral . sumCoins $ map snd stakes
    newStake :: Coin
    newStake =
      unsafeIntegerToCoin
        . round
        $ (stakeProbability * totalStake)
        / (1 - stakeProbability)
    newStakes :: [(StakeholderId, Coin)]
    newStakes = (addressHash pk, newStake) : stakes
    picks     = followTheSatoshi dummyEpochSlots seed newStakes
    pLen      = length picks
    newPresent =
      present + if stId `elem` picks then 1 / (fromIntegral numberOfRuns) else 0
    newChosen =
      chosen
        + fromIntegral (length (filter (== stId) (toList picks)))
        / (fromIntegral numberOfRuns * fromIntegral pLen)
