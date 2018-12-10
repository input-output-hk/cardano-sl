{-# LANGUAGE ViewPatterns #-}

-- | Specification of Pos.Chain.Lrc.OBFT (which is basically a pure
-- version of 'Pos.DB.Lrc.OBFT').

module Test.Pos.Chain.Lrc.ObftRoundRobinSpec
       ( spec
       ) where

import           Universum hiding (sort)

import           Data.List (scanl1)
import           Data.List.NonEmpty (sort, (!!))
import qualified Data.Set as S (deleteFindMin, fromList)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), Property, choose, suchThat,
                     (===))

import           Pos.Chain.Lrc (getEpochSlotLeaderScheduleObftPure,
                     getSlotLeaderObftPure)
import           Pos.Core (EpochIndex, SlotCount, SlotId, StakesList,
                     addressHash, flattenEpochOrSlot, mkCoin, unsafeAddCoin)
import           Pos.Crypto (PublicKey)

import           Test.Pos.Core.Arbitrary (genPositiveSlotCount)

spec :: Spec
spec = do
  describe "Pos.Chain.Lrc.OBFT" $ do
    describe "Round-robin" $ do
        modifyMaxSuccess (const 10000) $ do
            prop description_rrListLength
                 (rrListLength <$> genPositiveSlotCount)
            prop description_rrCorrectSlotLeader
                 (rrCorrectSlotLeader <$> genPositiveSlotCount)
 where
  description_rrListLength =
    "the amount of stakeholders is the same as the number of slots in an epoch"
  description_rrCorrectSlotLeader =
    "the correct slot leader is chosen given any epoch and slot"

-- | Type used to generate random stakes and a 'PublicKey' that
-- doesn't have any stake.
--
-- Two necessarily different public keys are generated, as well as a list of
-- public keys who will be our other stakeholders. To guarantee a non-empty
-- stakes map, one of these public keys is inserted in the list, which is
-- converted to a set and then to a map.
newtype StakeAndHolder = StakeAndHolder
    { getNoStake :: (PublicKey, StakesList)
    } deriving Show

instance Arbitrary StakeAndHolder where
    arbitrary = StakeAndHolder <$> do
        pk1 <- arbitrary
        pk2 <- arbitrary `suchThat` ((/=) pk1)
        listPks <- do
            n <- choose (2, 10)
            replicateM n arbitrary
        coins <- mkCoin <$> choose (1, 1000)
        let setPks :: Set PublicKey
            setPks = S.fromList $ pk1 : pk2 : listPks
            (myPk, restPks) = S.deleteFindMin setPks
            nRest = length restPks
            values = scanl1 unsafeAddCoin $ replicate nRest coins
            stakesList = map addressHash (toList restPks) `zip` values
        return (myPk, stakesList)

rrListLength
    :: SlotCount
    -> EpochIndex
    -> StakeAndHolder
    -> Property
rrListLength epochSlotCount epochIndex (getNoStake -> (_, stakes)) = do
    length (getEpochSlotLeaderScheduleObftPure epochIndex epochSlotCount stakeholders)
        === fromIntegral epochSlotCount
  where
    stakeholders = case nonEmpty (map fst stakes) of
        Just s  -> s
        Nothing -> error "rrListLength: Empty list of stakeholders"

rrCorrectSlotLeader
    :: SlotCount
    -> SlotId
    -> StakeAndHolder
    -> Property
rrCorrectSlotLeader epochSlotCount slotId (getNoStake -> (_, stakes)) = do
    actualSlotLeader === expectedSlotLeader
  where
    stakeholders = case nonEmpty (map fst stakes) of
        Just s  -> s
        Nothing -> error "rrCorrectSlotLeader: Empty list of stakeholders"
    flatSlotId = flattenEpochOrSlot epochSlotCount slotId
    expectedSlotLeaderIndex =
        (fromIntegral flatSlotId :: Int) `mod` (length stakeholders)
    expectedSlotLeader = (sort stakeholders) !! expectedSlotLeaderIndex
    actualSlotLeader = getSlotLeaderObftPure slotId epochSlotCount stakeholders
