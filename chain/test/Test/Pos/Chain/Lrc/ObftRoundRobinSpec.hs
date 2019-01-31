{-# LANGUAGE ViewPatterns #-}

-- | Specification of Pos.Chain.Lrc.OBFT (which is basically a pure
-- version of 'Pos.DB.Lrc.OBFT').

module Test.Pos.Chain.Lrc.ObftRoundRobinSpec
       ( spec
       ) where

import           Universum hiding (sort)

import           Data.List.NonEmpty (sort, (!!))
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Property, (===))

import           Pos.Chain.Lrc (getEpochSlotLeaderScheduleObftPure,
                     getSlotLeaderObftPure)
import           Pos.Core (EpochIndex, SlotCount, SlotId, flattenEpochOrSlot)

import           Test.Pos.Chain.Lrc.StakeAndHolder (StakeAndHolder (..))
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
