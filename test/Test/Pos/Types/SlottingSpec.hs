-- | Specification of Pos.Types.Slotting.

module Test.Pos.Types.SlottingSpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, (===))
import           Universum

import           Pos.Types             (SlotId, flattenSlotId, unflattenSlotId)

spec :: Spec
spec = describe "SlotId" $ do
    describe "Ord" $ do
        prop "is consistent with flatten/unflatten"
            flattenOrdConsistency

    describe "flattening" $ do
        prop "unflattening after flattening returns original SlotId"
            flattenThenUnflatten

flattenOrdConsistency :: SlotId -> SlotId -> Property
flattenOrdConsistency a b = a `compare` b === flattenSlotId a `compare` flattenSlotId b

flattenThenUnflatten :: SlotId -> Property
flattenThenUnflatten si = si === unflattenSlotId (flattenSlotId si)
