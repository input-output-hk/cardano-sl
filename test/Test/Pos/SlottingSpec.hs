-- | Pos.Slotting specification

module Test.Pos.SlottingSpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, (===))
import           Universum

import           Pos.Slotting          (flattenSlotId, unflattenSlotId)
import           Pos.Types             (SlotId)

import           Test.Pos.Util         ()

spec :: Spec
spec = describe "Slotting" $ do
    describe "getCurrentSlot" $ do
        return ()

    describe "flattening" $ do
        prop "unflattening after flattening returns original SlotId"
            flattenThenUnflatten

flattenThenUnflatten :: SlotId -> Property
flattenThenUnflatten si = si === unflattenSlotId (flattenSlotId si)
