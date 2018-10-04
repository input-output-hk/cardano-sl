{-# LANGUAGE TemplateHaskell #-}

module Test.Pos.DB.Epoch.Index
       ( tests
       ) where

import           Universum

import           Data.Maybe (catMaybes)
import           Hedgehog (MonadGen, Property, PropertyT, checkSequential,
                     discover, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)

import           Pos.Core (LocalSlotIndex (..), SlotCount (..),
                     localSlotIndices)
import           Pos.DB.Epoch.Index


prop_smallLookups :: Property
prop_smallLookups = property $ lookupProperty 100

prop_mediumLookups :: Property
prop_mediumLookups = property $ lookupProperty 5000

prop_largeLookups :: Property
prop_largeLookups = property $ lookupProperty 10000

prop_realLookups :: Property
prop_realLookups = property $ lookupProperty 21600

lookupProperty :: SlotCount -> PropertyT IO ()
lookupProperty epochSlots = do

    -- Generate a list of @Maybe Word64@ to represent the index
    maybeOffsets <- forAll $ Gen.list
        (Range.singleton $ fromIntegral epochSlots)
        (Gen.maybe $ genOffset epochSlots)

    -- Zip the offsets with the @LocalSlotIndex@s and prune @Nothing@ values
    let index =
            catMaybes
                $   fmap (uncurry SlotIndexOffset)
                .   sequenceA
                <$> zip (getSlotIndex <$> localSlotIndices epochSlots)
                        maybeOffsets

    -- Write the index and fetch back all the values
    fetchedOffsets <- liftIO .
        withSystemTempDirectory "index-test" $ \ dirPath -> do
            let indexPath = dirPath </> "test.index"
            writeEpochIndex epochSlots indexPath index
            traverse (getEpochBlundOffset indexPath)
                    $ localSlotIndices epochSlots

    -- Compare the original set of offsets to the fetched ones
    fetchedOffsets === maybeOffsets

genOffset :: MonadGen m => SlotCount -> m Word64
genOffset epochSlots =
    Gen.word64 (Range.linear 0 (2048 * fromIntegral epochSlots))

tests :: IO Bool
tests = checkSequential $$(discover)
