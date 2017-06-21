-- | This module tests Binary instances.

module Test.Pos.Types.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Universum

import qualified Pos.Types             as T

import           Test.Pos.Util         (binaryTest)

spec :: Spec
spec = describe "Types" $ do
    -- 100 is not enough to catch some bugs (e.g. there was a bug with
    -- addresses that only manifested when address's CRC started with 0x00)
    describe "Bi instances" $ do
        modifyMaxSuccess (const 10000) $ do
            binaryTest @T.EpochIndex
            binaryTest @T.LocalSlotIndex
            binaryTest @T.SlotId
            binaryTest @T.Coin
            binaryTest @T.Address
            binaryTest @T.SharedSeed
            binaryTest @T.ChainDifficulty
