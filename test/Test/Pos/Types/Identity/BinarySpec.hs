-- | This module tests Binary instances.

module Test.Pos.Types.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec    (Spec, describe)
import           Universum

import qualified Pos.Core.Fee  as Fee
import qualified Pos.Types     as T

import           Test.Pos.Util (binaryTest)

spec :: Spec
spec = describe "Types" $ do
    -- 100 is not enough to catch some bugs (e.g. there was a bug with
    -- addresses that only manifested when address's CRC started with 0x00)
    describe "Bi instances" $ do
        binaryTest @T.EpochIndex
        binaryTest @T.LocalSlotIndex
        binaryTest @T.SlotId
        binaryTest @T.Coin
        binaryTest @T.Address
        binaryTest @T.SharedSeed
        binaryTest @T.ChainDifficulty
        binaryTest @T.Timestamp
        binaryTest @Fee.Coeff
        binaryTest @Fee.TxSizeLinear
        binaryTest @Fee.TxFeePolicy
