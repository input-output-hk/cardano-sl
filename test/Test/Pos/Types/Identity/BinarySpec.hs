-- | This module tests Binary instances.

module Test.Pos.Types.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec                    (Spec, describe)
import           Universum

import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Core.Arbitrary            ()
import qualified Pos.Core.Fee                  as Fee
import           Pos.Data.Attributes           (Attributes (..))
import           Pos.Infra.Arbitrary           ()
import qualified Pos.Types                     as T

import           Test.Pos.Util                 (binaryTest)

spec :: Spec
spec = describe "Types" $ do
    -- 100 is not enough to catch some bugs (e.g. there was a bug with
    -- addresses that only manifested when address's CRC started with 0x00)
    describe "Bi instances" $ do
        describe "Core.Address" $ do
            binaryTest @T.Address
        describe "Core.Types" $ do
            binaryTest @T.Timestamp
            binaryTest @T.EpochIndex
            binaryTest @(Attributes ())
            binaryTest @T.Coin
            binaryTest @T.CoinPortion
            binaryTest @T.LocalSlotIndex
            binaryTest @T.SlotId
            binaryTest @T.EpochOrSlot
            binaryTest @T.SharedSeed
            binaryTest @T.ChainDifficulty
            binaryTest @T.BlockVersionData
            binaryTest @(DataMsg T.ProxySKHeavy)
            binaryTest @(DataMsg T.ProxySKLight)
        describe "Core.Fee" $ do
            binaryTest @Fee.Coeff
            binaryTest @Fee.TxSizeLinear
            binaryTest @Fee.TxFeePolicy
        describe "Core.Script" $ do
            binaryTest @T.Script
        describe "Core.Version" $ do
            binaryTest @T.ApplicationName
            binaryTest @T.SoftwareVersion
            binaryTest @T.BlockVersion
