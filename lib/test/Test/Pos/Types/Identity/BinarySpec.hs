-- | This module tests Binary instances.

module Test.Pos.Types.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec (Spec, describe)
import           Universum

import           Pos.Arbitrary.Core ()
import           Pos.Arbitrary.Infra ()
import qualified Pos.Core as T
import           Pos.Data.Attributes (Attributes (..))
import           Pos.Util.Chrono (NE, NewestFirst, OldestFirst)

import           Test.Pos.Cbor.CborSpec (U)
import           Test.Pos.Helpers (binaryTest, msgLenLimitedTest)
import           Test.Pos.Util (withDefConfiguration, withDefInfraConfiguration)

spec :: Spec
spec = withDefInfraConfiguration $ withDefConfiguration $ describe "Types" $ do
    -- 100 is not enough to catch some bugs (e.g. there was a bug with
    -- addresses that only manifested when address's CRC started with 0x00)
    describe "Bi instances" $ do
        describe "Core.Address" $ do
            binaryTest @T.Address
            binaryTest @T.Address'
            binaryTest @T.AddrType
            binaryTest @T.AddrStakeDistribution
            binaryTest @T.AddrSpendingData
        describe "Core.Types" $ do
            binaryTest @T.Timestamp
            binaryTest @T.TimeDiff
            binaryTest @T.EpochIndex
            binaryTest @T.Coin
            binaryTest @T.CoinPortion
            binaryTest @T.LocalSlotIndex
            binaryTest @T.SlotId
            binaryTest @T.EpochOrSlot
            binaryTest @T.SharedSeed
            binaryTest @T.ChainDifficulty
            binaryTest @T.SoftforkRule
            binaryTest @T.BlockVersionData
            binaryTest @(Attributes ())
            binaryTest @(Attributes T.AddrAttributes)
        describe "Core.Fee" $ do
            binaryTest @T.Coeff
            binaryTest @T.TxSizeLinear
            binaryTest @T.TxFeePolicy
        describe "Core.Script" $ do
            binaryTest @T.Script
        describe "Core.Vss" $ do
            binaryTest @T.VssCertificate
        describe "Core.Version" $ do
            binaryTest @T.ApplicationName
            binaryTest @T.SoftwareVersion
            binaryTest @T.BlockVersion
        describe "Util" $ do
            binaryTest @(NewestFirst NE U)
            binaryTest @(OldestFirst NE U)
    describe "Message length limit" $ do
        msgLenLimitedTest @T.VssCertificate
