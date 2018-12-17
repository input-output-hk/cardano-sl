-- | This module tests SafeCopy instances.

module Test.Pos.Types.Identity.SafeCopySpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)

import qualified Pos.Chain.Ssc as Ssc
import qualified Pos.Chain.Txp as Txp
import qualified Pos.Core as Core

import           Test.Pos.Binary.Helpers (safeCopyTest)
import           Test.Pos.Chain.Ssc.Arbitrary ()
import           Test.Pos.Chain.Txp.Arbitrary ()
import           Test.Pos.Infra.Arbitrary.Txp ()

spec :: Spec
spec = describe "Types" $ do
    describe "SafeCopy instances" $ do
        safeCopyTest @Core.EpochIndex
        safeCopyTest @Core.LocalSlotIndex
        safeCopyTest @Core.SlotId
        safeCopyTest @Core.Coin
        safeCopyTest @Core.Address
        safeCopyTest @Core.Address'
        safeCopyTest @Core.SharedSeed
        safeCopyTest @Core.ChainDifficulty
        safeCopyTest @Ssc.VssCertificate

        safeCopyTest @Txp.TxInWitness
        safeCopyTest @Txp.TxIn
        safeCopyTest @Txp.TxOut
        safeCopyTest @Txp.Tx
