-- | This module tests SafeCopy instances.

module Test.Pos.Types.Identity.SafeCopySpec
       ( spec
       ) where

import           Universum

import           Test.Hspec    (Spec, describe)

import qualified Pos.Core      as Core
import qualified Pos.Txp       as Txp

import           Test.Pos.Util (giveCoreConf, safeCopyTest)

spec :: Spec
spec = giveCoreConf $ describe "Types" $ do
    describe "SafeCopy instances" $ do
        safeCopyTest @Core.EpochIndex
        safeCopyTest @Core.LocalSlotIndex
        safeCopyTest @Core.SlotId
        safeCopyTest @Core.Coin
        safeCopyTest @Core.Address
        safeCopyTest @Core.SharedSeed
        safeCopyTest @Core.ChainDifficulty
        safeCopyTest @Core.VssCertificate

        safeCopyTest @Txp.TxInWitness
        safeCopyTest @Txp.TxIn
        safeCopyTest @Txp.TxOut
        safeCopyTest @Txp.Tx
