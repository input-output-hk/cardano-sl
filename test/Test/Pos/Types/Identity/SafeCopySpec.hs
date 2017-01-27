-- | This module tests SafeCopy instances.

module Test.Pos.Types.Identity.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec    (Spec, describe)
import           Universum

import qualified Pos.Types     as T

import           Test.Pos.Util (safeCopyTest)

spec :: Spec
spec = describe "Types" $ do
    describe "SafeCopy instances" $ do
        safeCopyTest @T.EpochIndex
        safeCopyTest @T.LocalSlotIndex
        safeCopyTest @T.SlotId
        safeCopyTest @T.Coin
        safeCopyTest @T.Address
        safeCopyTest @T.TxInWitness
        safeCopyTest @T.TxDistribution
        safeCopyTest @T.TxIn
        safeCopyTest @T.TxOut
        safeCopyTest @T.Tx
        safeCopyTest @T.SharedSeed
        safeCopyTest @T.ChainDifficulty
