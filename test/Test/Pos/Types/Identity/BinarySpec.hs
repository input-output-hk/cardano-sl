-- | This module tests Binary instances.

module Test.Pos.Types.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import qualified Pos.Types             as T

import           Test.Pos.Util         (binaryEncodeDecode)

spec :: Spec
spec = describe "Types" $ do
    describe "Bi instances" $ do
        prop "Epochindex" (binaryEncodeDecode @T.EpochIndex)
        prop "Localslotindex" (binaryEncodeDecode @T.LocalSlotIndex)
        prop "SlotId" (binaryEncodeDecode @T.SlotId)
        prop "Coin" (binaryEncodeDecode @T.Coin)
        prop "Address" (binaryEncodeDecode @T.Address)
        prop "TxInWitness" (binaryEncodeDecode @T.TxInWitness)
        prop "TxDistribution" (binaryEncodeDecode @T.TxDistribution)
        prop "TxIn" (binaryEncodeDecode @T.TxIn)
        prop "TxOut" (binaryEncodeDecode @T.TxOut)
        prop "Tx" (binaryEncodeDecode @T.Tx)
        prop "SharedSeed" (binaryEncodeDecode @T.SharedSeed)
        prop "Chaindifficulty" (binaryEncodeDecode @T.ChainDifficulty)
        prop "UpdateProposal" (binaryEncodeDecode @T.UpdateProposal)
        prop "UpdateVote" (binaryEncodeDecode @T.UpdateVote)
        prop "UpdateData" (binaryEncodeDecode @T.UpdateData)
        prop "SystemTag" (binaryEncodeDecode @T.SystemTag)
