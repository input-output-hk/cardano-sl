{-# LANGUAGE TypeApplications #-}

-- | This module tests SafeCopy instances.

module Test.Pos.Types.Identity.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import qualified Pos.Types             as T

import           Test.Pos.Util         (safeCopyEncodeDecode)

spec :: Spec
spec = describe "Types" $ do
    describe "SafeCopy instances" $ do
        prop "Epochindex" (safeCopyEncodeDecode @T.EpochIndex)
        prop "LocalSlotindex" (safeCopyEncodeDecode @T.LocalSlotIndex)
        prop "SlotId" (safeCopyEncodeDecode @T.SlotId)
        prop "Coin" (safeCopyEncodeDecode @T.Coin)
        prop "Address" (safeCopyEncodeDecode @T.Address)
        prop "TxIn" (safeCopyEncodeDecode @T.TxIn)
        prop "TxOut" (safeCopyEncodeDecode @T.TxOut)
        prop "Tx" (safeCopyEncodeDecode @T.Tx)
        prop "FtsSeed" (safeCopyEncodeDecode @T.FtsSeed)
        prop "Opening" (safeCopyEncodeDecode @T.Opening)
        prop "Chaindifficulty" (safeCopyEncodeDecode @T.ChainDifficulty)
        -- TODO: there's no such type anymore
        -- prop "MpcProof" (safeCopyEncodeDecode @T.MpcProof)
