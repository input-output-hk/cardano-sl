{-# LANGUAGE TypeApplications #-}

-- | This module tests SafeCopy instances.

<<<<<<< HEAD
<<<<<<< HEAD
module Test.Pos.Types.Identity.SafeCopySpec
=======
module Test.Pos.Types.SafeCopySpec
>>>>>>> [POS-22] Add identity testing for serialization
=======
module Test.Pos.Types.Identity.SafeCopySpec
>>>>>>> feature/pos39-benchmarks
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
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> feature/pos39-benchmarks
        prop "LocalSlotindex" (safeCopyEncodeDecode @T.LocalSlotIndex)
        prop "SlotId" (safeCopyEncodeDecode @T.SlotId)
        prop "Coin" (safeCopyEncodeDecode @T.Coin)
        prop "Address" (safeCopyEncodeDecode @T.Address)
        prop "TxIn" (safeCopyEncodeDecode @T.TxIn)
        prop "TxOut" (safeCopyEncodeDecode @T.TxOut)
        prop "Tx" (safeCopyEncodeDecode @T.Tx)
<<<<<<< HEAD
=======
        prop "Localslotindex" (safeCopyEncodeDecode @T.LocalSlotIndex)
        prop "SlotId" (safeCopyEncodeDecode @T.SlotId)
        prop "Coin" (safeCopyEncodeDecode @T.Coin)
--        prop "Address" (safeCopyEncodeDecode @T.Address) FatalError
        prop "TxIn" (safeCopyEncodeDecode @T.TxIn)
--        prop "TxOut" (safeCopyEncodeDecode @T.TxOut)
--        prop "Tx" (safeCopyEncodeDecode @T.Tx)
>>>>>>> [POS-22] Add identity testing for serialization
=======
>>>>>>> feature/pos39-benchmarks
        prop "FtsSeed" (safeCopyEncodeDecode @T.FtsSeed)
        prop "Opening" (safeCopyEncodeDecode @T.Opening)
        prop "Chaindifficulty" (safeCopyEncodeDecode @T.ChainDifficulty)
        prop "MpcProof" (safeCopyEncodeDecode @T.MpcProof)
