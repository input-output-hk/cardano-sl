module Explorer.Util.Data.Test where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Generic (gShow)
import Data.Identity (Identity)
import Data.Time.NominalDiffTime (mkTime)
import Explorer.Test.MockFactory (mkCBlockEntry, mkEmptyCTxEntry, setEpochSlotOfBlock, setHashOfBlock, setIdOfTx, setTimeOfBlock, setTimeOfTx)
import Explorer.Util.Data (sortBlocksByEpochSlot, sortBlocksByEpochSlot', sortBlocksByTime, sortBlocksByTime', sortTxsByTime, sortTxsByTime', unionBlocks, unionTxs)
import Explorer.Util.Factory (mkCHash, mkCTxId)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)


testDataUtil :: forall r. StateT (Array (Group (Aff r Unit))) Identity Unit
testDataUtil =
    describe "Explorer.Util.Data" do
        describe "sortBlocksByTime" do
            let blockA = setTimeOfBlock (mkTime 0.1) mkCBlockEntry
                blockB = setTimeOfBlock (mkTime 1.0) mkCBlockEntry
                blockC = setTimeOfBlock (mkTime 1.1) mkCBlockEntry
                blockD = setTimeOfBlock (mkTime 2.0) mkCBlockEntry
            it "sort ascending"
                let list =
                        [ blockD
                        , blockB
                        , blockA
                        , blockC
                        ]
                    expected =
                        [ blockA
                        , blockB
                        , blockC
                        , blockD
                        ]
                in (gShow $ sortBlocksByTime list) `shouldEqual` (gShow expected)
            it "sort descending"
                let list =
                        [ blockD
                        , blockB
                        , blockA
                        , blockC
                        ]
                    expected =
                        [ blockD
                        , blockC
                        , blockB
                        , blockA
                        ]
                in (gShow $ sortBlocksByTime' list) `shouldEqual` (gShow expected)
        describe "sortBlocksByEpochSlot" do
            let blockA = setEpochSlotOfBlock 0 1 mkCBlockEntry
                blockB = setEpochSlotOfBlock 0 2 mkCBlockEntry
                blockC = setEpochSlotOfBlock 1 2 mkCBlockEntry
                blockD = setEpochSlotOfBlock 2 0 mkCBlockEntry
            it "sort ascending (default)"
                let list =
                        [ blockD
                        , blockA
                        , blockC
                        , blockB
                        ]
                    expected =
                        [ blockA
                        , blockB
                        , blockC
                        , blockD
                        ]
                in (gShow $ sortBlocksByEpochSlot list) `shouldEqual` (gShow expected)
            it "sort descending"
                let list =
                        [ blockD
                        , blockA
                        , blockC
                        , blockB
                        ]
                    expected =
                        [ blockD
                        , blockC
                        , blockB
                        , blockA
                        ]
                in (gShow $ sortBlocksByEpochSlot' list) `shouldEqual` (gShow expected)

        describe "unionBlocks" do
            let blockA = setHashOfBlock (mkCHash "A") mkCBlockEntry
                blockB = setHashOfBlock (mkCHash "B") mkCBlockEntry
                blockC = setHashOfBlock (mkCHash "C") mkCBlockEntry
                blockD = setHashOfBlock (mkCHash "D") mkCBlockEntry
            it "to remove duplicates"
                let listA =
                        [ blockA
                        , blockB
                        , blockC
                        ]
                    listB =
                        [ blockD
                        , blockA
                        , blockA
                        , blockB
                        ]
                    expected =
                        [ blockA
                        , blockB
                        , blockC
                        , blockD
                        ]
                in (gShow $ unionBlocks listA listB) `shouldEqual` (gShow expected)

        describe "unionTxs" do
            let txA = setIdOfTx (mkCTxId "A") mkEmptyCTxEntry
                txB = setIdOfTx (mkCTxId "B") mkEmptyCTxEntry
                txC = setIdOfTx (mkCTxId "C") mkEmptyCTxEntry
                txD = setIdOfTx (mkCTxId "D") mkEmptyCTxEntry
            it "to remove duplicates"
                let listA =
                        [ txA
                        , txB
                        , txC
                        ]
                    listB =
                        [ txD
                        , txA
                        , txA
                        , txB
                        ]
                    expected =
                        [ txA
                        , txB
                        , txC
                        , txD
                        ]
                in (gShow $ unionTxs listA listB) `shouldEqual` (gShow expected)

        describe "sortTxsByTime" do
            let txA = setTimeOfTx (mkTime 0.1) mkEmptyCTxEntry
                txB = setTimeOfTx (mkTime 1.0) mkEmptyCTxEntry
                txC = setTimeOfTx (mkTime 1.1) mkEmptyCTxEntry
                txD = setTimeOfTx (mkTime 2.0) mkEmptyCTxEntry
            it "sort ascending"
                let list =
                        [ txD
                        , txB
                        , txA
                        , txC
                        ]
                    expected =
                        [ txA
                        , txB
                        , txC
                        , txD
                        ]
                in (gShow $ sortTxsByTime list) `shouldEqual` (gShow expected)
            it "sort descending"
                let list =
                        [ txD
                        , txB
                        , txA
                        , txC
                        ]
                    expected =
                        [ txD
                        , txC
                        , txB
                        , txA
                        ]
                in (gShow $ sortTxsByTime' list) `shouldEqual` (gShow expected)
