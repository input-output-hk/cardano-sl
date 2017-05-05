module Explorer.Util.Data.Test where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Generic (gShow)
import Data.Identity (Identity)
import Data.Time.NominalDiffTime (mkTime)
import Explorer.Test.MockFactory (mkCBlockEntry, setTimeOfBlock)
import Explorer.Util.Data (sortBlocksByTime, sortBlocksByTime')
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
