module Explorer.Update.Test where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Generic (gShow)
import Data.Identity (Identity)
import Data.Lens ((^.), set)
import Explorer.I18n.Lang (Language(..))
import Explorer.Lenses.State (lang, latestBlocks, pullLatestBlocks, totalBlocks)
import Explorer.State (initialState)
import Explorer.Test.MockFactory (mkCBlockEntry, setEpochSlotOfBlock, setHashOfBlock)
import Explorer.Types.Actions (Action(..))
import Explorer.Update (update)
import Explorer.Util.Factory (mkCHash)
import Network.RemoteData (RemoteData(..), withDefault)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)

testUpdate :: forall r. StateT (Array (Group (Aff r Unit))) Identity Unit
testUpdate =
    describe "Explorer.Update" do

        describe "uses action SetLanguage" do
            it "to update language"
                let effModel =  update (SetLanguage German) initialState
                    state = _.state effModel
                    result = state ^. lang
                in result `shouldEqual` German

        describe "uses action ReceiveTotalBlocks" do
            it "to update totalBlocks"
                let effModel =  update (ReceiveTotalBlocks (Right 12)) initialState
                    state = _.state effModel
                    result = withDefault 0 $ state ^. totalBlocks
                in result `shouldEqual` 12

        describe "uses action ReceiveInitialBlocks" do
            let blocks =  [ setEpochSlotOfBlock 0 3 mkCBlockEntry
                          , setEpochSlotOfBlock 0 2 mkCBlockEntry
                          , setEpochSlotOfBlock 0 1 mkCBlockEntry
                          ]
                effModel = update (ReceiveInitialBlocks (Right blocks)) initialState
                state = _.state effModel
                latestBlocks' = withDefault [] $ state ^. latestBlocks
            it "to update latestBlocks" do
                (gShow latestBlocks') `shouldEqual` (gShow blocks)
            it "to update pullLatestBlocks" do
                (state ^. pullLatestBlocks) `shouldEqual` true

        describe "uses action ReceiveBlocksUpdate" do
            -- Mock blocks with epoch, slots and hashes
            let blockA = setEpochSlotOfBlock 0 1 $ setHashOfBlock (mkCHash "A") mkCBlockEntry
                blockB = setEpochSlotOfBlock 0 2 $ setHashOfBlock (mkCHash "B") mkCBlockEntry
                blockC = setEpochSlotOfBlock 1 0 $ setHashOfBlock (mkCHash "C") mkCBlockEntry
                blockD = setEpochSlotOfBlock 1 1 $ setHashOfBlock (mkCHash "D") mkCBlockEntry
                currentBlocks =
                    [ blockA
                    , blockB
                    ]
                -- set `latestBlocks` + `totalBlocks` to simulate that we have already blocks before
                initialState' =
                    set latestBlocks (Success currentBlocks) $
                    set totalBlocks (Success $ length currentBlocks) initialState
                newBlocks =
                    [ blockB
                    , blockC
                    , blockD
                    ]
                effModel = update (ReceiveBlocksUpdate (Right newBlocks)) initialState'
                state = _.state effModel
            it "to update latestBlocks w/o duplicates"
                let result = withDefault [] $ state ^. latestBlocks
                    expected =
                        [ blockD
                        , blockC
                        , blockB
                        , blockA
                        ]
                in (gShow result) `shouldEqual` (gShow expected)
            it "to count totalBlocks"
                let result = withDefault 0 $ state ^. totalBlocks
                in result `shouldEqual` 4

        describe "uses action ReceivePaginatedBlocks" do
            -- Mock blocks with epoch, slots and hashes
            let blockA = setEpochSlotOfBlock 2 1 $ setHashOfBlock (mkCHash "A") mkCBlockEntry
                blockB = setEpochSlotOfBlock 2 0 $ setHashOfBlock (mkCHash "B") mkCBlockEntry
                blockC = setEpochSlotOfBlock 1 9 $ setHashOfBlock (mkCHash "C") mkCBlockEntry
                blockD = setEpochSlotOfBlock 1 8 $ setHashOfBlock (mkCHash "D") mkCBlockEntry
                blockE = setEpochSlotOfBlock 1 7 $ setHashOfBlock (mkCHash "E") mkCBlockEntry
                currentBlocks =
                    [ blockA
                    , blockB
                    ]
                -- set `latestBlocks` to simulate that we have already blocks before
                initialState' =
                    set latestBlocks (Success currentBlocks) initialState
                paginatedBlocks =
                    [ blockC
                    , blockD
                    , blockE
                    ]
                effModel = update (ReceivePaginatedBlocks (Right paginatedBlocks)) initialState'
                state = _.state effModel
            it "to add blocks to latestBlocks"
                let result = withDefault [] $ state ^. latestBlocks
                    expected =
                        [ blockA
                        , blockB
                        , blockC
                        , blockD
                        , blockE
                        ]
                in (gShow result) `shouldEqual` (gShow expected)
