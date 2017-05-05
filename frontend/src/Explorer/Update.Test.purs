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
import Explorer.Lenses.State (dbViewBlockPagination, lang, latestBlocks, pullLatestBlocks, totalBlocks)
import Explorer.State (initialState)
import Explorer.Test.MockFactory (mkCBlockEntry, setEpochSlotOfBlock, setHashOfBlock)
import Explorer.Types.Actions (Action(..))
import Explorer.Update (doPaginateBlocksRequest, limitPaginateBlocksRequest, offsetPaginateBlocksRequest, update)
import Explorer.Util.Factory (mkCHash)
import Explorer.View.Dashboard.Lenses (dashboardViewState)
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

        describe "doPaginateBlocksRequest" do
            let currentBlocks =
                    [ mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    ]
                state =
                    set latestBlocks (Success currentBlocks) $
                    set totalBlocks (Success 117) $
                    set (dashboardViewState <<< dbViewBlockPagination) 1 initialState

            it "should return true if we are on page 1, but were not at another page before"
                let result = doPaginateBlocksRequest state 2 10
                in result `shouldEqual` true

        describe "limitPaginateBlocksRequest" do
            let -- add 12 blocks
                currentBlocks =
                    [ mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    ]
                initialState' =
                    set latestBlocks (Success currentBlocks) initialState

            it "if we want to go from page 1 to 2"
                let state = set (dashboardViewState <<< dbViewBlockPagination) 1 initialState'
                    result = limitPaginateBlocksRequest state 2 10 2
                in result `shouldEqual` 10

            it "if we want to jump from page 1 to 30"
                let state = set (dashboardViewState <<< dbViewBlockPagination) 1 initialState'
                    result = limitPaginateBlocksRequest state 30 10 2
                in result `shouldEqual` 290

        describe "offsetPaginateBlocksRequest" do
            let -- add 12 blocks
                currentBlocks =
                    [ mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    , mkCBlockEntry
                    ]
                initialState' =
                    set latestBlocks (Success currentBlocks) initialState

            it "if we want to go from page 1 to 2"
                let state = set (dashboardViewState <<< dbViewBlockPagination) 1 initialState'
                    result = offsetPaginateBlocksRequest state 10
                in result `shouldEqual` 22
