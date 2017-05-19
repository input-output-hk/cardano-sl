module Explorer.Update.Test where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Generic (gShow)
import Data.Identity (Identity)
import Data.Lens ((^.), set)
import Data.Time.NominalDiffTime (mkTime)
import Explorer.I18n.Lang (Language(..))
import Explorer.Api.Types (RequestLimit(..), RequestOffset(..), SocketSubscription(..), SocketSubscriptionAction(..))
import Pos.Explorer.Socket.Methods (Subscription(..))
import Explorer.Lenses.State (connected, dbViewBlockPagination, lang, latestBlocks, latestTransactions, socket, totalBlocks, subscriptions)
import Explorer.State (initialState)
import Explorer.Test.MockFactory (mkCBlockEntry, mkEmptyCTxEntry, setEpochSlotOfBlock, setHashOfBlock, setIdOfTx, setTimeOfTx)
import Explorer.Types.Actions (Action(..))
import Explorer.Update (doPaginateBlocksRequest, limitPaginateBlocksRequest, offsetPaginateBlocksRequest, update)
import Explorer.Util.Factory (mkCHash, mkCTxId)
import Explorer.View.Dashboard.Lenses (dashboardViewState)
import Network.RemoteData (RemoteData(..), withDefault)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Pux (onlyEffects, mapEffects)

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

        describe "uses action SocketBlocksUpdated" do
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
                effModel = update (SocketBlocksUpdated (Right newBlocks)) initialState'
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
                    result = offsetPaginateBlocksRequest state
                in result `shouldEqual` 12

        describe "uses action SocketTxsUpdated" do
            -- Mock txs
            let txA = setTimeOfTx (mkTime 0.1) $ setIdOfTx (mkCTxId "A") mkEmptyCTxEntry
                txB = setTimeOfTx (mkTime 0.2) $ setIdOfTx (mkCTxId "B") mkEmptyCTxEntry
                txC = setTimeOfTx (mkTime 1.0) $ setIdOfTx (mkCTxId "C") mkEmptyCTxEntry
                txD = setTimeOfTx (mkTime 2.1) $ setIdOfTx (mkCTxId "D") mkEmptyCTxEntry
                currentTxs =
                    [ txA
                    , txB
                    ]
                -- set `latestTransactions` to simulate that we have already txs before
                initialState' =
                    set latestTransactions (Success currentTxs) initialState
                newTxs =
                    [ txA
                    , txC
                    , txD
                    ]
                effModel = update (SocketTxsUpdated (Right newTxs)) initialState'
                state = _.state effModel
            it "to update latestTransactions w/o duplicates and sorted by time"
                let result = withDefault [] $ state ^. latestTransactions
                    expected =
                        [ txD
                        , txC
                        , txB
                        , txA
                        ]
                in (gShow result) `shouldEqual` (gShow expected)

        describe "handles ReceiveInitialTxs action" do
            -- Mock txs
            let txA = setTimeOfTx (mkTime 0.1) $ setIdOfTx (mkCTxId "A") mkEmptyCTxEntry
                txB = setTimeOfTx (mkTime 0.2) $ setIdOfTx (mkCTxId "B") mkEmptyCTxEntry
                txC = setTimeOfTx (mkTime 1.0) $ setIdOfTx (mkCTxId "C") mkEmptyCTxEntry
                newTxs =
                    [ txA
                    , txC
                    , txB
                    ]
                effModel = update (SocketTxsUpdated (Right newTxs)) initialState
                state = _.state effModel
            it "to update latestTransactions sorted by time"
                let result = withDefault [] $ state ^. latestTransactions
                    expected =
                        [ txC
                        , txB
                        , txA
                        ]
                in (gShow result) `shouldEqual` (gShow expected)

        describe "uses action SocketConnected" do
            it "to update connection to connected"
                let effModel = update (SocketConnected true) initialState
                    state = _.state effModel
                    result = state ^. socket <<< connected
                in result `shouldEqual` true
            it "to update connection to disconnected"
                let effModel = update (SocketConnected false) initialState
                    state = _.state effModel
                    result = state ^. socket <<< connected
                in result `shouldEqual` false

        describe "uses action SocketUpdateSubscriptions with unsubscribe" do
            let subs = [ SocketSubscription SubBlock, SocketSubscription SubTx ]
                action = (SocketUpdateSubscriptions subs UnsubscribePrevSubscriptions)

            it "to update subscriptions if there are none"
                let effModel = update action initialState
                    state = _.state effModel
                    result = state ^. socket <<< subscriptions
                in (gShow result) `shouldEqual` (gShow subs)

            it "to update subscriptions to two subs there are none"
                let effModel = update action initialState
                    state = _.state effModel
                    result = state ^. socket <<< subscriptions
                in (length result) `shouldEqual` 2

            it "to update subscriptions if there are existing subs"
                let initialState' = set (socket <<< subscriptions) subs state
                    effModel = update action initialState
                    state = _.state effModel
                    result = state ^. socket <<< subscriptions
                in (gShow result) `shouldEqual` (gShow subs)

            it "to update subscriptions to two subs if there are existing subs"
                let initialState' = set (socket <<< subscriptions) subs state
                    effModel = update action initialState
                    state = _.state effModel
                    result = state ^. socket <<< subscriptions
                in (length result) `shouldEqual` 2

        -- todo (ks): We need a way to `tick` the Pux state.

        -- describe "uses action SocketUpdateSubscriptions with existing subs" do
        --     let existingSubs = [ SocketSubscription SubBlock
        --                        , SocketSubscription SubTx ]
        --         subs = [ SocketSubscription SubBlock
        --                , SocketSubscription SubTx ]
        --
        --         actionExisting = (SocketUpdateSubscriptions existingSubs KeepPrevSubscriptions)
        --         actionSubs = (SocketUpdateSubscriptions subs KeepPrevSubscriptions)
        --
        --         initialState' = set (socket <<< subscriptions) existingSubs initialState
        --
        --     it "to update subscriptions if there are existing subs"
        --         let effModel = update actionSubs initialState'
        --             state = _.state effModel
        --             result = state ^. socket <<< subscriptions
        --         in (gShow result) `shouldEqual` (gShow (existingSubs <> subs))
        --
        --     it "to update subscriptions to four if there are existing subs"
        --         let effModel = update actionSubs initialState'
        --             state = _.state effModel
        --             result = state ^. socket <<< subscriptions
        --             in (length result) `shouldEqual` 4
        --
        -- describe "uses action SocketUpdateSubscriptions with single existing sub" do
        --     let existingSubs = [ SocketSubscription SubTx ]
        --         subs = [ SocketSubscription SubBlock, SocketSubscription SubTx ]
        --         action = (SocketUpdateSubscriptions subs KeepPrevSubscriptions)
        --
        --         initialState' = set (socket <<< subscriptions) existingSubs initialState
        --
        --     it "to update subscriptions if there are existing subs"
        --         let effModel = update action initialState'
        --             effState = mapEffects id effModel
        --             state = _.state effState
        --             result = state ^. socket <<< subscriptions
        --         in (gShow result) `shouldEqual` (gShow (subs <> existingSubs))
        --
        --     it "to update subscriptions if there are existing subs"
        --         let effModel = update action initialState'
        --             effState = mapEffects id effModel
        --             state = _.state effState
        --             result = state ^. socket <<< subscriptions
        --         in (length result) `shouldEqual` 3
