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
import Explorer.Lenses.State (lang, latestBlocks, latestTransactions, totalBlocks)
import Explorer.State (initialState)
import Explorer.Test.MockFactory (mkCBlockEntry, mkEmptyCTxEntry, setEpochSlotOfBlock, setHashOfBlock, setIdOfTx, setTimeOfTx)
import Explorer.Types.Actions (Action(..))
import Explorer.Update (update)
import Explorer.Util.Factory (mkCHash, mkCTxId)
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
                    , blockC
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
                in (gShow result) `shouldEqual` (gShow paginatedBlocks)

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
