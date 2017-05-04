module Explorer.Update.Test where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Either (Either(..))
import Data.Generic (gShow)
import Data.Identity (Identity)
import Data.Lens ((^.))
import Data.Time.NominalDiffTime (mkTime)
import Explorer.I18n.Lang (Language(..))
import Explorer.Lenses.State (lang, latestBlocks, pullLatestBlocks, totalBlocks)
import Explorer.State (initialState)
import Explorer.Types.Actions (Action(..))
import Explorer.Update (update)
import Explorer.Util.Factory (mkCBlockEntryByTime)
import Network.RemoteData (withDefault)
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
            let blocks =  [ mkCBlockEntryByTime $ mkTime 1.0
                          , mkCBlockEntryByTime $ mkTime 2.0
                          , mkCBlockEntryByTime $ mkTime 3.0
                          ]
                effModel = update (ReceiveInitialBlocks (Right blocks)) initialState
                state = _.state effModel
                latestBlocks' = withDefault [] $ state ^. latestBlocks
            it "to update latestBlocks" do
                (gShow latestBlocks') `shouldEqual` (gShow blocks)
            it "to update pullLatestBlocks" do
                (state ^. pullLatestBlocks) `shouldEqual` true
