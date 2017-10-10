module Explorer.State.Test where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Data.Lens (set)
import Explorer.Api.Types (SocketSubscription(..), SocketSubscriptionData(..))
import Explorer.Lenses.State (socket, subscriptions)
import Explorer.State (hasSubscription, initialState, mkSocketSubscriptionItem)
import Explorer.Util.Factory (mkCAddress)
import Pos.Explorer.Socket.Methods (Subscription(..))
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)

testState :: forall r. StateT (Array (Group (Aff r Unit))) Identity Unit
testState =
    describe "Explorer.State" do
        describe "hasSubscription" do
            let initialState' = set (socket <<< subscriptions)
                                    [ mkSocketSubscriptionItem (SocketSubscription SubTx) SocketNoData
                                    , mkSocketSubscriptionItem (SocketSubscription SubAddr) SocketNoData
                                    , mkSocketSubscriptionItem (SocketSubscription SubAddr) (SocketCAddressData $ mkCAddress "my-address")
                                    ]
                                    initialState
            it "returns true if same subscription w/ SocketNoData was added before"
                let subItem = mkSocketSubscriptionItem (SocketSubscription SubAddr) SocketNoData
                    result = hasSubscription subItem initialState'
                in result `shouldEqual` true
            it "returns true if same subscription w/ some data was added before"
                let subItem = mkSocketSubscriptionItem (SocketSubscription SubAddr) (SocketCAddressData $ mkCAddress "my-address")
                    result = hasSubscription subItem initialState'
                in result `shouldEqual` true

            it "returns false if not the same subscription (and w/ SocketNoData) was added before"
                let subItem = mkSocketSubscriptionItem (SocketSubscription SubBlockLastPage) SocketNoData
                    result = hasSubscription subItem initialState'
                in result `shouldEqual` false

            it "returns false if a subscription w/ another data was added before"
                let subItem = mkSocketSubscriptionItem (SocketSubscription SubAddr) (SocketCAddressData $ mkCAddress "another-address")
                    result = hasSubscription subItem initialState'
                in result `shouldEqual` false

            it "returns false if list of subscriptions are emtpy"
                let subItem = mkSocketSubscriptionItem (SocketSubscription SubBlockLastPage) SocketNoData
                    result = hasSubscription subItem initialState
                in result `shouldEqual` false
