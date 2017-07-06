module Explorer.Api.Socket.Test where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Explorer.Api.Socket (toEvent)
import Pos.Explorer.Socket.Methods (ClientEvent(..), ServerEvent(..))
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)


testApiSocket :: forall r. StateT (Array (Group (Aff r Unit))) Identity Unit
testApiSocket =
    describe "Explorer.Api.Socket" do
        describe "toEvent" do
            it "converts a ClientEvent to an 'event' string" do
                (toEvent CallMe) `shouldEqual` "CallMe"
            it "converts a ServerEvent to an 'event' string " do
                (toEvent AddrUpdated) `shouldEqual` "AddrUpdated"
