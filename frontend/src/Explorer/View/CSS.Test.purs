module Explorer.View.CSS.Test where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Explorer.Routes (Route(..))
import Explorer.Util.Factory (mkCAddress, mkCHash, mkCTxId, mkEpochIndex, mkLocalSlotIndex)
import Explorer.View.CSS (route)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)

testCSS :: forall r. StateT (Array (Group (Aff r Unit))) Identity Unit
testCSS =
    describe "Explorer.View.CSS" do
        describe "creates css classes of each routes" do
            it "Dashboard"
              let result = route Dashboard
              in result `shouldEqual` "explorer-route-dashboard"
            it "Tx"
              let result = route <<< Tx $ mkCTxId "0"
              in result `shouldEqual` "explorer-route-tx"
            it "Address"
              let result = route <<< Address $ mkCAddress "0"
              in result `shouldEqual` "explorer-route-address"
            it "EpochSlot"
              let result = route $ EpochSlot (mkEpochIndex 0) (mkLocalSlotIndex 0)
              in result `shouldEqual` "explorer-route-epoch-slot"
            it "Epoch"
              let result = route <<< Epoch $ mkEpochIndex 0
              in result `shouldEqual` "explorer-route-epoch"
            it "Calculator"
              let result = route Calculator
              in result `shouldEqual` "explorer-route-calculator"
            it "Block"
              let result = route <<< Block $ mkCHash "0"
              in result `shouldEqual` "explorer-route-slot"
            it "Genesis"
              let result = route GenesisBlock
              in result `shouldEqual` "explorer-route-genesis"
            it "Playground"
              let result = route Playground
              in result `shouldEqual` "explorer-route-playground"
