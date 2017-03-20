module Explorer.Util.Factory.Test where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Data.Lens ((^.))
import Data.Tuple (Tuple(..))
import Explorer.Util.Factory (mkCAddress, mkCoin, sumCoinOfInputsOutputs)
import Pos.Core.Lenses.Types (_Coin, getCoin)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)

testFactoryUtil :: forall r. StateT (Array (Group (Aff r Unit))) Identity Unit
testFactoryUtil =
    describe "Explorer.Util.Factory" do
        describe "sumCoinOfInputsOutputs" do
          it "sums a list of Coins"
            let list =  [ (Tuple (mkCAddress "a") (mkCoin 3))
                        , (Tuple (mkCAddress "b") (mkCoin 3))
                        ]
                coin = sumCoinOfInputsOutputs list
                result = coin ^. (_Coin <<< getCoin)
            in result `shouldEqual` 6
          it "sums an empty list of Coins"
            let coin = sumCoinOfInputsOutputs []
                result = coin ^. (_Coin <<< getCoin)
            in result `shouldEqual` 0
