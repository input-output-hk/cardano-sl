module Data.Time.Test where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Data.Time.Duration (Seconds(..))
import Data.Time.NominalDiffTime (NominalDiffTime(..), mkTime, unwrapSeconds)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)

testNominalDiffTime :: forall r. StateT (Array (Group (Aff r Unit))) Identity Unit
testNominalDiffTime =
    describe "Data.Time.NominalDiffTime" do
        describe "mkTime" do
          it "makes an instance of NominalDiffTime"
            let seconds = 12.0
                time = mkTime seconds
            in time `shouldEqual` (NominalDiffTime $ Seconds seconds)
        describe "unwrapSeconds" do
          it "unwraps value of Seconds"
            let seconds = 12.0
                time = mkTime seconds
            in unwrapSeconds time `shouldEqual` seconds
