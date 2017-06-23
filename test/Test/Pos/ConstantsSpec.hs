-- | This module tests some invariants on constants.

module Test.Pos.ConstantsSpec
       ( spec
       ) where

import           Universum

import qualified Pos.Update.Constants as C

import           Test.Hspec           (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = describe "Constants" $ do
    describe "UpdateConstants" $ do
        it "genesisAppNames" $ do
            for_ C.genesisAppNames $ \(_, name) ->
                name `shouldSatisfy` isRight
