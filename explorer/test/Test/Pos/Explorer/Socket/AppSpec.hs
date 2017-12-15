
-- | Tests of Pos.Explorer.Socket.Util

module Test.Pos.Explorer.Socket.AppSpec
       ( spec
       ) where

import           Universum

import           Network.Wai.Handler.Warp (Settings, getPort)
import           Test.Hspec (Spec, describe, it, shouldBe)

import           Pos.Explorer.Socket.App (NotifierSettings (..), toConfig)
import           Test.Pos.Explorer.MockFactory (testLoggerName)

----------------------------------------------------------------------------
-- Spec
----------------------------------------------------------------------------

-- stack test cardano-sl-explorer --fast --test-arguments "-m Test.Pos.Explorer.Socket"

spec :: Spec
spec =
    describe "App" $
        describe "toConfig" $
            it "maps config into warp settings" $ do
                let p = 8080
                    ns = NotifierSettings { nsPort = p }
                    s :: Settings
                    s = toConfig ns testLoggerName
                -- currently we test port only,
                -- which is the only mapped property so far
                getPort s `shouldBe` fromIntegral p
