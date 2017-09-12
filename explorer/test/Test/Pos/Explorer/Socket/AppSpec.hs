
-- | Tests of Pos.Explorer.Socket.Util

module Test.Pos.Explorer.Socket.AppSpec
       ( spec
       ) where

import           Universum

import           Network.Wai.Handler.Warp       (Settings, getPort)
import           Test.Hspec                     (Spec, describe, it, shouldBe)

import           Pos.Explorer.Socket.App        (toConfig)
import           Pos.Explorer.Socket            (NotifierSettings (..))
import           System.Wlog                    (LoggerName(..))


----------------------------------------------------------------------------
-- Spec
----------------------------------------------------------------------------

-- stack test cardano-sl-explorer --fast --test-arguments "-m Test.Pos.Explorer.Socket"

spec :: Spec
spec =
    describe "App" $
        describe "toConfig" $
            it "adds port to warp settings" $
                let ns :: NotifierSettings
                    ns = NotifierSettings { nsPort = 8888 :: Word16 }
                    ln :: LoggerName
                    ln = LoggerName "my-logger"
                    s :: Settings
                    s = toConfig ns ln
                in
                getPort s `shouldBe` 8888
