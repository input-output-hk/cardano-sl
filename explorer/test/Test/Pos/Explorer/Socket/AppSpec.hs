
-- | Tests of Pos.Explorer.Socket.Util

module Test.Pos.Explorer.Socket.AppSpec
       ( spec
       ) where

import           Universum

import           Network.Wai.Handler.Warp       (Settings, getPort)
import           System.Wlog                    (LoggerName)
import           Test.Hspec                     (Spec, describe)
import           Test.Hspec.QuickCheck          (modifyMaxSize, prop)

import           Pos.Explorer.Socket.App        (NotifierSettings(..), toConfig)
import           Test.Pos.Arbitrary.Explorer    ()

----------------------------------------------------------------------------
-- Spec
----------------------------------------------------------------------------

-- stack test cardano-sl-explorer --fast --test-arguments "-m Test.Pos.Explorer.Socket"

spec :: Spec
spec =
    describe "App" $
        describe "toConfig" $
            modifyMaxSize (const 1000) $
                prop "maps config into warp settings"
                    notifierSettingsAreMappedIntoSettings

-- | It tests if `NotifierSettings` is mapped into `Settings` properly
notifierSettingsAreMappedIntoSettings :: NotifierSettings -> LoggerName -> Bool
notifierSettingsAreMappedIntoSettings ns ln =
    let p :: Word16
        p = nsPort ns
        s :: Settings
        s = toConfig ns ln
    in
    -- currently we test port only,
    -- which is the only mapped property so far
    getPort s == fromIntegral p
