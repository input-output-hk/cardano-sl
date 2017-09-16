-- | Configuration for running tests

module Test.Pos.Configuration (testConf) where

import           Universum

import           Pos.Launcher.Configuration (Configuration (..))

testConf :: Configuration
testConf = Configuration
    { ccCore   = undefined
    , ccInfra  = undefined
    , ccUpdate = undefined
    , ccGt     = undefined
    , ccNode   = undefined
    }
