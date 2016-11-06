{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Specification of Pos.FollowTheSatoshi

module Test.Pos.FollowTheSatoshiSpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe, pending)
import           Test.Hspec.QuickCheck (prop)
import           Universum

spec :: Spec
spec = describe "FollowTheSatoshi" $ do
    prop "followTheSatoshi" pending
