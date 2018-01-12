{-# LANGUAGE RankNTypes #-}

-- | Helpers for 'BlockProperty'.

module Test.Pos.Block.Property
       ( blockPropertySpec
       ) where

import           Universum

import           Test.Hspec (Spec)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Testable)

import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Core (HasConfiguration)
import           Pos.Delegation (HasDlgConfiguration)
import           Pos.Ssc.Configuration (HasSscConfiguration)
import           Test.Pos.Block.Logic.Mode (BlockProperty, blockPropertyTestable)

-- | Specialized version of 'prop' function from 'hspec'.
blockPropertySpec ::
       (HasNodeConfiguration, HasDlgConfiguration, HasSscConfiguration, Testable a)
    => String
    -> (HasConfiguration => BlockProperty a)
    -> Spec
blockPropertySpec description bp = prop description (blockPropertyTestable bp)
