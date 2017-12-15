{-# LANGUAGE RankNTypes #-}

module Test.Pos.Helpers
       ( blockPropertySpec
       ) where

import           Universum

import           Test.Hspec (Spec)
import           Test.Hspec.QuickCheck (prop)

import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Core (HasConfiguration)
import           Pos.Delegation (HasDlgConfiguration)
import           Pos.Ssc.Configuration (HasSscConfiguration)
import           Test.Pos.Block.Logic.Mode (BlockProperty, blockPropertyTestable)

----------------------------------------------------------------------------
-- Various properties and predicates
----------------------------------------------------------------------------

-- | Specialized version of 'prop' function from 'hspec'.
blockPropertySpec ::
       (HasNodeConfiguration, HasDlgConfiguration, HasSscConfiguration)
    => String
    -> (HasConfiguration => BlockProperty a)
    -> Spec
blockPropertySpec description bp = prop description (blockPropertyTestable bp)
