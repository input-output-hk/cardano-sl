{-# LANGUAGE RankNTypes #-}

-- | Helpers for 'BlockProperty'.

module Test.Pos.Block.Property
       ( blockPropertySpec
       ) where

import           Universum

import           Test.Hspec (Spec)
import           Test.Hspec.QuickCheck (prop)

import           Pos.Chain.Delegation (HasDlgConfiguration)
import           Pos.Chain.Genesis as Genesis (Config)
import           Pos.Chain.Update (HasUpdateConfiguration, updateConfiguration)

import           Test.Pos.Block.Logic.Mode (BlockProperty,
                     blockPropertyTestable)
import           Test.QuickCheck.Property (Testable)

-- | Specialized version of 'prop' function from 'hspec'.
blockPropertySpec ::
       (HasDlgConfiguration, HasUpdateConfiguration, Testable a)
    => String
    -> (Genesis.Config -> BlockProperty a)
    -> Spec
blockPropertySpec description bp =
    prop description (blockPropertyTestable updateConfiguration bp)
