{-# LANGUAGE RankNTypes #-}

-- | Helpers for 'BlockProperty'.

module Test.Pos.Block.Property
       ( blockPropertySpec
       ) where

import           Universum

import           Test.Hspec (Spec)
import           Test.Hspec.QuickCheck (prop)

import           Pos.Core (HasConfiguration)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Delegation (HasDlgConfiguration)

import           Test.Pos.Block.Logic.Mode (BlockProperty, blockPropertyTestable)
import           Test.QuickCheck.Property (Testable)

-- | Specialized version of 'prop' function from 'hspec'.
blockPropertySpec ::
       (HasDlgConfiguration, Testable a)
    => ProtocolMagic
    -> String
    -> (HasConfiguration => BlockProperty a)
    -> Spec
blockPropertySpec pm description bp = prop description (blockPropertyTestable pm bp)
