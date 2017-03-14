{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Instances and other stuff for 'Undefined'. We don't want GHC to
-- emit warnings about them, so we have to put them into a separate module.
module Pos.Util.Undefined () where

import           Universum

import           Data.Binary     (Binary)
import           Data.Hashable   (Hashable)
import           Data.SafeCopy   (base, deriveSafeCopySimple)
import           Test.QuickCheck (Arbitrary (..))


instance Binary Undefined
instance Hashable Undefined

instance Arbitrary Undefined where
    arbitrary = pure Undefined

deriveSafeCopySimple 0 'base ''Undefined
