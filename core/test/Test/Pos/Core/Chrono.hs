{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | Chronological sequences.

module Test.Pos.Core.Chrono
       (
       ) where

import           Pos.Core.Chrono

import           Test.QuickCheck (Arbitrary)

deriving instance Arbitrary (f a) => Arbitrary (NewestFirst f a)
deriving instance Arbitrary (f a) => Arbitrary (OldestFirst f a)
