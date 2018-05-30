{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | Chronological sequences.

module Test.Pos.Util.Chrono
       (
       ) where

import           Pos.Util.Chrono

import           Test.QuickCheck (Arbitrary)

deriving instance Arbitrary (f a) => Arbitrary (NewestFirst f a)
deriving instance Arbitrary (f a) => Arbitrary (OldestFirst f a)
