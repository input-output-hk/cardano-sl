-- | Wrappers around Test.QuickCheck that use 'Buildable' instead of 'Show'
--
-- Intended as a drop-in replacement for "Test.QuickCheck" (but only for the
-- test drivers; the properties themselves should just use "Test.QuickCheck").
module Util.Buildable.QuickCheck (
    -- * Wrappers
    forAll
    -- * Re-exports
  , QC.Property
  , QC.Gen
  , QC.conjoin
  , QC.choose
  , QC.generate
  , QC.arbitrary
  ) where

import qualified Test.QuickCheck as QC
import           Universum

import           Util.Buildable

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

forAll :: (Buildable a, QC.Testable prop)
       => QC.Gen a -> (a -> prop) -> QC.Property
forAll gen f = QC.forAll (STB <$> gen) (f . unSTB)
