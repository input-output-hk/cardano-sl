-- | Wrappers around Test.QuickCheck that use 'Buildable' instead of 'Show'
--
-- Intended as a drop-in replacement for "Test.QuickCheck" (but only for the
-- test drivers; the properties themselves should just use "Test.QuickCheck").
module Util.Buildable.QuickCheck (
    -- * Wrappers
    forAll
  , forAllShrink
    -- * Re-exports
  , QC.Property
  , QC.Gen
  , QC.conjoin
  , QC.choose
  ) where

import           Universum

import           Data.Coerce (coerce)
import qualified Test.QuickCheck as QC

import           Util.Buildable

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

forAll :: (Buildable a, QC.Testable prop)
       => QC.Gen a -> (a -> prop) -> QC.Property
forAll gen p = QC.forAll (STB <$> gen) (p . unSTB)

forAllShrink :: (Buildable a, QC.Testable prop)
             => QC.Gen a -> (a -> [a]) -> (a -> prop) -> QC.Property
forAllShrink gen f p = QC.forAllShrink (STB <$> gen) (coerce f) (p . unSTB)
