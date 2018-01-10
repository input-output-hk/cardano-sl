-- | Utility to generate a random block using an Arbitrary instance.

module Pos.Block.Util.Generate
    ( generateBlock
    ) where

import           Universum

import           Test.QuickCheck (arbitrary)
import qualified Test.QuickCheck.Gen as QC
import qualified Test.QuickCheck.Random as QC

import           Pos.Core (MainBlock, HasConfiguration)
import qualified Pos.Arbitrary.Block ()

-- The arbitrary instances requires configuration, unfortunately.
-- That's because it does verification. Yes, indeed, the arbitrary instance
-- does verification, which uses a protocol magic, epoch slot data, etc. etc.


generateBlock
    :: ( HasConfiguration )
    => Int -- ^ Seed for random generator.
    -> Int -- ^ Size of the generated value (see QuickCheck docs).
    -> MainBlock
generateBlock genSeed = QC.unGen arbitrary qcGen
  where
    qcGen = QC.mkQCGen genSeed
