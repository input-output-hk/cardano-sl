-- | Utility to generate a random block using an Arbitrary instance.

module Pos.Arbitrary.Block.Generate
    ( generateBlock
    ) where

import           Universum

import           Test.QuickCheck (Arbitrary, arbitrary)
import qualified Test.QuickCheck.Gen as QC
import qualified Test.QuickCheck.Random as QC

import qualified Pos.Arbitrary.Block ()
import qualified Pos.Arbitrary.Ssc ()
import           Pos.Core (MainBlock)
import           Pos.Crypto (ProtocolMagic)

-- The arbitrary instances requires configuration, unfortunately.
-- That's because it does verification. Yes, indeed, the arbitrary instance
-- does verification, which uses a protocol magic, epoch slot data, etc. etc.


generateBlock
    :: ( Arbitrary ProtocolMagic )
    => Int -- ^ Seed for random generator.
    -> Int -- ^ Size of the generated value (see QuickCheck docs).
    -> MainBlock
generateBlock genSeed = QC.unGen arbitrary qcGen
  where
    qcGen = QC.mkQCGen genSeed
