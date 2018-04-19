-- | Utility to generate a random block using an Arbitrary instance.

module Pos.Arbitrary.Block.Generate
    ( generateMainBlock
    , generateMainBlockWithConfiguration
    ) where

import           Universum

import           Test.QuickCheck (arbitrary)
import qualified Test.QuickCheck.Gen as QC
import qualified Test.QuickCheck.Random as QC

import           Pos.Core (MainBlock, HasProtocolConstants, HasProtocolMagic,
                           HasGenesisHash, ProtocolConstants, ProtocolMagic)
-- Also brings in the 'Arbitrary' instance for 'MainBlock'.
import           Pos.Arbitrary.Block (genMainBlock)

-- | Use 'Arbitrary' instances to generate a 'MainBlock'.
-- These require magical configurations.
generateMainBlockWithConfiguration
    :: ( HasProtocolConstants
       , HasProtocolMagic
       , HasGenesisHash
       )
    => Int -- ^ Seed for random generator.
    -> Int -- ^ Size of the generated value (see QuickCheck docs).
    -> MainBlock
generateMainBlockWithConfiguration genSeed = QC.unGen arbitrary qcGen
  where
    qcGen = QC.mkQCGen genSeed

-- | Get some arbitrary (probably invalid) 'MainBlock'. The previous header
-- hash and difficulty, body, etc. are all chosen at random.
generateMainBlock
    :: ( )
    => ProtocolMagic
    -> ProtocolConstants
    -> Int
    -> Int
    -> MainBlock
generateMainBlock pm pc genSeed = QC.unGen generator qcGen
  where
    qcGen = QC.mkQCGen genSeed
    generator = do
        prevHash <- arbitrary
        difficulty <- arbitrary
        genMainBlock pm pc prevHash difficulty
