{-# LANGUAGE FlexibleContexts #-}

-- | Utility to generate a random block using an Arbitrary instance.

module Test.Pos.Block.Arbitrary.Generate
    ( generateMainBlock
    , generateMainBlockWithConfiguration
    ) where

import           Test.QuickCheck (arbitrary)
import qualified Test.QuickCheck.Gen as QC
import qualified Test.QuickCheck.Random as QC

import           Pos.Core (HasGenesisHash, MainBlock, ProtocolMagic)

-- Also brings in the 'Arbitrary' instance for 'MainBlock'.
import           Test.Pos.Block.Arbitrary (genMainBlock)

-- | Use 'Arbitrary' instances to generate a 'MainBlock'.
-- These require magical configurations.
generateMainBlockWithConfiguration
    :: HasGenesisHash
    => Int -- ^ Seed for random generator.
    -> Int -- ^ Size of the generated value (see QuickCheck docs).
    -> MainBlock
generateMainBlockWithConfiguration genSeed = QC.unGen arbitrary qcGen
  where
    qcGen = QC.mkQCGen genSeed

-- | Get some arbitrary (probably invalid) 'MainBlock'. The previous header
-- hash and difficulty, body, etc. are all chosen at random.
generateMainBlock
    :: ProtocolMagic
    -> Int
    -> Int
    -> MainBlock
generateMainBlock pm genSeed = QC.unGen generator qcGen
  where
    qcGen = QC.mkQCGen genSeed
    generator = do
        prevHash <- arbitrary
        difficulty <- arbitrary
        genMainBlock pm prevHash difficulty
