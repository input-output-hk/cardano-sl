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

-- Goal now: to be able to generate an arbitrary block *without* using
-- any configurations.
--
-- Would also be nice to get a block chain as an unfold:
--
--   unfoldr mkBlockWithParent initialBlock :: [Block]
--
-- We already have 'mkMainBlock' and 'mkGenesisBlock' which don't require
-- configurations. It's just daunting, getting values for their parameters:
--
--   SlotLeaders - required for a genesis block
--   ProxySKBlockInfo = required for main block
--
-- Perhaps what we want to start with is indeed a 'SlotLeaders'.
-- Yeah ok, on the one hand we can make a random block using QuickCheck
-- generators. The block may not be valid. That's all we need for now so
-- whatever, just do that.
--
-- So what do we need? Most of the work is in generating an arbitrary
-- body/payloads without the configurations. So let's start there.

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
