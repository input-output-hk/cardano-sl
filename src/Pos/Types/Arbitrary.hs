{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | `Arbitrary` instances for core types for using in tests and benchmarks

module Pos.Types.Arbitrary () where

import           Data.DeriveTH              (derive, makeArbitrary)
import           Pos.Constants              (epochSlots)
import           Pos.Crypto                 (deterministicVssKeyGen, sign, toVssPublicKey)
import           Pos.Types.Mpc              (genCommitmentAndOpening)
import           Pos.Types.Types            (Address (..), ChainDifficulty (..),
                                             Coin (..), Commitment (..), EpochIndex (..),
                                             FtsSeed (..), LocalSlotIndex (..),
                                             MpcProof (..), Opening (..), SlotId (..),
                                             Tx (..), TxIn (..), TxOut (..))
import           System.Random              (Random)
import           Test.QuickCheck            (Arbitrary (..), choose, elements)
import           Universum

import           Pos.Crypto.Arbitrary       ()
import           Pos.Types.Arbitrary.Unsafe ()
import           Pos.Util.Arbitrary         (Nonrepeating (..), sublistN, unsafeMakePool)

----------------------------------------------------------------------------
-- Commitments and openings
----------------------------------------------------------------------------

data CommitmentOpening = CommitmentOpening
    { coCommitment :: !Commitment
    , coOpening    :: !Opening
    }

-- | Generate 50 commitment/opening pairs in advance
-- (see `Pos.Crypto.Arbitrary` for explanations)
commitmentsAndOpenings :: [CommitmentOpening]
commitmentsAndOpenings =
    map (uncurry CommitmentOpening) $
    unsafeMakePool "[generating Commitments and Openings for tests...]" 50 $
    genCommitmentAndOpening
        1
        [toVssPublicKey $ deterministicVssKeyGen "aaaaaaaaaaaaaaaaaaaaaassss"]
{-# NOINLINE commitmentsAndOpenings #-}

instance Arbitrary CommitmentOpening where
    arbitrary = elements commitmentsAndOpenings

instance Nonrepeating CommitmentOpening where
    nonrepeating n = sublistN n commitmentsAndOpenings

instance Arbitrary Commitment where
    arbitrary = coCommitment <$> arbitrary

instance Arbitrary Opening where
    arbitrary = coOpening <$> arbitrary

----------------------------------------------------------------------------
-- Arbitrary core types
----------------------------------------------------------------------------

deriving instance Arbitrary Coin
deriving instance Arbitrary Address
deriving instance Arbitrary FtsSeed
deriving instance Arbitrary ChainDifficulty

derive makeArbitrary ''SlotId
derive makeArbitrary ''TxOut
derive makeArbitrary ''Tx
derive makeArbitrary ''MpcProof

maxReasonableEpoch :: Integral a => a
maxReasonableEpoch = 5 * 1000 * 1000 * 1000 * 1000  -- 5 * 10^12, because why not

deriving instance Random EpochIndex

instance Arbitrary EpochIndex where
    arbitrary = choose (0, maxReasonableEpoch)

deriving instance Random LocalSlotIndex

instance Arbitrary LocalSlotIndex where
    arbitrary = choose (0, epochSlots - 1)

instance Arbitrary TxIn where
    arbitrary = do
        txId <- arbitrary
        txIdx <- arbitrary
        sk <- arbitrary
        let signature = sign sk (txId, txIdx, [])
        return $ TxIn txId txIdx signature
