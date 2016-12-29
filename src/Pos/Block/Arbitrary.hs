{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Block.Arbitrary () where

import           Test.QuickCheck           (Arbitrary (..), Gen, listOf, oneof)
import           Universum

import           Pos.Binary                (Bi)
import           Pos.Block.Network         as T
import           Pos.Constants             (magic)
import           Pos.Crypto                (Hash)
import           Pos.Data.Attributes       (Attributes (..), mkAttributes)
import           Pos.Merkle                (MerkleRoot (..), MerkleTree, mkMerkleTree)
import           Pos.Ssc.Class.Types       (Ssc (..))
import qualified Pos.Types                 as T
import           Pos.Util                  (Raw, makeSmall)

------------------------------------------------------------------------------------------
-- Arbitrary instances for Blockchain related types
------------------------------------------------------------------------------------------

instance (Arbitrary (SscProof ssc), Bi Raw, Ssc ssc) =>
    Arbitrary (T.BlockSignature ssc) where
    arbitrary = oneof [ T.BlockSignature <$> arbitrary
                      , T.BlockPSignature <$> arbitrary
                      ]

------------------------------------------------------------------------------------------
-- GenesisBlockchain
------------------------------------------------------------------------------------------

instance Ssc ssc => Arbitrary (T.GenesisBlockHeader ssc) where
    arbitrary = T.GenericBlockHeader
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> pure magic

instance Arbitrary (T.BodyProof (T.GenesisBlockchain ssc)) where
    arbitrary = T.GenesisProof <$> arbitrary

instance Arbitrary (T.ConsensusData (T.GenesisBlockchain ssc)) where
    arbitrary = T.GenesisConsensusData
        <$> arbitrary
        <*> arbitrary

instance Arbitrary (T.Body (T.GenesisBlockchain ssc)) where
    arbitrary = T.GenesisBody <$> arbitrary

instance Ssc ssc => Arbitrary (T.GenericBlock (T.GenesisBlockchain ssc)) where
    arbitrary = T.GenericBlock
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

------------------------------------------------------------------------------------------
-- MainBlockchain
------------------------------------------------------------------------------------------

instance Bi Raw => Arbitrary (MerkleRoot T.Tx) where
    arbitrary = MerkleRoot <$> (arbitrary :: Gen (Hash Raw))

instance Arbitrary (MerkleTree T.Tx) where
    arbitrary = mkMerkleTree <$> arbitrary

instance (Arbitrary (SscProof ssc), Bi Raw, Ssc ssc) =>
    Arbitrary (T.MainBlockHeader ssc) where
    arbitrary = T.GenericBlockHeader
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> pure magic

instance Arbitrary h => Arbitrary (Attributes h) where
    arbitrary = Attributes
        <$> arbitrary
        <*> arbitrary

instance Arbitrary T.MainExtraHeaderData where
    arbitrary = T.MainExtraHeaderData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary T.MainExtraBodyData where
    arbitrary = T.MainExtraBodyData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

instance (Arbitrary (SscProof ssc), Bi Raw) =>
    Arbitrary (T.BodyProof (T.MainBlockchain ssc)) where
    arbitrary = T.MainProof
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance (Arbitrary (SscProof ssc), Bi Raw, Ssc ssc) =>
    Arbitrary (T.ConsensusData (T.MainBlockchain ssc)) where
    arbitrary = T.MainConsensusData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

-- | In the main blockchain's body, the number of transactions must be the same as the
-- number of transaction witnesses.
--
-- Furthermore, for every transaction in index i of the list, the length of its output
-- list must be the same as the length of the i-th item in the TxDistribution list.
--
-- Because of this, the Arbitrary instance for Ssc ssc => Body (MainBlockchain ssc)
-- ensures that for every transaction generated, a transaction witness is generated as
-- well, and the lengths of its list of outputs must also be the same as the length of its
-- corresponding TxDistribution item.

txOutDistGen :: Gen [(T.Tx, T.TxDistribution, T.TxWitness)]
txOutDistGen = listOf $ do
    txInW <- arbitrary
    txIns <- arbitrary
    (txOuts, txDist) <- second T.TxDistribution . unzip <$> arbitrary
    return $ (T.Tx txIns txOuts $ mkAttributes (), txDist, txInW)

instance Arbitrary (SscPayload ssc) => Arbitrary (T.Body (T.MainBlockchain ssc)) where
    arbitrary = makeSmall $ do
        (txList, txDists, txInW) <- unzip3 <$> txOutDistGen
        mpcData <- arbitrary
        return $ T.MainBody (mkMerkleTree txList) txDists txInW mpcData

instance (Arbitrary (SscProof ssc), Arbitrary (SscPayload ssc), Ssc ssc) =>
    Arbitrary (T.GenericBlock (T.MainBlockchain ssc)) where
    arbitrary = T.GenericBlock
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

------------------------------------------------------------------------------------------
-- Block network types
------------------------------------------------------------------------------------------

instance Ssc ssc => Arbitrary (T.MsgGetHeaders ssc) where
    arbitrary = T.MsgGetHeaders
        <$> arbitrary
        <*> arbitrary

instance Ssc ssc => Arbitrary (T.MsgGetBlocks ssc) where
    arbitrary = T.MsgGetBlocks
        <$> arbitrary
        <*> arbitrary

instance (Arbitrary (SscProof ssc), Bi Raw, Ssc ssc) => Arbitrary (T.MsgHeaders ssc) where
    arbitrary = T.MsgHeaders <$> arbitrary

instance (Arbitrary (SscProof ssc), Arbitrary (SscPayload ssc), Ssc ssc) =>
    Arbitrary (T.MsgBlock ssc) where
    arbitrary = T.MsgBlock <$> arbitrary
