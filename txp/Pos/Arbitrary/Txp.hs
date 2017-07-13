{-# LANGUAGE TemplateHaskell #-}

-- | `Arbitrary` instances for Txp types

module Pos.Arbitrary.Txp
       ( BadSigsTx (..)
       , GoodTx (..)
       , goodTxToTxAux
       , SmallBadSigsTx (..)
       , SmallGoodTx (..)
       , SmallTxPayload (..)
       ) where

import           Universum

import           Data.Default                      (Default (def))
import           Data.List.NonEmpty                ((<|))
import qualified Data.List.NonEmpty                as NE
import qualified Data.Vector                       as V
import           Test.QuickCheck                   (Arbitrary (..), Gen, choose, listOf,
                                                    oneof, scale)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Core                ()
import           Pos.Binary.Class                  (Bi, Raw)
import           Pos.Binary.Txp.Core               ()
import           Pos.Core.Address                  (makePubKeyAddress)
import           Pos.Core.Types                    (Coin)
import           Pos.Crypto                        (Hash, SecretKey, SignTag (SignTxIn),
                                                    hash, sign, toPublic)
import           Pos.Data.Attributes               (mkAttributes)
import           Pos.Merkle                        (MerkleNode (..), MerkleRoot (..),
                                                    MerkleTree, mkMerkleTree)
import           Pos.Txp.Core.Types                (Tx (..), TxAux (..),
                                                    TxDistribution (..), TxIn (..),
                                                    TxInWitness (..), TxOut (..),
                                                    TxOutAux (..), TxPayload (..),
                                                    TxProof (..), TxSigData (..), mkTx,
                                                    mkTxPayload)
import           Pos.Util.Arbitrary                (makeSmall)

----------------------------------------------------------------------------
-- Arbitrary txp types
----------------------------------------------------------------------------

instance Arbitrary TxOut where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxOutAux where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxSigData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxInWitness where
    arbitrary = oneof [
        PkWitness <$> arbitrary <*> arbitrary,
        -- this can generate a redeemer script where a validator script is
        -- needed and vice-versa, but it doesn't matter
        ScriptWitness <$> arbitrary <*> arbitrary,
        RedeemWitness <$> arbitrary <*> arbitrary,
        UnknownWitnessType <$> choose (3, 255) <*> scale (min 150) arbitrary ]
    shrink = \case
        UnknownWitnessType n a -> UnknownWitnessType n <$> shrink a
        ScriptWitness a b -> uncurry ScriptWitness <$> shrink (a, b)
        _ -> []

instance Arbitrary TxDistribution where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxIn where
    arbitrary = genericArbitrary
    shrink = genericShrink


-- | Arbitrary transactions generated from this instance will only be valid
-- with regards to 'mxTx'
instance Arbitrary Tx where
    arbitrary =
        mkTx <$> arbitrary <*> arbitrary <*>
        pure (mkAttributes ()) <&> \case
            Left err -> error $ "Arbitrary Tx: " <> err
            Right res -> res
    shrink = genericShrink

-- | Type used to generate valid ('verifyTx')
-- transactions and accompanying input information.
-- It's not entirely general because it only generates transactions whose
-- outputs are in the same number as its inputs in a one-to-one correspondence.
--
-- The GoodTx type is a nonempty list of quadruples. It contains
-- previous transaction input came from, the input itself, output and
-- witness for the input. Number of inputs is equal to number of outputs.
--
-- The OverflowTx type is the same as GoodTx, except its values, both for
-- inputs as well as outputs, are very close to maxBound :: Coin so as to cause
-- overflow in the Coin type if they are summed.
--
-- The BadSigTx type is also the same as GoodTx, with the difference that all
-- signatures in the transaction's inputs have been replaced with a bogus one.

buildProperTx
    :: NonEmpty (Tx, SecretKey, SecretKey, Coin)
    -> (Coin -> Coin, Coin -> Coin)
    -> NonEmpty ((Tx, TxDistribution), TxIn, TxOutAux, TxInWitness)
buildProperTx triplesList (inCoin, outCoin) = fmap newTx txList
  where
    fun (UnsafeTx txIn txOut _, fromSk, toSk, c) =
        let inC = inCoin c
            outC = outCoin c
            txToBeSpent =
                UnsafeTx
                    txIn
                    ((makeTxOutput fromSk inC) <| txOut)
                    (mkAttributes ())
        in (txToBeSpent, fromSk, makeTxOutput toSk outC)
    -- why is it called txList? I've no idea what's going on here (@neongreen)
    txList = fmap fun triplesList
    txOutsHash = hash $ fmap (view _3) txList
    distrHash = hash (TxDistribution (NE.fromList $ replicate (length txList) []))
    makeNullDistribution tx =
        TxDistribution (NE.fromList $ replicate (length (_txOutputs tx)) [])
    newTx (tx, fromSk, txOutput) =
        let txHash = hash tx
            txIn = TxIn txHash 0
            witness =
                PkWitness
                { twKey = toPublic fromSk
                , twSig = sign SignTxIn fromSk TxSigData{
                             txSigInput = txIn,
                             txSigOutsHash = txOutsHash,
                             txSigDistrHash = distrHash }
                }
        in ((tx, makeNullDistribution tx), txIn, (TxOutAux txOutput []), witness)
    makeTxOutput s c = TxOut (makePubKeyAddress $ toPublic s) c

-- | Well-formed transaction 'Tx'.
newtype GoodTx = GoodTx
    { getGoodTx :: NonEmpty ((Tx, TxDistribution), TxIn, TxOutAux, TxInWitness)
    } deriving (Generic, Show)

newtype SmallGoodTx = SmallGoodTx { getSmallGoodTx :: GoodTx } deriving (Generic, Show)

goodTxToTxAux :: GoodTx -> TxAux
goodTxToTxAux (GoodTx l) = TxAux tx witness distr
  where
    tx = fromMaybe (error "goodTxToTxAux created malformed tx") $
         mkTx (map (view _2) l) (map (toaOut . view _3) l) def
    witness = V.fromList $ NE.toList $ map (view _4) l
    distr = TxDistribution $ map (toaDistr . view _3) l

instance Arbitrary GoodTx where
    arbitrary =
        GoodTx <$> (buildProperTx <$> arbitrary <*> pure (identity, identity))
    shrink = genericShrink

instance Arbitrary SmallGoodTx where
    arbitrary = SmallGoodTx <$> makeSmall arbitrary
    shrink = genericShrink

-- | Ill-formed 'Tx' with bad signatures.
newtype BadSigsTx = BadSigsTx
    { getBadSigsTx :: NonEmpty ((Tx, TxDistribution), TxIn, TxOutAux, TxInWitness)
    } deriving (Generic, Show)

newtype SmallBadSigsTx =
    SmallBadSigsTx BadSigsTx
    deriving (Generic, Show)

instance Arbitrary BadSigsTx where
    arbitrary = BadSigsTx <$> do
        goodTxList <- getGoodTx <$> arbitrary
        badSig <- arbitrary
        return $ map (set _4 badSig) goodTxList
    shrink = genericShrink

instance Arbitrary SmallBadSigsTx where
    arbitrary = SmallBadSigsTx <$> makeSmall arbitrary
    shrink = genericShrink

instance Arbitrary (MerkleRoot Tx) where
    arbitrary = MerkleRoot <$> (arbitrary @(Hash Raw))
    shrink = genericShrink

instance Arbitrary (MerkleNode Tx) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (MerkleTree Tx) where
    arbitrary = mkMerkleTree <$> arbitrary
    shrink = genericShrink

instance Arbitrary TxProof where
    arbitrary = makeSmall genericArbitrary
    shrink = genericShrink

instance Arbitrary TxAux where
    arbitrary = genericArbitrary
    shrink = genericShrink

----------------------------------------------------------------------------
-- Utilities used in 'Pos.Block.Arbitrary'
----------------------------------------------------------------------------

txOutDistGen :: Gen [TxAux]
txOutDistGen =
    listOf $ do
        txInW <- arbitrary
        txIns <- arbitrary
        (txOuts, txDist) <- second TxDistribution . NE.unzip <$> arbitrary
        let tx =
                either
                    (error . mappend "failed to create tx in txOutDistGen: ")
                    identity $
                mkTx txIns txOuts (mkAttributes ())
        return $ TxAux tx (txInW) txDist

instance Arbitrary TxPayload where
    arbitrary =
        fromMaybe (error "arbitrary@TxPayload: mkTxPayload failed") .
        mkTxPayload <$>
        txOutDistGen
    shrink = genericShrink

newtype SmallTxPayload =
    SmallTxPayload TxPayload
    deriving (Generic, Show, Eq, Bi)

instance Arbitrary SmallTxPayload where
    arbitrary = SmallTxPayload <$> makeSmall arbitrary
    shrink = genericShrink
