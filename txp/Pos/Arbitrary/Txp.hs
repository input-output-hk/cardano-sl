{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | `Arbitrary` instances for Txp types

module Pos.Arbitrary.Txp
       ( BadSigsTx (..)
       , DoubleInputTx (..)
       , GoodTx (..)
       , goodTxToTxAux

       -- | Standalone generators.
       , genTx
       , genTxIn
       , genTxInWitness
       , genTxOutDist
       , genTxPayload
       ) where

import           Universum

import           Data.Default (Default (def))
import           Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import           Test.QuickCheck (Arbitrary (..), Gen, choose, listOf, oneof, scale)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Core ()
import           Pos.Binary.Class (Raw)
import           Pos.Binary.Core ()
import           Pos.Core.Common (Coin, IsBootstrapEraAddr (..), makePubKeyAddress)
import           Pos.Core.Txp (Tx (..), TxAux (..), TxIn (..), TxInWitness (..), TxOut (..),
                               TxOutAux (..), TxPayload (..), TxProof (..), TxSigData (..),
                               mkTxPayload)
import           Pos.Crypto (Hash, ProtocolMagic, SecretKey, SignTag (SignTx), hash, sign, toPublic)
import           Pos.Crypto.Configuration (HasProtocolMagic, protocolMagic)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.Merkle (MerkleNode (..), MerkleRoot (..))

import           Test.Pos.Crypto.Arbitrary (genRedeemSignature, genSignature)

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

-- | Generator for a 'TxInWitness'. 'ProtocolMagic' is needed because it
-- contains signatures.
genTxInWitness :: ProtocolMagic -> Gen TxInWitness
genTxInWitness pm = oneof
    [ PkWitness <$> arbitrary <*> genSignature pm arbitrary
      -- this can generate a redeemer script where a validator script is
      -- needed and vice-versa, but it doesn't matter
    , ScriptWitness <$> arbitrary <*> arbitrary
    , RedeemWitness <$> arbitrary <*> genRedeemSignature pm arbitrary
    , UnknownWitnessType <$> choose (3, 255) <*> scale (min 150) arbitrary
    ]

instance HasProtocolMagic => Arbitrary TxInWitness where
    arbitrary = genTxInWitness protocolMagic
    shrink = \case
        UnknownWitnessType n a -> UnknownWitnessType n <$> shrink a
        ScriptWitness a b -> uncurry ScriptWitness <$> shrink (a, b)
        _ -> []

genTxIn :: Gen TxIn
genTxIn = oneof
    [ TxInUtxo <$> arbitrary <*> arbitrary
    , TxInUnknown <$> choose (1, 255) <*> scale (min 150) arbitrary
    ]

instance Arbitrary TxIn where
    arbitrary = genTxIn
    shrink = genericShrink

genTx :: Gen Tx
genTx = UnsafeTx <$> arbitrary <*> arbitrary <*> pure (mkAttributes ())

-- | Arbitrary transactions generated from this instance will only be valid
-- with regards to 'mxTx'
instance Arbitrary Tx where
    arbitrary = genTx
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
    :: ( )
    => ProtocolMagic
    -> NonEmpty (Tx, SecretKey, SecretKey, Coin)
    -> (Coin -> Coin, Coin -> Coin)
    -> NonEmpty (Tx, TxIn, TxOutAux, TxInWitness)
buildProperTx pm inputList (inCoin, outCoin) =
    txList <&> \(tx, txIn, fromSk, txOutput) ->
        ( tx
        , txIn
        , TxOutAux txOutput
        , mkWitness fromSk
        )
  where
    fun (UnsafeTx txIn txOut _, fromSk, toSk, c) =
        let inC = inCoin c
            outC = outCoin c
            txToBeSpent =
                UnsafeTx
                    txIn
                    ((makeTxOutput fromSk inC) <| txOut)
                    (mkAttributes ())
        in ( txToBeSpent
           , TxInUtxo (hash txToBeSpent) 0
           , fromSk
           , makeTxOutput toSk outC )
    -- why is it called txList? I've no idea what's going on here (@neongreen)
    txList = fmap fun inputList
    newTx = UnsafeTx ins outs def
    newTxHash = hash newTx
    ins  = fmap (view _2) txList
    outs = fmap (view _4) txList
    mkWitness fromSk = PkWitness
        { twKey = toPublic fromSk
        , twSig = sign pm SignTx fromSk TxSigData {
                      txSigTxHash = newTxHash } }
    makeTxOutput s c =
        TxOut (makePubKeyAddress (IsBootstrapEraAddr True) $ toPublic s) c

-- | Well-formed transaction 'Tx'.
--
-- TODO: this type is hard to use and should be rewritten as a record
newtype GoodTx = GoodTx
    { getGoodTx :: NonEmpty (Tx, TxIn, TxOutAux, TxInWitness)
    } deriving (Generic, Show)

goodTxToTxAux :: GoodTx -> TxAux
goodTxToTxAux (GoodTx l) = TxAux tx witness
  where
    tx = UnsafeTx (map (view _2) l) (map (toaOut . view _3) l) def
    witness = V.fromList $ NE.toList $ map (view _4) l

instance HasProtocolMagic => Arbitrary GoodTx where
    arbitrary =
        GoodTx <$> (buildProperTx protocolMagic <$> arbitrary <*> pure (identity, identity))
    shrink = const []  -- used to be “genericShrink”, but shrinking is broken
                       -- because naive shrinking may turn a good transaction
                       -- into a bad one (by setting one of outputs to 0, for
                       -- instance)

-- | Ill-formed 'Tx' with bad signatures.
newtype BadSigsTx = BadSigsTx
    { getBadSigsTx :: NonEmpty (Tx, TxIn, TxOutAux, TxInWitness)
    } deriving (Generic, Show)

-- | Ill-formed 'Tx' that spends an input twice.
newtype DoubleInputTx = DoubleInputTx
    { getDoubleInputTx :: NonEmpty (Tx, TxIn, TxOutAux, TxInWitness)
    } deriving (Generic, Show)

instance HasProtocolMagic => Arbitrary BadSigsTx where
    arbitrary = BadSigsTx <$> do
        goodTxList <- getGoodTx <$> arbitrary
        badSig <- arbitrary
        return $ map (set _4 badSig) goodTxList
    shrink = genericShrink

instance HasProtocolMagic => Arbitrary DoubleInputTx where
    arbitrary = DoubleInputTx <$> do
        inputs <- arbitrary
        pure $ buildProperTx protocolMagic
                             (NE.cons (NE.head inputs) inputs)
                             (identity, identity)
    shrink = const []

instance Arbitrary (MerkleRoot Tx) where
    arbitrary = MerkleRoot <$> (arbitrary @(Hash Raw))
    shrink = genericShrink

instance Arbitrary (MerkleNode Tx) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxProof where
    arbitrary = genericArbitrary
    shrink = genericShrink

genTxAux :: ProtocolMagic -> Gen TxAux
genTxAux pm = TxAux <$> genTx <*> (V.fromList <$> listOf (genTxInWitness pm))

instance HasProtocolMagic => Arbitrary TxAux where
    arbitrary = genTxAux protocolMagic
    shrink = genericShrink

----------------------------------------------------------------------------
-- Utilities used in 'Pos.Block.Arbitrary'
----------------------------------------------------------------------------

genTxOutDist :: ProtocolMagic -> Gen [TxAux]
genTxOutDist pm =
    listOf $ do
        txInW <- V.fromList <$> listOf (genTxInWitness pm)
        txIns <- arbitrary
        txOuts <- arbitrary
        let tx = UnsafeTx txIns txOuts (mkAttributes ())
        return $ TxAux tx txInW

genTxPayload :: ProtocolMagic -> Gen TxPayload
genTxPayload pm = mkTxPayload <$> genTxOutDist pm

instance HasProtocolMagic => Arbitrary TxPayload where
    arbitrary = genTxPayload protocolMagic
    shrink = genericShrink
