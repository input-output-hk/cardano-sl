{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | `Arbitrary` instances for Txp types

module Test.Pos.Chain.Txp.Arbitrary
       ( BadSigsTx (..)
       , DoubleInputTx (..)
       , GoodTx (..)
       , goodTxToTxAux

       -- | Standalone generators.
       , genAddBloated
       , genAddrAttribBloated
       , genAddressBloatedTx
       , genTxAttributesBloated
       , genTxOutBloated
       , genTx
       , genTxAux
       , genTxIn
       , genTxInWitness
       , genTxOutDist
       , genTxPayload
       , genGoodTxWithMagic
       ) where

import           Universum

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as CB
import           Data.Default (Default (def))
import           Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Vector as V
import           Test.QuickCheck (Arbitrary (..), Gen, arbitraryUnicodeChar,
                     choose, listOf, oneof, scale, vectorOf)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Pos.Binary.Class (Raw)
import           Pos.Chain.Txp (Tx (..), TxAttributes, TxAux (..), TxIn (..),
                     TxInWitness (..), TxOut (..), TxOutAux (..),
                     TxPayload (..), TxProof (..), TxSigData (..),
                     TxValidationRules (..), mkTxPayload)
import           Pos.Core.Attributes (Attributes, mkAttributes,
                     mkAttributesWithUnparsedFields)
import           Pos.Core.Common (AddrAttributes, Address (..), Coin,
                     IsBootstrapEraAddr (..), makePubKeyAddress)
import           Pos.Core.Merkle (MerkleNode (..), MerkleRoot (..))
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Crypto (Hash, ProtocolMagic, SecretKey, SignTag (SignTx),
                     hash, sign, toPublic)

import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Crypto.Arbitrary (genRedeemSignature, genSignature)
import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)

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

-- This instance avoids the failure of tests that
-- use checkTx but aren't explicitly checking if
-- checkTx is behaving as it should.
instance Arbitrary TxValidationRules where
  arbitrary = do
      e0 <- arbitrary
      e1 <- arbitrary
      let [currentEpoch, cutoffEpoch] = sort [e0, e1]
      pure $ TxValidationRules cutoffEpoch currentEpoch 1 1
  shrink = genericShrink

genAddrAttribBloated :: Gen (Attributes AddrAttributes)
genAddrAttribBloated =
    mkAttributesWithUnparsedFields <$> arbitrary <*> genBloatedUnparsedFields

-- Generates a Tx bloated via its `Attributes AddrAttributes` or `TxAttributes`
genAddressBloatedTx :: Gen Tx
genAddressBloatedTx =
    oneof [ UnsafeTx <$> arbitrary <*> genTxOutBloated <*> pure (mkAttributes ())
          , UnsafeTx <$> arbitrary <*> arbitrary <*> genTxAttributesBloated
          ]

genAddBloated :: Gen Address
genAddBloated = Address <$> arbitrary <*> genAddrAttribBloated <*> arbitrary

genTxAttributesBloated :: Gen TxAttributes
genTxAttributesBloated = mkAttributesWithUnparsedFields () <$> genBloatedUnparsedFields

genTxOutBloated :: Gen (NonEmpty TxOut)
genTxOutBloated = NE.fromList <$> ((: []) <$> (TxOut <$> genAddBloated <*> arbitrary))

genBloatedUnparsedFields :: Gen (M.Map Word8 LBS.ByteString)
genBloatedUnparsedFields = do
    strSize <- choose (128,256)
    str <- vectorOf strSize arbitraryUnicodeChar
    pure $ M.singleton 0 (CB.pack str)

-- | Generator for a 'TxInWitness'. 'ProtocolMagic' is needed because it
-- contains signatures.
genTxInWitness :: ProtocolMagic -> Gen TxInWitness
genTxInWitness pm = oneof
    [ PkWitness <$> arbitrary <*> genSignature pm arbitrary
      -- this can generate a redeemer script where a validator script is
      -- needed and vice-versa, but it doesn't matter
    , RedeemWitness <$> arbitrary <*> genRedeemSignature pm arbitrary
    , UnknownWitnessType <$> choose (3, 255) <*> scale (min 150) arbitrary
    ]

instance Arbitrary TxInWitness where
    arbitrary = genTxInWitness dummyProtocolMagic
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
    mkWitness fromSk =
        PkWitness (toPublic fromSk) (sign pm SignTx fromSk $ TxSigData newTxHash)
    makeTxOutput s c =
        TxOut (makePubKeyAddress (makeNetworkMagic pm) (IsBootstrapEraAddr True) $ toPublic s) c

-- | Well-formed transaction 'Tx'.
--
-- TODO: this type is hard to use and should be rewritten as a record
newtype GoodTx = GoodTx
    { getGoodTx :: NonEmpty (Tx, TxIn, TxOutAux, TxInWitness)
    } deriving (Generic, Show)

genGoodTxWithMagic :: ProtocolMagic -> Gen GoodTx
genGoodTxWithMagic pm =
        GoodTx <$> (buildProperTx pm
                        <$> arbitrary
                        <*> pure (identity, identity))

goodTxToTxAux :: GoodTx -> TxAux
goodTxToTxAux (GoodTx l) = TxAux tx witness
  where
    tx = UnsafeTx (map (view _2) l) (map (toaOut . view _3) l) def
    witness = V.fromList $ NE.toList $ map (view _4) l

instance Arbitrary GoodTx where
    arbitrary =
        GoodTx <$> (buildProperTx dummyProtocolMagic <$> arbitrary <*> pure (identity, identity))
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

instance Arbitrary BadSigsTx where
    arbitrary = BadSigsTx <$> do
        goodTxList <- getGoodTx <$> arbitrary
        badSig <- arbitrary
        return $ map (set _4 badSig) goodTxList
    shrink = genericShrink

instance Arbitrary DoubleInputTx where
    arbitrary = DoubleInputTx <$> do
        inputs <- arbitrary
        pure $ buildProperTx dummyProtocolMagic
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

instance Arbitrary TxAux where
    arbitrary = genTxAux dummyProtocolMagic
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

instance Arbitrary TxPayload where
    arbitrary = genTxPayload dummyProtocolMagic
    shrink = genericShrink
