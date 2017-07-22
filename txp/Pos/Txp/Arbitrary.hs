{-# LANGUAGE TemplateHaskell #-}

-- | `Arbitrary` instances for Txp types

module Pos.Txp.Arbitrary
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

import           Pos.Binary.Class                  (Bi, Raw)
import           Pos.Binary.Txp.Core               ()
import           Pos.Core.Address                  (makePubKeyAddress)
import           Pos.Core.Arbitrary                ()
import           Pos.Core.Types                    (Coin)
import           Pos.Crypto                        (Hash, SecretKey, SignTag (SignTx),
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
buildProperTx triplesList (inCoin, outCoin) =
    txList <&> \(tx, txIn, fromSk, txOutput) ->
        ( (tx, makeNullDistribution tx)
        , txIn
        , TxOutAux txOutput []
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
           , TxIn (hash txToBeSpent) 0
           , fromSk
           , makeTxOutput toSk outC )
    -- why is it called txList? I've no idea what's going on here (@neongreen)
    txList = fmap fun triplesList
    newDistr = TxDistribution (NE.fromList $ replicate (length txList) [])
    newDistrHash = hash newDistr
    newTx = fromMaybe (error "buildProperTx: can't create tx") $
            mkTx ins outs def
    newTxHash = hash newTx
    ins  = fmap (view _2) txList
    outs = fmap (view _4) txList
    mkWitness fromSk = PkWitness
        { twKey = toPublic fromSk
        , twSig = sign SignTx fromSk TxSigData {
                      txSigTxHash = newTxHash,
                      txSigTxDistrHash = newDistrHash } }
    makeNullDistribution tx =
        TxDistribution (NE.fromList $ replicate (length (_txOutputs tx)) [])
    makeTxOutput s c = TxOut (makePubKeyAddress $ toPublic s) c

-- | Well-formed transaction 'Tx'.
--
-- TODO: this type is hard to use and should be rewritten as a record
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
    shrink = const []  -- used to be “genericShrink”, but shrinking is broken
                       -- because naive shrinking may turn a good transaction
                       -- into a bad one (by setting one of outputs to 0, for
                       -- instance)

instance Arbitrary SmallGoodTx where
    arbitrary = SmallGoodTx <$> makeSmall arbitrary
    shrink = const []  -- genericShrink

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
