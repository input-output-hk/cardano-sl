-- | `Arbitrary` instances for core types for using in tests and benchmarks

module Pos.Types.Arbitrary
       ( BadSigsTx (..)
       , CoinPairOverflowSum (..)
       , CoinPairOverflowSub (..)
       , CoinPairOverflowMul (..)
       , DoubleInZeroToOneRange (..)
       , IntegerToCoinNoOverflow (..)
       , IntegerToCoinOverflow (..)
       , GoodTx (..)
       , goodTxToTxAux
       , LessThanZeroOrMoreThanOne (..)
       , SafeCoinPairMul (..)
       , SafeCoinPairSum (..)
       , SafeCoinPairSub (..)
       , SafeWord (..)
       , SmallBadSigsTx (..)
       , SmallHashMap (..)
       , SmallGoodTx (..)
       ) where

import           Universum

import           Data.Default              (def)
import           Data.DeriveTH             (derive, makeArbitrary)
import           Data.List.NonEmpty        ((<|))
import qualified Data.List.NonEmpty        as NE
import qualified Data.Vector               as V
import           Test.QuickCheck           (Arbitrary (..), choose, oneof, scale)
import           Test.QuickCheck.Instances ()

import           Pos.Binary.Class          (Raw)
import           Pos.Binary.Core           ()
import           Pos.Binary.Crypto         ()
import           Pos.Binary.Txp            ()
import           Pos.Core.Address          (makePubKeyAddress)
import           Pos.Core.Arbitrary        (CoinPairOverflowMul (..),
                                            CoinPairOverflowSub (..),
                                            CoinPairOverflowSum (..),
                                            DoubleInZeroToOneRange (..),
                                            IntegerToCoinNoOverflow (..),
                                            IntegerToCoinOverflow (..),
                                            LessThanZeroOrMoreThanOne (..),
                                            SafeCoinPairMul (..), SafeCoinPairSub (..),
                                            SafeCoinPairSum (..), SafeWord (..),
                                            SmallHashMap (..))
import           Pos.Core.Types            (Coin)
import           Pos.Crypto                (Hash, SecretKey, SignTag (SignTxIn), hash,
                                            sign, toPublic)
import           Pos.Crypto.Arbitrary      ()
import           Pos.Data.Attributes       (mkAttributes)
import           Pos.Merkle                (MerkleRoot (..), MerkleTree, mkMerkleTree)
import           Pos.Txp.Core.Types        (Tx (..), TxAux (..), TxDistribution (..),
                                            TxIn (..), TxInWitness (..), TxOut (..),
                                            TxOutAux (..), TxProof (..), TxSigData (..),
                                            mkTx)
import           Pos.Util                  (makeSmall)

----------------------------------------------------------------------------
-- Arbitrary core types
----------------------------------------------------------------------------

-- It conflicts with core arbitrary
--instance Arbitrary Script where
--    arbitrary = elements [intValidator, goodIntRedeemer, badIntRedeemer]

derive makeArbitrary ''TxOut
derive makeArbitrary ''TxOutAux
derive makeArbitrary ''TxSigData

instance Arbitrary TxInWitness where
    arbitrary = oneof [
        PkWitness <$> arbitrary <*> arbitrary,
        -- this can generate a redeemer script where a validator script is
        -- needed and vice-versa, but it doesn't matter
        ScriptWitness <$> arbitrary <*> arbitrary,
        RedeemWitness <$> arbitrary <*> arbitrary,
        UnknownWitnessType <$> choose (3, 255) <*> scale (min 150) arbitrary ]

derive makeArbitrary ''TxDistribution
derive makeArbitrary ''TxIn

-- | Arbitrary transactions generated from this instance will only be valid
-- with regards to 'mxTx'
instance Arbitrary Tx where
    arbitrary =
        mkTx <$> arbitrary <*> arbitrary <*>
        pure (mkAttributes ()) <&> \case
            Left err -> error $ "Arbitrary Tx: " <> err
            Right res -> res

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
    } deriving (Show)

newtype SmallGoodTx = SmallGoodTx { getSmallGoodTx :: GoodTx } deriving (Show)

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

instance Arbitrary SmallGoodTx where
    arbitrary = SmallGoodTx <$> makeSmall arbitrary

-- | Ill-formed 'Tx' with bad signatures.
newtype BadSigsTx = BadSigsTx
    { getBadSigsTx :: NonEmpty ((Tx, TxDistribution), TxIn, TxOutAux, TxInWitness)
    } deriving (Show)

newtype SmallBadSigsTx =
    SmallBadSigsTx BadSigsTx
    deriving Show

instance Arbitrary BadSigsTx where
    arbitrary = BadSigsTx <$> do
        goodTxList <- getGoodTx <$> arbitrary
        badSig <- arbitrary
        return $ map (set _4 badSig) goodTxList

instance Arbitrary SmallBadSigsTx where
    arbitrary = SmallBadSigsTx <$> makeSmall arbitrary

instance Arbitrary (MerkleRoot Tx) where
    arbitrary = MerkleRoot <$> (arbitrary @(Hash Raw))

instance Arbitrary (MerkleTree Tx) where
    arbitrary = mkMerkleTree <$> arbitrary

instance Arbitrary TxProof where
    arbitrary = TxProof <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
